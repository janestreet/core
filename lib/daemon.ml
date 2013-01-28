open Std_internal
module Unix = Core_unix

module Thread = Core_thread

let default_umask = 0

let check_threads () =
  (* forking, especially to daemonize, when running multiple threads is tricky, and
     generally a mistake.  It's so bad, and so hard to catch, that we test in two
     different ways *)
  if Thread.threads_have_been_created () then
    failwith
      "Daemon.check_threads: may not be called \
      if any threads have ever been created";
  begin match Thread.num_threads () with
  | None -> ()  (* This is pretty bad, but more likely to be a problem with num_threads *)
  | Some (1 | 2) -> () (* main thread, or main + ticker - both ok *)
  | Some _ ->
    failwith
      "Daemon.check_threads: may not be called if more than 2 threads \
        (hopefully the main thread + ticker thread) are running"
  end;
;;

type do_redirect =
[ `Dev_null
| `File_append of string
| `File_truncate of string
]

type redirect_fds =
[ `Do_not_redirect | do_redirect ]
;;

let dup_null ~skip_regular_files ~mode ~src ~dst =
  match src with
  | `Do_not_redirect -> ()
  | #do_redirect as src ->
    let is_regular () =
      try (Unix.fstat dst).Unix.st_kind = Unix.S_REG
      with Unix.Unix_error (Unix.EBADF, _, _) -> false
    in
    let should_skip = skip_regular_files && is_regular () in
    if not should_skip then begin
      let src = match src with
        | `Dev_null -> Unix.openfile "/dev/null" ~mode:[mode] ~perm:0o777
        | `File_append file -> Unix.openfile file ~mode:[mode; Unix.O_CREAT] ~perm:0o777
        | `File_truncate file ->
          let fd = Unix.openfile file ~mode:[mode; Unix.O_CREAT] ~perm:0o777 in
          Unix.ftruncate fd ~len:Int64.zero;
          fd
      in
      Unix.dup2 ~src ~dst;
      Unix.close src;
    end;
;;

let redirect_stdio_fds ~skip_regular_files ~stdout ~stderr =

  dup_null ~skip_regular_files ~mode:Unix.O_RDONLY ~src:`Dev_null ~dst:Unix.stdin;
  dup_null ~skip_regular_files ~mode:Unix.O_WRONLY ~src:stdout ~dst:Unix.stdout;
  dup_null ~skip_regular_files ~mode:Unix.O_WRONLY ~src:stderr ~dst:Unix.stderr;
;;

let daemonize ?(redirect_stdout=`Dev_null) ?(redirect_stderr=`Dev_null)
    ?(cd = "/") ?umask:(umask_value = default_umask) () =
  check_threads ();
  let fork_no_parent () =
    match Unix.handle_unix_error Unix.fork with
    | `In_the_child -> ()
    | `In_the_parent _ -> exit 0
  in
  (* Fork into the background, parent exits, child continues. *)
  fork_no_parent ();
  (* Become session leader. *)
  ignore (Unix.Terminal_io.setsid ());
  (* Fork again to ensure that we will never regain a controlling terminal. *)
  fork_no_parent ();
  (* Release old working directory. *)
  Unix.chdir cd;
  (* Ensure sensible umask.  Adjust as needed. *)
  ignore (Unix.umask umask_value);
  redirect_stdio_fds ~skip_regular_files:false
    ~stdout:redirect_stdout ~stderr:redirect_stderr;
;;

let fail_wstopped ~pid ~i =
  failwithf "Bug: waitpid on process %i returned WSTOPPED %i, \
    but waitpid not called with WUNTRACED.  This should not happen" i pid ()

let daemonize_wait ?(redirect_stdout=`Dev_null) ?(redirect_stderr=`Dev_null)
    ?(cd = "/") ?umask:(umask_value = default_umask) () =
  check_threads ();
  match Unix.handle_unix_error Unix.fork with
  | `In_the_child ->
    ignore (Unix.Terminal_io.setsid ());
    let read_end, write_end = Unix.pipe () in
    let buf = "done" in
    let len = String.length buf in
    begin match Unix.handle_unix_error Unix.fork with
    | `In_the_child ->
      (* The process that will become the actual daemon. *)
      Unix.close read_end;
      Unix.chdir cd;
      ignore (Unix.umask umask_value);
      (fun () ->
        redirect_stdio_fds ~skip_regular_files:true
          ~stdout:redirect_stdout ~stderr:redirect_stderr;
        let old_sigpipe_behavior = Signal.signal Signal.pipe `Ignore in
        (try ignore (Unix.write write_end ~buf ~pos:0 ~len : int) with _ -> ());
        Signal.set Signal.pipe old_sigpipe_behavior;
        Unix.close write_end
      )
    | `In_the_parent pid ->
      let pid = Pid.to_int pid in
      (* The middle process, after it has forked its child. *)
      Unix.close write_end;
      let rec loop () =
        let wait_result, process_status =
          Caml.UnixLabels.waitpid ~mode:[Caml.UnixLabels.WNOHANG] pid in
        if wait_result = 0 then begin
          match Caml.Unix.select [read_end] [] [] 0.1 with
          | [read_end], [], [] ->
            (* If the child process exits before detaching and the middle process
               happens to be in this call to select, the pipe will be closed and select
               will return a ready file descriptor, but with zero bytes to read.
               In this case, we want to loop back again and call waitpid to obtain
               the correct exit status to propagate on to the outermost parent
               (otherwise we might incorrectly return a success). *)
            if Unix.read read_end ~buf:(String.create len) ~pos:0 ~len > 0 then
              exit 0
            else
              loop ()
          | _, _, _ -> loop ()
        end else
          match process_status with
          | Caml.Unix.WEXITED i | Caml.Unix.WSIGNALED i -> exit i
          | Caml.Unix.WSTOPPED i -> fail_wstopped ~pid ~i
      in loop ()
    end
  | `In_the_parent pid ->
    let pid = Pid.to_int pid in
    match snd (Caml.UnixLabels.waitpid ~mode:[] pid) with
    | Caml.Unix.WEXITED i | Caml.Unix.WSIGNALED i -> exit i
    | Caml.Unix.WSTOPPED i -> fail_wstopped ~pid ~i
;;
