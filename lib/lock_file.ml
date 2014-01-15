open Core_kernel.Std
open Int.Replace_polymorphic_compare let _ = _squelch_unused_module_warning_
module Unix = Core_unix

(* We have reason to believe that lockf doesn't work properly on CIFS mounts.  The idea
   behind requiring both lockf and flock is to prevent programs taking locks on
   network filesystems where they may not be sound.  *)

let flock fd = Unix.flock fd Unix.Flock_command.lock_exclusive

let lockf ?(mode = Unix.F_TLOCK) fd =
  try
    Unix.lockf fd ~mode ~len:Int64.zero;
    true
  with
  | _ -> false

let lock fd =
  (* [lockf] doesn't throw any exceptions, so if an exception is raised from this
     function, it must have come from [flock]. *)
  let flocked = flock fd in
  let lockfed = lockf fd in
  flocked && lockfed

let create
    ?(message = Pid.to_string (Unix.getpid ()))
    ?(close_on_exec = true)
    ?(unlink_on_exit = false)
    path =
  let message = sprintf "%s\n" message in
  (* We use [~perm:0o664] rather than our usual default perms, [0o666], because
     lock files shouldn't rely on the umask to disallow tampering by other. *)
  let fd = Unix.openfile path ~mode:[Unix.O_WRONLY; Unix.O_CREAT] ~perm:0o664 in
  try
    if lock fd then begin
      if close_on_exec then Unix.set_close_on_exec fd;
      if unlink_on_exit then at_exit (fun () -> try Unix.unlink path with _ -> ());
      Unix.ftruncate fd ~len:Int64.zero;
      ignore (Unix.write fd ~buf:message ~pos:0 ~len:(String.length message));
      (* we truncated the file, so we need the region lock back.  We don't really
         understand why/if this call is needed, but experimental evidence indicates that
         we need to do it. *)
      ignore (lockf fd);
      true
    end else begin
      Unix.close fd; (* releases any locks from [flock] and/or [lockf] *)
      false
    end
  with
  | e ->
    Unix.close fd; (* releases any locks from [flock] and/or [lockf] *)
    raise e

let create_exn ?message ?close_on_exec ?unlink_on_exit path =
  if not (create ?message ?close_on_exec ?unlink_on_exit path) then
    failwithf "Lock_file.create_exn '%s' was unable to acquire the lock" path ()

(* no timeout specified = wait indefinitely *)
let repeat_with_timeout ?timeout lockf path =
  match timeout with
  | None ->
    let rec loop () =
      try (lockf path)
      with | _ -> begin
        Unix.sleep 1;
        loop ()
      end
    in
    loop ()
  | Some timeout ->
    let start_time = Time.now () in
    let rec loop () =
      try lockf path
      with
      | e -> begin
        let since_start = Time.abs_diff start_time (Time.now ()) in
        if Time.Span.(since_start > timeout) then
          failwithf "Lock_file: '%s' timed out waiting for existing lock. \
                     Last error was %s" path (Exn.to_string e) ()
        else begin
          Unix.sleep 1;
          loop ()
        end
      end
    in
    loop ()

(* default timeout is to wait indefinitely *)
let blocking_create ?timeout ?message ?close_on_exec ?unlink_on_exit path =
  repeat_with_timeout ?timeout
    (fun path -> create_exn ?message ?close_on_exec ?unlink_on_exit path) path

let is_locked path =
  try
    let fd      = Unix.openfile path ~mode:[Unix.O_RDONLY] ~perm:0o664 in
    let flocked = flock fd in
    let lockfed = lockf fd ~mode:Unix.F_TEST in
    Unix.close fd; (* releases any locks from [flock] and/or [lockf] *)
    if flocked && lockfed then false
    else true
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> false
  | e -> raise e

TEST_MODULE = struct
  let lock_file = Filename.temp_file "lock_file" "unit_test"
  let () = Unix.unlink lock_file
  TEST = create lock_file
  TEST = not (create lock_file)
  TEST = is_locked lock_file

  let nolock_file = Filename.temp_file "nolock_file" "unit_test"
  let () =
    Unix.unlink nolock_file;
    (* Create an empty file. *)
    Unix.close (Unix.openfile nolock_file ~mode:[Unix.O_CREAT; Unix.O_WRONLY])
  TEST =
    (* Check that file exists. *)
    try ignore (Unix.stat nolock_file); true
    with Unix.Unix_error (Unix.ENOENT, _, _) -> false
  TEST = not (is_locked nolock_file)
end

module Nfs = struct
  module Info = struct
    type t = {
      host    : string;
      pid     : Pid.t;
      message : string
    } with sexp
  end

  let lock_path path = path ^ ".nfs_lock"

  let get_info path =
    try
      let contents = In_channel.read_all path in
      Some (Info.t_of_sexp (Sexp.of_string (String.strip contents)))
    with
    | _ -> None
  ;;

  let get_hostname_and_pid path =
    Option.map (get_info path) ~f:(fun info -> (info.Info.host, info.Info.pid))
  ;;

  let get_message path = Option.map (get_info path) ~f:(fun info -> info.Info.message)

  let unlock_safely_exn ~unlock_myself path =
    (* Make sure error messages contain a reference to "lock.nfs_lock", which is the
       actually important file. *)
    let lock_path = lock_path path in
    let error s =
      failwithf
        "Lock_file.Nfs.unlock_safely_exn: unable to unlock %s: %s" lock_path s ()
    in
    match Core_sys.file_exists ~follow_symlinks:false lock_path with
    | `Unknown -> error (sprintf "unable to read %s" lock_path)
    | `No      -> ()
    | `Yes     ->
      match get_hostname_and_pid lock_path with
      | None -> error "lock file doesn't contain hostname and pid"
      | Some (locking_hostname, locking_pid) ->
        let my_pid      = Unix.getpid () in
        let my_hostname = Unix.gethostname () in
        if String.(<>) my_hostname locking_hostname then
          error (sprintf "locked from %s, unlock attempted from %s"
                   locking_hostname my_hostname)
        else
          (* Check if the process is running: sends signal 0 to pid, which should work if
             the process is running and is owned by the user running this code. If the
             process is not owned by the user running this code we should fail to unlock
             either earlier (unable to read the file) or later (unable to remove the
             file). *)
          if (unlock_myself && Pid.(=) locking_pid my_pid)
            || not (Signal.can_send_to locking_pid)
          then
            try
              Unix.unlink path;
              Unix.unlink lock_path
            with | e -> error (Exn.to_string e)
          else
            error (sprintf "locking process (pid %i) still running on %s"
              (Pid.to_int locking_pid) locking_hostname)
  ;;

  (* See mli for more information on the algorithm we use for locking over NFS.  Ensure
     that you understand it before you make any changes here. *)
  let create_exn ?(message = "") path =
    try
      unlock_safely_exn ~unlock_myself:false path;
      let fd = Unix.openfile path ~mode:[Unix.O_WRONLY; Unix.O_CREAT] in
      protect
        ~finally:(fun () -> Unix.close fd)
        ~f:(fun () ->
          Unix.link ~target:path ~link_name:(lock_path path) ();
          Unix.ftruncate fd ~len:0L;
          let info =
            { Info.
              host = Unix.gethostname ();
              pid  = Unix.getpid ();
              message
            }
          in
          (* if this fprintf fails, empty lock file would be left behind, and
             subsequent calls to [Lock_file.Nfs.create_exn] would be unable to
             figure out that it is stale/corrupt and remove it. So we need to
             remove it ourselves *)
          try
            fprintf (Unix.out_channel_of_descr fd) "%s\n%!"
              (Sexp.to_string_hum (Info.sexp_of_t info))
          with | (Sys_error _) as err -> begin
            Unix.unlink path;
            Unix.unlink (lock_path path);
            raise err
          end
        );
      at_exit (fun () -> try unlock_safely_exn ~unlock_myself:true path with _ -> ());
    with
    | e ->
      failwithf "Lock_file.Nfs.create_exn: unable to lock '%s' - %s" path
        (Exn.to_string e) ()
  ;;

  let create ?message path = Or_error.try_with (fun () -> create_exn ?message path)

  (* default timeout is to wait indefinitely *)
  let blocking_create ?timeout ?message path =
    repeat_with_timeout
      ?timeout
      (fun path -> create_exn ?message path)
      path
  ;;

  let critical_section ?message path ~timeout ~f =
    blocking_create ~timeout ?message path;
    Exn.protect ~f ~finally:(fun () -> unlock_safely_exn ~unlock_myself:true path)
  ;;

  let unlock_exn path = unlock_safely_exn ~unlock_myself:true path

  let unlock path = Or_error.try_with (fun () -> unlock_exn path)

  TEST_MODULE = struct
    let create_bool path = match create path with Ok () -> true | Error _ -> false
    let path = Filename.temp_file "lock_file" "unit_test"
    let () = Unix.unlink path
    TEST = create_bool path
    TEST = not (create_bool path)
    let () = unlock_exn path
    TEST = create_bool path
    let () = unlock_exn path
  end

end
