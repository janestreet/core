open! Import
open! Int.Replace_polymorphic_compare
open Import_time

module Unix = Core_unix

[%%import "config.h"]

(* We have reason to believe that lockf doesn't work properly on CIFS mounts.  The idea
   behind requiring both lockf and flock is to prevent programs taking locks on
   network filesystems where they may not be sound.

   However, this assumes that [lockf] and [flock] take independent locks, which
   is true on local Linux filesystems, but is false on many OSes (for example, Mac OS X),
   so we use just [flock] on non-linux OSes and give up the fail-on-CIFS-and-NFS property.

   We prefer [flock] and not [lockf] because [lockf] has bad semantics if used multiple
   times within the same process: for example [lockf a; lockf b; close a] succeeds (bad!)
   and leaves the file unlocked (bad!) if [a] and [b] are unrelated file descriptors for
   the same file. *)

let flock fd = Unix.flock fd Unix.Flock_command.lock_exclusive

let lockf ?(mode = Unix.F_TLOCK) fd =
  try
    Unix.lockf fd ~mode ~len:Int64.zero;
    true
  with
  | _ -> false

[%%ifdef JSC_LINUX_EXT]
let lock fd =
  (* [lockf] doesn't throw any exceptions, so if an exception is raised from this
     function, it must have come from [flock]. *)
  let flocked = flock fd in
  let lockfed = lockf fd in
  flocked && lockfed
[%%else]
let lock = flock
[%%endif]

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
      let pid_when_lock_file_was_created = Unix.getpid () in
      if unlink_on_exit then at_exit (fun () ->
        (* Do not unlink if we are in a different process than the one
           that created the lock file (e.g. a forked child)
        *)
        if (Pid.(=) pid_when_lock_file_was_created (Unix.getpid ()))
        then begin
          try Unix.unlink path with _ -> ()
        end
      );
      Unix.ftruncate fd ~len:Int64.zero;
      ignore (Unix.write_substring fd ~buf:message ~pos:0 ~len:(String.length message));
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

let random = lazy (Random.State.make_self_init ())

(* no timeout specified = wait indefinitely *)
let repeat_with_timeout ?timeout lockf path =
  let max_delay = 0.3 in
  match timeout with
  | None ->
    let rec loop () =
      try (lockf path)
      with | _ -> begin
          let (_ : float) =
            Unix.nanosleep (Random.State.float (Lazy.force random) max_delay)
          in
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
            let (_ : float) =
              Unix.nanosleep (Random.State.float (Lazy.force random) max_delay)
            in
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
  | Unix.Unix_error (ENOENT, _, _) -> false
  | e -> raise e

let%test_module _ = (module struct
  let lock_file = Filename.temp_file "lock_file" "unit_test"
  let () = Unix.unlink lock_file
  let%test _ = create lock_file
  let%test _ = not (create lock_file)
  let%test _ = is_locked lock_file

  let nolock_file = Filename.temp_file "nolock_file" "unit_test"
  let () =
    Unix.unlink nolock_file;
    (* Create an empty file. *)
    Unix.close (Unix.openfile nolock_file ~mode:[Unix.O_CREAT; Unix.O_WRONLY])
  let%test _ =
    (* Check that file exists. *)
    try ignore (Unix.stat nolock_file); true
    with Unix.Unix_error (ENOENT, _, _) -> false
  let%test _ = not (is_locked nolock_file)
end)

let read_file_and_convert ~of_string path =
  Option.try_with
    ( fun () ->
        In_channel.read_all path
        |> String.strip
        |> of_string
    )
;;

let get_pid path =
  let of_string string =
    Int.of_string string
    |> Pid.of_int
  in
  read_file_and_convert ~of_string path
;;

module Nfs = struct
  module Info = struct
    type t = {
      host    : string;
      pid     : Pid.Stable.V1.t;
      message : string
    } [@@deriving sexp]
  end

  let lock_path path = path ^ ".nfs_lock"

  let get_info path =
    let of_string string =
      Sexp.of_string string
      |> Info.t_of_sexp
    in
    read_file_and_convert ~of_string path
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
        then begin
          (* We need to be able to recover from situation where [path] does not
             exist for whatever reason, but [lock_path] is present.
             Error during unlink of [path] are ignored to be able to cope with this
             situation and properly clean up stale locks.
          *)
          begin
            try
              Unix.unlink path
            with | Unix.Unix_error (ENOENT, _, _) -> ()
                 | e -> error (Exn.to_string e)
          end;
          try
            Unix.unlink lock_path
          with | e -> error (Exn.to_string e)
        end
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
      let cleanup = ref (fun () -> Unix.close fd) in
      protect
        ~finally:(fun () -> !cleanup ())
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
            let out_channel = Unix.out_channel_of_descr fd in
            cleanup := (fun () -> Caml.close_out_noerr out_channel);
            fprintf out_channel "%s\n%!"
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

  let%test_module _ = (module struct
    let create_bool path = match create path with Ok () -> true | Error _ -> false
    let path = Filename.temp_file "lock_file" "unit_test"
    let () = Unix.unlink path
    let%test _ = create_bool path
    let%test _ = not (create_bool path)
    let () = unlock_exn path
    let%test _ = create_bool path
    let () = unlock_exn path
  end)

end
