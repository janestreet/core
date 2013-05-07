open Std_internal
open Int.Replace_polymorphic_compare let _ = _squelch_unused_module_warning_
module Unix = Core_unix

(* We have reason to believe that lockf doesn't work properly on CIFS mounts.  The idea
   behind requiring both lockf and flock is to prevent programs taking locks on
   network filesystems where they may not be sound.  *)

let flock fd = Unix.flock fd Unix.Flock_command.lock_exclusive

let lockf fd =
  try
    Unix.lockf fd ~mode:Unix.F_TLOCK ~len:Int64.zero;
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

let rec blocking_create ?message ?close_on_exec ?unlink_on_exit path =
  if not (create ?message ?close_on_exec ?unlink_on_exit path) then begin
    Time.pause (Span.of_sec 1.);
    blocking_create ?message ?close_on_exec ?unlink_on_exit path
  end

let is_locked path =
  try
    let fd      = Unix.openfile path ~mode:[Unix.O_WRONLY] ~perm:0o664 in
    let flocked = flock fd in
    let lockfed = lockf fd in
    Unix.close fd; (* releases any locks from [flock] and/or [lockf] *)
    if flocked && lockfed then false
    else true
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) -> false
  | e -> raise e

module Nfs = struct
  let lock_path path = path ^ ".nfs_lock"

  let get_hostname_and_pid path =
    let fd = Unix.openfile path ~mode:[Unix.O_RDONLY] in
    (* presumed to be plenty big to hold a hostname and pid *)
    let buf = String.create 2048 in
    let len = Unix.read fd ~buf in
    match String.rsplit2 ~on:':' (String.strip (String.sub buf ~pos:0 ~len)) with
    | None                -> None
    | Some (hostname, pid) ->
      try
        Some (hostname, Pid.of_string pid)
      with
      | _ -> None
  ;;

  (** [unlock_safely path] unlocks [path] if [path] was locked from the same
      host and the pid in the file is not in the list of running processes. *)
  let unlock_safely path =
    (* Make sure error messages contain a reference to "lock.nfs_lock", which is the
       actually important file. *)
    let path = lock_path path in
    let error s =
      failwithf
        "Lock_file.Nfs.unlock_safely: unable to unlock %s: %s" path s ()
    in
    match Sys.file_exists ~follow_symlinks:false path with
    | `Unknown -> error "unable to read path"
    | `No      -> ()
    | `Yes     ->
      match get_hostname_and_pid path with
      | None -> error "lock file doesn't contain hostname and pid"
      | Some (locking_hostname, pid) ->
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
          if Pid.(<>) pid my_pid && Signal.can_send_to pid then
            error (sprintf "locking process (pid %i) still running on %s"
              (Pid.to_int pid) locking_hostname)
          else
            try Unix.unlink path with | e -> error (Exn.to_string e)
  ;;

  (* See mli for more information on the algorithm we use for locking over NFS.  Ensure
     that you understand it before you make any changes here. *)
  let create ?message path =
    unlock_safely path;
    let fd = Unix.openfile path ~mode:[Unix.O_WRONLY; Unix.O_CREAT] in
    let got_lock =
      try
        Unix.link ~target:path ~link_name:(lock_path path) ();
        Unix.ftruncate fd ~len:0L;
        let message =
          match message with
          | None -> Unix.gethostname () ^ ":" ^ Pid.to_string (Unix.getpid ())
          | Some m -> m
        in
        fprintf (Unix.out_channel_of_descr fd) "%s\n%!" message;
        true
      with
      | _ -> false
    in
    Unix.close fd;
    if got_lock then at_exit (fun () -> try unlock_safely path with _ -> ());
    got_lock
  ;;

  let create_exn ?message path =
    if create ?message path then ()
    else failwithf "Lock_file.Nfs.lock_exn '%s' was unable to acquire the lock" path ()
  ;;

  let blocking_create ?message path =
    let rec loop () =
      if create ?message path then ()
      else begin
        Unix.sleep 1;
        loop ()
      end
    in
    loop ()
  ;;

  let critical_section ?message path ~f =
    create_exn ?message path;
    Exn.protect ~f ~finally:(fun () -> unlock_safely path)
  ;;
end
