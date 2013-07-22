open Core_kernel.Std
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

  let unlock_safely path =
    (* Make sure error messages contain a reference to "lock.nfs_lock", which is the
       actually important file. *)
    let lock_path = lock_path path in
    let error s =
      failwithf
        "Lock_file.Nfs.unlock_safely: unable to unlock %s: %s" lock_path s ()
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
          if Pid.(<>) locking_pid my_pid && Signal.can_send_to locking_pid then
            error (sprintf "locking process (pid %i) still running on %s"
              (Pid.to_int locking_pid) locking_hostname)
          else
            try
              Unix.unlink path;
              Unix.unlink lock_path
            with | e -> error (Exn.to_string e)
  ;;

  (* See mli for more information on the algorithm we use for locking over NFS.  Ensure
     that you understand it before you make any changes here. *)
  let create ?(message = "") path =
    try
      unlock_safely path;
      let fd = Unix.openfile path ~mode:[Unix.O_WRONLY; Unix.O_CREAT] in
      let got_lock =
        try
          Unix.link ~target:path ~link_name:(lock_path path) ();
          Unix.ftruncate fd ~len:0L;
          let info =
            { Info.
              host = Unix.gethostname ();
              pid  = Unix.getpid ();
              message
            }
          in
          fprintf (Unix.out_channel_of_descr fd) "%s\n%!"
            (Sexp.to_string_hum (Info.sexp_of_t info));
          true
        with
        | _ -> false
      in
      Unix.close fd;
      if got_lock then at_exit (fun () -> try unlock_safely path with _ -> ());
      got_lock
    with
    | _ -> false
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
