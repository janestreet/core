open Std_internal
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

  let unlock path =
    try
      Unix.unlink (lock_path path);
    with
    | e -> failwithf "Lock_file.Nfs.unlock '%s' failed: %s" path (Exn.to_string e) ()
  ;;

  (* See mli for more information on the algorithm we use for locking over NFS.  Ensure
     that you understand it before you make any changes here. *)
  let create ?message path =
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
    if got_lock then at_exit (fun () -> try unlock path with _ -> ());
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
    Exn.protect ~f ~finally:(fun () -> unlock path)
  ;;

  let get_hostname_and_pid path =
    let fd = Unix.openfile path ~mode:[Unix.O_RDONLY] in
    (* presumed to be plenty big to hold a hostname and pid *)
    let buf = String.create 2048 in
    let len = Unix.read fd ~buf in
    match String.rsplit2 ~on:':' (String.strip (String.sub buf ~pos:0 ~len)) with
    | None                -> None
    | Some (hostname,pid) ->
      try
        Some (hostname, Int.of_string pid)
      with
      | _ -> None
  ;;
end
