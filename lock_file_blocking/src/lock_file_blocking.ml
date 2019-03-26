open! Core
open! Import
open! Int.Replace_polymorphic_compare

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

let read_file_and_convert ~of_string path =
  Option.try_with (fun () ->
    In_channel.read_all path
    |> String.strip
    |> of_string)
;;

let get_pid path =
  let of_string string =
    Int.of_string string
    |> Pid.of_int
  in
  read_file_and_convert ~of_string path
;;

module Nfs = struct
  let process_start_time pid =
    (* Find the start time for a process, without requiring the [Procfs] library
       -- start time is represented in USER_HZ units in /proc/<pid>/stat (confusingly
       referred to as 'jiffies' in the man page); USER_HZ is almost certainly 100, for
       mostly historical reasons, but just to be sure we'll ask sysconf.
    *)
    match Linux_ext.Sysinfo.sysinfo with
    | Error _    -> None
    | Ok sysinfo ->
      let of_string stat =
        (* [read_file_and_convert] will catch any exceptions raised here *)
        let boot_time = Time.sub (Time.now ()) (sysinfo ()).Linux_ext.Sysinfo.uptime in
        let jiffies =
          let fields =
            String.rsplit2_exn stat ~on:')'
            |> snd
            |> String.strip
            |> String.split ~on:' '
          in
          List.nth_exn fields 19
          |> Float.of_string
        in
        let hz =
          Unix.sysconf Unix.CLK_TCK
          |> Option.value_exn
          |> Int64.to_float
        in
        jiffies /. hz
        |> Time.Span.of_sec
        |> Time.add boot_time
      in
      read_file_and_convert (sprintf !"/proc/%{Pid}/stat" pid) ~of_string
  ;;

  module Info = struct
    type t =
      { host       : string
      ; pid        : Pid.Stable.V1.t
      ; message    : string
      ; start_time : Time.Stable.With_utc_sexp.V2.t option [@sexp.option]
      }
    [@@deriving sexp, fields]

    let create ~message =
      let pid = Unix.getpid () in
      { host = Unix.gethostname ()
      ; pid
      ; message
      ; start_time = process_start_time pid }
    ;;

    let of_string string =
      Sexp.of_string string
      |> t_of_sexp
    ;;

    let of_file = read_file_and_convert ~of_string
  end

  let lock_path path = path ^ ".nfs_lock"

  let get_hostname_and_pid path =
    Option.map (Info.of_file path) ~f:(fun info -> (Info.host info), (Info.pid info))
  ;;

  let get_message path = Option.map (Info.of_file path) ~f:Info.message

  let unlock_safely_exn ~unlock_myself path =
    (* Make sure error messages contain a reference to "lock.nfs_lock", which is the
       actually important file. *)
    let lock_path = lock_path path in
    let error s =
      failwithf
        "Lock_file.Nfs.unlock_safely_exn: unable to unlock %s: %s" lock_path s ()
    in
    match Sys.file_exists ~follow_symlinks:false lock_path with
    | `Unknown -> error (sprintf "unable to read %s" lock_path)
    | `No      -> ()
    | `Yes     ->
      match Info.of_file lock_path with
      | None      -> error "unknown lock file format"
      | Some info ->
        let my_pid           = Unix.getpid      ()   in
        let my_hostname      = Unix.gethostname ()   in
        let locking_hostname = Info.host        info in
        let locking_pid      = Info.pid         info in
        if String.(<>) my_hostname locking_hostname
        then
          error (sprintf "locked from %s, unlock attempted from %s"
                   locking_hostname my_hostname)
        else
          (* Check if the process is running: sends signal 0 to pid, which should work if
             the process is running and is owned by the user running this code. If the
             process is not owned by the user running this code we should fail to unlock
             either earlier (unable to read the file) or later (unable to remove the
             file). *)
          let pid_start_matches_lock pid =
            match Option.both (Info.start_time info) (process_start_time pid) with
            | None -> true (* don't have both start times: fall back to old behaviour *)
            | Some (lock_start, pid_start) ->
              (* our method of calculating start time is open to some inaccuracy, so let's
                 be generous and allow for up to 1s of difference (this would only allow
                 for a collision if pids get reused within 1s, which seems unlikely) *)
              let epsilon = Time.Span.of_sec 1. in
              Time.Span.(<) (Time.abs_diff lock_start pid_start) epsilon
          in
          let is_locked_by_me () =
            (Pid.equal locking_pid my_pid)
            && (pid_start_matches_lock my_pid)
          in
          let locking_pid_exists () =
            (Signal.can_send_to locking_pid)
            && (pid_start_matches_lock locking_pid)
          in
          if (unlock_myself && (is_locked_by_me ())) || not (locking_pid_exists ())
          then begin
            (* We need to be able to recover from situation where [path] does not exist
               for whatever reason, but [lock_path] is present. Error during unlink of
               [path] are ignored to be able to cope with this situation and properly
               clean up stale locks. *)
            begin
              try
                Unix.unlink path
              with
              | Unix.Unix_error (ENOENT, _, _) -> ()
              | e                              -> error (Exn.to_string e)
            end;
            try
              Unix.unlink lock_path
            with
            | e -> error (Exn.to_string e)
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
          let info = Info.create ~message in
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
end

(* The reason this function is used is to make sure the file the path is pointing to
   remains stable across [chdir]. In fact we'd prefer for it to remain stable over
   other things, such as [rename] of a parent directory.
   That could be achieved if we [open] the [dir] and use the resulting file descriptor
   with linkat, unlinkat, etc system calls, but that's less portable and most
   programs that use locks will break anyway if their directory is renamed. *)
let canonicalize_dirname path =
  let dir, name = Filename.dirname path, Filename.basename path in
  let dir = Filename.realpath dir in
  dir ^/ name

module Mkdir = struct
  type t = Locked of { lock_path : string }

  let lock_exn ~lock_path =
    let lock_path = canonicalize_dirname lock_path in
    match Unix.mkdir lock_path with
    | exception (Core.Unix.Unix_error (EEXIST, _, _)) -> `Somebody_else_took_it
    | () -> `We_took_it (Locked { lock_path })

  let unlock_exn (Locked { lock_path }) = Unix.rmdir lock_path
end

module Symlink = struct
  type t = Locked of { lock_path : string }

  let lock_exn ~lock_path ~metadata =
    let lock_path = canonicalize_dirname lock_path in
    match Unix.symlink ~link_name:lock_path ~target:metadata with
    | exception (Core.Unix.Unix_error (EEXIST, _, _)) ->
      `Somebody_else_took_it (Or_error.try_with (fun () -> Unix.readlink lock_path))
    | () -> `We_took_it (Locked { lock_path })

  let unlock_exn (Locked { lock_path }) = Unix.unlink lock_path
end

module Flock = struct

  type t = {
    fd : Caml.Unix.file_descr;
    mutable unlocked : bool;
  }

  let lock_exn ~lock_path =
    let fd =
      Core.Unix.openfile ~perm:0o664 ~mode:[O_CREAT; O_WRONLY; O_CLOEXEC] lock_path
    in
    match flock fd with
    | false ->
      Core.Unix.close ~restart:true fd;
      `Somebody_else_took_it
    | true -> `We_took_it { fd; unlocked = false }
    | exception exn ->
      Core.Unix.close ~restart:true fd;
      raise exn

  let unlock_exn t =
    if t.unlocked
    then raise_s [%sexp "Lock_file_blocking.Flock.unlock_exn called twice"];
    t.unlocked <- true;
    Core.Unix.close ~restart:true t.fd;
end
