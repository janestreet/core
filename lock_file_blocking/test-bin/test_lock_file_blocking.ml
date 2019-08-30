open Core

(* Currently this test doesn't always pass for Nfs lock: it detects both a safety bug
   where we're cleaning up someone else's lock and a liveness bug
   where we're trying to unlock an empty [lockfile]. *)

(* this is so that after safety violation everything stops *)
let maybe_die () =
  match Unix.access "die" [`Read] with
  | Error _exn -> ()
  | Ok () ->
    Signal.send_exn Signal.kill (`Pid (Unix.getpid ()))

let nfs_critical_section path ~f =
  let rec obtain () =
    maybe_die ();
    match Lock_file_blocking.Nfs.create path with
    | Error _e ->
      ignore (Core.Unix.nanosleep 0.0003);
      obtain ()
    | Ok () -> ()
  in
  obtain ();
  f ();
  Lock_file_blocking.Nfs.unlock_exn path

let critical_section (type a) ~lock ~unlock ~(f : unit -> a) : a =
  let rec obtain () =
    maybe_die ();
    match lock () with
    | `Somebody_else_took_it ->
      ignore (Core.Unix.nanosleep 0.0003);
      obtain ()
    | `We_took_it lock -> lock
  in
  let lock = obtain () in
  protect ~f
    ~finally:(fun () -> unlock lock)

(* not quite a critical section because it only ends when the process dies,
   but close enough *)
let local_critical_section path ~f =
  let rec obtain () =
    maybe_die ();
    match Lock_file_blocking.create path with
    | false ->
      ignore (Core.Unix.nanosleep 0.0003);
      obtain ()
    | true -> ()
  in
  obtain ();
  f ()

let mkdir_critical_section (type a) path ~(f : unit -> a) : a =
  critical_section
    ~lock:(fun () ->
      Lock_file_blocking.Mkdir.lock_exn ~lock_path:path)
    ~unlock:(Lock_file_blocking.Mkdir.unlock_exn)
    ~f

let symlink_critical_section path ~f =
  critical_section
    ~lock:(fun () ->
      match Lock_file_blocking.Symlink.lock_exn ~lock_path:path ~metadata:"blah-blah" with
      | `Somebody_else_took_it _metadata -> `Somebody_else_took_it
      | `We_took_it lock -> `We_took_it lock)
    ~unlock:(Lock_file_blocking.Symlink.unlock_exn)
    ~f

let mkdir_or_symlink_critical_section (type a) path ~(f : unit -> a) : a =
  let critical_section =
    match Random.bool () with
    | false -> mkdir_critical_section
    | true -> symlink_critical_section
  in
  critical_section path ~f

let flock_critical_section path ~f =
  critical_section
    ~lock:(fun () -> Lock_file_blocking.Flock.lock_exn ~lock_path:path)
    ~unlock:(Lock_file_blocking.Flock.unlock_exn)
    ~f

let critical_section ~which_lock path ~f = match which_lock with
  | `Nfs -> nfs_critical_section path ~f
  | `Local -> local_critical_section path ~f
  | `Mkdir_or_symlink -> mkdir_or_symlink_critical_section path ~f
  | `Symlink -> symlink_critical_section path ~f
  | `Flock -> flock_critical_section path ~f

let save file contents =
  let fd = Unix.openfile file ~mode:[Unix.O_WRONLY; Unix.O_CREAT] in
  let out_channel = Unix.out_channel_of_descr fd in
  fprintf out_channel "%s\n%!" contents

let go ~which_lock () =
  match Unix.fork () with
  | `In_the_child ->
    let () =
      critical_section ~which_lock "lockfile" ~f:(fun () ->
        maybe_die ();
        let pid = Pid.to_string (Unix.getpid ()) in
        save pid pid;
        (match Unix.mkdir "zoo" with
         | exception exn ->
           Unix.mkdir "die";
           raise exn
         | _ -> ());
        ignore (Core.Unix.nanosleep 0.002);
        maybe_die ();
        Unix.rmdir "zoo";
        Unix.unlink pid
      )
    in
    exit 0
  | `In_the_parent pid ->
    pid
;;

let () =
  Command.run (
    Command.basic
      ~summary:"This puts a lock file [lockfile] in the directory test-lock-file under \
                heavy contention" (
      let%map_open.Command
        which_lock =
        flag ~doc:"Nfs|Local|Mkdir|Flock which lock protocol to use"
          "which" (required (
            sexp_conv [%of_sexp: [`Nfs | `Local | `Mkdir_or_symlink | `Symlink | `Flock]]))
      in
      fun () ->
        Unix.mkdir "test-lock-file";
        Unix.chdir "test-lock-file";
        let ps = List.init 2000 ~f:(fun _i -> go ~which_lock ()) in
        List.iter ps ~f:Unix.waitpid_exn
    ))
