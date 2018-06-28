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
    match Core.Lock_file.Nfs.create path with
    | Error _e ->
      ignore (Core.Unix.nanosleep 0.0003);
      obtain ()
    | Ok () -> ()
  in
  obtain ();
  f ();
  Core.Lock_file.Nfs.unlock_exn path

(* not quite a critical section because it only ends when the process dies,
   but close enough *)
let local_critical_section path ~f =
  let rec obtain () =
    maybe_die ();
    match Core.Lock_file.create path with
    | false ->
      ignore (Core.Unix.nanosleep 0.0003);
      obtain ()
    | true -> ()
  in
  obtain ();
  f ()

let critical_section ~which_lock path ~f = match which_lock with
  | `Nfs -> nfs_critical_section path ~f
  | `Local -> local_critical_section path ~f

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
      ~summary:"This puts a lock file [lockfile] in the current directory under \
                heavy contention" (
      let%map_open.Command.Let_syntax
        which_lock =
        flag ~doc:"Nfs|Local which lock protocol to use"
          "which" (required (sexp_conv [%of_sexp: [`Nfs | `Local]]))
      in
      fun () ->
        let ps = List.init 200 ~f:(fun _i -> go ~which_lock ()) in
        List.iter ps ~f:Unix.waitpid_exn
    ))
