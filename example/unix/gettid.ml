open Core
open Async
module Unix = Core.Unix

let gettid_exn () = Or_error.ok_exn Unix.gettid ()
;;

let main () =
  Core.Printf.printf "top -Hp %s\n%!" (Pid.to_string (Unix.getpid ()));
  Clock.after (sec 5.)
  >>= fun () ->
  Clock.every (sec 1.) (fun () ->
    don't_wait_for (In_thread.run (fun () ->
      Core.Printf.printf "%d\n%!" (Unix.Thread_id.to_int (gettid_exn ()));
      (* wait for a little while, to force more than one thread to be used *)
      Unix.sleep 5
    )));
  Deferred.never ()
;;

Command.async_spec
  ~summary:"demonstrate thread ID call"
  Command.Spec.empty
  main
|> Command.run
