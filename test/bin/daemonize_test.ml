open Core

let rec sleep x = if x <= 0. then () else sleep (Unix.nanosleep x)

let () =
  let reader, writer = Unix.pipe () in
  match Unix.fork () with
  | `In_the_child ->
    (* the child tries to daemonize itself, and also ignores USR1 signal *)
    let _ = Core.Signal.Expert.handle Signal.usr1 (fun _ -> ()) in
    let release = Daemon.daemonize_wait () in
    sleep 1.;
    Staged.unstage release ();
    ignore (Unix.write_substring writer ~buf:"x")
  | `In_the_parent child ->
    (* the parent tries kills the child with USR1 and expects the signal to be ignored *)
    sleep 0.2;
    UnixLabels.kill ~pid:(Pid.to_int child) ~signal:(Signal.to_caml_int Signal.usr1);
    let buf = Bytes.create 1 in
    Unix.waitpid_exn child;
    assert (Unix.read reader ~buf = 1);
    assert (Bytes.to_string buf = "x");

