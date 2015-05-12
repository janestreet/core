(* Test parent death notification *)

open Core.Std
open Sys
open Linux_ext
open OUnit

let handle_signal s =
  if Signal.equal s Signal.hup then
    ()
  else (
    printf "Got unknown signal: %s\n%!" (Signal.to_string s);
    assert false
  )

let run () =
  Signal.handle Signal.hup handle_signal;
  match Unix.fork () with
  | `In_the_child ->
    pr_set_pdeathsig Signal.hup;
    if Unix.getppid_exn () = Pid.init then
      ignore (Signal.send Signal.kill (`Pid (Unix.getpid ())));
    Unix.sleep 3
  | `In_the_parent _ ->
    Unix.sleep 1

let test = "Parent_death_test" >::: [
  "test" >:: (fun () ->
    "1" @? (try run (); true with _ -> false));
]
