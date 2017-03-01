open Core
open Expect_test_helpers

module Let_syntax         = Async.Let_syntax
module Expect_test_config = Async.Expect_test_config

let%expect_test "Unix.fork_exec" =
  let pid = Unix.fork_exec ~prog:"program_that_doesnt_exist" ~argv:[] () in
  let res = Unix.waitpid pid |> Unix.Exit_or_signal.or_error in
  print_s [%sexp (res : unit Or_error.t)];
  [%expect{| (Error (Unix.Exit_or_signal (Exit_non_zero 127))) |}]
;;

let%expect_test "at_exit handlers not executed in child process" =
  let do_exec = ref true in
  Caml.at_exit (fun () -> if !do_exec then print_endline "at_exit handler executed");
  let pid = Unix.fork_exec ~prog:"program_that_doesnt_exist" ~argv:[] () in
  let res = Unix.waitpid pid |> Unix.Exit_or_signal.or_error in
  print_s [%sexp (res : unit Or_error.t)];
  let%map () = [%expect{|
    (Error (Unix.Exit_or_signal (Exit_non_zero 127))) |}]
  in
  do_exec := false; (* Disable this [at_exit] for other tests *)
;;
