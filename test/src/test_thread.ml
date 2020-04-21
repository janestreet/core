open! Core
open! Async
open! Import
open! Expect_test_helpers_core
open! Expect_test_helpers_async

let%expect_test "[sexp_of_t]" =
  let t = Thread.create ignore () ~on_uncaught_exn:`Print_to_stderr in
  print_s [%sexp (t : Thread.t)];
  (* We expect thread id [2] to be deterministically printed because we inline test each
     file on its own, and there is no top-level effect that will create a thread prior to
     this. *)
  let%bind () = [%expect {| (thread (id 2)) |}] in
  Thread.join t;
  print_s [%sexp (t : Thread.t)];
  let%bind () = [%expect {| (thread (id 2)) |}] in
  return ()
;;

let thread_on_uncaught_exn_exe = "../bin/thread_on_uncaught_exn.exe"

let%expect_test "~on_uncaught_exn:`Print_to_stderr" =
  let%bind () =
    run thread_on_uncaught_exn_exe [ "core"; "-on-uncaught-exn"; "print-to-stderr" ]
  in
  [%expect
    {|
    About to create thread
    Start of thread_main
    Thread joined; main done
    Caml.at_exit callback ran
    Core.at_exit callback ran
    --- STDERR ---
    Uncaught exception:

      (Failure "exception thrown out of thread_main") |}]
;;

let%expect_test "~on_uncaught_exn:`Kill_whole_process" =
  let%bind () =
    run thread_on_uncaught_exn_exe [ "core"; "-on-uncaught-exn"; "kill-whole-process" ]
  in
  [%expect
    {|
    About to create thread
    Start of thread_main
    Caml.at_exit callback ran
    Core.at_exit callback ran
    ("Unclean exit" (Exit_non_zero 1))
    --- STDERR ---
    Uncaught exception:

      (Failure "exception thrown out of thread_main") |}]
;;

let%expect_test "Caml.Thread.create's behaviour with exceptions, for comparison" =
  let%bind () = run thread_on_uncaught_exn_exe [ "caml" ] in
  [%expect
    {|
    About to create thread
    Start of thread_main
    Thread joined; main done
    Caml.at_exit callback ran
    Core.at_exit callback ran
    --- STDERR ---
    Thread 1 killed on uncaught exception Failure("exception thrown out of thread_main") |}]
;;
