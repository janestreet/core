open! Core
open! Expect_test_helpers_core

let%expect_test "[am_running_test]" =
  print_s [%message (am_running_test : bool)];
  [%expect {| (am_running_test true) |}]
;;
