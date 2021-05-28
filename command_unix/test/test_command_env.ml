open! Core
open! Async
open Expect_test_helpers_async
open Deferred.Let_syntax

let run sub_cmd =
  run "env" [ "-i"; "DUMMY_VARIABLE=DUMMY"; "../test-bin/command_env_test.exe"; sub_cmd ]
;;

let%expect_test "do nothing" =
  let%map () = run "noop" in
  [%expect {|
    DUMMY_VARIABLE=DUMMY |}]
;;

let%expect_test "clear" =
  let%map () = run "clear" in
  [%expect {| |}]
;;

let%expect_test "set" =
  let%map () = run "set" in
  [%expect {|
    TEST_VAR=TEST_VALUE
    DUMMY_VARIABLE=DUMMY |}]
;;
