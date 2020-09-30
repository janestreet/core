open! Core
open! Import

(** Not all tests of command completion need to go here. But this is the catch-all place
    for completion tests that don't have a better home. *)

let param =
  let%map_open.Command (_ : bool) = anon ("BOOL" %: bool)
  and (_ : bool list) = anon (sequence ("BOOL" %: bool)) in
  ()
;;

let test args = Command_test_helpers.complete param ~args

let%expect_test "completion of anons" =
  test [ "" ];
  [%expect {|
    false
    true
    (command.ml.Exit_called (status 0)) |}];
  test [ "t" ];
  [%expect {|
    true
    (command.ml.Exit_called (status 0)) |}];
  test [ "f" ];
  [%expect {|
    false
    (command.ml.Exit_called (status 0)) |}];
  test [ "true"; "" ];
  [%expect {|
    false
    true
    (command.ml.Exit_called (status 0)) |}];
  test [ "true"; "t" ];
  [%expect {|
    true
    (command.ml.Exit_called (status 0)) |}];
  (* First argument is invalid, but we can still complete later arguments. *)
  test [ "bool"; "" ];
  [%expect {|
    false
    true
    (command.ml.Exit_called (status 0)) |}];
  ()
;;

let%expect_test "completion after [-help]" =
  test [ "-" ];
  [%expect
    {|
    -build-info
    -help
    -version
    (command.ml.Exit_called (status 0)) |}];
  test [ "-h" ];
  [%expect {|
    -help
    (command.ml.Exit_called (status 0)) |}];
  test [ "-help"; "" ];
  [%expect {|
    false
    true
    (command.ml.Exit_called (status 0)) |}]
;;
