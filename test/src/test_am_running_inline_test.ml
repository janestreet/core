open! Core

let%expect_test _ =
  Unix.system "../bin/am_running_inline_test.exe"
  |> Unix.Exit_or_signal.or_error
  |> ok_exn;
  [%expect {|
    true |}];
;;
