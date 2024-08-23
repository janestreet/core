open! Core

(* Test both bindings of [stage]/[unstage] on locals. *)
let%expect_test "local" =
  let local_ string = "printme" in
  print_endline (String.globalize (Staged.unstage (Staged.stage string)));
  [%expect {| printme |}];
  print_endline (String.globalize (unstage (stage string)));
  [%expect {| printme |}]
;;
