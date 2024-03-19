open Core

let%expect_test "collect_to_string" =
  let output =
    Printf.collect_to_string (fun { printf } ->
      printf "hello ";
      printf "%s%c" "world" '!')
  in
  print_s [%sexp (output : string)];
  [%expect {| "hello world!" |}]
;;

let%expect_test "collect_to_string - try to use printf after [collect_to_string] returned"
  =
  let captured_printf = Set_once.create () in
  let output =
    Printf.collect_to_string (fun { printf } ->
      printf "inside";
      Set_once.set_exn captured_printf [%here] printf)
  in
  [%expect {| |}];
  print_s [%sexp (output : string)];
  [%expect {| inside |}];
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
    (Set_once.get_exn captured_printf [%here]) "outside");
  [%expect {| "[printf] used after [collect_to_string] returned" |}]
;;
