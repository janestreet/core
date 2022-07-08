open! Core
open Expect_test_helpers_core
open Array

let ar1 = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]

let%expect_test "slice" =
  let test start stop = print_s [%sexp (slice ar1 start stop : int array)] in
  test 0 0;
  [%expect {| (1 2 3 4 5 6 7 8 9 10) |}];
  test 1 3;
  [%expect {| (2 3) |}];
  test 0 (-1);
  [%expect {| (1 2 3 4 5 6 7 8 9) |}];
  test (-1) 0;
  [%expect {| (10) |}];
  test (-5) (-4);
  [%expect {| (6) |}]
;;

let%test_module "nget" =
  (module struct
    let%expect_test "neg" = require_equal [%here] (module Base.Int) (nget ar1 (-3)) 8
    let%expect_test "pos" = require_equal [%here] (module Base.Int) (nget ar1 3) ar1.(3)

    let%expect_test "invalid" =
      require_does_raise [%here] (fun () : int -> nget ar1 (-100));
      [%expect {| (Invalid_argument "index out of bounds") |}]
    ;;
  end)
;;
