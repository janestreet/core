open! Core
open! Import

let%expect_test "Tuple2.sort is stable" =
  let test a b =
    let sorted = Tuple2.sort ((a, "a"), (b, "b")) ~compare:[%compare: int * _] in
    print_s [%sexp (sorted : (int * string) * (int * string))]
  in
  test 0 1;
  test 0 0;
  test 1 0;
  [%expect
    {|
    ((0 a)
     (1 b))
    ((0 a)
     (0 b))
    ((0 b)
     (1 a))
    |}]
;;
