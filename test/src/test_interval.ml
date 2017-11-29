open Core

let%expect_test "list_intersect" =
  let i = Interval.create in
  let x = Interval.list_intersect [i 4 7; i 9 15] [i 2 4; i 5 10; i 14 20] in
  print_s [%sexp (x : int Interval.t list)];
  [%expect {| ((4 4) (5 7) (9 10) (14 15)) |}]
;;
