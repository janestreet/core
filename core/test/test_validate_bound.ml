open! Core
open Validate
open Expect_test_helpers_core

let print t = List.iter (errors t) ~f:print_endline

let%expect_test "int bounds" =
  let res =
    name_list
      "foo"
      [ name "ok" (Int.validate_bound ~min:(Incl 0) ~max:(Incl 100) 5)
      ; name "incl_lower" (Int.validate_bound ~min:(Incl 0) ~max:(Incl 100) (-1))
      ; name "incl_lower" (Int.validate_bound ~min:(Incl 0) ~max:(Incl 100) 0)
      ; name "incl_upper" (Int.validate_bound ~min:(Incl 0) ~max:(Incl 100) 101)
      ; name "incl_upper" (Int.validate_bound ~min:(Incl 0) ~max:(Incl 100) 100)
      ; name "excl_lower" (Int.validate_bound ~min:(Excl 0) ~max:(Excl 100) 0)
      ; name "excl_upper" (Int.validate_bound ~min:(Excl 0) ~max:(Excl 100) 100)
      ; name "excl_lower" (Int.validate_bound ~min:(Excl 0) ~max:(Excl 100) 1)
      ; name "excl_upper" (Int.validate_bound ~min:(Excl 0) ~max:(Excl 100) 99)
      ]
  in
  print res;
  [%expect
    {|
    (foo.incl_lower "value -1 < bound 0")
    (foo.incl_upper "value 101 > bound 100")
    (foo.excl_lower "value 0 <= bound 0")
    (foo.excl_upper "value 100 >= bound 100")
    |}]
;;

let%expect_test "inf/nan" =
  let res =
    name_list
      "bar"
      [ name "ok" (Float.validate_bound ~min:(Incl 0.) ~max:(Incl 100.) 5.)
      ; name "nan" (Float.validate_bound ~min:(Incl 0.) ~max:(Incl 100.) Float.nan)
      ; name "inf" (Float.validate_bound ~min:(Incl 0.) ~max:(Incl 100.) Float.infinity)
      ]
  in
  print res;
  [%expect {|
    (bar.nan "value is NaN")
    (bar.inf "value is infinite")
    |}]
;;
