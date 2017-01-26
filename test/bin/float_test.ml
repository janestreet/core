open OUnit;;
open Core

let terse_test =
  "terse_test" >::
  fun () ->
    let test number expected =
      assert_equal ~printer:Fn.id expected (Float.Terse.to_string number);
      let sexp = Float.Terse.sexp_of_t number in
      assert_equal ~printer:Fn.id expected (Sexp.to_string sexp)
    in
    test 0.0123456789 "0.012345679";
    test 0.012345 "0.012345";
    test 123456789.123 "1.2345679E+08"

let test =
  TestList
    [
      terse_test
    ]
;;
