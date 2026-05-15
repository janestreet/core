open! Core
open! Import
open! Debug

let%expect_test _ =
  let x = 45 in
  let y = "abc" in
  Debug.am [%here];
  [%expect {| lib/core/test/test_debug.ml:8:11 |}];
  Debug.am_s [%message "hello" (x : int) (y : string)];
  [%expect
    {|
    lib/core/test/test_debug.ml:10:2:
    (hello (x 45) (y abc))
    |}];
  Debug.amf [%here] "hello (%s, %s)" (Int.to_string x) y;
  [%expect
    {|
    lib/core/test/test_debug.ml:16:12:
    hello (45, abc)
    |}];
  Debug.ams [%here] "hello" (x, y) [%sexp_of: int * string];
  [%expect
    {|
    lib/core/test/test_debug.ml:22:12:
    (hello (45 abc))
    |}]
;;
