open! Core
open! Import

let%expect_test "[raise_s sexp] raises an exn whose [sexp_of_t] is [sexp]" =
  let sexp = [%sexp "foo"] in
  match Error.raise_s sexp with
  | (_ : Nothing.t) -> .
  | exception exn -> require (phys_equal [%sexp (exn : exn)] sexp)
;;

let%expect_test "[failwiths] handles [Lexing.dummy_pos]" =
  (* Location information should be excluded when [here] is [Lexing.dummy_pos], which is
     the default value of [here] in the external version of Base for [%call_pos]
     arguments *)
  require_does_raise ~hide_positions:true (fun () ->
    Error.failwiths "hello world" 13 sexp_of_int);
  [%expect {| ("hello world" 13 lib/core/test/test_error.ml:LINE:COL) |}];
  require_does_raise ~hide_positions:true (fun () ->
    Error.failwiths ~here:Lexing.dummy_pos "hello world" 13 sexp_of_int);
  [%expect {| ("hello world" 13) |}]
;;
