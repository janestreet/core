open! Core
open! Source_code_position.With_hiding

let%expect_test "[to_string]" =
  print_endline (to_string [%here]);
  [%expect {| lib/core/test/test_source_code_position.ml:LINE:COL |}]
;;

let%expect_test "[sexp_of_t]" =
  print_s [%sexp ([%here] : t)];
  [%expect {| lib/core/test/test_source_code_position.ml:LINE:COL |}]
;;
