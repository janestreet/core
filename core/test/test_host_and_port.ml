open! Core
open! Import

let%expect_test "validate sexp grammar" =
  require_ok
    [%here]
    (Sexp_grammar_validation.validate_grammar
       (module struct
         type t = Host_and_port.Stable.V1.t [@@deriving quickcheck, sexp, sexp_grammar]
       end));
  [%expect {| (Union (String (List (Cons String (Cons Integer Empty))))) |}]
;;
