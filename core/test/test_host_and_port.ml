open! Core
open! Import

let%expect_test "validate sexp grammar" =
  require_ok
    (Sexp_grammar_validation.validate_grammar
       (module struct
         type t = Host_and_port.Stable.V1.t [@@deriving quickcheck, sexp, sexp_grammar]
       end));
  [%expect {| (Union (String (List (Cons String (Cons Integer Empty))))) |}]
;;

let%expect_test "test [Hide_port_in_test.to_string]" =
  print_endline
    (Host_and_port.Hide_port_in_test.to_string
       (Host_and_port.create ~host:"localhost" ~port:1234));
  [%expect {| localhost:PORT |}]
;;
