open! Core
open! Import
open! Signal

let%test_unit _ =
  [%test_eq: string] (to_string bus) "sigbus";
  [%test_eq: string] (Sexp.to_string (sexp_of_t bus)) {|"<unknown signal -22>"|};
  [%test_eq: string]
    (Sexp.to_string (Stable.V1.sexp_of_t bus))
    {|"<unknown signal -22>"|};
  [%test_eq: string] (Sexp.to_string (Stable.V2.sexp_of_t bus)) "sigbus"
;;
