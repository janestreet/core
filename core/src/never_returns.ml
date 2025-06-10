open! Import

type never_returns = Nothing.t [@@deriving sexp_of]

let%template never_returns : never_returns -> (_ : k) = function
  | _ -> .
[@@kind k = (value_or_null, float64, bits32, bits64, word)]
;;
