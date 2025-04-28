open! Import

type never_returns = Nothing.t [@@deriving sexp_of]

let%template never_returns : never_returns -> _ = function
  | _ -> .
[@@kind k = (value, float64, bits32, bits64, word)]
;;
