open! Import

type never_returns = Nothing.t [@@deriving sexp_of]

let never_returns : never_returns -> _ = function
  | _ -> .
;;
