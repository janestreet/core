open! Import

type never_returns = Nothing.t [@@deriving sexp_of]

let%template never_returns : never_returns -> (_ : k) = function
  | _ -> .
[@@kind k = (base_or_null, value_or_null & value_or_null)]
;;
