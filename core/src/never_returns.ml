open! Import

type never_returns = Nothing.t [@@deriving sexp_of]

let never_returns = Nothing.unreachable_code
