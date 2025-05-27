open! Import

type t = Base.Ordering.t =
  | Less
  | Equal
  | Greater
[@@deriving bin_io, compare ~localize, hash, sexp, sexp_grammar]

module type Base_mask = module type of Base.Ordering with type t := t

include (Base.Ordering : Base_mask)
