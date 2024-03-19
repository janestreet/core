open! Import
include Ofday_intf.S with type underlying = float and module Span := Span_float

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving bin_io, compare, hash, sexp, sexp_grammar, stable_witness, diff]
  end
end
