(** An alias to the [Float.t] type that causes the sexp and bin-io serializers to fail
    when provided with [nan] or [infinity].

    Note that while it makes sense to use this on the definition of a type in the ml
    file, where it will influence the construction of the sexp and bin-io serializers, it
    does {e not} make sense to use this in an mli, since it makes no guarantee at that
    level.
*)

open! Import

type t = float [@@deriving bin_io, sexp, sexp_grammar, compare, hash, equal]

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving bin_io, sexp, sexp_grammar, compare, hash, equal, stable_witness]
  end
end
