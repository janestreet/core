(** This module extends {{!Base.Lazy}[Base.Lazy]}. *)

open! Import

type 'a t = 'a Base.Lazy.t
[@@deriving
  bin_io ~localize, compare ~localize, hash, quickcheck, sexp, sexp_grammar, typerep]

include module type of Base.Lazy with type 'a t := 'a t (** @inline *)

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io ~localize, equal, sexp_grammar]

    include Stable_module_types.With_stable_witness.S1 with type 'a t := 'a t
    include Ppx_compare_lib.Comparable.S_local1 with type 'a t := 'a t
  end
end
