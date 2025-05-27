@@ portable

(** This module extends {{!Base.Lazy} [Base.Lazy]}. *)

open! Import

type 'a t = 'a Base.Lazy.t
[@@deriving
  bin_io ~localize
  , compare ~localize
  , hash
  , quickcheck
  , sexp ~localize
  , sexp_grammar
  , typerep]

include module type of Base.Lazy with type 'a t := 'a t (** @inline *)

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t
    [@@deriving bin_io ~localize, compare ~localize, equal ~localize, sexp_grammar]

    include%template
      Stable_module_types.With_stable_witness.S1 [@mode local] with type 'a t := 'a t
  end
end
