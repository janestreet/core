(** This module extends {{!Base.Lazy}[Base.Lazy]}. *)

open! Import

type 'a t = 'a Base.Lazy.t
[@@deriving bin_io, compare, hash, quickcheck, sexp, sexp_grammar, typerep]

include module type of Base.Lazy with type 'a t := 'a t (** @inline *)

module Stable : sig
  module V1 : Stable_module_types.S1 with type 'a t = 'a t
end
