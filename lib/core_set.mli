(** This module defines the [Set] module for [Core.Std].  We use "core_set" as the file
    name rather than "set" to avoid conflicts with OCaml's standard set module.

    This module uses the same organizational approach as [Core_map].  See the
    documentation in core_map.mli for a description of the approach. *)

open Core_set_intf

type ('elt, 'comparator) t with compare
type ('elt, 'comparator) tree
type 'a elt = 'a

include Creators
  with type ('a, 'b) t    := ('a, 'b) t
  with type ('a, 'b) set  := ('a, 'b) t
  with type ('a, 'b) tree := ('a, 'b) tree
  with type 'a elt := 'a elt
  with type ('a, 'b, 'c) options := ('a, 'b, 'c) with_comparator

include Accessors
  with type ('a, 'b) t    := ('a, 'b) t
  with type ('a, 'b) tree := ('a, 'b) tree
  with type 'a elt := 'a elt
  with type ('a, 'b, 'c) options := ('a, 'b, 'c) without_comparator

val comparator : ('a, 'comparator) t -> ('a, 'comparator) Comparator.t

module Poly : sig
  type ('a, 'b) set
  type 'elt t = ('elt, Comparator.Poly.comparator) set with bin_io, compare, sexp
  type ('a, 'b) t_ = 'a t

  include Creators_and_accessors
    with type ('a, 'b) t   := ('a, 'b) t_
    with type ('a, 'b) set := ('a, 'b) set
    with type 'a elt := 'a elt
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) without_comparator

  (* [empty] has the same spec in [Creators_and_accessors], but adding it here prevents a
     type-checker issue with nongeneralizable type variables. *)
  val empty : _ t

  module Tree : sig
    type 'elt t = ('elt, Comparator.Poly.comparator) tree with sexp
    type ('a, 'b) t_ = 'a t

    include Creators_and_accessors
      with type ('a, 'b) t   := ('a, 'b) t_
      with type ('a, 'b) set := ('a, 'b) tree
      with type 'a elt := 'a elt
      with type ('a, 'b, 'c) options := ('a, 'b, 'c) without_comparator
  end
end
  with type ('a, 'b) set := ('a, 'b) t

module type Elt = Elt

module type Elt_binable = Elt_binable

module type S = S
  with type ('a, 'b) set  = ('a, 'b) t
  with type ('a, 'b) tree = ('a, 'b) tree

module type S_binable = S_binable
  with type ('a, 'b) set  = ('a, 'b) t
  with type ('a, 'b) tree = ('a, 'b) tree

module Make (Elt : Elt) : S with type Elt.t = Elt.t

module Make_using_comparator (Elt : Comparator.S)
  : S with type Elt.t = Elt.t
    with type Elt.comparator = Elt.comparator

module Make_binable (Elt : Elt_binable) : S_binable with type Elt.t = Elt.t

module Make_binable_using_comparator (Elt : Comparator.S_binable)
  : S_binable with type Elt.t = Elt.t
    with type Elt.comparator = Elt.comparator

module Tree : sig
  type ('a, 'comparator) t = ('a, 'comparator) tree with sexp_of

  include Creators_and_accessors
    with type ('a, 'b) t := ('a, 'b) t
    with type 'a elt := 'a elt
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) with_comparator
end
