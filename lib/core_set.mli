open T
open Core_set_intf

type ('elt, 'comparator) t
type ('elt, 'comparator) tree
type 'a elt = 'a

include Creators
  with type ('a, 'b) set := ('a, 'b) t
  with type ('a, 'b) t := ('a, 'b) t
  with type ('a, 'b) tree := ('a, 'b) tree
  with type 'a elt := 'a elt
  with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) create_options_with_comparator

include Accessors
  with type ('a, 'b) t := ('a, 'b) t
  with type ('a, 'b) tree := ('a, 'b) tree
  with type 'a elt := 'a elt

module Poly : sig
  type ('a, 'b) set
  type 'elt t = ('elt, Comparator.Poly.comparator) set with bin_io, sexp
  type ('a, 'b) t_ = 'a t

  include Creators
    with type ('a, 'b) set := ('a, 'b) set
    with type ('a, 'b) t := ('a, 'b) t_
    with type ('a, 'b) tree := ('a, 'b) tree
    with type 'a elt := 'a elt
    with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) create_options_without_comparator

  (* [empty] has the same spec in [Creators], but adding it here prevents a type-checker
     issue with nongeneralizable type variables. *)
  val empty : _ t

  include Accessors
    with type ('a, 'b) t := ('a, 'b) t_
    with type ('a, 'b) tree := ('a, 'b) tree
    with type 'a elt := 'a elt
end
  with type ('a, 'b) set := ('a, 'b) t

module type Elt = Elt
module type Elt_binable = Elt_binable
module type S         = S         with type ('a, 'b) set := ('a, 'b) t
module type S_binable = S_binable with type ('a, 'b) set := ('a, 'b) t

module Make (Elt : Elt) : S with type Elt.t = Elt.t

module Make_using_comparator (Elt : Comparator.S)
  : S with type Elt.t = Elt.t
      with type Elt.comparator = Elt.comparator

module Make_binable (Elt : Elt_binable) : S_binable with type Elt.t = Elt.t

module Make_binable_using_comparator (Elt : Comparator.S_binable)
  : S_binable
      with type Elt.t = Elt.t
      with type Elt.comparator = Elt.comparator
