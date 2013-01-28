(* A mutable set of elements *)

open Hash_set_intf
open T

type 'a t with sexp_of
(* We use [with sexp_of] but not [with sexp] because we want people to be explicit
   about the hash and comparison functions used when creating hashtables.  One can
   use [Hash_set.Poly.t], which does have [with sexp], to use polymorphic comparison and
   hashing. *)

include Creators
  with type 'a t := 'a t
  with type 'a elt = 'a
  with type ('key, 'z) create_options := ('key, 'z) create_options_with_hashable_required

include Accessors with type 'a t := 'a t with type 'a elt := 'a elt

module type Elt = Core_hashtbl.Key
module type S         = S         with type 'a hash_set = 'a t
module type S_binable = S_binable with type 'a hash_set = 'a t

(* A hash set that uses polymorphic comparison *)
module Poly : sig

  type 'a t with sexp

  include Creators
    with type 'a t := 'a t
    with type 'a elt = 'a
    with type ('key, 'z) create_options := ('key, 'z) create_options_without_hashable

  include Accessors with type 'a t := 'a t with type 'a elt := 'a elt

end with type 'a t = 'a t

module Make (Elt : Elt) : S with type elt = Elt.t

module Make_binable (Elt : sig
  include Elt
  include Binable.S with type t := t
end) : S_binable with type elt = Elt.t
