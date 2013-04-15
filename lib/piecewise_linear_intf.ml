open Std_internal

module type Key = sig
  type t with bin_io, sexp
  include Floatable with type t := t
end

(* CR-soon chardin for chardin: The module type Value is identical to module type Key, and
   it is not just a coincidence that it is identical to Key: The whole premise of
   S_invertible is that we can reverse the roles of the module parameters Key and Value.
   So, I don't think we should introduce a separate module type Value, when it is
   *logically* (not just incidentally) identical to the module type Key.  Put another way:
   The module *parameters* Key and Value (in Make and Make_invertible) are logically
   distinct, but their underlying module *types* are identical by design, and the
   interface should reflect that the module types are identical.

   That said, it would look a bit odd to have Make (Key : Key) (Value : Key).
   Should we just have a single module type Floatable_binable_sexpable, and
   have Make (Key : Floatable_binable_sexpable) (Value : Floatable_binable_sexpable)
   in the interface?  (Though it is a bit wordy, users of the module don't actually have
   to write Floatable_binable_sexpable anywhere in their code--they just have to use
   Key and Value modules that are in fact floatable, binable, and sexpable, as they
   already have to.)

   yminsky: this seems reasonable, but we should unblock this feature.  Making into a
   CR-soon, and this can go in your upcoming feature with the binomial search.

   chardin: Okay.

   mrussell: For what it's worth, I agree your proposal is cleaner than my implementation.
*)
module type Value = sig
  type t with bin_io, sexp
  include Floatable with type t := t
end

module type S = sig
  type key
  type value

  type t with bin_io, sexp, compare

  (** [create] enforces that key values are non-decreasing. *)
  val create : (key * value) list -> t Or_error.t
  val get : t -> key -> value
  val to_knots : t -> (key * value) list
end

module type S_invertible = sig
  (** [create] enforces that the x (key) values are strictly increasing.  It also enforces
      that the y (value) values are either strictly increasing or strictly
      decreasing. These two properties give us invertibility.
  *)
  include S

  (** [get_inverse t value] is the inverse operation of [get t key].
  *)
  val get_inverse : t -> value -> key
end

