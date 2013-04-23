open Std_internal

module type Key = sig
  type t with bin_io, sexp
  include Floatable with type t := t
end

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

