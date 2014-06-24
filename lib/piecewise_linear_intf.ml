open Core_kernel.Std

module type Float_like = sig
  type t with bin_io, sexp
  include Floatable with type t := t
end

module type S = sig
  type ('k, 'v) t_
  type key
  type value

  type t = (key, value) t_ with bin_io, sexp, compare

  (** [create] enforces that x (key) values are non-decreasing.

      It also enforces certain finiteness conditions: the x and y values must be finite
      (non-nan, and non-infinite), and differences of consecutive x values and consecutive
      y values must be finite.
  *)
  val create : (key * value) list -> t Or_error.t



  (** [get t x] evaluates the piecewise linear function [t] at [x].

      It is possible to get discontinuous functions by using repeated x-values in the
      knots.  In that case, the function is evaluated in such a way that it is
      right-continuous.  For example, if [t] has knots
      [[(0.,0.5); (1.,1.5); (1.,10.); (2.,11.)]], then [get t 1.] returns [10.],
      [get t 0.999] returns [1.499], and [get t 1.001] returns [10.001].
  *)
  val get : t -> key -> value

  (* O(n) *)
  val to_knots : t -> (key * value) list

  (* O(n) *)
  val to_knots' : t -> key array * value array
end

module type S_invertible = sig
  (** [create] enforces that the x (key) values are strictly increasing.  It also enforces
      that the y (value) values are either strictly increasing or strictly
      decreasing. These two properties give us invertibility.

      It also enforces certain finiteness conditions: the x and y values must be finite
      (non-nan, and non-infinite), and differences of consecutive x values and consecutive
      y values must be finite.

      (Conceivably, one might have a case where one wants to loosen the conditions on
      x and y values to non-strict monotonicity, so that one does not have true
      invertibility, but only a sort of formal invertibility.  If that use case
      arises, a separate functor like [Make_formally_invertible] could be added,
      so that [Make_invertible] maintains its stricter semantics.)
  *)
  include S

  (** [get_inverse t value] is the inverse operation of [get t key]. *)
  val get_inverse : t -> value -> key
end
