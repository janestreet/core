open! Import

module type Float_like = sig
  type t [@@deriving bin_io, sexp]
  include Floatable with type t := t
end

module type S = sig
  type ('k, 'v) t_
  type key
  type value

  type t = (key, value) t_ [@@deriving bin_io, sexp, compare]

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

  (** O(1) *)
  val first_knot : t -> (key * value) option
  val last_knot  : t -> (key * value) option

  (** O(n) *)
  val to_knots : t -> (key * value) list

  (** O(n) *)
  val to_knots' : t -> key array * value array


  (** [precache t] computes and stores a lookup table in [t] that speeds up subsequent
      calls to [get t].  Any call to [get] needs to find the knots that define the
      interval in which the key lies.  This is done by bisection.  Ordinarily the
      bisection starts on the whole domain of the piecewise linear function.  Precaching
      builds a lookup table based on an equispaced division of the domain.  This allows
      [get] to quickly determine a (potentially very) small initial interval on which to
      start the bisection.

      This works best for knots that are reasonably evenly distributed.

      [density] is the ratio of the size of the lookup table to the size of the knot
      array.

      Calling [precache] multiple times is safe.  If the existing lookup density is the
      same or higher density than the requested density, the lookup table will not be
      recomputed. *)
  val precache
    :  ?density:float  (** default is [1] *)
    -> t
    -> unit

  (** Returns the [t] such that [get t key] = sum ([get t_i key]) * [weight_i]. This will
      fail if given an empty list as an argument, if any weights are not finite, or if
      any of the input [t]s has a discontinuity.

      The domain of each [t] does not have to be the same. The domain of the [t] that is
      returned will be the connected union of the domains.

      There are cases in [S_invertible] in which all [t]s could be valid and invertible,
      but the linear combination is not invertible. I.e. if one [t] is downward sloping,
      the other [t] is upward sloping, and the linear combination is sometimes upward and
      sometimes downward sloping.
  *)
  val create_from_linear_combination : (t * float) list -> t Or_error.t
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
