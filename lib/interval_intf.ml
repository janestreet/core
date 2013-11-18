module type Gen = sig
  type 'a t

  (* [bound] is the type of points in the interval (and also of the bounds, which are
     points; hence the name).  [bound] is instantiated in two different ways below: in
     [module type S] as a monotype and in [module type S1] as ['a]. *)
  type 'a bound

  (** Module for simple closed intervals over arbitrary types that are ordered correctly
      using polymorphic compare. *)

  (** [create l u] returns the interval with lower bound [l] and upper bound [u], unless
      [l > u], in which case [create] returns the empty interval. *)
  val create : 'a bound -> 'a bound -> 'a t

  val empty : 'a t

  val intersect : 'a t -> 'a t -> 'a t

  val is_empty : 'a t -> bool

  val is_empty_or_singleton : 'a t -> bool

  val bounds : 'a t -> ('a bound * 'a bound) option
  val lbound : 'a t -> 'a bound option
  val ubound : 'a t -> 'a bound option

  val bounds_exn : 'a t -> ('a bound * 'a bound)
  val lbound_exn : 'a t -> 'a bound
  val ubound_exn : 'a t -> 'a bound

  (** [convex_hull ts] returns an interval whose upperbound is the greatest upperbound of
      the intervals in the list, and whose lowerbound is the least lowerbound of the
      list. *)
  val convex_hull : 'a t list -> 'a t

  val contains : 'a t -> 'a bound -> bool

  val compare_value : 'a t -> 'a bound ->
    [ `Below | `Within | `Above | `Interval_is_empty ]

  (** [bound t x] returns [None] iff [is_empty t].  If [bounds t = Some (a, b)], then
      [bound] returns [Some y] where [y] is the element of [t] closest to [x].  I.e.:

      |  y = a  if x < a
      |  y = x  if a <= x <= b
      |  y = b  if x > b
  *)
  val bound : 'a t -> 'a bound -> 'a bound option

  (** [is_superset i1 of_:i2] is whether i1 contains i2.  The empty interval is
      contained in every interval. *)
  val is_superset : 'a t -> of_:'a t -> bool
  val is_subset   : 'a t -> of_:'a t -> bool

  (** [map t ~f] returns [create (f l) (f u)] if [bounds t = Some (l, u)], and [empty] if
      [t] is empty.  Note that if [f l > f u], the result of [map] is [empty], by the
      definition of [create].

      If one thinks of an interval as a set of points, rather than a pair of its bounds,
      then [map] is not the same as the usual mathematical notion of mapping [f] over that
      set.  For example, [~f:(fun x -> x * x)] maps the interval {v [-1,1] v} to {v [1,1]
      v}, not to {v [0,1] v}. *)
  val map : 'a t -> f:('a bound -> 'b bound) -> 'b t

  (** [are_disjoint ts] returns [true] iff the intervals in [ts] are pairwise disjoint. *)
  val are_disjoint : 'a t list -> bool

  (** Returns true iff a given set of intervals would be disjoint if considered as open
      intervals.  i.e.,  (3,4) and (4,5) would count as disjoint. *)
  val are_disjoint_as_open_intervals : 'a t list -> bool

  (** Assuming that [ilist1] and [ilist2] are lists of (disjoint) intervals,
      [list_intersect ilist1 ilist2] returns the list of disjoint intervals that
      correspond to the intersection of [ilist1] with [ilist2]. *)
  val list_intersect : 'a t list -> 'a t list -> 'a t list

  (* Returns true if the intervals, when considered as half-open-intervals, nestle up
     cleanly one to the next.  i.e., if you sort the intervals by the lower bound, then
     the upper bound of the nth interval is equal to the lower bound of the n+1th
     interval.  The intervals do not need to partition the entire space, they just
     need to partition their union. *)
  val half_open_intervals_are_a_partition : 'a t list -> bool
end

module type Gen_set = sig
  type 'a t
  type 'a bound
  type 'a interval
  (* An interval set is a set of nonempty disjoint intervals. *)

  (* [create] creates an interval set containing values between each pair of
     values.  It is an error if the pairs overlap. *)
  val create : ('a bound * 'a bound) list -> 'a t

  (* [create_from_intervals] creates an interval set.  Empty intervals are
     dropped.  It is an error if the nonempty intervals are not disjoint. *)
  val create_from_intervals : 'a interval list -> 'a t

  val contains : 'a t -> 'a bound -> bool

  val contains_set : container:('a t) -> contained:('a t) -> bool

  (* The largest and smallest element of the interval set, respectively.  Raises
     Invalid_argument on empty sets. *)
  val ubound_exn : 'a t -> 'a bound
  val lbound_exn : 'a t -> 'a bound

  val ubound : 'a t -> 'a bound option
  val lbound : 'a t -> 'a bound option
end

module type S = sig
  type t with bin_io, sexp
  type bound

  type 'a t_ = t
  type 'a bound_ = bound
  include Gen
    with type 'a t := 'a t_
    with type 'a bound := 'a bound_
  (* [create] has the same type as in [Gen], but adding it here prevents a type-checker
     issue with nongeneralizable type variables. *)
  val create : bound -> bound -> t

  type 'a poly_t
  val to_poly : t -> bound poly_t

  type 'a poly_set
  module Set : sig
    type t with bin_io, sexp
    type 'a t_ = t
    include Gen_set
      with type 'a t := 'a t_
      with type 'a bound := 'a bound_

    val to_poly : t -> bound poly_set
  end
    with type 'a interval := 'a t_
end

module type S1 = sig
  type 'a t with bin_io, sexp
  type 'a bound_ = 'a

  include Gen
    with type 'a t := 'a t
    with type 'a bound := 'a bound_

  module Set : sig
    type 'a t with bin_io, sexp
    include Gen_set with type 'a t := 'a t
  end
    with type 'a bound := 'a bound_
    with type 'a interval := 'a t
end
