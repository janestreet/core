open! Import
open Import_time
module Time_ns = Core_time_ns
module Zone = Time.Zone

module type Gen = sig
  type 'a t

  (** [bound] is the type of points in the interval (and therefore of the bounds).
      [bound] is instantiated in two different ways below: in [module type S] as a
      monotype and in [module type S1] as ['a]. *)
  type 'a bound

  (** [create l u] returns the interval with lower bound [l] and upper bound [u], unless
      [l > u], in which case it returns the empty interval. *)
  val create : 'a bound -> 'a bound -> 'a t

  val empty : 'a t

  val intersect : 'a t -> 'a t -> 'a t

  val is_empty : 'a t -> bool

  val is_empty_or_singleton : 'a t -> bool

  (*_ If you are looking for a simple interval type where the bounds are not optional,
    consider Min_max_pair.t. *)
  val bounds : 'a t -> ('a bound * 'a bound) option
  val lbound : 'a t -> 'a bound option
  val ubound : 'a t -> 'a bound option

  val bounds_exn : 'a t -> ('a bound * 'a bound)
  val lbound_exn : 'a t -> 'a bound
  val ubound_exn : 'a t -> 'a bound

  (** [convex_hull ts] returns an interval whose upper bound is the greatest upper bound
      of the intervals in the list, and whose lower bound is the least lower bound of the
      list.

      Suppose you had three intervals [a], [b], and [c]:

      {v
             a:  (   )
             b:    (     )
             c:            ( )

          hull:  (           )
      v}

      In this case the hull goes from [lbound_exn a] to [ubound_exn c].
  *)
  val convex_hull : 'a t list -> 'a t

  val contains : 'a t -> 'a bound -> bool

  val compare_value : 'a t -> 'a bound ->
    [ `Below | `Within | `Above | `Interval_is_empty ]

  (** [bound t x] returns [None] iff [is_empty t].  If [bounds t = Some (a, b)], then
      [bound] returns [Some y] where [y] is the element of [t] closest to [x].  I.e.:

      {v
        y = a  if x < a
        y = x  if a <= x <= b
        y = b  if x > b
      v}
  *)
  val bound : 'a t -> 'a bound -> 'a bound option

  (** [is_superset i1 of_:i2] is whether i1 contains i2. The empty interval is
      contained in every interval. *)
  val is_superset : 'a t -> of_:'a t -> bool
  val is_subset   : 'a t -> of_:'a t -> bool

  (** [map t ~f] returns [create (f l) (f u)] if [bounds t = Some (l, u)], and [empty] if
      [t] is empty.  Note that if [f l > f u], the result of [map] is [empty], by the
      definition of [create].

      If you think of an interval as a set of points, rather than a pair of its bounds,
      then [map] is not the same as the usual mathematical notion of mapping [f] over that
      set. For example, [map ~f:(fun x -> x * x)] maps the interval [[-1,1]] to [[1,1]],
      not to [[0,1]]. *)
  val map : 'a t -> f:('a bound -> 'b bound) -> 'b t

  (** [are_disjoint ts] returns [true] iff the intervals in [ts] are pairwise disjoint. *)
  val are_disjoint : 'a t list -> bool

  (** Returns true iff a given set of intervals would be disjoint if considered as open
      intervals, e.g., [(3,4)] and [(4,5)] would count as disjoint according to this
      function. *)
  val are_disjoint_as_open_intervals : 'a t list -> bool

  (** Assuming that [ilist1] and [ilist2] are lists of disjoint intervals, [list_intersect
      ilist1 ilist2] considers the intersection [(intersect i1 i2)] of every pair of
      intervals [(i1, i2)], with [i1] drawn from [ilist1] and [i2] from [ilist2],
      returning just the non-empty intersections. By construction these intervals will be
      disjoint, too. For example:

      {[
        let i = Interval.create;;
        list_intersect [i 4 7; i 9 15] [i 2 4; i 5 10; i 14 20];;
        [(4, 4), (5, 7), (9, 10), (14, 15)]
      ]}

      Raises an exception if either input list is non-disjoint.
  *)
  val list_intersect : 'a t list -> 'a t list -> 'a t list

  (** Returns true if the intervals, when considered as half-open intervals, nestle up
      cleanly one to the next. I.e., if you sort the intervals by the lower bound,
      then the upper bound of the [n]th interval is equal to the lower bound of the
      [n+1]th interval. The intervals do not need to partition the entire space, they just
      need to partition their union.
  *)
  val half_open_intervals_are_a_partition : 'a t list -> bool
end

module type Gen_set = sig
  type 'a t
  type 'a bound
  type 'a interval
  (** An interval set is a set of nonempty disjoint intervals. *)

  (** [create] creates an interval set containing intervals whose lower and upper bounds
      are given by the pairs passed to the function. It is an error if the pairs overlap.
  *)
  val create : ('a bound * 'a bound) list -> 'a t

  (** [create_from_intervals] creates an interval set. Empty intervals are dropped. It is
      an error if the nonempty intervals are not disjoint. *)
  val create_from_intervals : 'a interval list -> 'a t

  val contains : 'a t -> 'a bound -> bool

  (** [contains_set] returns true iff for every interval in the contained set, there
      exists an interval in the container set that is its superset.
  *)
  val contains_set : container:('a t) -> contained:('a t) -> bool

  (** The largest and smallest element of the interval set, respectively.  Raises
      Invalid_argument on empty sets. *)
  val ubound_exn : 'a t -> 'a bound
  val lbound_exn : 'a t -> 'a bound

  val ubound : 'a t -> 'a bound option
  val lbound : 'a t -> 'a bound option
end

module type S = sig
  type t [@@deriving bin_io, sexp]
  type bound

  type 'a t_ = t
  type 'a bound_ = bound
  include Gen
    with type 'a t := 'a t_
    with type 'a bound := 'a bound_ (** @inline *)

  (** [create] has the same type as in [Gen], but adding it here prevents a type-checker
      issue with nongeneralizable type variables. *)
  val create : bound -> bound -> t

  type 'a poly_t
  val to_poly : t -> bound poly_t

  type 'a poly_set
  module Set : sig
    type t [@@deriving bin_io, sexp]
    type 'a t_ = t
    include Gen_set
      with type 'a t := 'a t_
      with type 'a bound := 'a bound_ (** @inline *)

    val to_poly : t -> bound poly_set
  end
  with type 'a interval := 'a t_
end

module type S1 = sig
  (** This type [t] supports bin-io and sexp conversion by way of the
      [[@@deriving bin_io, sexp]] extensions, which inline the relevant function
      signatures (like [bin_read_t] and [t_of_sexp]). *)
  type 'a t [@@deriving bin_io, sexp]
  type 'a bound_ = 'a

  include Gen
    with type 'a t := 'a t
    with type 'a bound := 'a bound_ (** @inline *)

  module Set : sig
    type 'a t [@@deriving bin_io, sexp]
    include Gen_set with type 'a t := 'a t (** @inline *)
  end
  with type 'a bound := 'a bound_
  with type 'a interval := 'a t
end

module type S_time = sig
  module Time : sig
    type t
    module Ofday : sig
      type t
    end
  end

  include S with type bound = Time.t (** @open *)


  (** [create_ending_after ?zone (od1, od2) ~now] returns the smallest interval [(t1 t2)]
      with minimum [t2] such that [t2 >= now], [to_ofday t1 = od1], and [to_ofday t2 =
      od2]. If a zone is specified, it is used to translate [od1] and [od2] into times,
      otherwise the machine's time zone is used.

      It is not guaranteed that the interval will contain [now]: for instance if it's
      11:15am, [od1] is 12pm, and [od2] is 2pm, the returned interval will be 12pm-2pm
      today, which obviously doesn't include 11:15am. In general [contains (t1 t2) now]
      will only be true when now is between [to_ofday od1] and [to_ofday od2].

      You might want to use this function if, for example, there's a daily meeting from
      10:30am-11:30am and you want to find the next instance of the meeting, relative to
      now. *)
  val create_ending_after :
    ?zone:Zone.t -> Time.Ofday.t * Time.Ofday.t -> now:Time.t -> t

  (** [create_ending_before ?zone (od1, od2) ~ubound] returns the smallest interval [(t1
      t2)] with maximum [t2] such that [t2 <= ubound], [to_ofday t1 = od1], and [to_ofday
      t2 = od2]. If a zone is specified, it is used to translate [od1] and [od2] into
      times, otherwise the machine's time zone is used.


      You might want to use this function if, for example, there's a lunch hour from
      noon to 1pm and you want to find the first instance of that lunch hour (an interval)
      before [ubound]. The result will either be on the same day as [ubound], if
      [to_ofday ubound] is after 1pm, or the day before, if [to_ofday ubound] is any
      earlier. *)
  val create_ending_before :
    ?zone:Zone.t -> Time.Ofday.t * Time.Ofday.t -> ubound:Time.t -> t
end
