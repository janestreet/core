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
  type t [@@deriving bin_io, sexp, compare, hash]
  type bound

  include Gen
    with type 'a t := t
    with type 'a bound := bound (** @inline *)

  (** [create] has the same type as in [Gen], but adding it here prevents a type-checker
      issue with nongeneralizable type variables. *)
  val create : bound -> bound -> t

  type 'a poly_t
  val to_poly : t -> bound poly_t

  type 'a poly_set
  module Set : sig
    type t [@@deriving bin_io, sexp]
    include Gen_set
      with type 'a t := t
      with type 'a bound := bound
    (** @inline *)

    val to_poly : t -> bound poly_set
  end
  with type 'a interval := t
end

module type S1 = sig
  (** This type [t] supports bin-io and sexp conversion by way of the
      [[@@deriving bin_io, sexp]] extensions, which inline the relevant function
      signatures (like [bin_read_t] and [t_of_sexp]). *)
  type 'a t [@@deriving bin_io, sexp, compare, hash]

  include Gen
    with type 'a t := 'a t
    with type 'a bound := 'a (** @inline *)

  module Set : sig
    type 'a t [@@deriving bin_io, sexp]
    include Gen_set
      with type 'a t := 'a t
      with type 'a bound := 'a
      (** @inline *)
  end
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

(** Module for simple closed intervals over arbitrary types. Used by calling the
    {{!module:Core.Interval.Make}[Make]} functor with a type that satisfies
    {{!module:Base.Comparable}[Comparable]} (for correctly ordering elements).

    Note that the actual interface for intervals is in
    {{!modtype:Core__.Interval_intf.Gen}[Interval_intf.Gen]}, following a Core pattern of
    defining an interface once in a [Gen] module, then reusing it across monomorphic ([S])
    and polymorphic ([S1], [S2], ... [SN]) variants, where [SN] denotes a signature of N
    parameters. Here, [S1] is included in this module because the signature of one ['a]
    parameter is the default.

    See the documentation of {{!module:Core.Interval.Make}[Interval.Make]} for a more
    detailed usage example. *)
module type Interval = sig

  (**
     {2 Intervals using polymorphic compare}

     This part of the interface is for polymorphic intervals, which are well ordered by
     polymorphic compare. Using this with types that are not (like sets) will lead to crazy
     results.
  *)
  include S1 (** @inline *)

  (** {2 Type-specialized intervals}

      The module type [S] is used to define signatures for intervals over a specific type,
      like [Interval.Ofday] (whose bounds are [Time.Ofday.t]) or [Interval.Float], whose
      bounds are floats.

      Note the heavy use of destructive substitution, which removes the redefined type or
      module from the signature. This allows for clean type constraints in codebases, like
      Core's, where there are lots of types going by the same name (e.g., "t").
  *)

  (** {3 Signatures }

      The following signatures are used for specifying the types of the type-specialized
      intervals.

  *)

  module type S1 = S1

  module type S = S
    with type 'a poly_t := 'a t
    with type 'a poly_set := 'a Set.t

  (**
     [S_time] is a signature that's used below to define the interfaces for [Time] and
     [Time_ns] without duplication.
  *)
  module type S_time = S_time
    with type 'a poly_t := 'a t
    with type 'a poly_set := 'a Set.t

  (** {3 Specialized interval types} *)

  module Ofday    : S with type bound = Time.Ofday.t
  module Ofday_ns : S with type bound = Time_ns.Ofday.t

  module Time    : S_time with module Time := Time
                           and type t = Time.t t
  module Time_ns : S_time with module Time := Time_ns
                           and type t = Time_ns.t t

  module Float : S with type bound = Float.t

  module Int : sig
    include S with type bound = Int.t (** @open *)

    include Container.S0        with type t := t with type elt := bound
    include Binary_searchable.S with type t := t with type elt := bound

    (**/**)
    (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

      https://opensource.janestreet.com/standards/#private-submodules *)
    module Private : sig
      val get : t -> int -> int
    end
  end

  (** [Interval.Make] is a functor that takes a type that you'd like to create intervals for
      and returns a module with functions over intervals of that type.

      For example, suppose you had a [Percent.t] type and wanted to work with intervals over
      it, i.e., inclusive ranges like 40-50% or 0-100%. You would create your
      [Percent_interval] module by calling:

      {[module Percent_interval = Interval.Make(Percent)]}

      You now have a module with lots of functionality ready to use. For instance you could
      call [Percent_interval.empty] to create an empty interval, or:

      {[Percent_interval.create (Percent.of_percentage 3) (Percent.of_percentage 30)]}

      to get an actual interval that ranges from [3%] to [30%]. You can then ask questions
      of this interval, like whether it's a {{!val:is_subset} subset} of another interval or
      whether it {!val:contains} a particular value.

      NB. In order to use the [Interval.Make] functor, your type must satisfy
      Comparable and support bin-io and s-expression conversion. At a minimum, then,
      [Percent] must look like this:

      {[
        module Percent = struct
          module T = struct
            type t = float [@@deriving bin_io, compare, sexp]
          end
          include T
          include Comparable.Make_binable(T)
        end
      ]}
  *)
  module Make (Bound : sig
      type t [@@deriving bin_io, sexp, hash]
      include Comparable.S with type t := t
    end)
    : S with type bound = Bound.t

  (**
     [Stable] is used to build stable protocols. It ensures backwards compatibility by
     checking the sexp and bin-io representations of a given module. Here it's also applied
     to the [Float], [Int], [Time], [Time_ns], and [Ofday] intervals.
  *)
  module Stable : sig
    module V1 : sig
      type nonrec 'a t = 'a t [@@deriving bin_io, compare, sexp]
      module Float   : Stable with type t = Float.  t
      module Int     : Stable with type t = Int.    t
      module Time    : Stable with type t = Time.   t
      module Time_ns : Stable with type t = Time_ns.t
      module Ofday   : Stable with type t = Ofday.  t

      (**/**)
      (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

        https://opensource.janestreet.com/standards/#private-submodules *)
      module Private : sig
        type 'a t =
          | Interval of 'a * 'a
          | Empty
        [@@deriving compare, variants]

        val to_float : float t -> Float.t
        val to_int : int t -> Int.t
        val to_ofday : Core_kernel.Time.Ofday.t t -> Ofday.t
        val to_time : Core_kernel.Time.t t -> Time.t
      end
    end
  end
end
