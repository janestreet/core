module type T = sig
  type 'a bound
  type 'a t
end

module Gen(T:T) = struct
  open T
  module type S = sig
    (** Module for simple closed intervals over arbitrary types that are ordered correctly
        using polymorphic compare. *)

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

    val contains : 'a t -> 'a bound -> bool

    val compare_value : 'a t -> 'a bound ->
      [ `Below | `Within | `Above | `Interval_is_empty ]

    (** [bound i x] bounds the value x to the interval i.  It returns None if the
        interval is empty and Some x' otherwise, where x' is the bounded value. *)
    val bound : 'a t -> 'a bound -> 'a bound option

    (** [is_superset i1 of_:i2] is whether i1 contains i2.  The empty interval is
        contained in every interval. *)
    val is_superset : 'a t -> of_:'a t -> bool
    val is_subset : 'a t -> of_:'a t -> bool

    val map : f : ('a bound -> 'b bound) -> 'a t -> 'b t

(* val map2 : f : ('a option -> 'b option -> 'c) -> 'a t -> 'b t -> 'c t *)


    (** Returns true iff a given set of intervals are disjoint *)
    val are_disjoint : 'a t list -> bool

    (** Returns true iff a given set of intervals would be disjoint if considered as open
        intervals.  i.e.,  (3,4) and (4,5) would count as disjoint. *)
    val are_disjoint_as_open_intervals : 'a t list -> bool

    (** Assuming that [ilist1] and [ilist2] are lists of (disjoint) intervals,
        [list_intersect ilist1 ilist2] returns the list of disjoint intervals that correspond
        to the intersection of [ilist1] with [ilist2].
    *)
    val list_intersect : 'a t list -> 'a t list -> 'a t list

    (* Returns true if the intervals, when considered as half-open-intervals, nestle up
       cleanly one to the next.  i.e., if you sort the intervals by the lower bound, then
       the upper bound of the nth interval is equal to the lower bound of the n+1th
       interval *)
    val half_open_intervals_are_a_partition : 'a t list -> bool

  end
end

module type T_set = sig
  type 'a t
  type 'a bound
  type 'a interval
end

module GenSet(T : T_set) = struct
  open T
  module type S = sig
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
end


module type S = sig
  type t with bin_io, sexp
  type 'a poly_t with bin_io, sexp

  type interval = t
  type bound

  module T : T with type 'a bound = bound with type 'a t = t
  include Gen(T).S
  val to_poly : t -> bound poly_t

  module Set : sig
    type t with bin_io, sexp
    type 'a poly_t with bin_io, sexp
    module T : T_set
      with type 'a t = t with type 'a interval = interval with type 'a bound = bound
    include GenSet(T).S
    val to_poly : t -> bound poly_t
  end
end

module type S1 = sig
  type 'a t with bin_io, sexp
  type 'a interval = 'a t
  module T : T with type 'a bound = 'a with type 'a t = 'a t
  include Gen(T).S

  module Set : sig
    type 'a t with bin_io, sexp
    module T : T_set
      with type 'a t = 'a t with type 'a interval = 'a interval with type 'a bound = 'a
    include GenSet(T).S
  end
end
