open Std_internal

(** Module for simple closed intervals over arbitrary types that are ordered
    correctly using polymorphic compare. *)

module type S = Interval_intf.S
module type S1 = Interval_intf.S1

include Interval_intf.S1

module Make(M : sig
  type t
  include Comparable.S with type t := t
  (* Sexps are () for empty interval and (3 5) for an interval containing 3, 4, and 5. *)
  include Sexpable.S with type t := t
  include Binable.S with type t := t
end)
  : S with type bound = M.t and type 'a poly_t = M.t t

module Float : S with type bound = Float.t    and type 'a poly_t = Float.t    t
module Int   : S with type bound = Core_int.t and type 'a poly_t = Core_int.t t
module Time : sig
  include      S with type bound = Time.t     and type 'a poly_t = Time.t     t

  (** [create_ending_after ?zone (od1, od2) ~now] returns the smallest interval [(t1 t2)]
      with minimum [t2] such that [t2 >= now], [to_ofday t1 = od1], and [to_ofday t2 =
      od2].  If zone is specified, it is used to translate od1 and od2 into times,
      otherwise the machine's time zone is used.  It is not guaranteed that [contains (t1
      t2) now], which will be false iff there is no interval containing [now] with
      [to_ofday t1 = od1] and [to_ofday t2 = od1] . *)
  val create_ending_after : ?zone:Zone.t -> Ofday.t * Ofday.t -> now:Time.t -> t

  (** [create_ending_before ?zone (od1, od2) ~ubound] returns the smallest interval [(t1
      t2)] with maximum [t2] such that [t2 <= ubound], [to_ofday t1 = od1], and [to_ofday
      t2 = od2]. If zone is specified, it is used to translate od1 and od2 into times,
      otherwise the machine's time zone is used. *)
  val create_ending_before : ?zone:Zone.t -> Ofday.t * Ofday.t -> ubound:Time.t -> t
end
