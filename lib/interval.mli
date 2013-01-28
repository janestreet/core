(** Module for simple closed intervals over arbitrary types that are ordered
    correctly using polymorphic compare. *)

open Std_internal
open Interval_intf

module type S1 = S1

include S1

module type S = S
  with type 'a poly_t := 'a t
  with type 'a poly_set := 'a Set.t

module Make (Bound : sig
  (* Sexps are () for empty interval and (3 5) for an interval containing 3, 4, and 5. *)
  type t with bin_io, sexp
  include Comparable.S with type t := t
end)
  : S with type bound = Bound.t

module Float : S with type bound = Float.t
module Int   : S with type bound = Core_int.t

module Time : sig
  include S with type bound = Time.t

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

(* The spec for [Ofday] must be below the spec for [Time], so as not to shadow the uses
   of [Ofday] in the spec for [Time]. *)
module Ofday : S with type bound = Ofday.t

module Stable : sig
  module V1 : sig
    module Float : sig
      type t = Float.t with sexp, bin_io
    end
    module Int : sig
      type t = Int.t with sexp, bin_io
    end
    module Ofday : sig
      type t = Ofday.t with sexp, bin_io
    end
  end
end
