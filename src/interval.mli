(** Module for simple closed intervals over arbitrary types that are ordered
    correctly using polymorphic compare. *)

open! Import
open Interval_intf

module type S1 = S1

(** Sexps are () for empty interval and (3 5) for an interval containing 3, 4, and 5. *)
include S1

module type S = S
  with type 'a poly_t := 'a t
  with type 'a poly_set := 'a Set.t

module type S_time = S_time
  with type 'a poly_t := 'a t
  with type 'a poly_set := 'a Set.t

module Make (Bound : sig
    type t [@@deriving bin_io, sexp]
    include Comparable.S with type t := t
  end)
  : S with type bound = Bound.t

module Float : S with type bound = Float.t

module Int : sig
  include S with type bound = Int.t

  include Container.S0        with type t := t with type elt := bound
  include Binary_searchable.S with type t := t with type elt := bound
end

module Ofday    : S with type bound = Time.Ofday.t
module Ofday_ns : S with type bound = Time_ns.Ofday.t

module Time    : S_time with module Time := Time
                         and type t = Time.t t
module Time_ns : S_time with module Time := Time_ns
                         and type t = Time_ns.t t

module Stable : sig
  module V1 : sig
    module Float   : Stable with type t = Float.  t
    module Int     : Stable with type t = Int.    t
    module Time    : Stable with type t = Time.   t
    module Time_ns : Stable with type t = Time_ns.t
    module Ofday   : Stable with type t = Ofday.  t
  end
end
