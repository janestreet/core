@@ portable

(** Extends {{!Base.Comparator} [Base.Comparator]}, providing a type-indexed value that
    allows you to compare values of that type. *)

open! Import

type ('a, 'witness) t = ('a, 'witness) Base.Comparator.t = private
  { compare : 'a -> 'a -> int
  ; sexp_of_t : 'a -> Base.Sexp.t
  }

include module type of Base.Comparator with type ('a, 'witness) t := ('a, 'witness) t
(** @inline *)

(** The following module types and functors may be used to define stable modules *)

module%template Stable : sig
  module V1 : sig
    type nonrec ('a, 'b) t = ('a, 'b) t = private
      { compare : 'a -> 'a -> int
      ; sexp_of_t : 'a -> Base.Sexp.t
      }

    type ('a, 'b) comparator = ('a, 'b) t

    module type S = sig
      type t
      type comparator_witness

      val comparator : (t, comparator_witness) comparator
    end

    module type S1 = sig
      type 'a t
      type comparator_witness

      val comparator : ('a t, comparator_witness) comparator
    end

    [@@@modality.default p = (portable, nonportable)]

    val make
      :  compare:('a -> 'a -> int) @ p
      -> sexp_of_t:('a -> Base.Sexp.t) @ p
      -> ((module S_fc with type comparable_t = 'a)[@modality p])
    [@@conflate_modality_as_mode p]

    module Make : module type of Make [@modality p]
    module Make1 : module type of Make1 [@modality p]
  end
end
