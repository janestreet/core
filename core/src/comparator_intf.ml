open! Import

module Definitions = struct
  module Stable = struct
    module%template V1 = struct
      [@@@modality.default p = (nonportable, portable)]

      module type S = sig
        type t
        type comparator_witness : value mod p

        val comparator : (t, comparator_witness) Base.Comparator.t
      end

      module type S1 = sig
        type 'a t
        type comparator_witness : value mod p

        val comparator : ('a t, comparator_witness) Base.Comparator.t
      end
    end
  end
end

(** Extends {{!Base.Comparator} [Base.Comparator]}, providing a type-indexed value that
    allows you to compare values of that type. *)
module type Comparator = sig @@ portable
  type ('a, 'witness) t = ('a, 'witness) Base.Comparator.t

  include module type of Base.Comparator with type ('a, 'witness) t := ('a, 'witness) t
  (** @inline *)

  (** The following module types and functors may be used to define stable modules *)

  module%template Stable : sig
    module V1 : sig
      include module type of struct
        include Definitions.Stable.V1
      end

      type nonrec ('a, 'b) t = ('a, 'b) t
      type ('a, 'b) comparator = ('a, 'b) t

      [@@@modality.default p = (portable, nonportable)]

      val make
        :  compare:('a -> 'a -> int) @ p
        -> sexp_of_t:('a -> Base.Sexp.t) @ p
        -> ((module S_fc with type comparable_t = 'a)[@modality p])

      [@@@mode.default m = (global, local)]

      module Make : module type of Make [@mode m] [@modality p]
      module Make1 : module type of Make1 [@mode m] [@modality p]
    end
  end
end
