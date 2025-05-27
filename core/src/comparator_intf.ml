open! Import

module Definitions = struct
  module Stable = struct
    module V1 = struct
      module type S = sig
        type t
        type comparator_witness

        val comparator : (t, comparator_witness) Base.Comparator.t
      end

      module type%template [@modality portable] S = sig @@ portable
        type comparator_witness : value mod portable

        include S with type comparator_witness := comparator_witness
      end

      module type S1 = sig
        type 'a t
        type comparator_witness

        val comparator : ('a t, comparator_witness) Base.Comparator.t
      end

      module type%template [@modality portable] S1 = sig @@ portable
        type comparator_witness : value mod portable

        include S1 with type comparator_witness := comparator_witness
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

      module Make : module type of Make [@modality p]
      module Make1 : module type of Make1 [@modality p]
    end
  end
end
