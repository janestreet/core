(** This module extends {{!Base.Uchar}[Base.Uchar]}, adding [Comparable] and [Hashable]
    functionality, [bin_io] support, and [Quickcheckable] to facilitate automated testing
    with pseudorandom data. *)

type t = Base.Uchar.t [@@deriving bin_io]

(** {2 The signature included from [Base.Uchar]} *)

(** @inline *)
include module type of struct
    include Base.Uchar
  end
  with type t := t

include
  Comparable.S_binable with type t := t and type comparator_witness := comparator_witness

include Hashable.S_binable with type t := t

(** {2 Quickcheck Support} *)

include Quickcheckable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, equal, hash, sexp_grammar]

    include
      Stable_comparable.With_stable_witness.V1
        with type t := t
        with type comparator_witness = comparator_witness

    include Hashable.Stable.V1.With_stable_witness.S with type key := t
  end
end
