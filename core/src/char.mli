(** This module extends {{!Base.Char}[Base.Char]}, adding [Identifiable] for making char
    identifiers and [Quickcheckable] to facilitate automated testing with pseudorandom
    data.
*)

type t = char [@@deriving typerep, bin_io ~localize]

(** {2 The signature included from [Base.Char]} *)

(** @inline *)
include module type of struct
    include Base.Char
  end
  with type t := t

(** {2 Extensions} *)

(** [Caseless] compares and hashes characters ignoring case, so that for example
    [Caseless.equal 'A' 'a'] and [Caseless.('a' < 'B')] are [true], and
    [Caseless.Map], [Caseless.Table] lookup and [Caseless.Set] membership is
    case-insensitive. *)
module Caseless : sig
  type nonrec t = t [@@deriving bin_io ~localize, hash, sexp]

  include Comparable.S_binable with type t := t
  include Hashable.S_binable with type t := t
end

include Identifiable.S with type t := t and type comparator_witness := comparator_witness

(** {3 Quickcheck Support} *)

include Quickcheckable.S with type t := t

val gen_digit : t Quickcheck.Generator.t
val gen_lowercase : t Quickcheck.Generator.t
val gen_uppercase : t Quickcheck.Generator.t
val gen_alpha : t Quickcheck.Generator.t
val gen_alphanum : t Quickcheck.Generator.t
val gen_print : t Quickcheck.Generator.t
val gen_whitespace : t Quickcheck.Generator.t

(** Generates characters between the given inclusive bounds in ASCII order. Raises if
    bounds are in decreasing order. *)
val gen_uniform_inclusive : t -> t -> t Quickcheck.Generator.t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving equal, hash, sexp_grammar, typerep]

    include
      Stable_comparable.With_stable_witness.V1
        with type t := t
         and type comparator_witness = comparator_witness
  end
end
