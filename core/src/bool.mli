(** This module extends {{!Base.Bool}[Base.Bool]}. *)

type t = bool [@@deriving bin_io ~localize, typerep]

include module type of Base.Bool with type t := t

include
  Identifiable.S
    with type t := t
     and type comparator_witness := Base.Bool.comparator_witness

(**
   Human readable parsing. Accepted inputs are (case insensitive):
   - true/false
   - yes/no
   - 1/0
   - t/f
   - y/n
*)
val of_string_hum : string -> t

include Quickcheckable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io ~localize, compare, equal, hash, sexp]

    include
      Stable_comparable.With_stable_witness.V1
        with type t := t
        with type comparator_witness = comparator_witness
  end
end
