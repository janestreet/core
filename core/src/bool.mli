(** This module extends {{!Base.Bool}[Base.Bool]}. *)

type t = bool [@@deriving bin_io, typerep]

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
    type nonrec t = t [@@deriving compare, sexp, bin_io, stable_witness]
  end
end
