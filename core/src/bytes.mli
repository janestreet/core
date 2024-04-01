(** This module extends {{!Base.Bytes}[Base.Bytes]}. *)

open! Import

type t = bytes [@@deriving bin_io ~localize, typerep]

(** @inline *)
include module type of struct
    include Base.Bytes
  end
  with type t := t

include Hexdump.S with type t := t
include Quickcheckable.S with type t := t

(** Like [gen], but generate bytes with the given distribution of characters. *)
val gen' : char Quickcheck.Generator.t -> t Quickcheck.Generator.t

(** Like [gen'], but generate bytes with the given length. *)
val gen_with_length : int -> char Quickcheck.Generator.t -> t Quickcheck.Generator.t

(** Note that [bytes] is already stable by itself, since as a primitive type it is an
    integral part of the sexp / bin_io protocol. [Bytes.Stable] exists only to provide
    interface uniformity with other stable types. *)
module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io ~localize, equal]
    type nonrec comparator_witness = comparator_witness

    include
      Stable_module_types.With_stable_witness.S0
        with type t := t
        with type comparator_witness := comparator_witness
  end
end
