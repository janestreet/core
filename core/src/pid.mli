(** Process ID. *)

open! Import

type t [@@deriving bin_io, hash, sexp, quickcheck] [@@immediate]

include Identifiable.S with type t := t

val of_int : int -> t
val to_int : t -> int

(** The pid of the "init" process, which is [1] by convention. *)
val init : t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving equal]

    include
      Stable_comparable.With_stable_witness.V1
        with type t := t
         and type comparator_witness = comparator_witness
  end
end
