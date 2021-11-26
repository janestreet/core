(** Process ID. *)

open! Import

type t [@@deriving bin_io, hash, sexp]

include Identifiable.S with type t := t

val of_int : int -> t
val to_int : t -> int

(** The pid of the "init" process, which is [1] by convention. *)
val init : t

module Stable : sig
  module V1 :
    Stable_comparable.V1 with type t = t and type comparator_witness = comparator_witness
end
