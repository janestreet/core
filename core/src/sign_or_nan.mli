(** This module extends {{!Base.Sign_or_nan}[Base.Sign_or_nan]} with bin_io. *)

open! Import

type t = Base.Sign_or_nan.t =
  | Neg
  | Zero
  | Pos
  | Nan
[@@deriving typerep]

include module type of Base.Sign_or_nan with type t := t (** @inline *)

(** This provides [to_string]/[of_string], sexp/bin_io conversion, Map, Hashtbl, etc. *)
include Identifiable.S with type t := t and type comparator_witness := comparator_witness

module Stable : sig
  module V1 : sig
    type nonrec t = t =
      | Neg
      | Zero
      | Pos
      | Nan
    [@@deriving bin_io, compare, hash, sexp]
  end
end
