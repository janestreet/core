(** This module extends {{!Base.Or_null} [Base.Or_null]} with bin_io and quickcheck. *)

type 'a t = 'a Base.Or_null.t [@@deriving bin_io ~localize, typerep, quickcheck ~portable]

(** @inline *)
include module type of struct
    include Base.Or_null
  end
  with type 'a t := 'a Base.Or_null.t

val validate : null:unit Validate.check -> this:'a Validate.check -> 'a t Validate.check

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t
    [@@deriving
      bin_io ~localize, compare ~localize, equal ~localize, hash, sexp, stable_witness]
  end
end
