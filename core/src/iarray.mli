open! Import

type 'a t = 'a Base.Iarray.t

[%%rederive: type nonrec 'a t = 'a t [@@deriving bin_io]]
[%%rederive: type 'a t = 'a Base.Iarray.t [@@deriving quickcheck, typerep]]

include Base.Iarray.Public with type 'a t := 'a t

(** @inline *)

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving equal, sexp, sexp_grammar]

    [%%rederive: type nonrec 'a t = 'a t [@@deriving globalize, hash]]

    include Stable_module_types.With_stable_witness.S1 with type 'a t := 'a t

    (*_ Rederive [compare] here, because deriving it above will cause it to be shadowed by
        the [compare] from [Stable_module_types], which only supports [value]s *)
    [%%rederive: type nonrec 'a t = 'a t [@@deriving compare]]
  end
end
