@@ portable

open! Import

type ('a : any mod separable) t = 'a Base.Iarray.t

[%%rederive:
  type 'a t = 'a Base.Iarray.t
  [@@deriving bin_io ~localize, quickcheck ~portable, typerep]]

include Base.Iarray.Public with type ('a : any mod separable) t := 'a t (** @inline *)

module Stable : sig
  module V1 : sig
    type nonrec ('a : value_or_null mod separable) t = 'a t
    [@@deriving equal ~localize, sexp, sexp_grammar]

    [%%rederive: type nonrec 'a t = 'a t [@@deriving globalize, hash]]

    include%template
      Stable_module_types.With_stable_witness.S1 [@mode local] with type 'a t := 'a t

    (*_ Rederive [compare] here, because deriving it above will cause it to be shadowed by
        the [compare] from [Stable_module_types], which only supports [value]s *)
    [%%rederive:
      type nonrec ('a : value_or_null mod separable) t = 'a t
      [@@deriving compare ~localize]]
  end
end
