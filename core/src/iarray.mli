@@ portable

open! Import

type ('a : any mod separable) t = 'a Base.Iarray.t

[%%template:
type ('a : k mod separable) t = ('a Base.Iarray.t[@kind k])
[@@kind k = (base_non_value, value_or_null mod external64)]

[@@@kind k = (base_or_null, value_or_null mod external64)]

[%%rederive:
  type nonrec ('a : k mod separable) t = ('a t[@kind k])
  [@@deriving bin_io ~localize] [@@kind k]]]

[%%rederive: type 'a t = 'a Base.Iarray.t [@@deriving quickcheck ~portable, typerep]]

include%template
  Base.Iarray.Public
[@with:
  type ('a : any mod separable) t := 'a t

  type ('a : k mod separable) t := ('a t[@kind k])
  [@@kind k = (base_non_value, value_or_null mod external64)]]
(** @inline *)

module Stable : sig
  module V1 : sig
    type nonrec ('a : value_or_null mod separable) t = 'a t
    [@@deriving equal ~localize, sexp ~stackify, sexp_grammar]

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
