@@ portable

(** This module extends {{!Base.Or_error} [Base.Or_error]} with bin_io, diff, and
    quickcheck. *)

open! Import

type%template ('a : k) t = ('a Base.Or_error.t[@kind k])
[@@deriving bin_io ~localize] [@@kind k = base_non_value]

type ('a : value_or_null) t = ('a, Error.t) Result.t
[@@deriving bin_io ~localize, diff ~extra_derive:[ sexp ], quickcheck]

(** @inline *)
include%template (module type of struct
    include Base.Or_error
  end
  with type ('a : value_or_null) t := 'a t
 [@with: type ('a : any) t := ('a t[@kind k]) [@@kind k = base_non_value]])

module Expect_test_config : Expect_test_config_types.S with type 'a IO.t = 'a t

module Expect_test_config_with_unit_expect = Expect_test_config
[@@deprecated "[since 2022-05] Use [Expect_test_config] instead, it is equivalent."]

module Stable : sig
  (** [Or_error.t] is wire compatible with [V2.t], but not [V1.t], like [Info.Stable] and
      [Error.Stable]. *)
  module%template V1 :
    Stable_module_types.With_stable_witness.S1 [@mode local] with type 'a t = 'a t

  module V2 : sig
    type nonrec ('a : value_or_null) t = 'a t
    [@@deriving equal ~localize, sexp_grammar, diff ~extra_derive:[ sexp; bin_io ]]

    include%template
      Stable_module_types.With_stable_witness.S1 [@mode local] with type 'a t := 'a t
  end
end
