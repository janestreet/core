@@ portable

(** This module extends {{!Base.Or_error} [Base.Or_error]} with bin_io, diff, and
    quickcheck. *)

open! Import

type%template ('a : k) t = ('a Base.Or_error.t[@kind k])
[@@deriving bin_io] [@@kind k = (float64, bits32, bits64, word)]

type 'a t = ('a, Error.t) Result.t
[@@deriving bin_io, diff ~extra_derive:[ sexp ], quickcheck]

(** @inline *)
include module type of struct
    include Base.Or_error
  end
  with type 'a t := 'a t
  with type 'a t__float64 := 'a t__float64
  with type 'a t__bits32 := 'a t__bits32
  with type 'a t__bits64 := 'a t__bits64
  with type 'a t__word := 'a t__word

module Expect_test_config : Expect_test_config_types.S with type 'a IO.t = 'a t

module Expect_test_config_with_unit_expect = Expect_test_config
[@@deprecated "[since 2022-05] Use [Expect_test_config] instead, it is equivalent."]

module Stable : sig
  (** [Or_error.t] is wire compatible with [V2.t], but not [V1.t], like [Info.Stable] and
      [Error.Stable]. *)
  module%template V1 :
    Stable_module_types.With_stable_witness.S1 [@mode local] with type 'a t = 'a t

  module V2 : sig
    type nonrec 'a t = 'a t
    [@@deriving equal ~localize, sexp_grammar, diff ~extra_derive:[ sexp; bin_io ]]

    include%template
      Stable_module_types.With_stable_witness.S1 [@mode local] with type 'a t := 'a t
  end
end
