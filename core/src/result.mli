@@ portable

(** This module extends {{!Base.Result} [Base.Result]}. *)

open! Import

type%template ('a : k, 'b : value_or_null) t = (('a, 'b) Base.Result.t[@kind k]) =
  | Ok of 'a
  | Error of 'b
[@@deriving bin_io ~localize] [@@kind k = (float64, bits32, bits64, word)]

type ('a : value_or_null, 'b : value_or_null) t = ('a, 'b) Base.Result.t =
  | Ok of 'a
  | Error of 'b
[@@deriving bin_io ~localize, diff ~extra_derive:[ sexp ], quickcheck, typerep]

include
  module type of Base.Result
  with type ('a : value_or_null, 'b : value_or_null) t :=
    ('a : value_or_null, 'b : value_or_null) t
  with type ('a, 'b : value_or_null) t__float64 := ('a, 'b : value_or_null) t__float64
  with type ('a, 'b : value_or_null) t__bits32 := ('a, 'b : value_or_null) t__bits32
  with type ('a, 'b : value_or_null) t__bits64 := ('a, 'b : value_or_null) t__bits64
  with type ('a, 'b : value_or_null) t__word := ('a, 'b : value_or_null) t__word
(** @inline *)

module Stable : sig
  module V1 : sig
    type nonrec ('ok : value_or_null, 'err : value_or_null) t = ('ok, 'err) t =
      | Ok of 'ok
      | Error of 'err
    [@@deriving bin_io ~localize, equal ~localize, globalize, sexp_grammar]

    include%template
      Stable_module_types.With_stable_witness.S2
      [@mode local]
      with type ('ok, 'err) t := ('ok, 'err) t

    include
      Diffable.S2
      with type ('ok, 'err) t := ('ok, 'err) t
       and type ('ok, 'err, 'ok_diff, 'err_diff) Diff.t =
        ('ok, 'err, 'ok_diff, 'err_diff) Diff.t
  end

  (** We export the unit test arg rather than instantiate the functor inside result.ml in
      order to avoid circular dependencies. The functor is instantiated in stable.ml. *)
  module V1_stable_unit_test : Stable_unit_test_intf.Arg
end
