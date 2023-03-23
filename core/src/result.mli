(** This module extends {{!Base.Result}[Base.Result]}. *)

open! Import

type ('a, 'b) t = ('a, 'b) Base.Result.t =
  | Ok of 'a
  | Error of 'b
[@@deriving bin_io, compare, equal, globalize, hash, sexp]

include module type of Base.Result with type ('a, 'b) t := ('a, 'b) t (** @inline *)

module Stable : sig
  module V1 : sig
    type nonrec ('ok, 'err) t = ('ok, 'err) t =
      | Ok of 'ok
      | Error of 'err

    include
      Stable_module_types.With_stable_witness.S2 with type ('ok, 'err) t := ('ok, 'err) t
  end

  (** We export the unit test arg rather than instantiate the functor inside result.ml in
      order to avoid circular dependencies.  The functor is instantiated in stable.ml. *)
  module V1_stable_unit_test : Stable_unit_test_intf.Arg
end
