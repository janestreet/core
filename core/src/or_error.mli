(** This module extends {{!Base.Or_error} [Base.Or_error]} with bin_io, diff, and
    quickcheck. *)

open! Import

type 'a t = ('a, Error.t) Result.t
[@@deriving bin_io, diff ~extra_derive:[ sexp ], quickcheck]

(** @inline *)
include module type of struct
    include Base.Or_error
  end
  with type 'a t := 'a t

module Portable : sig
  type 'a t = ('a, Error.Portable.t) Result.t
  [@@deriving bin_io, diff ~extra_derive:[ sexp ], quickcheck]

  include module type of Portable with type 'a t := 'a t
end

module Expect_test_config : Expect_test_config_types.S with type 'a IO.t = 'a t

module Expect_test_config_with_unit_expect = Expect_test_config
[@@deprecated "[since 2022-05] Use [Expect_test_config] instead, it is equivalent."]

module Stable : sig
  (** [Or_error.t] is wire compatible with [V2.t], but not [V1.t], like [Info.Stable] and
      [Error.Stable]. *)
  module V1 : Stable_module_types.With_stable_witness.S1 with type 'a t = 'a t

  module V2 : sig
    type nonrec 'a t = 'a t
    [@@deriving equal, sexp_of, sexp_grammar, diff ~extra_derive:[ sexp; bin_io ]]

    include Stable_module_types.With_stable_witness.S1 with type 'a t := 'a t
  end

  module Portable : sig
    module V1 : sig
      (** Wire compatible with [Core.Or_error.Stable.V1] *)
      type 'a t = 'a Portable.t

      include Stable_module_types.With_stable_witness.S1 with type 'a t := 'a t
    end

    module V2 : sig
      (** Wire compatible with [Core.Or_error.Stable.V2] *)
      type 'a t = 'a Portable.t
      [@@deriving
        globalize, equal, hash, sexp_grammar, diff ~extra_derive:[ sexp; bin_io ]]

      include Stable_module_types.With_stable_witness.S1 with type 'a t := 'a t
    end
  end
end
