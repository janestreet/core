open! Import

module Stable = struct
  module V1 = struct
    module T = struct
      include Base.Int

      type t = int [@@deriving bin_io ~localize, hash, sexp, stable_witness]
    end

    include T

    include%template Comparable.Stable.V1.With_stable_witness.Make [@modality portable] (T)
  end
end

module Binable = struct
  type t = int [@@deriving bin_io ~localize]
end

include Binable

include%template Identifiable.Extend [@modality portable] (Base.Int) (Binable)

module Replace_polymorphic_compare = Base.Int
include Base.Int

include%template Comparable.Validate_with_zero [@modality portable] (Base.Int)

(* This is already defined by Comparable.Validate_with_zero, but Sign.of_int is
   more direct. *)
let sign = Sign.of_int

type t = int [@@deriving typerep]

module Binary = struct
  include Binary

  type nonrec t = t [@@deriving typerep, bin_io ~localize]
end

module Hex = struct
  include Hex

  type nonrec t = t [@@deriving typerep, bin_io ~localize]
end

let quickcheck_generator = Base_quickcheck.Generator.int
let quickcheck_observer = Base_quickcheck.Observer.int
let quickcheck_shrinker = Base_quickcheck.Shrinker.int
let gen_incl = Base_quickcheck.Generator.int_inclusive
let gen_uniform_incl = Base_quickcheck.Generator.int_uniform_inclusive
let gen_log_incl = Base_quickcheck.Generator.int_log_inclusive
let gen_log_uniform_incl = Base_quickcheck.Generator.int_log_uniform_inclusive
