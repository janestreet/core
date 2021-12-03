open! Import

include
  Identifiable.Extend
    (Base.Int32)
    (struct
      type t = int32 [@@deriving bin_io]
    end)

include Base.Int32
include Comparable.Validate_with_zero (Base.Int32)

type t = int32 [@@deriving typerep]

module Hex = struct
  include Hex

  type nonrec t = t [@@deriving typerep, bin_io]
end

let quickcheck_generator = Base_quickcheck.Generator.int32
let quickcheck_observer = Base_quickcheck.Observer.int32
let quickcheck_shrinker = Base_quickcheck.Shrinker.int32
let gen_incl = Base_quickcheck.Generator.int32_inclusive
let gen_uniform_incl = Base_quickcheck.Generator.int32_uniform_inclusive
let gen_log_incl = Base_quickcheck.Generator.int32_log_inclusive
let gen_log_uniform_incl = Base_quickcheck.Generator.int32_log_uniform_inclusive
