open! Import
include Stable_string_intf

module V1 = struct
  module T = struct
    include Base.String

    type t = string [@@deriving bin_io ~localize, stable_witness]

    let quickcheck_generator = Base_quickcheck.Generator.string
    let quickcheck_observer = Base_quickcheck.Observer.string
    let quickcheck_shrinker = Base_quickcheck.Shrinker.string
  end

  include T

  let to_string = Fn.id
  let%template[@alloc stack] to_string = Fn.id
  let of_string = Fn.id

  include%template Comparable.Stable.V1.With_stable_witness.Make [@modality portable] (T)
  include%template Hashable.Stable.V1.With_stable_witness.Make [@modality portable] (T)
  include%template Diffable.Atomic.Make [@modality portable] (T)
end
