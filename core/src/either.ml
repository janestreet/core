module Stable = struct
  module V1 = struct
    type ('f, 's) t = ('f, 's) Base.Either.t =
      | First of 'f
      | Second of 's
    [@@deriving bin_io, compare, hash, sexp, typerep]

    let map x ~f1 ~f2 =
      match x with
      | First x1 -> First (f1 x1)
      | Second x2 -> Second (f2 x2)
    ;;
  end
end

include Stable.V1
include Base.Either

include Comparator.Derived2 (struct
    type nonrec ('a, 'b) t = ('a, 'b) t [@@deriving sexp_of, compare]
  end)

let quickcheck_generator = Base_quickcheck.Generator.either
let quickcheck_observer = Base_quickcheck.Observer.either
let quickcheck_shrinker = Base_quickcheck.Shrinker.either
