open! Import
include Base.Or_null

type 'a t = 'a or_null [@@deriving bin_io ~localize, stable_witness, typerep]

let validate ~null ~this t =
  match t with
  | Null -> Validate.name "null" (Validate.protect null ())
  | This x -> Validate.name "this" (Validate.protect this x)
;;

[%%template
[@@@mode.default p = (nonportable, portable)]

let quickcheck_generator = (Base_quickcheck.Generator.or_null [@mode p])
let quickcheck_observer = (Base_quickcheck.Observer.or_null [@mode p])
let quickcheck_shrinker = (Base_quickcheck.Shrinker.or_null [@mode p])]

module Stable = struct
  module V1 = struct
    type nonrec 'a t = 'a t
    [@@deriving
      bin_io ~localize, compare ~localize, equal ~localize, hash, sexp, stable_witness]
  end
end
