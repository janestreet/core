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
    module T = struct
      type nonrec 'a t = 'a t
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , stable_witness]
    end

    include T

    module Bin_shape_same_as_option = struct
      include T

      (* Keep the shared bin-io implementation in [T] so deriving [bin_io] creates only
         one [Bin_prot.Shape.group]. Re-deriving [bin_io] here would allocate an otherwise
         unused group id and perturb tests that print raw bin shapes. *)
      let bin_shape_t bin_shape_a = [%bin_shape: a option]
      let bin_t bin_a = { (bin_t bin_a) with shape = [%bin_shape: t] bin_a.shape }
    end
  end
end
