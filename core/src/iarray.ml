open! Import
include Base.Iarray

module Stable = struct
  module V1 = struct
    type nonrec ('a : value_or_null mod separable) t = 'a t
    [@@deriving compare ~localize, equal ~localize]

    [%%rederive type nonrec 'a t = 'a t [@@deriving globalize, hash]]

    let map = map
    let t_of_sexp = t_of_sexp
    let sexp_of_t = sexp_of_t
    let t_sexp_grammar = t_sexp_grammar

    [%%rederive type 'a t = 'a iarray [@@deriving bin_io ~localize ~portable]]

    let stable_witness elt_witness =
      Stable_witness.of_serializable
        (stable_witness_array elt_witness)
        unsafe_of_array__promise_no_mutation
        unsafe_to_array__promise_no_mutation
    ;;
  end
end

include struct
  open Base_quickcheck

  [%%template
  [@@@mode.default p = (nonportable, portable)]

  let quickcheck_generator elt_generator =
    (Generator.list [@mode p]) elt_generator |> (Generator.map [@mode p]) ~f:of_list
  ;;

  let quickcheck_observer elt_observer =
    (Observer.list [@mode p]) elt_observer |> (Observer.unmap [@mode p]) ~f:to_list
  ;;

  let quickcheck_shrinker elt_shrinker =
    (Shrinker.list [@mode p]) elt_shrinker
    |> (Shrinker.map [@mode p]) ~f:of_list ~f_inverse:to_list
  ;;]
end

include (
struct
  type nonrec 'a t = 'a t [@@deriving typerep ~abstract]
end :
  Typerep_lib.Typerepable.S1 with type 'a t := 'a t)

include (
  Stable.V1 :
    module type of struct
      include Stable.V1
    end
    with type ('a : value_or_null mod separable) t := 'a t)
