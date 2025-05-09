open! Import
include Base.Iarray

module Stable = struct
  module V1 = struct
    type nonrec 'a t = 'a t
    [@@deriving compare ~localize, equal ~localize, globalize, hash]

    let map = map

    include%template
      Sexpable.Stable.Of_sexpable1.V1 [@modality portable]
        (struct
          type 'a t = 'a array [@@deriving sexp]
        end)
        (struct
          type nonrec 'a t = 'a t

          let to_sexpable = unsafe_to_array__promise_no_mutation
          let of_sexpable = unsafe_of_array__promise_no_mutation
        end)

    let t_sexp_grammar (type a) (a_sexp_grammar : a Sexplib.Sexp_grammar.t) =
      Sexplib.Sexp_grammar.coerce [%sexp_grammar: a array]
    ;;

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

  let quickcheck_generator elt_generator =
    Generator.list elt_generator |> Generator.map ~f:of_list
  ;;

  let quickcheck_observer elt_observer =
    Observer.list elt_observer |> Observer.unmap ~f:to_list
  ;;

  let quickcheck_shrinker elt_shrinker =
    Shrinker.list elt_shrinker |> Shrinker.map ~f:of_list ~f_inverse:to_list
  ;;
end

include (
struct
  type nonrec 'a t = 'a t [@@deriving typerep ~abstract]
end :
  Typerep_lib.Typerepable.S1 with type 'a t := 'a t)

include Stable.V1
