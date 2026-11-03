open! Import
open Base_quickcheck.Export
include Base.Iarray

module Stable = struct
  module V1 = struct
    type nonrec 'a t = 'a t [@@deriving compare, equal, globalize]

    [%%rederive type nonrec 'a t = 'a t [@@deriving hash]]

    let map = map
    let t_of_sexp = t_of_sexp
    let sexp_of_t = sexp_of_t
    let t_sexp_grammar = t_sexp_grammar

    [%%rederive type 'a t = 'a iarray [@@deriving bin_io]]

    let stable_witness elt_witness =
      Stable_witness.of_serializable
        (stable_witness_array elt_witness)
        unsafe_of_array__promise_no_mutation
        unsafe_to_array__promise_no_mutation
    ;;
  end
end

[%%rederive.portable type 'a t = 'a iarray [@@deriving quickcheck]]

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
    with type 'a t := 'a t)

[%%rederive.portable type nonrec 'a t = 'a iarray [@@deriving bin_io]]
