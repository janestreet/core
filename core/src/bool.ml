open! Import

module Stable = struct
  module V1 = struct
    module T = struct
      include Base.Bool

      type t = bool
      [@@deriving bin_io, compare, equal, sexp, sexp_grammar, stable_witness, typerep]
    end

    include T
    include Comparable.Stable.V1.With_stable_witness.Make (T)
  end
end

include Stable.V1
module String = Base.String

type t = bool [@@deriving typerep]

include
  Identifiable.Extend
    (Base.Bool)
    (struct
      type nonrec t = t [@@deriving bin_io]
    end)

module Replace_polymorphic_compare = Base.Bool

include (
  Base.Bool :
  sig
    include module type of struct
        include Base.Bool
      end
      with type t := t
  end)

include Comparable.Validate (Base.Bool)

let of_string_hum =
  let raise_invalid input =
    let expected_case_insensitive =
      [ "true"; "t"; "yes"; "y"; "1"; "false"; "f"; "no"; "n"; "0" ]
      |> List.sort ~compare:compare_string
    in
    raise_s
      [%message
        "Bool.of_string_hum: invalid input"
          (input : string)
          (expected_case_insensitive : string list)]
  in
  fun string ->
    match String.lowercase string with
    | "true" | "t" | "yes" | "y" | "1" -> true
    | "false" | "f" | "no" | "n" | "0" -> false
    | _ -> raise_invalid string
;;

let quickcheck_generator = Base_quickcheck.Generator.bool
let quickcheck_observer = Base_quickcheck.Observer.bool
let quickcheck_shrinker = Base_quickcheck.Shrinker.bool
