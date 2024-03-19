open! Import

module Stable = struct
  module V1 = struct
    module T = struct
      include Base.Bool

      type t = bool [@@deriving compare, sexp, bin_io ~localize, stable_witness]
    end

    include T
    include Comparable.Stable.V1.With_stable_witness.Make (T)
  end
end

include Stable.V1

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
    module type of struct
      include Base.Bool
    end
    with type t := t)

include Comparable.Validate (Base.Bool)

let of_string_hum =
  let table =
    lazy
      (let table = String.Caseless.Table.create () in
       [ false, [ "false"; "no"; "0" ]; true, [ "true"; "yes"; "1" ] ]
       |> List.iter ~f:(fun (bool, strings) ->
            List.iter strings ~f:(fun string ->
              Hashtbl.set table ~key:string ~data:bool;
              Hashtbl.set table ~key:(String.prefix string 1) ~data:bool));
       table)
  in
  let raise_invalid input =
    let expected_case_insensitive = String.Set.of_list (Hashtbl.keys (force table)) in
    raise_s
      [%message
        "Bool.of_string_hum: invalid input"
          (input : string)
          (expected_case_insensitive : String.Set.t)]
  in
  fun string ->
    Hashtbl.find_and_call (force table) string ~if_found:Fn.id ~if_not_found:raise_invalid
;;

let quickcheck_generator = Base_quickcheck.Generator.bool
let quickcheck_observer = Base_quickcheck.Observer.bool
let quickcheck_shrinker = Base_quickcheck.Shrinker.bool
