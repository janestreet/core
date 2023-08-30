open! Import
open! Typerep_lib.Std
include Base.List

type 'a t = 'a list [@@deriving bin_io ~localize, typerep, stable_witness]

module Assoc = struct
  include Assoc

  type ('a, 'b) t = ('a * 'b) list [@@deriving bin_io ~localize]

  let compare (type a b) compare_a compare_b = [%compare: (a * b) list]
end

let to_string ~f t =
  Sexplib.Sexp.to_string (sexp_of_t (fun x -> Sexplib.Sexp.Atom x) (map t ~f))
;;

include Comparator.Derived (struct
  type nonrec 'a t = 'a t [@@deriving sexp_of, compare]
end)

let quickcheck_generator = Base_quickcheck.Generator.list
let gen_non_empty = Base_quickcheck.Generator.list_non_empty

let gen_with_length length quickcheck_generator =
  Base_quickcheck.Generator.list_with_length quickcheck_generator ~length
;;

let gen_filtered = Base_quickcheck.Generator.list_filtered
let gen_permutations = Base_quickcheck.Generator.list_permutations
let quickcheck_observer = Base_quickcheck.Observer.list
let quickcheck_shrinker = Base_quickcheck.Shrinker.list
