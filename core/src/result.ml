open! Import
module Result = Base.Result

module Stable = struct
  module V1 = struct
    type ('a, 'b) t = ('a, 'b) Result.t =
      | Ok of 'a
      | Error of 'b
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , diff
      , equal ~localize
      , globalize
      , hash
      , sexp ~localize
      , sexp_grammar
      , stable_witness
      , typerep]

    let map x ~f1 ~f2 =
      match x with
      | Error err -> Error (f2 err)
      | Ok x -> Ok (f1 x)
    ;;
  end

  module V1_stable_unit_test = struct
    type t = (string, int) V1.t
    [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp]

    let tests =
      [ V1.Ok "foo", "(Ok foo)", "\000\003foo"; V1.Error 7, "(Error 7)", "\001\007" ]
    ;;
  end
end

include Stable.V1
include Result

type%template ('a : k, 'b) t = (('a, 'b) Result.t[@kind k]) =
  | Ok of 'a
  | Error of 'b
[@@deriving bin_io ~localize] [@@kind k = (float64, bits32, bits64, word)]

let quickcheck_generator = Base_quickcheck.Generator.result
let quickcheck_observer = Base_quickcheck.Observer.result
let quickcheck_shrinker = Base_quickcheck.Shrinker.result
