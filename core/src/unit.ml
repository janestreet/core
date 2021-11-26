module Stable = struct
  open Base.Export
  open Bin_prot.Std

  module V1 = struct
    module T = struct
      type t = unit [@@deriving bin_io, compare, sexp]
    end

    include T
    include Comparator.Stable.V1.Make (T)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 86ba5df747eec837f0b391dd49f33f9e |}]
    ;;
  end
end

open! Import

include
  Identifiable.Extend
    (Base.Unit)
    (struct
      type t = unit [@@deriving bin_io]
    end)

include Base.Unit

type t = unit [@@deriving typerep]

let quickcheck_generator = Base_quickcheck.Generator.unit
let quickcheck_observer = Base_quickcheck.Observer.unit
let quickcheck_shrinker = Base_quickcheck.Shrinker.unit

module type S = sig end

type m = (module S)
