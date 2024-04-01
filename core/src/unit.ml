module Stable = struct
  open Stable_witness.Export
  open Base.Export
  open Bin_prot.Std

  module V1 = struct
    module T = struct
      type t = unit [@@deriving bin_io ~localize, compare, sexp, stable_witness]
    end

    include T
    include Comparator.Stable.V1.Make (T)

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| 86ba5df747eec837f0b391dd49f33f9e |}]
    ;;
  end

  module V2 = struct
    type t = unit [@@deriving compare, equal, sexp, stable_witness]
    type comparator_witness = V1.comparator_witness

    let comparator = V1.comparator
    let bin_name = "unit_v2"

    let __bin_read_t__ (_ : Bin_prot.Common.buf) ~pos_ref (_ : int) =
      Bin_prot.Common.raise_variant_wrong_type bin_name !pos_ref
    ;;

    let bin_read_t (_ : Bin_prot.Common.buf) ~pos_ref:(_ : int ref) = ()

    let bin_reader_t =
      { Bin_prot.Type_class.read = bin_read_t; vtag_read = __bin_read_t__ }
    ;;

    let bin_shape_t = Bin_prot.Shape.(basetype (Uuid.of_string bin_name)) []
    let bin_size_t () = 0
    let bin_size_t__local () = 0
    let bin_write_t (_ : Bin_prot.Common.buf) ~pos () = pos
    let bin_write_t__local (_ : Bin_prot.Common.buf) ~pos () = pos
    let bin_writer_t = { Bin_prot.Type_class.size = bin_size_t; write = bin_write_t }

    let bin_t =
      { Bin_prot.Type_class.shape = bin_shape_t
      ; writer = bin_writer_t
      ; reader = bin_reader_t
      }
    ;;

    let%expect_test _ =
      print_endline [%bin_digest: t];
      [%expect {| ffbd1a307a4f7ebe8023040fecebf697 |}]
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

type t = unit [@@deriving typerep, bin_io ~localize]

let quickcheck_generator = Base_quickcheck.Generator.unit
let quickcheck_observer = Base_quickcheck.Observer.unit
let quickcheck_shrinker = Base_quickcheck.Shrinker.unit

module type S = sig end

type m = (module S)
