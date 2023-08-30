open! Core

let bin_prot_test
  (type a)
  gen
  ~(bin : a Bin_prot.Type_class.t)
  ~compare:compare_a
  ~sexp_of:sexp_of_a
  =
  Quickcheck.test gen ~f:(fun a ->
    let size = bin.writer.size a in
    let bigstring = Bigstring.create size in
    let pos = bin.writer.write bigstring ~pos:0 a in
    [%test_result: int] pos ~expect:size;
    let pos_ref = ref 0 in
    let b = bin.reader.read bigstring ~pos_ref in
    [%test_result: int] !pos_ref ~expect:size;
    [%test_result: a] b ~expect:a)
;;

module Make (M : sig
  type t = unit [@@deriving bin_io]

  val expected_size : int
end) =
struct
  let%test_unit "size" = [%test_result: int] (M.bin_size_t ()) ~expect:M.expected_size

  let%test_unit "just unit" =
    bin_prot_test
      [%quickcheck.generator: unit]
      ~bin:[%bin_type_class: M.t]
      ~compare:[%compare: unit]
      ~sexp_of:[%sexp_of: unit]
  ;;

  let%test_unit "at the end" =
    bin_prot_test
      [%quickcheck.generator: int * unit]
      ~bin:[%bin_type_class: int * M.t]
      ~compare:[%compare: int * unit]
      ~sexp_of:[%sexp_of: int * unit]
  ;;

  let%test_unit "at the beginning" =
    bin_prot_test
      [%quickcheck.generator: unit * int]
      ~bin:[%bin_type_class: M.t * int]
      ~compare:[%compare: unit * int]
      ~sexp_of:[%sexp_of: unit * int]
  ;;

  let%test_unit "in the middle" =
    bin_prot_test
      [%quickcheck.generator: int * unit * int]
      ~bin:[%bin_type_class: int * M.t * int]
      ~compare:[%compare: int * unit * int]
      ~sexp_of:[%sexp_of: int * unit * int]
  ;;
end

include Make (struct
  include Unit.Stable.V1

  let expected_size = 1
end)

include Make (struct
  include Unit.Stable.V2

  let expected_size = 0
end)

(* Should work like V1 *)
include Make (struct
  type t = unit [@@deriving bin_io]

  let expected_size = 1
end)
