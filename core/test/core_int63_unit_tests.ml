open Core

module Make_tests (Int : Base.Int.S) : sig end = struct
  open Int

  let p1 = of_int64_exn 1L
  let p2 = of_int64_exn 2L
  let p3 = of_int64_exn 3L
  let p10 = of_int64_exn 10L
  let n1 = of_int64_exn (-1L)
  let n2 = of_int64_exn (-2L)
  let n3 = of_int64_exn (-3L)
  let n10 = of_int64_exn (-10L)
  let%test_unit _ = [%test_eq: int64] (to_int64 (p2 + p2)) 4L
  let%test_unit _ = [%test_eq: int64] (to_int64 (max_value + p1)) (to_int64 min_value)
  let%test_unit _ = [%test_eq: int64] (to_int64 (max_value + max_value)) (-2L)
  let%test_unit _ = [%test_eq: int64] (to_int64 (p2 - p2)) 0L
  let%test_unit _ = [%test_eq: int64] (to_int64 (min_value - one)) (to_int64 max_value)
  let%test_unit _ = [%test_eq: int64] (to_int64 (min_value - min_value)) 0L
  let%test_unit _ = [%test_eq: int64] (to_int64 (p2 * p2)) 4L
  let%test_unit _ = [%test_eq: int64] (to_int64 (max_value * p2)) (-2L)
  let%test_unit _ = [%test_eq: int64] (to_int64 (min_value * p2)) 0L
  let%test_unit _ = [%test_eq: int64] (to_int64 (max_value * max_value)) 1L
  let%test_unit _ = [%test_eq: int64] (to_int64 (min_value * min_value)) 0L
  let%test_unit _ = [%test_eq: int64] (to_int64 (p10 / p2)) 5L
  let%test_unit _ = [%test_eq: int64] (to_int64 (n10 / p2)) (-5L)
  let%test_unit _ = [%test_eq: int64] (to_int64 (n10 / n2)) 5L
  let%test_unit _ = [%test_eq: int64] (to_int64 (min_value / n1)) (to_int64 min_value)
  let%test_unit _ = [%test_eq: int64] (to_int64 (min_value / n2)) 2305843009213693952L
  let%test_unit _ = [%test_eq: int64] (to_int64 (rem p3 p2)) 1L
  let%test_unit _ = [%test_eq: int64] (to_int64 (rem n3 p2)) (-1L)
  let%test_unit _ = [%test_eq: int64] (to_int64 (rem p3 n2)) 1L

  let%test_unit _ =
    [%test_eq: int64] (to_int64 (rem max_value min_value)) (to_int64 max_value)
  ;;

  let%test_unit _ = [%test_eq: int64] (to_int64 (rem min_value max_value)) (-1L)
  let%test_unit _ = [%test_eq: int64] (to_int64 (abs min_value)) (to_int64 min_value)

  let%test_unit _ =
    [%test_eq: int64] (to_int64 (abs (min_value + p1))) (to_int64 max_value)
  ;;

  let%test_unit _ = [%test_eq: int64] (to_int64 (bit_not n1)) 0L
  let%test_unit _ = [%test_eq: int64] (to_int64 (bit_not zero)) (-1L)
  let%test_unit _ = [%test_eq: int64] (to_int64 (bit_and p1 p1)) 1L
  let%test_unit _ = [%test_eq: int64] (to_int64 (bit_and p1 p2)) 0L
  let%test_unit _ = [%test_eq: int64] (to_int64 (bit_xor p1 p1)) 0L
  let%test_unit _ = [%test_eq: int64] (to_int64 (bit_xor p1 p3)) 2L
  let%test_unit _ = [%test_eq: int64] (to_int64 (bit_or p1 p1)) 1L
  let%test_unit _ = [%test_eq: int64] (to_int64 (bit_or p1 p3)) 3L

  let%test_unit _ =
    [%test_eq: int64] (to_int64 (pow n2 (of_int64_exn 61L))) (-2305843009213693952L)
  ;;

  let%test_unit _ =
    [%test_eq: int64] (to_int64 (pow p2 (of_int64_exn 61L))) 2305843009213693952L
  ;;

  let%test_unit _ =
    [%test_eq: int64] (to_int64 (pow p10 (of_int64_exn 15L))) 1000000000000000L
  ;;

  let%test _ = Exn.does_raise (fun () -> pow p2 (of_int64_exn 62L))
  let%test _ = Exn.does_raise (fun () -> pow n2 (of_int64_exn 62L))
  let%test_unit _ = [%test_eq: t] (of_string "0x4000") (of_int64_exn 0x4000L)
  let%test_unit _ = [%test_eq: t] (of_string "0x4000_0000_0000_0000") min_value
  let%test_unit _ = [%test_eq: t] (of_string "-0x4000_0000_0000_0000") min_value
  let%test_unit _ = [%test_eq: t] (of_string "0x3fff_ffff_ffff_ffff") max_value
  let%test_unit _ = [%test_eq: t] (of_string "-0x3fff_ffff_ffff_ffff") (min_value + one)
  let%test_unit _ = [%test_eq: t] (of_string "0o777777777777777777777") (neg one)
  let%test _ = Exn.does_raise (fun () -> of_string "0xffff_ffff_ffff_ffff")

  let%test _ =
    Exn.does_raise (fun () -> of_string (Int64.to_string 0x7fff_ffff_ffff_ffffL))
  ;;

  let%test _ = Exn.does_raise (fun () -> of_string "0o1111111111111111111111")
  let%test_unit _ = [%test_result: int] (popcount zero) ~expect:0
  let%test_unit _ = [%test_result: int] (popcount one) ~expect:1
  let%test_unit _ = [%test_result: int] (popcount minus_one) ~expect:63
  let%test_unit _ = [%test_result: int] (popcount max_value) ~expect:62
  let%test_unit _ = [%test_result: int] (popcount min_value) ~expect:1

  let%test_unit _ =
    [%test_result: string] (Hex.to_string max_value) ~expect:"0x3fffffffffffffff"
  ;;

  let%test_unit _ =
    [%test_result: string] (Hex.to_string (max_value - one)) ~expect:"0x3ffffffffffffffe"
  ;;

  let%test_unit _ =
    [%test_result: string] (Hex.to_string min_value) ~expect:"-0x4000000000000000"
  ;;

  let%test_unit _ =
    [%test_result: string] (Hex.to_string (min_value + one)) ~expect:"-0x3fffffffffffffff"
  ;;

  let%test_unit _ = [%test_result: string] (Hex.to_string zero) ~expect:"0x0"
  let%test_unit _ = [%test_result: string] (Hex.to_string one) ~expect:"0x1"
  let%test_unit _ = [%test_result: string] (Hex.to_string (neg one)) ~expect:"-0x1"

  let%test_unit _ =
    [%test_result: t] (Hex.of_string "0x3fffffffffffffff") ~expect:max_value
  ;;

  let%test_unit _ =
    [%test_result: t] (Hex.of_string "0x3ffffffffffffffe") ~expect:(max_value - one)
  ;;

  let%test_unit _ =
    [%test_result: t] (Hex.of_string "-0x4000000000000000") ~expect:min_value
  ;;

  let%test_unit _ =
    [%test_result: t] (Hex.of_string "-0x3fffffffffffffff") ~expect:(min_value + one)
  ;;

  let%test_unit _ = [%test_result: t] (Hex.of_string "0x0") ~expect:zero
  let%test_unit _ = [%test_result: t] (Hex.of_string "0x1") ~expect:one
  let%test_unit _ = [%test_result: t] (Hex.of_string "-0x1") ~expect:(neg one)

  let%test_unit _ =
    if Core.Int.( = ) Sys.word_size_in_bits 64
    then [%test_result: t] (of_string "0u4611686018427387904") ~expect:min_value
  ;;
end

let%test_module "Int63_emul" = (module Make_tests (Base.Int63.Private.Emul))
let%test_module "Int63_maybe_native" = (module Make_tests (Int63))

module Make_tests_bin_io (B : sig
  type t = Int63.t [@@deriving bin_io]
end) : sig end = struct
  let test int63 str =
    let open Int63 in
    let s = B.bin_size_t int63 in
    let bs = Bigstring.create s in
    let _pos = B.bin_write_t bs ~pos:0 int63 in
    let str_2 = Bigstring.to_string bs in
    let bs_2 = Bigstring.of_string str in
    let int63_2 = B.bin_read_t bs_2 ~pos_ref:(ref 0) in
    [%test_eq: string] str str_2;
    [%test_eq: t] int63 int63_2
  ;;

  let%test_unit _ =
    let open Int63 in
    test min_value "\252\000\000\000\000\000\000\000\192";
    test max_value "\252\255\255\255\255\255\255\255?";
    test zero "\000";
    test one "\001";
    test (neg one) "\255\255"
  ;;

  let%test _ =
    let open Bin_prot.Shape in
    Poly.( = )
      (eval_to_digest_string B.bin_shape_t)
      (eval_to_digest_string bin_shape_int63)
  ;;
end

let%test_module "Int63_bin_io_maybe_native" = (module Make_tests_bin_io (Int63))

let%test_module "Int63_bin_io_maybe_native_stable" =
  (module Make_tests_bin_io (Int63.Stable.V1))
;;
