open! Core
open Expect_test_helpers_core

let%expect_test "Stable.V1" =
  print_and_check_stable_type
    [%here]
    (module Uchar.Stable.V1)
    [ Uchar.min_value (* 0x0000 *)
    ; Uchar.of_scalar_exn 0x007F
    ; Uchar.of_scalar_exn 0x0080
    ; Uchar.of_scalar_exn 0x07FF
    ; Uchar.of_scalar_exn 0x0800
    ; Uchar.of_scalar_exn 0xD7FF
    ; Uchar.of_scalar_exn 0xE000
    ; Uchar.of_scalar_exn 0xFFFF
    ; Uchar.of_scalar_exn 0x10000
    ; Uchar.max_value (* 0x10FFFF *)
    ];
  [%expect
    {|
    (bin_shape_digest 40749bdc617e2a32c1f87913f6829eaa)
    ((sexp   U+0000)
     (bin_io "\000"))
    ((sexp   U+007F)
     (bin_io "\127"))
    ((sexp   U+0080)
     (bin_io "\254\128\000"))
    ((sexp   U+07FF)
     (bin_io "\254\255\007"))
    ((sexp   U+0800)
     (bin_io "\254\000\b"))
    ((sexp   U+D7FF)
     (bin_io "\253\255\215\000\000"))
    ((sexp   U+E000)
     (bin_io "\253\000\224\000\000"))
    ((sexp   U+FFFF)
     (bin_io "\253\255\255\000\000"))
    ((sexp   U+10000)
     (bin_io "\253\000\000\001\000"))
    ((sexp   U+10FFFF)
     (bin_io "\253\255\255\016\000"))
    |}]
;;

let%expect_test "Test [Uchar.t] quickcheck generator" =
  let test_can_generate =
    Quickcheck.test_can_generate Uchar.quickcheck_generator ~sexp_of:[%sexp_of: Uchar.t]
  in
  test_can_generate ~f:(fun uchar -> Uchar.Utf8.byte_length uchar = 1);
  test_can_generate ~f:(fun uchar -> Uchar.Utf8.byte_length uchar = 2);
  test_can_generate ~f:(fun uchar -> Uchar.Utf8.byte_length uchar = 3);
  test_can_generate ~f:(fun uchar -> Uchar.Utf8.byte_length uchar = 4);
  test_can_generate ~f:(fun uchar -> Uchar.Utf16le.byte_length uchar = 2);
  test_can_generate ~f:(fun uchar -> Uchar.Utf16le.byte_length uchar = 4);
  Quickcheck.test_distinct_values
    Uchar.quickcheck_generator
    ~sexp_of:[%sexp_of: Uchar.t]
    ~trials:1_000
    ~distinct_values:500
    ~compare:Uchar.compare;
  [%expect {| |}]
;;
