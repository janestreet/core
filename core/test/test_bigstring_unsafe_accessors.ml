open! Core
open Poly
open! Bigstring

let buf = create 256

let test_accessor ~buf to_str ~fget ~fset vals =
  List.foldi ~init:true vals ~f:(fun i passing x ->
    fset buf ~pos:0 x;
    let y = fget buf ~pos:0 in
    if x <> y then eprintf "Value %d: expected %s, got %s\n" i (to_str x) (to_str y);
    x = y && passing)
;;

let%test _ =
  test_accessor
    ~buf
    Int.to_string
    ~fget:unsafe_get_int16_le
    ~fset:(fun x ~pos value -> unsafe_set_int16_le x ~pos value)
    [ -32768; -1; 0; 1; 32767 ]
;;

let%test _ =
  test_accessor
    ~buf
    Int.to_string
    ~fget:unsafe_get_uint16_le
    ~fset:unsafe_set_uint16_le
    [ 0; 1; 65535 ]
;;

let%test _ =
  test_accessor
    ~buf
    Int.to_string
    ~fget:unsafe_get_int16_be
    ~fset:(fun t ~pos value -> unsafe_set_int16_be t ~pos value)
    [ -32768; -1; 0; 1; 32767 ]
;;

let%test _ =
  test_accessor
    ~buf
    Int.to_string
    ~fget:unsafe_get_uint16_be
    ~fset:unsafe_set_uint16_be
    [ 0; 1; 65535 ]
;;

let%test (_ [@tags "64-bits-only"]) =
  test_accessor
    ~buf
    Int.to_string
    ~fget:unsafe_get_int32_le
    ~fset:(fun t ~pos value -> unsafe_set_int32_le t ~pos value)
    [ Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L ]
;;

let%test (_ [@tags "64-bits-only"]) =
  test_accessor
    ~buf
    Int.to_string
    ~fget:unsafe_get_int32_be
    ~fset:(fun t ~pos value -> unsafe_set_int32_be t ~pos value)
    [ Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L ]
;;

let%test (_ [@tags "64-bits-only"]) =
  test_accessor
    ~buf
    Int.to_string
    ~fget:unsafe_get_int64_le_exn
    ~fset:unsafe_set_int64_le
    [ Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L ]
;;

let%test (_ [@tags "64-bits-only"]) =
  test_accessor
    ~buf
    Int.to_string
    ~fget:unsafe_get_int64_be_exn
    ~fset:unsafe_set_int64_be
    [ Int64.to_int_exn (-0x4000_0000_0000_0000L)
    ; Int64.to_int_exn (-2147483648L)
    ; -1
    ; 0
    ; 1
    ; Int64.to_int_exn 2147483647L
    ; Int64.to_int_exn 0x3fff_ffff_ffff_ffffL
    ]
;;

let%test (_ [@tags "64-bits-only"]) =
  List.for_all
    [ unsafe_get_uint64_be_exn, unsafe_set_uint64_be
    ; unsafe_get_uint64_le_exn, unsafe_set_uint64_le
    ]
    ~f:(fun (fget, fset) ->
      test_accessor
        ~buf
        Int.to_string
        ~fget
        ~fset
        ([ 0L; 1L; 0xffff_ffffL; 0x3fff_ffff_ffff_ffffL ] |> List.map ~f:Int64.to_int_exn))
;;

let%test_unit _ =
  List.iter
    [ "\x40\x00\x00\x00\x00\x00\x00\x00"
    ; "\x80\x00\x00\x00\x00\x00\x00\x00"
    ; "\xA0\x00\x00\x00\x00\x00\x00\x00"
    ; "\xF0\x00\x00\x00\x00\x00\x00\x00"
    ; "\x4F\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
    ; "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF"
    ]
    ~f:(fun string ->
    assert (Exn.does_raise (fun () -> unsafe_get_uint64_be_exn ~pos:0 (of_string string)));
    assert (
      Exn.does_raise (fun () ->
        unsafe_get_uint64_le_exn ~pos:0 (of_string (String.rev string)))))
;;

let%test _ =
  test_accessor
    ~buf
    Int64.to_string
    ~fget:(fun t ~pos -> unsafe_get_int64_t_le t ~pos)
    ~fset:(fun t ~pos v -> unsafe_set_int64_t_le t ~pos v)
    [ -0x8000_0000_0000_0000L
    ; -0x789A_BCDE_F012_3456L
    ; -0xFFL
    ; Int64.minus_one
    ; Int64.zero
    ; Int64.one
    ; 0x789A_BCDE_F012_3456L
    ; 0x7FFF_FFFF_FFFF_FFFFL
    ]
;;

let%test _ =
  test_accessor
    ~buf
    Int64.to_string
    ~fget:(fun t ~pos -> unsafe_get_int64_t_be t ~pos)
    ~fset:(fun t ~pos v -> unsafe_set_int64_t_be t ~pos v)
    [ -0x8000_0000_0000_0000L
    ; -0x789A_BCDE_F012_3456L
    ; -0xFFL
    ; Int64.minus_one
    ; Int64.zero
    ; Int64.one
    ; 0x789A_BCDE_F012_3456L
    ; 0x7FFF_FFFF_FFFF_FFFFL
    ]
;;

let%test _ =
  test_accessor
    ~buf
    Int64.to_string
    ~fget:(fun t ~pos -> unsafe_get_int64_t_be t ~pos)
    ~fset:(fun t ~pos v -> unsafe_set_int64_t_be t ~pos v)
    [ -0x8000_0000_0000_0000L
    ; -0x789A_BCDE_F012_3456L
    ; -0xFFL
    ; Int64.minus_one
    ; Int64.zero
    ; Int64.one
    ; 0x789A_BCDE_F012_3456L
    ; 0x7FFF_FFFF_FFFF_FFFFL
    ]
;;

(* Test 63/64-bit precision boundary.

   Seen on a data stream the constant 0x4000_0000_0000_0000 is supposed to represent a
   64-bit positive integer (2^62).

   Whilst this bit pattern does fit in an OCaml [int] on a 64-bit machine, it is the
   representation of a negative number ([Int.min_value]), and in particular is not the
   representation of 2^62.  It is thus suitable for this test. *)
let test_int64 get_exn get_trunc set_t double_check_set =
  List.iter
    [ 0x4000_0000_0000_0000L
    ; Int64.succ (Int64.of_int Int.max_value)
    ; Int64.pred (Int64.of_int Int.min_value)
    ; Int64.min_value
    ; Int64.max_value
    ; Int64.succ Int64.min_value
    ; Int64.pred Int64.max_value
    ]
    ~f:(fun too_big ->
      let trunc = Int.of_int64_trunc too_big in
      try
        set_t buf ~pos:0 too_big;
        [%test_result: int64] ~expect:too_big (double_check_set buf ~pos:0);
        let test_get name got =
          [%test_pred: string Or_error.t]
            is_error
            ~message:name
            (Or_error.map ~f:(fun i -> sprintf "%d = 0x%x" i i) got)
        in
        let got_exn = Or_error.try_with (fun () -> get_exn buf ~pos:0) in
        test_get "get_exn" got_exn;
        [%test_result: int] ~message:"get_trunc" ~expect:trunc (get_trunc buf ~pos:0)
      with
      | e ->
        failwiths
          ~here:[%here]
          "test_int64"
          ( sprintf "too_big = %LdL = 0x%LxL" too_big too_big
          , sprintf "trunc = %d = 0x%x" trunc trunc
          , e )
          [%sexp_of: string * string * exn])
;;

let%test_unit "unsafe_get_int64_le" =
  test_int64
    unsafe_get_int64_le_exn
    unsafe_get_int64_le_trunc
    unsafe_set_int64_t_le
    unsafe_get_int64_t_le
;;

let%test_unit "unsafe_get_int64_be" =
  test_int64
    unsafe_get_int64_be_exn
    unsafe_get_int64_be_trunc
    unsafe_set_int64_t_be
    unsafe_get_int64_t_be
;;
