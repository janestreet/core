(* [Popcount] is in [Core_kernel], but we test it with [Core] so we can use
   [Version_utils]. *)
open Core

let test_int       n bits = [%test_result: int] (Int.popcount       n) ~expect:bits
let test_int32     n bits = [%test_result: int] (Int32.popcount     n) ~expect:bits
let test_int64     n bits = [%test_result: int] (Int64.popcount     n) ~expect:bits
let test_nativeint n bits = [%test_result: int] (Nativeint.popcount n) ~expect:bits

(* test simple constants and boundary conditions *)

let%test_unit _ = test_int       0                   0
let%test_unit _ = test_int       1                   1
let%test_unit _ = test_int       (-1)                Int.num_bits
let%test_unit _ = test_int       Int.max_value       (Int.num_bits - 1)
let%test_unit _ = test_int       Int.min_value       1

let%test_unit _ = test_int32     0l                  0
let%test_unit _ = test_int32     1l                  1
let%test_unit _ = test_int32     (-1l)               32
let%test_unit _ = test_int32     Int32.max_value     31
let%test_unit _ = test_int32     Int32.min_value     1

let%test_unit _ = test_int64     0L                  0
let%test_unit _ = test_int64     1L                  1
let%test_unit _ = test_int64     (-1L)               64
let%test_unit _ = test_int64     Int64.max_value     63
let%test_unit _ = test_int64     Int64.min_value     1

let%test_unit _ = test_nativeint 0n                  0
let%test_unit _ = test_nativeint 1n                  1
let%test_unit _ = test_nativeint (-1n)               Nativeint.num_bits
let%test_unit _ = test_nativeint Nativeint.max_value (Nativeint.num_bits - 1)
let%test_unit _ = test_nativeint Nativeint.min_value 1

(* test that we can account for each bit individually *)

let%test_unit _ =
  for i = 0 to Int.num_bits - 1 do
    let n = 1 lsl i in
    test_int n 1;
    test_int (lnot n) (Int.num_bits - 1)
  done

let%test_unit _ =
  for i = 0 to 31 do
    let n = Int32.shift_left 1l i in
    test_int32 n 1;
    test_int32 (Int32.bit_not n) 31
  done

let%test_unit _ =
  for i = 0 to 63 do
    let n = Int64.shift_left 1L i in
    test_int64 n 1;
    test_int64 (Int64.bit_not n) 63
  done

let%test_unit _ =
  for i = 0 to Nativeint.num_bits - 1 do
    let n = Nativeint.shift_left 1n i in
    test_nativeint n 1;
    test_nativeint (Nativeint.bit_not n) (Nativeint.num_bits - 1)
  done

(* Make sure unboxing works as expected and so forth, which it wouldn't if we used C
   stubs with boxed values for [int64], [int32], and [nativeint].  Use random inputs
   to make sure the compiler can't inline and precompute results. *)

let does_not_allocate f =
  let test () =
    let len = 100 in
    let inputs = ArrayLabels.init len ~f:(fun _ -> Random.bits ()) in
    let minor_before = Gc.minor_words () in
    for i = 0 to len-1 do
      ignore (f inputs.(i) : int)
    done;
    let minor_after = Gc.minor_words () in
    [%test_result: int]
      (minor_after - minor_before)
      ~expect:0
      ~message:"number of words allocated"
  in
  (* On 32-bit systems, int64 cannot be unboxed, so this test only makes sense on 64-bit
     systems.  Also, without cross-library inlining, the arguments to popcount cannot be
     unboxed. *)
  match Word_size.word_size with
  | W64 when Version_util.x_library_inlining -> test ()
  | _ -> ()

let%test_unit _ = does_not_allocate (fun x -> Int.popcount x)
let%test_unit _ = does_not_allocate (fun x -> Int32.popcount     (Caml.Int32.of_int     x))
let%test_unit _ = does_not_allocate (fun x -> Int64.popcount     (Caml.Int64.of_int     x))
let%test_unit _ = does_not_allocate (fun x -> Nativeint.popcount (Caml.Nativeint.of_int x))

