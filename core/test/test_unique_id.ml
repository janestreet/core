open! Core
open! Import

(* These tests pin down the [bin_shape] digests of [Unique_id]'s [Int63]-based functors.
   The library is built and tested both natively and under [js_of_ocaml], so these
   assertions hold on both flavors. In particular, [Unique_id.Atomic.Int63] is implemented
   differently depending on [Core.Int63.Private.repr] (native [int] vs [Int63_emul]);
   having the digests line up across platforms is the whole point of overriding
   [bin_shape_t] in the [Int]-repr branch. *)

let digest_of bin_shape_t = Bin_prot.Shape.eval_to_digest_string bin_shape_t

let%expect_test "Int63-based [Unique_id] functors share bin_shape with [Int63]" =
  let module Atomic_int63 = Unique_id.Atomic.Int63 () in
  let module Plain_int63 = Unique_id.Int63 () in
  let int63_digest = digest_of Int63.bin_shape_t in
  let test ~(here : [%call_pos]) shape =
    require_equal ~here (module String) (digest_of shape) int63_digest
  in
  test Atomic_int63.bin_shape_t;
  test Plain_int63.bin_shape_t;
  test Atomic_int63.Stable.V1.bin_shape_t;
  test Plain_int63.Stable.V1.bin_shape_t;
  print_endline int63_digest;
  [%expect {| 2b528f4b22f08e28876ffe0239315ac2 |}]
;;

let%expect_test "Int-based [Unique_id] functors share bin_shape with [Int]" =
  let module Atomic_int = Unique_id.Atomic.Int () in
  let module Plain_int = Unique_id.Int () in
  let int_digest = digest_of Int.bin_shape_t in
  let test ~(here : [%call_pos]) shape =
    require_equal ~here (module String) (digest_of shape) int_digest
  in
  test Atomic_int.bin_shape_t;
  test Plain_int.bin_shape_t;
  test Atomic_int.Stable.V1.bin_shape_t;
  test Plain_int.Stable.V1.bin_shape_t;
  print_endline int_digest;
  [%expect {| 698cfa4093fe5e51523842d37b92aeac |}]
;;
