open! Core
open! Import

(* Check that the Or_error.t generator is indeed hitting both cases with non-trivial
   frequency. Out of 500 coin flips, the probability of getting <125 or >375 heads
   is about 2e-30. *)
let%test_unit "generator" =
  let generator = [%quickcheck.generator: int Or_error.t] in
  Base_quickcheck.Test.with_sample_exn generator ~f:(fun sequence ->
    let num_ok = Sequence.take sequence 500 |> Sequence.count ~f:Or_error.is_ok in
    assert (125 <= num_ok && num_ok <= 375))
;;
