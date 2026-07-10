open! Core
open! Import
open! Or_null

module%test [@name "shrinker"] _ = struct
  module Shrinker = Quickcheck.Shrinker

  let t1 = Shrinker.create (Fn.const (Sequence.singleton 1))

  let%test_unit _ =
    [%test_result: int or_null list]
      (Sequence.to_list (Shrinker.shrink (quickcheck_shrinker t1) Null))
      ~expect:[]
  ;;

  let%test_unit _ =
    let sort = List.sort ~compare:[%compare: int or_null] in
    let expect = [ Null; This 1 ] |> sort in
    let results =
      Shrinker.shrink (quickcheck_shrinker t1) (This 5) |> Sequence.to_list |> sort
    in
    [%test_result: int or_null list] ~expect results
  ;;
end

let%expect_test "unsafe_value" =
  let test x =
    require (phys_equal x (Optional_syntax.Optional_syntax.unsafe_value (This x)))
  in
  test 5;
  [%expect {| |}];
  test "hello";
  [%expect {| |}]
;;

let%expect_test "Stable.V1" =
  let diff =
    let diff_printer = unstage (Expect_test_patdiff.diff_printer None ~context:0) in
    fun () -> diff_printer (expect_test_output ())
  in
  let examples =
    [ Null; This 0; This (-1); This 1; This 0x3fff_ffff; This (-0x3fff_ffff) ]
  in
  (* Start by testing Stable.V1 normally. *)
  print_and_check_stable_type
    (module struct
      type t = int Stable.V1.t [@@deriving bin_io, compare, sexp]
    end)
    examples;
  diff ();
  [%expect
    {|
    (bin_shape_digest 7704b32b74cc1656f64d5d60f5baf936)
    ((sexp ()) (bin_io "\000"))
    ((sexp (0)) (bin_io "\001\000"))
    ((sexp (-1)) (bin_io "\001\255\255"))
    ((sexp (1)) (bin_io "\001\001"))
    ((sexp (1_073_741_823)) (bin_io "\001\253\255\255\255?"))
    ((sexp (-1_073_741_823)) (bin_io "\001\253\001\000\000\192"))
    |}];
  (* Diff with Bin_shape_same_as_option: only the digest changes. *)
  print_and_check_stable_type
    (module struct
      type t = int Stable.V1.Bin_shape_same_as_option.t [@@deriving bin_io, compare, sexp]
    end)
    examples;
  diff ();
  [%expect
    {|
    === DIFF HUNK ===
    -|(bin_shape_digest 7704b32b74cc1656f64d5d60f5baf936)
    +|(bin_shape_digest 33fd4ff7bde530bddf13dfa739207fae)
    |}];
  (* Diff that with Option.Stable.V1: should be identical. *)
  print_and_check_stable_type
    (module struct
      type t = int Option.Stable.V1.t [@@deriving bin_io, compare, sexp]
    end)
    (List.map examples ~f:Or_null.to_option);
  diff ();
  [%expect {| |}]
;;
