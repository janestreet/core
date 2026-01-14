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
