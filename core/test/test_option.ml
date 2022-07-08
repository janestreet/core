open! Core
open! Import
open! Option

let%test_module "shrinker" =
  (module struct
    module Shrinker = Quickcheck.Shrinker

    let t1 = Shrinker.create (Fn.const (Sequence.singleton 1))

    let%test_unit _ =
      [%test_result: int option list]
        (Sequence.to_list (Shrinker.shrink (quickcheck_shrinker t1) None))
        ~expect:[]
    ;;

    let%test_unit _ =
      let sort = List.sort ~compare:[%compare: int option] in
      let expect = [ None; Some 1 ] |> sort in
      let results =
        Shrinker.shrink (quickcheck_shrinker t1) (Some 5) |> Sequence.to_list |> sort
      in
      [%test_result: int option list] ~expect results
    ;;
  end)
;;

let%expect_test "unsafe_value" =
  let test x =
    require [%here] (phys_equal x (Optional_syntax.Optional_syntax.unsafe_value (Some x)))
  in
  test 5;
  [%expect {| |}];
  test "hello";
  [%expect {| |}]
;;
