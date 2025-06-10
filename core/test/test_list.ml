open! Core
open! Expect_test_helpers_core
open! List

let print2 x1 x2 = print_s [%message (x1 : int) (x2 : int)]
let print3 x1 x2 x3 = print_s [%message (x1 : int) (x2 : int) (x3 : int)]

let print2_and_return result x1 x2 =
  print2 x1 x2;
  result
;;

let test2 list_f f sexp_of_result =
  print_s [%message "" ~result:(list_f [ 1 ] [ 2; 3 ] ~f : result Or_unequal_lengths.t)];
  print_s
    [%message "" ~result:(list_f [ 1; 2 ] [ 3; 4 ] ~f : result Or_unequal_lengths.t)]
;;

let test3 list_f f sexp_of_result =
  print_s
    [%message
      "" ~result:(list_f [ 1 ] [ 2; 3 ] [ 4; 5 ] ~f : result Or_unequal_lengths.t)];
  print_s
    [%message
      "" ~result:(list_f [ 1; 2 ] [ 3; 4 ] [ 5; 6 ] ~f : result Or_unequal_lengths.t)]
;;

let%expect_test "[exists2]" =
  test2 exists2 (print2_and_return false) [%sexp_of: bool];
  [%expect
    {|
    (result Unequal_lengths)
    ((x1 1)
     (x2 3))
    ((x1 2)
     (x2 4))
    (result (Ok false))
    |}];
  test2 exists2 (print2_and_return true) [%sexp_of: bool];
  [%expect
    {|
    (result Unequal_lengths)
    ((x1 1)
     (x2 3))
    (result (Ok true))
    |}]
;;

let%expect_test "[fold2]" =
  test2
    (fold2 ~init:[])
    (fun ac x1 x2 ->
      print2 x1 x2;
      (x1, x2) :: ac)
    [%sexp_of: (int * int) list];
  [%expect
    {|
    (result Unequal_lengths)
    ((x1 1)
     (x2 3))
    ((x1 2)
     (x2 4))
    (result (
      Ok (
        (2 4)
        (1 3))))
    |}]
;;

let%expect_test "[for_all2]" =
  test2 for_all2 (print2_and_return false) [%sexp_of: bool];
  [%expect
    {|
    (result Unequal_lengths)
    ((x1 1)
     (x2 3))
    (result (Ok false))
    |}];
  test2 for_all2 (print2_and_return true) [%sexp_of: bool];
  [%expect
    {|
    (result Unequal_lengths)
    ((x1 1)
     (x2 3))
    ((x1 2)
     (x2 4))
    (result (Ok true))
    |}]
;;

let%expect_test "[iter2]" =
  test2 iter2 (print2_and_return ()) [%sexp_of: unit];
  [%expect
    {|
    (result Unequal_lengths)
    ((x1 1)
     (x2 3))
    ((x1 2)
     (x2 4))
    (result (Ok ()))
    |}]
;;

let%expect_test "[map2]" =
  test2
    map2
    (fun x1 x2 ->
      print2 x1 x2;
      x1, x2)
    [%sexp_of: (int * int) list];
  [%expect
    {|
    (result Unequal_lengths)
    ((x1 1)
     (x2 3))
    ((x1 2)
     (x2 4))
    (result (
      Ok (
        (1 3)
        (2 4))))
    |}]
;;

let%expect_test "[map3]" =
  test3
    map3
    (fun x1 x2 x3 ->
      print3 x1 x2 x3;
      x1, x2, x3)
    [%sexp_of: (int * int * int) list];
  [%expect
    {|
    (result Unequal_lengths)
    ((x1 1)
     (x2 3)
     (x3 5))
    ((x1 2)
     (x2 4)
     (x3 6))
    (result (
      Ok (
        (1 3 5)
        (2 4 6))))
    |}]
;;

let%expect_test "[rev_map2]" =
  test2
    rev_map2
    (fun x1 x2 ->
      print2 x1 x2;
      x1, x2)
    [%sexp_of: (int * int) list];
  [%expect
    {|
    (result Unequal_lengths)
    ((x1 1)
     (x2 3))
    ((x1 2)
     (x2 4))
    (result (
      Ok (
        (2 4)
        (1 3))))
    |}]
;;

let%expect_test "[rev_map3]" =
  test3
    rev_map3
    (fun x1 x2 x3 ->
      print3 x1 x2 x3;
      x1, x2, x3)
    [%sexp_of: (int * int * int) list];
  [%expect
    {|
    (result Unequal_lengths)
    ((x1 1)
     (x2 3)
     (x3 5))
    ((x1 2)
     (x2 4)
     (x3 6))
    (result (
      Ok (
        (2 4 6)
        (1 3 5))))
    |}]
;;

module%test [@name "zip_with_remainder"] _ = struct
  let check left right ~expect =
    [%test_result: (int * string) list * (int list, string list) Either.t option]
      (List.zip_with_remainder left right)
      ~expect
  ;;

  let numbers = [ 1; 2; 3 ]
  let words = [ "One"; "Two"; "Three" ]

  let%test_unit "equal length" =
    let expect = [ 1, "One"; 2, "Two"; 3, "Three" ], None in
    check numbers words ~expect
  ;;

  let%test_unit "right is longer" =
    let expect = [ 1, "One" ], Some (Second [ "Two"; "Three" ]) in
    check [ 1 ] words ~expect
  ;;

  let%test_unit "left is longer" =
    let expect = [ 1, "One" ], Some (First [ 2; 3 ]) in
    check numbers [ "One" ] ~expect
  ;;

  let%test_unit "empty" = check [] [] ~expect:([], None)
end

(* Test functions with templated definitions *)

module%test Nonvalue_layout_tests = struct
  module Generator = Base_quickcheck.Generator
  module Shrinker = Base_quickcheck.Shrinker
  open Generator.Let_syntax

  [%%template
  module
    [@kind k = (bits32, bits64, word, float64)] Test_against_boxed (T : sig
      module Boxed : Unboxed_test_harness.Boxed

      type t : k [@@deriving sexp_of]

      include
        Unboxed_test_harness.Boxable [@kind k] with type boxed := Boxed.t with type t := t

      val of_int : int -> t
      val to_int : t -> int
    end) : sig end = struct
    module Harness =
      Unboxed_test_harness.Make [@kind k]
        (T)
        (struct
          (* Almost all of these functions allocate, so we're not really concerned about
             testing allocation behavior specifically in optimized builds. *)
          let flambda2 = false
          let filenames_to_suppress_in_backtraces = [ "list0.ml"; "list.ml" ]
        end)

    open Harness
    open T

    let filter_fun i x = if i < 0 then to_int x < 1 else to_int x > -1
    let map_fun i x = x |> to_int |> ( + ) i |> ( * ) 2 |> of_int
    let fold_fun i x y = i + to_int x + to_int y |> of_int
    let fold_init () = of_int (-5)

    let make_iter_fun () =
      let acc = ref 0 in
      (fun i x -> acc := !acc + i + to_int x), acc
    ;;

    module List_pair = struct
      type t =
        { b_list : Boxed.t List.t
        ; u_list : (T.t List.t[@kind k])
        }
      [@@deriving sexp_of]

      let quickcheck_generator =
        let%bind b_list = [%generator: Boxed.t list] in
        let u_list = b_list |> (List.map [@kind value k]) ~f:unbox in
        return { b_list; u_list }
      ;;

      (* To avoid breaking invariants, don't shrink *)
      let quickcheck_shrinker : t Shrinker.t = Quickcheck.Shrinker.empty ()
    end

    let require_equal_lists ~f_boxed ~f_unboxed ({ b_list; u_list } : List_pair.t) =
      require_compare_equal
        ~is_zero_alloc_with_flambda2:false
        [%here]
        (module struct
          type t = Boxed.t List.t [@@deriving sexp_of, compare]
        end)
        (fun () -> f_boxed b_list)
        (fun () -> f_unboxed u_list |> (List.map [@kind k value]) ~f:box)
    ;;

    let test name ~f =
      let confirm_test = lazy (print_endline {%string|testing [%{name}]|}) in
      quickcheck_m (module List_pair) ~f:(fun pair ->
        force confirm_test;
        f pair)
    ;;

    let test_sexp_of () =
      test "sexp_of" ~f:(fun { b_list; u_list } ->
        require_equal
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (module Sexp)
          (fun () -> [%sexp_of: Boxed.t List.t] b_list)
          (fun () -> [%sexp_of: (T.t List.t[@kind k])] u_list))
    ;;

    let test_iter () =
      test "iter" ~f:(fun { b_list; u_list } ->
        require_equal
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (module Int)
          (fun () ->
            let f, r = make_iter_fun () in
            List.iter b_list ~f:(fun x -> f 0 (unbox x));
            !r)
          (fun () ->
            let f, r = make_iter_fun () in
            (List.iter [@kind k]) u_list ~f:(fun x -> f 0 x);
            !r))
    ;;

    let test_iteri () =
      test "iteri" ~f:(fun { b_list; u_list } ->
        require_equal
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (module Int)
          (fun () ->
            let f, r = make_iter_fun () in
            List.iteri b_list ~f:(fun i x -> f i (unbox x));
            !r)
          (fun () ->
            let f, r = make_iter_fun () in
            (List.iteri [@kind k]) u_list ~f:(fun i x -> f i x);
            !r))
    ;;

    let test_length () =
      test "length" ~f:(fun { b_list; u_list } ->
        require_equal
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (module Int)
          (fun () -> List.length b_list)
          (fun () -> (List.length [@kind k]) u_list))
    ;;

    let test_nth_exn () =
      let confirm_test = lazy (print_endline {%string|testing [nth_exn]|}) in
      quickcheck_m
        (module struct
          type t =
            List_pair.t
            * (int[@quickcheck.generator Generator.small_positive_or_zero_int])
          [@@deriving quickcheck ~generator ~shrinker, sexp_of]
        end)
        ~f:(fun ({ b_list; u_list }, n) ->
          force confirm_test;
          Harness.require_compare_equal_wrapped_allow_exn
            ~is_zero_alloc_with_flambda2:false
            [%here]
            (fun () -> List.nth_exn b_list n)
            (fun () -> (List.nth_exn [@kind k]) u_list n))
    ;;

    let test_nth () =
      let confirm_test = lazy (print_endline {%string|testing [nth]|}) in
      quickcheck_m
        (module struct
          type t =
            List_pair.t
            * (int[@quickcheck.generator Generator.small_positive_or_zero_int])
          [@@deriving quickcheck ~generator ~shrinker, sexp_of]
        end)
        ~f:(fun ({ b_list; u_list }, n) ->
          force confirm_test;
          let here = [%here] in
          Harness.require_compare_equal_wrapped_allow_exn
            ~is_zero_alloc_with_flambda2:false
            [%here]
            (fun () -> List.nth b_list n |> Option.value_exn ~here)
            (fun () ->
              (List.nth [@kind k]) u_list n |> (Option.value_exn [@kind k]) ~here))
    ;;

    let test_filter () =
      let filter_fun = filter_fun 0 in
      test
        "filter"
        ~f:
          (require_equal_lists
             ~f_boxed:(List.filter ~f:(fun x -> filter_fun (unbox x)))
             ~f_unboxed:((List.filter [@kind k]) ~f:(fun x -> filter_fun x)))
    ;;

    let test_filteri () =
      test
        "filteri"
        ~f:
          (require_equal_lists
             ~f_boxed:(List.filteri ~f:(fun i x -> filter_fun i (unbox x)))
             ~f_unboxed:((List.filteri [@kind k]) ~f:filter_fun))
    ;;

    let test_mapi () =
      test
        "mapi"
        ~f:
          (require_equal_lists
             ~f_boxed:(List.mapi ~f:(fun i x -> box (map_fun i (unbox x))))
             ~f_unboxed:((List.mapi [@kind k k]) ~f:map_fun))
    ;;

    let test_map () =
      let map_fun = map_fun 0 in
      test
        "map"
        ~f:
          (require_equal_lists
             ~f_boxed:(List.map ~f:(fun x -> box (map_fun (unbox x))))
             ~f_unboxed:((List.map [@kind k k]) ~f:(fun x -> map_fun x)))
    ;;

    let test_append () =
      test
        "append"
        ~f:
          (require_equal_lists
             ~f_boxed:(fun l -> List.append l l)
             ~f_unboxed:(fun l -> (List.append [@kind k]) l l))
    ;;

    let test_init () =
      let confirm_test = lazy (print_endline "testing [init]") in
      quickcheck_m
        (module struct
          type t = (int[@quickcheck.generator Generator.small_positive_or_zero_int])
          [@@deriving quickcheck ~generator ~shrinker, sexp_of]
        end)
        ~f:(fun n ->
          force confirm_test;
          require_compare_equal
            ~is_zero_alloc_with_flambda2:false
            [%here]
            (module struct
              type t = Boxed.t List.t [@@deriving sexp_of, compare]
            end)
            (fun () -> List.init n ~f:(fun i -> box (of_int i)))
            (fun () ->
              (List.init [@kind k]) n ~f:of_int |> (List.map [@kind k value]) ~f:box))
    ;;

    let test_rev_append () =
      test
        "rev_append"
        ~f:
          (require_equal_lists
             ~f_boxed:(fun l -> List.rev_append l l)
             ~f_unboxed:(fun l -> (List.rev_append [@kind k]) l l))
    ;;

    let test_rev () =
      test
        "rev"
        ~f:(require_equal_lists ~f_boxed:List.rev ~f_unboxed:(List.rev [@kind k]))
    ;;

    let test_rev_map () =
      let map_fun = map_fun 0 in
      test
        "rev_map"
        ~f:
          (require_equal_lists
             ~f_boxed:(List.rev_map ~f:(fun x -> box (map_fun (unbox x))))
             ~f_unboxed:((List.rev_map [@kind k k]) ~f:(fun x -> map_fun x)))
    ;;

    let test_rev_mapi () =
      test
        "rev_mapi"
        ~f:
          (require_equal_lists
             ~f_boxed:(List.rev_mapi ~f:(fun i x -> box (map_fun i (unbox x))))
             ~f_unboxed:((List.rev_mapi [@kind k k]) ~f:map_fun))
    ;;

    let test_filter_map () =
      let filter_fun = filter_fun 0 in
      let map_fun = map_fun 0 in
      test
        "filter_map"
        ~f:
          (require_equal_lists
             ~f_boxed:
               (List.filter_map ~f:(fun x ->
                  if filter_fun (unbox x) then Some (box (map_fun (unbox x))) else None))
             ~f_unboxed:
               ((List.filter_map [@kind k k]) ~f:(fun x ->
                  if filter_fun x
                  then (Some (map_fun x) : (_ Option.t[@kind k]))
                  else None)))
    ;;

    let test_filter_mapi () =
      test
        "filter_mapi"
        ~f:
          (require_equal_lists
             ~f_boxed:
               (List.filter_mapi ~f:(fun i x ->
                  if filter_fun i (unbox x)
                  then Some (box (map_fun i (unbox x)))
                  else None))
             ~f_unboxed:
               ((List.filter_mapi [@kind k k]) ~f:(fun i x ->
                  if filter_fun i x
                  then (Some (map_fun i x) : (_ Option.t[@kind k]))
                  else None)))
    ;;

    let test_fold () =
      let fold_fun = fold_fun 0 in
      test "fold" ~f:(fun { b_list; u_list } ->
        Harness.require_compare_equal_wrapped
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (fun () ->
            List.fold
              b_list
              ~init:(fold_init () |> box)
              ~f:(fun acc x -> fold_fun (unbox acc) (unbox x) |> box))
          (fun () -> (List.fold [@kind k k]) u_list ~init:(fold_init ()) ~f:fold_fun))
    ;;

    let test_foldi () =
      test "foldi" ~f:(fun { b_list; u_list } ->
        Harness.require_compare_equal_wrapped
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (fun () ->
            List.foldi
              b_list
              ~init:(fold_init () |> box)
              ~f:(fun i acc x -> fold_fun i (unbox acc) (unbox x) |> box))
          (fun () -> (List.foldi [@kind k k]) u_list ~init:(fold_init ()) ~f:fold_fun))
    ;;

    let () =
      test_sexp_of ();
      test_iter ();
      test_iteri ();
      test_length ();
      test_nth_exn ();
      test_nth ();
      test_filter ();
      test_filteri ();
      test_mapi ();
      test_map ();
      test_append ();
      test_init ();
      test_rev_append ();
      test_rev ();
      test_rev_map ();
      test_rev_mapi ();
      test_filter_map ();
      test_filter_mapi ();
      test_fold ();
      test_foldi ()
    ;;
  end

  let%expect_test "float64" =
    let module _ =
      Test_against_boxed [@kind float64] (struct
        include Float_u
        module Boxed = Float

        let to_int x = iround x |> Core.Option.value ~default:(-5)
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [iter]
      testing [iteri]
      testing [length]
      testing [nth_exn]
      testing [nth]
      testing [filter]
      testing [filteri]
      testing [mapi]
      testing [map]
      testing [append]
      testing [init]
      testing [rev_append]
      testing [rev]
      testing [rev_map]
      testing [rev_mapi]
      testing [filter_map]
      testing [filter_mapi]
      testing [fold]
      testing [foldi]
      |}]
  ;;

  let%expect_test "bits32" =
    let module _ =
      Test_against_boxed [@warning "-60"] [@kind bits32] (struct
        include Int32_u
        module Boxed = Int32

        let of_int = of_int_trunc
        let to_int = to_int_trunc
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [iter]
      testing [iteri]
      testing [length]
      testing [nth_exn]
      testing [nth]
      testing [filter]
      testing [filteri]
      testing [mapi]
      testing [map]
      testing [append]
      testing [init]
      testing [rev_append]
      testing [rev]
      testing [rev_map]
      testing [rev_mapi]
      testing [filter_map]
      testing [filter_mapi]
      testing [fold]
      testing [foldi]
      |}]
  ;;

  let%expect_test "bits64" =
    let module _ =
      Test_against_boxed [@warning "-60"] [@kind bits64] (struct
        include Int64_u
        module Boxed = Int64

        let to_int = to_int_trunc
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [iter]
      testing [iteri]
      testing [length]
      testing [nth_exn]
      testing [nth]
      testing [filter]
      testing [filteri]
      testing [mapi]
      testing [map]
      testing [append]
      testing [init]
      testing [rev_append]
      testing [rev]
      testing [rev_map]
      testing [rev_mapi]
      testing [filter_map]
      testing [filter_mapi]
      testing [fold]
      testing [foldi]
      |}]
  ;;

  let%expect_test "word" =
    let module _ =
      Test_against_boxed [@warning "-60"] [@kind word] (struct
        include Nativeint_u
        module Boxed = Nativeint

        let to_int = to_int_trunc
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [iter]
      testing [iteri]
      testing [length]
      testing [nth_exn]
      testing [nth]
      testing [filter]
      testing [filteri]
      testing [mapi]
      testing [map]
      testing [append]
      testing [init]
      testing [rev_append]
      testing [rev]
      testing [rev_map]
      testing [rev_mapi]
      testing [filter_map]
      testing [filter_mapi]
      testing [fold]
      testing [foldi]
      |}]
  ;;]
end
