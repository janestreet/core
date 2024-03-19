open! Core
module Merge_to_sequence_element = Set_intf.Merge_to_sequence_element
module With_comparator = Set_intf.With_comparator
module Without_comparator = Set_intf.Without_comparator
module With_first_class_module = Set_intf.With_first_class_module
module Named = Set.Named
open Expect_test_helpers_core

module Unit_tests (Elt : sig
  type 'a t [@@deriving sexp, hash]

  val of_int : int -> int t
  val to_int : int t -> int
end) (Set : sig
  type ('a, 'b) t_
  type ('a, 'b) set
  type ('a, 'b) tree
  type ('a, 'b, 'c) create_options
  type ('a, 'b, 'c) access_options

  include
    Set_intf.Creators_and_accessors_generic
      with type ('a, 'b) t := ('a, 'b) t_
      with type ('a, 'b) set := ('a, 'b) set
      with type ('a, 'b) tree := ('a, 'b) tree
      with type 'a elt := 'a Elt.t
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) create_options
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) access_options

  val simplify_creator : (int, Int.comparator_witness, 'c) create_options -> 'c
  val simplify_accessor : (int, Int.comparator_witness, 'c) access_options -> 'c
  val kind : [ `Set | `Tree ]
  val is_poly : bool
end) : Set_intf.Creators_and_accessors_generic = struct
  module Set = struct
    include Set

    let add = simplify_accessor add
    let remove = simplify_accessor remove
    let mem = simplify_accessor mem

    (* let filter          = simplify_accessor filter *)
    let compare_direct = simplify_accessor compare_direct
    let equal = simplify_accessor equal
    let inter = simplify_accessor inter
    let union = simplify_accessor union
    let is_subset = simplify_accessor is_subset
    let are_disjoint = simplify_accessor are_disjoint

    module Named = struct
      let is_subset = simplify_accessor Set.Named.is_subset
      let equal = simplify_accessor Set.Named.equal
    end

    let iter2 = simplify_accessor iter2
    let invariants = simplify_accessor invariants
    let to_map = simplify_accessor to_map
    let quickcheck_shrinker = simplify_accessor quickcheck_shrinker
    let to_list = to_list
    let to_array = to_array

    let to_sequence ?order ?greater_or_equal_to ?less_or_equal_to x =
      simplify_accessor to_sequence ?order ?greater_or_equal_to ?less_or_equal_to x
    ;;

    let binary_search = simplify_accessor binary_search
    let binary_search_segmented = simplify_accessor binary_search_segmented

    let merge_to_sequence ?order ?greater_or_equal_to ?less_or_equal_to x y =
      simplify_accessor
        merge_to_sequence
        ?order
        ?greater_or_equal_to
        ?less_or_equal_to
        x
        y
    ;;

    let empty () = simplify_creator empty
    let singleton = simplify_creator singleton
    let of_list = simplify_creator of_list
    let of_sequence = simplify_creator of_sequence
    let of_hash_set = simplify_creator of_hash_set
    let of_hashtbl_keys = simplify_creator of_hashtbl_keys
    let of_sorted_array = simplify_creator of_sorted_array
    let of_sorted_array_unchecked = simplify_creator of_sorted_array_unchecked

    (* let of_tree        = simplify_creator of_tree *)
    let quickcheck_generator = simplify_creator quickcheck_generator
    let symmetric_diff = simplify_accessor symmetric_diff
    let split = simplify_accessor split
    let split_le_gt = simplify_accessor split_le_gt
    let split_lt_ge = simplify_accessor split_lt_ge
    let diff = simplify_accessor diff
    let sexp_of_t_ t = [%sexp_of: int Elt.t list] (to_list t)

    type t = (int, Int.comparator_witness) t_

    let sexp_of_t = sexp_of_t_
    let compare = compare_direct
  end

  type ('a, 'b) t = Unit_test_follows
  type ('a, 'b) tree = ('a, 'b) t
  type ('a, 'b) set = ('a, 'b) t
  type 'a elt
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) Without_comparator.t
  type ('a, 'b, 'c) access_options = ('a, 'b, 'c) Without_comparator.t
  type 'a cmp

  module Elt = struct
    open Elt

    let of_int = of_int
    let to_int = to_int

    let quickcheck_generator =
      let open Quickcheck.Generator in
      Int.quickcheck_generator >>| of_int
    ;;

    let quickcheck_observer = Quickcheck.Observer.unmap Int.quickcheck_observer ~f:to_int

    module T = struct
      type t = int Elt.t [@@deriving sexp, hash]

      let compare t t' = Poly.compare (to_int t) (to_int t')
      let hash t = Hashtbl.hash (to_int t)
    end

    include T
    include Comparable.Make (T)
    include Hashable.Make (T)

    let samples = List.dedup_and_sort ~compare (List.init 10 ~f:(fun i -> of_int (i + 1)))
    let absent = of_int 0
    let present = of_int 1
    let () = assert (List.mem ~equal samples present)
    let () = assert (not (List.mem ~equal samples absent))
  end

  let set_empty = Set.empty ()
  let set_nonempty = Set.of_list Elt.samples

  let gen_set =
    let open Quickcheck.Generator in
    List.quickcheck_generator Elt.quickcheck_generator >>| Set.of_list
  ;;

  let add _ = assert false
  let of_list _ = assert false
  let of_sequence _ = assert false
  let of_hash_set _ = assert false
  let of_hashtbl_keys _ = assert false
  let mem _ = assert false
  let%test _ = List.for_all Elt.samples ~f:(fun e -> Set.mem set_nonempty e)
  let is_empty _ = assert false
  let%test _ = Set.is_empty set_empty
  let%test _ = not (Set.is_empty set_nonempty)

  let%test _ =
    let set' = List.fold Elt.samples ~init:(Set.empty ()) ~f:Set.add in
    Set.equal set_nonempty set'
  ;;

  let inter _ = assert false
  let%test _ = Set.is_empty (Set.inter set_empty set_nonempty)
  let%test _ = Set.is_empty (Set.inter set_nonempty set_empty)

  let%test _ =
    let single = Set.singleton Elt.absent in
    Set.equal single (Set.inter single (Set.add set_nonempty Elt.absent))
  ;;

  let%test _ = Set.equal set_nonempty (Set.inter set_nonempty set_nonempty)

  let%test_unit "inter = all members of s1 that are members of s2" =
    Quickcheck.test
      (Quickcheck.Generator.tuple2 gen_set gen_set)
      ~sexp_of:[%sexp_of: Set.t * Set.t]
      ~f:(fun (s1, s2) ->
      [%test_result: Set.t]
        (Set.inter s1 s2)
        ~expect:
          (Set.fold s1 ~init:(Set.empty ()) ~f:(fun inter elt ->
             if Set.mem s2 elt then Set.add inter elt else inter)))
  ;;

  let is_subset _ ~of_:_ = assert false
  let%test _ = Set.is_subset set_empty ~of_:set_nonempty
  let%test _ = not (Set.is_subset set_nonempty ~of_:set_empty)
  let%test _ = Set.is_subset set_nonempty ~of_:set_nonempty
  let%test _ = Set.is_subset set_empty ~of_:set_empty
  let%test _ = not (Set.is_subset set_nonempty ~of_:(Set.singleton Elt.present))

  let%test_unit "is_subset = all members of subset are members of superset" =
    Quickcheck.test
      (Quickcheck.Generator.tuple2 gen_set gen_set)
      ~sexp_of:[%sexp_of: Set.t * Set.t]
      ~f:(fun (superset, subset) ->
      [%test_result: bool]
        (Set.is_subset subset ~of_:superset)
        ~expect:(Set.for_all subset ~f:(Set.mem superset)))
  ;;

  let are_disjoint _ _ = assert false

  let%test_unit "are_disjoint = is_empty inter" =
    Quickcheck.test
      (Quickcheck.Generator.tuple2 gen_set gen_set)
      ~sexp_of:[%sexp_of: Set.t * Set.t]
      ~f:(fun (s1, s2) ->
      [%test_result: bool]
        (Set.are_disjoint s1 s2)
        ~expect:(Set.is_empty (Set.inter s1 s2)))
  ;;

  module Named = struct
    let is_subset _ ~of_:_ = assert false

    let%expect_test "validate_subset" =
      let (map1 : _ Named.t) = { name = "the empty set"; set = set_empty } in
      let (map2 : _ Named.t) =
        { name = "the set of integers from 1 to 10"; set = set_nonempty }
      in
      print_s [%sexp (Set.Named.is_subset map2 ~of_:map1 : unit Or_error.t)];
      if Set.is_poly
      then
        [%expect
          {|
          (Error (
            "the set of integers from 1 to 10 is not a subset of the empty set"
            (invalid_elements (_ _ _ _ _ _ _ _ _ _))))
          |}]
      else
        [%expect
          {|
          (Error (
            "the set of integers from 1 to 10 is not a subset of the empty set"
            (invalid_elements (1 2 3 4 5 6 7 8 9 10))))
          |}];
      print_s [%sexp (Set.Named.is_subset map1 ~of_:map2 : unit Or_error.t)];
      [%expect {| (Ok ()) |}]
    ;;

    let equal _ _ = assert false

    let%expect_test "validate_subset" =
      let (map1 : _ Named.t) =
        { name = "the set of integers from 1 to 10"; set = set_nonempty }
      in
      let (map2 : _ Named.t) =
        { name = "the set of integers from 10 to 19"
        ; set = Set.of_list (List.init 10 ~f:(fun i -> Elt.of_int (i + 10)))
        }
      in
      print_s [%sexp (Set.Named.equal map1 map2 : unit Or_error.t)];
      if Set.is_poly
      then
        [%expect
          {|
          (Error (
            ("the set of integers from 1 to 10 is not a subset of the set of integers from 10 to 19"
             (invalid_elements (_ _ _ _ _ _ _ _ _)))
            ("the set of integers from 10 to 19 is not a subset of the set of integers from 1 to 10"
             (invalid_elements (_ _ _ _ _ _ _ _ _)))))
          |}]
      else
        [%expect
          {|
          (Error (
            ("the set of integers from 1 to 10 is not a subset of the set of integers from 10 to 19"
             (invalid_elements (1 2 3 4 5 6 7 8 9)))
            ("the set of integers from 10 to 19 is not a subset of the set of integers from 1 to 10"
             (invalid_elements (11 12 13 14 15 16 17 18 19)))))
          |}];
      print_s [%sexp (Set.Named.equal map1 map1 : unit Or_error.t)];
      [%expect {| (Ok ()) |}]
    ;;
  end

  let to_list _ = assert false

  let%test _ =
    let elts = Set.to_list set_nonempty in
    List.for_all elts ~f:(fun elt -> Set.mem set_nonempty elt)
  ;;

  let rec is_list_ordered_ascending xs =
    match xs with
    | [] | [ _ ] -> true
    | a :: b :: xs' -> Elt.compare a b < 0 && is_list_ordered_ascending (b :: xs')
  ;;

  let%test _ = is_list_ordered_ascending (Set.to_list set_nonempty)
  let to_array _ = assert false

  let%test _ =
    let a = Set.to_array set_nonempty in
    List.equal Elt.equal (Array.to_list a) (Set.to_list set_nonempty)
  ;;

  let to_sequence ?order:_ ?greater_or_equal_to:_ ?less_or_equal_to:_ _ : _ Sequence.t =
    assert false
  ;;

  let%test_module "to_sequence" =
    (module struct
      let ( <=> ) observed expected =
        [%test_eq: Elt.t list] (Sequence.to_list observed) expected
      ;;

      let m = set_nonempty

      (* Calibration: make sure [m] contains elements less than 4, greater than 8, and
         between these two. Otherwise the tests aren't testing what we want. *)
      let%test _ =
        let l = Set.to_list m in
        List.exists l ~f:(fun x -> Elt.(x < of_int 4))
        && List.exists l ~f:(fun x -> Elt.(x >= of_int 4) && Elt.(x <= of_int 8))
        && List.exists l ~f:(fun x -> Elt.(x > of_int 8))
      ;;

      let%test_unit _ = Set.to_sequence ~order:`Increasing m <=> Set.to_list m
      let%test_unit _ = Set.to_sequence ~order:`Decreasing m <=> List.rev (Set.to_list m)

      let%test_unit _ =
        Set.to_sequence ~order:`Increasing ~greater_or_equal_to:(Elt.of_int 4) m
        <=> List.filter ~f:(fun x -> Elt.(x >= of_int 4)) (Set.to_list m)
      ;;

      let%test_unit _ =
        Set.to_sequence
          m
          ~order:`Increasing
          ~greater_or_equal_to:(Elt.of_int 4)
          ~less_or_equal_to:(Elt.of_int 8)
        <=> List.filter
              ~f:(fun x -> Elt.(x >= of_int 4) && Elt.(x <= of_int 8))
              (Set.to_list m)
      ;;

      let%test_unit _ =
        Set.to_sequence ~order:`Decreasing ~less_or_equal_to:(Elt.of_int 4) m
        <=> List.filter ~f:(fun x -> Elt.(x <= of_int 4)) (List.rev (Set.to_list m))
      ;;

      let%test_unit _ =
        Set.to_sequence
          m
          ~order:`Decreasing
          ~less_or_equal_to:(Elt.of_int 8)
          ~greater_or_equal_to:(Elt.of_int 4)
        <=> List.filter
              ~f:(fun x -> Elt.(x <= of_int 8) && Elt.(x >= of_int 4))
              (List.rev (Set.to_list m))
      ;;

      let%test_unit _ = Set.to_sequence ~order:`Increasing (Set.empty ()) <=> []
      let%test_unit _ = Set.to_sequence ~order:`Decreasing (Set.empty ()) <=> []

      let%test_unit _ =
        Set.to_sequence ~order:`Increasing ~greater_or_equal_to:(Elt.of_int 11) m <=> []
      ;;

      let%test_unit _ =
        Set.to_sequence ~order:`Decreasing ~less_or_equal_to:(Elt.of_int ~-1) m <=> []
      ;;
    end)
  ;;

  let binary_search _ = assert false
  let binary_search_segmented _ = assert false

  let%test_module "binary_search" =
    (module struct
      let small_set = Set.of_list (List.map ~f:Elt.of_int [ 1; 2; 3 ])
      let compare_elt elt v = Int.compare (Elt.to_int elt) v

      let%test _ =
        [%equal: Elt.t option]
          (Set.binary_search (Set.empty ()) ~compare:compare_elt `First_equal_to 1)
          None
      ;;

      let%test _ =
        [%equal: Elt.t option]
          (Set.binary_search small_set ~compare:compare_elt `First_equal_to 2)
          (Some (Elt.of_int 2))
      ;;

      let%test _ =
        [%equal: Elt.t option]
          (Set.binary_search
             small_set
             ~compare:compare_elt
             `First_greater_than_or_equal_to
             2)
          (Some (Elt.of_int 2))
      ;;

      let%test _ =
        [%equal: Elt.t option]
          (Set.binary_search
             small_set
             ~compare:compare_elt
             `First_strictly_greater_than
             3)
          None
      ;;

      let%test _ =
        [%equal: Elt.t option]
          (Set.binary_search_segmented
             (Set.empty ())
             ~segment_of:(fun _ -> assert false)
             `First_on_right)
          None
      ;;

      let%test _ =
        [%equal: Elt.t option]
          (Set.binary_search_segmented
             small_set
             ~segment_of:(fun elt -> if Elt.to_int elt < 3 then `Left else `Right)
             `First_on_right)
          (Some (Elt.of_int 3))
      ;;

      let%test _ =
        [%equal: Elt.t option]
          (Set.binary_search_segmented
             small_set
             ~segment_of:(fun elt -> if Elt.to_int elt < 3 then `Left else `Right)
             `Last_on_left)
          (Some (Elt.of_int 2))
      ;;

      let%test _ =
        [%equal: Elt.t option]
          (Set.binary_search_segmented
             small_set
             ~segment_of:(fun elt -> if Elt.to_int elt > 3 then `Right else `Left)
             `First_on_right)
          None
      ;;
    end)
  ;;

  let merge_to_sequence ?order:_ ?greater_or_equal_to:_ ?less_or_equal_to:_ _ _
    : _ Sequence.t
    =
    assert false
  ;;

  let%test_module "merge_to_sequence Quickcheck" =
    (module struct
      open Quickcheck

      module Merge_to_sequence_args = struct
        type t =
          { order : [ `Increasing | `Decreasing ] option
          ; greater_or_equal_to : Elt.t option
          ; less_or_equal_to : Elt.t option
          ; x : (int, Int.comparator_witness) Set.t_
          ; y : (int, Int.comparator_witness) Set.t_
          }

        let quickcheck_generator =
          let open Generator.Monad_infix in
          let int_set_gen =
            Generator.small_non_negative_int
            >>= fun size ->
            List.gen_with_length size Int.quickcheck_generator
            >>| fun ints -> Set.of_list (List.map ints ~f:Elt.of_int)
          in
          Generator.tuple5
            (Option.quickcheck_generator (Generator.of_list [ `Increasing; `Decreasing ]))
            (Option.quickcheck_generator (Int.quickcheck_generator >>| Elt.of_int))
            (Option.quickcheck_generator (Int.quickcheck_generator >>| Elt.of_int))
            int_set_gen
            int_set_gen
          >>| fun (order, greater_or_equal_to, less_or_equal_to, x, y) ->
          { order; greater_or_equal_to; less_or_equal_to; x; y }
        ;;
      end

      let%test_unit "merge_to_sequence = symmetric diff + inter" =
        Quickcheck.test
          Merge_to_sequence_args.quickcheck_generator
          ~f:(fun { order; greater_or_equal_to; less_or_equal_to; x; y } ->
          let open Set_intf.Merge_to_sequence_element in
          let value = function
            | Left x | Right x | Both (x, _) -> Elt.to_int x
          in
          let expect =
            List.concat
              [ List.map ~f:(fun x -> Both (x, x)) (Set.to_list (Set.inter x y))
              ; List.map ~f:(fun x -> Left x) (Set.to_list (Set.diff x y))
              ; List.map ~f:(fun x -> Right x) (Set.to_list (Set.diff y x))
              ]
          in
          let expect =
            Option.fold greater_or_equal_to ~init:expect ~f:(fun elts min ->
              let min = Elt.to_int min in
              List.filter elts ~f:(fun x -> Int.( >= ) (value x) min))
          in
          let expect =
            Option.fold less_or_equal_to ~init:expect ~f:(fun elts max ->
              let max = Elt.to_int max in
              List.filter elts ~f:(fun x -> Int.( <= ) (value x) max))
          in
          let expect =
            match order with
            | None | Some `Increasing ->
              List.sort expect ~compare:(fun a b -> Int.compare (value a) (value b))
            | Some `Decreasing ->
              List.sort expect ~compare:(fun a b -> Int.compare (value b) (value a))
          in
          [%test_result: (Elt.t, Elt.t) Merge_to_sequence_element.t list]
            (Sequence.to_list
               (Set.merge_to_sequence ?order ?greater_or_equal_to ?less_or_equal_to x y))
            ~expect)
      ;;
    end)
  ;;

  let of_sorted_array _ = assert false
  let of_sorted_array_unchecked _ = assert false
  let of_increasing_iterator_unchecked ~len:_ = assert false
  let _ = of_increasing_iterator_unchecked

  (* tested in of_sorted_array *)

  let%test _ = Set.of_sorted_array [||] |> Result.is_ok
  let%test _ = Set.of_sorted_array [| Elt.of_int 0 |] |> Result.is_ok
  let%test _ = Set.of_sorted_array [| Elt.of_int 0; Elt.of_int 0 |] |> Result.is_error

  let%test _ =
    Set.of_sorted_array [| Elt.of_int 1; Elt.of_int 0; Elt.of_int 1 |] |> Result.is_error
  ;;

  let%test _ =
    let list = List.init 100 ~f:Elt.of_int in
    let array = Array.of_list list in
    let hash_set = Elt.Hash_set.of_list list in
    let hashtbl = Elt.Table.of_alist_exn (List.map list ~f:(fun e -> e, e)) in
    let rev_array = Array.of_list (List.rev list) in
    let sequence = Sequence.of_list list in
    let set = Set.of_list list in
    Set.length set = List.length list
    && Set.equal set (Set.of_sorted_array_unchecked array)
    && Set.equal set (Set.of_sorted_array_unchecked rev_array)
    && Set.equal set (Set.of_hash_set hash_set)
    && Set.equal set (Set.of_hashtbl_keys hashtbl)
    && Set.equal set (Set.of_sequence sequence)
  ;;

  let invariants _ = assert false

  let%test_unit _ =
    for n = 0 to 100 do
      let list = List.init n ~f:Elt.of_int in
      assert (List.permute list |> Set.of_list |> Set.invariants);
      assert (Array.of_list list |> Set.of_sorted_array_unchecked |> Set.invariants);
      assert (
        List.rev list |> Array.of_list |> Set.of_sorted_array_unchecked |> Set.invariants)
    done
  ;;

  let iter2 _ = assert false

  let%test_unit _ =
    let test l1 l2 expected =
      let result = ref [] in
      let set_of_list l = Set.of_list (List.map l ~f:Elt.of_int) in
      Set.iter2 (set_of_list l1) (set_of_list l2) ~f:(fun a -> result := a :: !result);
      let result =
        List.rev_map !result ~f:(function
          | `Left a -> `Left (Elt.to_int a)
          | `Right a -> `Right (Elt.to_int a)
          | `Both (a, b) -> `Both (Elt.to_int a, Elt.to_int b))
      in
      assert (
        [%equal: [ `Both of int * int | `Left of int | `Right of int ] list]
          result
          expected)
    in
    test [] [] [];
    test [ 0 ] [] [ `Left 0 ];
    test [] [ 0 ] [ `Right 0 ];
    test
      [ 0; 1; 3; 4 ]
      [ 3; 4; 5; 6 ]
      [ `Left 0; `Left 1; `Both (3, 3); `Both (4, 4); `Right 5; `Right 6 ]
  ;;

  (* Ensure polymorphic equality raises for sets. *)
  let%test_unit _ =
    match Set.kind with
    | `Tree -> ()
    | `Set ->
      let ts = [ Set.empty (); Set.of_list [ Elt.of_int 13 ] ] in
      List.iter ts ~f:(fun t1 ->
        List.iter ts ~f:(fun t2 -> assert (Exn.does_raise (fun () -> Poly.equal t1 t2))))
  ;;

  let to_map _ = assert false
  let%test_unit _ = assert (Map.is_empty (Set.to_map set_empty ~f:Elt.to_int))

  let%test_unit _ =
    let s = set_nonempty in
    let m = Set.to_map s ~f:Elt.to_int in
    assert (Set.length s = Map.length m);
    Map.iteri m ~f:(fun ~key ~data -> assert (Elt.to_int key = data));
    assert ([%equal: Elt.t list] (Set.to_list s) (Map.keys m))
  ;;

  let of_map_keys _ = assert false

  let%test_unit _ =
    assert (Set.is_empty (Set.of_map_keys (Set.to_map set_empty ~f:Elt.to_int)))
  ;;

  let%test_unit _ =
    assert (
      Set.equal (Set.of_map_keys (Set.to_map set_nonempty ~f:Elt.to_int)) set_nonempty)
  ;;

  let symmetric_diff _ = assert false

  let%test_unit "symmetric diff quick" =
    let symmetric_diff_set s1 s2 =
      Set.symmetric_diff s1 s2
      |> Sequence.to_list
      |> List.map ~f:(function First elt | Second elt -> elt)
      |> Set.of_list
    in
    (* Textbook definition of symmetric difference: *)
    let symmetric_diff_spec s1 s2 = Set.diff (Set.union s1 s2) (Set.inter s1 s2) in
    Quickcheck.test
      ~seed:(`Deterministic "core set symmetric diff")
      (Quickcheck.Generator.tuple2 gen_set gen_set)
      ~sexp_of:[%sexp_of: Set.t_ * Set.t_]
      ~f:(fun (s1, s2) ->
      let expect = symmetric_diff_spec s1 s2 in
      let actual = symmetric_diff_set s1 s2 in
      assert (Set.equal actual expect))
  ;;

  let%test _ =
    let m1 = set_nonempty in
    [%equal: (Elt.t, Elt.t) Either.t list]
      (Sequence.to_list (Set.symmetric_diff m1 m1))
      []
  ;;

  let%test _ =
    let elt = Elt.of_int 7 in
    let m1 = Set.empty () in
    let m1 = Set.add m1 (Elt.of_int 1) in
    let m2 = Set.add m1 elt in
    [%equal: (Elt.t, Elt.t) Either.t list]
      (Sequence.to_list (Set.symmetric_diff m1 m2))
      [ Second elt ]
  ;;

  let%test _ =
    let m1 = set_nonempty in
    let m2 =
      List.fold (Set.to_list m1) ~init:(Set.empty ()) ~f:(fun m k -> Set.add m k)
    in
    [%equal: (Elt.t, Elt.t) Either.t list]
      (Sequence.to_list (Set.symmetric_diff m1 m2))
      []
  ;;

  let%test _ =
    let elt = Elt.of_int 20 in
    let m1 = set_nonempty in
    let m2 = Set.add m1 elt in
    [%equal: (Elt.t, Elt.t) Either.t list]
      (Sequence.to_list (Set.symmetric_diff m1 m2))
      [ Second elt ]
  ;;

  let%test _ =
    let elt = Elt.of_int 5 in
    let m1 = set_nonempty in
    let m2 = Set.remove m1 elt in
    [%equal: (Elt.t, Elt.t) Either.t list]
      (Sequence.to_list (Set.symmetric_diff m1 m2))
      [ First elt ]
  ;;

  let set_of_int_list l = Set.of_list (List.map ~f:Elt.of_int l)

  let%test _ =
    let map1 = Set.empty () in
    let map2 = set_of_int_list [ 1; 2; 3; 4; 5 ] in
    let diff = Set.symmetric_diff map1 map2 in
    Sequence.length diff = 5
  ;;

  let%test _ =
    let map1 = set_of_int_list [ 1; 2; 3; 4; 5 ] in
    let map2 = Set.empty () in
    let diff = Set.symmetric_diff map1 map2 in
    Sequence.length diff = 5
  ;;

  let%test _ =
    let map1 = set_of_int_list [ 1; 2 ] in
    let map2 =
      List.fold [ 3; 4; 5 ] ~init:map1 ~f:(fun acc elt -> Set.add acc (Elt.of_int elt))
    in
    let diff = Set.symmetric_diff map1 map2 in
    Sequence.length diff = 3
  ;;

  let%test _ =
    let map2 = set_of_int_list [ 1; 2 ] in
    let map1 =
      List.fold [ 3; 4; 5 ] ~init:map2 ~f:(fun acc elt -> Set.add acc (Elt.of_int elt))
    in
    let diff = Set.symmetric_diff map1 map2 in
    Sequence.length diff = 3
  ;;

  let split _ = assert false
  let split_le_gt _ = assert false
  let split_lt_ge _ = assert false

  module Simple_int_set = struct
    type t = int list [@@deriving compare, sexp]

    let init i = List.init i ~f:Fn.id
    let to_set = set_of_int_list

    let split t i =
      let l = List.filter t ~f:(Int.( > ) i) in
      let r = List.filter t ~f:(Int.( < ) i) in
      let x = if List.mem t i ~equal:Int.( = ) then Some (Elt.of_int i) else None in
      to_set l, x, to_set r
    ;;
  end

  let%test_unit _ =
    let n = 16 in
    let t = set_of_int_list (List.init n ~f:Fn.id) in
    let t' = Simple_int_set.init n in
    let check i =
      let l', x', r' = Simple_int_set.split t' i in
      let () =
        let l, x, r = Set.split t (Elt.of_int i) in
        assert (Set.equal l l');
        [%test_eq: Elt.t option] x x';
        assert (Set.equal r r')
      in
      let () =
        let l, r = Set.split_le_gt t (Elt.of_int i) in
        let l' = Option.value_map x' ~default:l' ~f:(Set.add l') in
        assert (Set.equal l l');
        assert (Set.equal r r')
      in
      let () =
        let l, r = Set.split_lt_ge t (Elt.of_int i) in
        let r' = Option.value_map x' ~default:r' ~f:(Set.add r') in
        assert (Set.equal l l');
        assert (Set.equal r r')
      in
      ()
    in
    for i = 0 to n - 1 do
      check i
    done
  ;;

  let quickcheck_generator _ = assert false

  let%test_module _ =
    (module struct
      open Quickcheck

      let sexp_of = [%sexp_of: Set.t]
      let compare = [%compare: Set.t]
      let quickcheck_generator = Set.quickcheck_generator Elt.quickcheck_generator
      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ = can_generate (fun t -> Set.is_empty t)
      let%test_unit _ = can_generate (fun t -> Set.length t = 1)
      let%test_unit _ = can_generate (fun t -> Set.length t = 2)
      let%test_unit _ = can_generate (fun t -> Set.length t >= 3)

      let%test_unit _ =
        can_generate (fun t -> Set.exists t ~f:(fun elt -> Elt.to_int elt >= 0))
      ;;

      let%test_unit _ =
        can_generate (fun t -> Set.exists t ~f:(fun elt -> Elt.to_int elt < 0))
      ;;
    end)
  ;;

  let quickcheck_observer _ = assert false

  let%test_module _ =
    (module struct
      open Quickcheck

      module Set' = struct
        include Set

        let hash_fold_t hash t = [%hash_fold: Elt.t list] hash (to_list t)
        let hash t = hash_fold_t (Hash.alloc ()) t |> Hash.get_hash_value
      end

      module F =
        Fn_for_testing.Make (Set') (Int)
          (struct
            let examples =
              [] :: Elt.samples :: List.map Elt.samples ~f:(fun x -> [ x ])
              |> List.map ~f:Set.of_list
            ;;
          end)

      let sexp_of = [%sexp_of: F.t]
      let compare = [%compare: F.t]

      let quickcheck_generator =
        (* memoizing these functions makes [test_no_duplicates] run much faster *)
        let hashable = Hashtbl_intf.Hashable.of_key (module Set') in
        Generator.(
          fn (Set.quickcheck_observer Elt.quickcheck_observer) Int.quickcheck_generator
          |> map ~f:(fun f -> Memo.general f ~hashable))
      ;;

      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f

      let%test_unit (_ [@tags "no-js"]) =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ =
        can_generate (fun f -> f (Set.singleton (Elt.of_int 0)) <> f (Set.empty ()))
      ;;

      let%test_unit _ =
        can_generate (fun f ->
          f (Set.singleton (Elt.of_int 0)) <> f (Set.singleton (Elt.of_int 1)))
      ;;

      let%test_unit _ =
        can_generate (fun f ->
          f (Set.singleton (Elt.of_int 0))
          <> f (Set.of_list [ Elt.of_int 0; Elt.of_int 1 ]))
      ;;
    end)
  ;;

  let quickcheck_shrinker _ = assert false

  let%test_module _ =
    (module struct
      open Quickcheck

      let elt_test_shrinker =
        Shrinker.create (fun t ->
          let n = Elt.to_int t in
          if n <= 0 then Sequence.empty else Sequence.singleton (Elt.of_int (n - 1)))
      ;;

      let quickcheck_shrinker = Set.quickcheck_shrinker elt_test_shrinker
      let normalize_list list = List.sort list ~compare:[%compare: int]

      let normalize_lists lists =
        List.map lists ~f:normalize_list |> List.sort ~compare:[%compare: int list]
      ;;

      let set_of_list list = List.map list ~f:Elt.of_int |> Set.of_list
      let list_of_set set = Set.to_list set |> List.map ~f:Elt.to_int

      let test list shrunk_lists =
        [%test_result: int list list]
          (set_of_list list
           |> Shrinker.shrink quickcheck_shrinker
           |> Sequence.to_list
           |> List.map ~f:list_of_set
           |> normalize_lists)
          ~expect:(normalize_lists shrunk_lists)
      ;;

      let%test_unit _ = test [] []
      let%test_unit _ = test [ 0 ] [ [] ]
      let%test_unit _ = test [ 1 ] [ []; [ 0 ] ]
      let%test_unit _ = test [ 0; 1 ] [ [ 0 ]; [ 1 ] ]
    end)
  ;;

  let compare_direct _ = assert false

  let%expect_test "compare and compare_direct" =
    let set_of_list ints = Set.of_list (List.map ints ~f:Elt.of_int) in
    require_compare_equal [%here] (module Set) (Set.empty ()) (Set.empty ());
    require [%here] (Set.compare (Set.empty ()) (set_of_list [ 0 ]) <> 0);
    require_compare_equal
      [%here]
      (module Set)
      (set_of_list [ 1; 2; 3 ])
      (set_of_list [ 3; 2; 1 ]);
    require
      [%here]
      (Set.compare (set_of_list [ 0; 1; 2; 3 ]) (set_of_list [ 1; 1; 2; 3 ]) <> 0)
  ;;

  let to_tree _ = assert false
  let remove_index _ = assert false
  let nth _ = assert false
  let find_exn _ = assert false
  let group_by _ = assert false
  let choose_exn _ = assert false
  let choose _ = assert false
  let max_elt_exn _ = assert false
  let max_elt _ = assert false
  let min_elt_exn _ = assert false
  let min_elt _ = assert false
  let elements _ = assert false
  let partition_tf _ = assert false
  let filter _ = assert false
  let fold_right _ = assert false
  let fold_until _ = assert false
  let equal _ = assert false
  let diff _ = assert false
  let union _ = assert false
  let remove _ = assert false
  let find_map _ = assert false
  let find _ = assert false
  let count _ = assert false
  let sum _ = assert false
  let for_all _ = assert false
  let exists _ = assert false
  let fold _ = assert false
  let iter _ = assert false
  let length _ = assert false
  let of_tree _ = assert false
  let filter_map _ = assert false
  let map _ = assert false
  let stable_dedup_list _ = assert false
  let of_array _ = assert false
  let union_list _ = assert false
  let singleton _ = assert false
  let fold_result _ = assert false
  let empty = Unit_test_follows
end

module Elt_int = struct
  type 'a t = int [@@deriving sexp, hash]

  let of_int = Fn.id
  let to_int = Fn.id
end

module Elt_poly = struct
  type 'a t = 'a [@@deriving sexp, hash]

  let of_int = Fn.id
  let to_int = Fn.id
end

module Create_options_with_comparator = struct
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) With_comparator.t

  let simplify_creator f = f ~comparator:Int.comparator
end

module Create_options_with_first_class_module = struct
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) With_first_class_module.t

  let simplify_creator f =
    f (module Int : Comparator.S with type t = _ and type comparator_witness = _)
  ;;
end

module Create_options_without_comparator = struct
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) Without_comparator.t

  let simplify_creator = Fn.id
end

module Access_options_without_comparator = struct
  type ('a, 'b, 'c) access_options = ('a, 'b, 'c) Without_comparator.t

  let simplify_accessor = Fn.id
end

module Access_options_with_comparator = struct
  type ('a, 'b, 'c) access_options = ('a, 'b, 'c) With_comparator.t

  let simplify_accessor f = f ~comparator:Int.comparator
end

module Access_options_with_poly_comparator = struct
  type ('a, 'b, 'c) access_options =
    ('a, Comparator.Poly.comparator_witness, 'c) With_comparator.t

  let simplify_accessor f = f ~comparator:Comparator.Poly.comparator
end

module Access_options_with_int_comparator = struct
  type ('a, 'b, 'c) access_options = (int, Int.comparator_witness, 'c) With_comparator.t

  let simplify_accessor f = f ~comparator:Int.comparator
end

let%test_module "Set" =
  (module Unit_tests
            (Elt_poly)
            (struct
              include Set

              type ('a, 'b) t_ = ('a, 'b) t
              type ('a, 'b) set = ('a, 'b) t
              type ('a, 'b) tree = ('a, 'b) Tree.t
              type 'a cmp = 'a

              include Create_options_with_first_class_module
              include Access_options_without_comparator

              let kind = `Set
              let is_poly = false
            end))
;;

let%test_module "Set.Poly" =
  (module Unit_tests
            (Elt_poly)
            (struct
              include Set
              include Set.Poly

              type ('a, 'b) set = ('a, 'b) Set.t
              type ('a, 'b) t_ = 'a t
              type ('a, 'b) tree = 'a Tree.t
              type 'a cmp = Comparator.Poly.comparator_witness

              include Create_options_without_comparator
              include Access_options_without_comparator

              let kind = `Set
              let is_poly = true
            end))
;;

let%test_module "Int.Set" =
  (module Unit_tests
            (Elt_int)
            (struct
              include Set
              include Int.Set
              module Tree = Set.Make_tree (Int.Set.Elt)

              type ('a, 'b) set = ('a, 'b) Set.t
              type ('a, 'b) t_ = t
              type ('a, 'b) tree = Tree.t
              type 'a cmp = Int.comparator_witness

              include Create_options_without_comparator
              include Access_options_without_comparator

              let kind = `Set
              let is_poly = false
            end))
;;

let%test_module "Set.Tree" =
  (module Unit_tests
            (Elt_poly)
            (struct
              include Set.Tree

              type ('a, 'b) set = ('a, 'b) Set.Tree.t
              type ('a, 'b) t_ = ('a, 'b) t
              type ('a, 'b) tree = ('a, 'b) t
              type 'a cmp = 'a

              include Create_options_with_comparator
              include Access_options_with_comparator

              let kind = `Tree
              let is_poly = false
            end))
;;

let%test_module "Set.Poly.Tree" =
  (module Unit_tests
            (Elt_poly)
            (struct
              include Set.Tree
              include Set.Poly.Tree

              type ('a, 'b) set = ('a, 'b) Set.Tree.t
              type ('a, 'b) t_ = 'a t
              type ('a, 'b) tree = 'a t
              type 'a cmp = Comparator.Poly.comparator_witness

              include Create_options_without_comparator
              include Access_options_with_poly_comparator

              let kind = `Tree
              let is_poly = true
            end))
;;

let%test_module "Int.Set.Tree" =
  (module Unit_tests
            (Elt_int)
            (struct
              include Set.Tree
              include Set.Make_tree (Int.Set.Elt)

              type ('a, 'b) set = ('a, 'b) Set.Tree.t
              type ('a, 'b) t_ = t
              type ('a, 'b) tree = t
              type 'a cmp = Int.comparator_witness

              include Create_options_without_comparator
              include Access_options_with_int_comparator

              let equal ~comparator:_ = equal
              let kind = `Tree
              let is_poly = false
            end))
;;

let%expect_test "t_of_sexp raises on duplicate elements" =
  let no_dup_sexp = Sexp.of_string "(a b c d)" in
  require_equal
    [%here]
    (module Sexp)
    no_dup_sexp
    ([%sexp_of: String.Set.t] ([%of_sexp: String.Set.t] no_dup_sexp));
  let dup_sexp = Sexp.of_string "(a b a d)" in
  require_does_raise [%here] (fun () -> [%of_sexp: String.Set.t] dup_sexp);
  [%expect
    {| (Of_sexp_error "Set.t_of_sexp: duplicate element in set" (invalid_sexp a)) |}]
;;

let%expect_test "bin_read_t raises on duplicate elements" =
  let max_n = 20 in
  let bstr = Bigstring.create (max_n + 1) in
  for n = 0 to max_n do
    let s1 = Set.Poly.of_array (Array.init n ~f:succ) in
    let pos = Set.Poly.bin_write_t Int.bin_write_t bstr ~pos:0 s1 in
    require_equal [%here] (module Int) pos (n + 1);
    let pos_ref = ref 0 in
    let s2 = Set.Poly.bin_read_t Int.bin_read_t bstr ~pos_ref in
    require_equal [%here] (module Int) !pos_ref (n + 1);
    require_equal
      [%here]
      (module struct
        type t = int Set.Poly.t [@@deriving sexp_of]

        let equal = Set.equal
      end)
      s1
      s2;
    if n >= 2
    then (
      bstr.{1} <- 'x';
      bstr.{2} <- 'x';
      pos_ref := 0;
      require_does_raise [%here] (fun () ->
        Set.Poly.bin_read_t Int.bin_read_t bstr ~pos_ref);
      [%expect {| (Failure "Set.bin_read_t: duplicate element in set") |}];
      require_equal [%here] (module Int) !pos_ref (n + 1))
  done
;;

let%expect_test _ =
  let open Expect_test_helpers_core in
  print_and_check_stable_type
    [%here]
    (module struct
      type t = Set.M(Int).t [@@deriving bin_io, compare, sexp]
    end)
    (List.map ~f:(Set.of_list (module Int)) [ []; [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ] ]);
  [%expect
    {|
    (bin_shape_digest 3564446b0bfa871d8c3ebf31ab342fe7)
    ((sexp ()) (bin_io "\000"))
    ((sexp (1)) (bin_io "\001\001"))
    ((sexp (1 2)) (bin_io "\002\001\002"))
    ((sexp (1 2 3)) (bin_io "\003\001\002\003"))
    |}]
;;
