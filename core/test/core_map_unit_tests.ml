(* This module defines a functor, [Unit_tests], that does unit tests on a generic map,
   and then instantiates that functor to create unit tests for [Map], [Map.Poly], and
   [Int.Map]. *)

module Caml_map = Map
open! Core
open Expect_test_helpers_core
module With_comparator = Map_intf.With_comparator
module With_first_class_module = Map_intf.With_first_class_module
module Without_comparator = Map_intf.Without_comparator

module Unit_tests (Key : sig
  type 'a t [@@deriving sexp, compare, hash]

  val of_int : int -> int t
  val to_int : int t -> int
end) (Map : sig
  type ('a, 'b, 'c) t_
  type ('a, 'b, 'c) tree
  type ('a, 'b, 'c) create_options
  type ('a, 'b, 'c) access_options

  include
    Map_intf.Creators_and_accessors_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
      with type 'a key := 'a Key.t
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) create_options
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) access_options

  val simplify_creator : (int, Int.comparator_witness, 'c) create_options -> 'c
  val simplify_accessor : (int, Int.comparator_witness, 'c) access_options -> 'c
  val kind : [ `Map | `Tree ]
end) : Map_intf.Creators_and_accessors_generic =
(* The result signature doesn't actually mean anything -- the values are required so
   that implementors are reminded to add a unit test for each one. *)
struct
  module Map = struct
    include Map

    let set x = simplify_accessor set x
    let add_multi x = simplify_accessor add_multi x
    let find_multi x = simplify_accessor find_multi x
    let remove_multi x = simplify_accessor remove_multi x
    let change x = simplify_accessor change x
    let update x = simplify_accessor update x
    let find x = simplify_accessor find x
    let find_exn x = simplify_accessor find_exn x
    let invariants x = simplify_accessor invariants x
    let remove x = simplify_accessor remove x
    let mem x = simplify_accessor mem x
    let combine_errors x = simplify_accessor combine_errors x
    let compare_direct x = simplify_accessor compare_direct x
    let equal x = simplify_accessor equal x
    let iter2 x = simplify_accessor iter2 x
    let fold2 x = simplify_accessor fold2 x
    let symmetric_diff x = simplify_accessor symmetric_diff x
    let fold_symmetric_diff x = simplify_accessor fold_symmetric_diff x
    let merge x = simplify_accessor merge x
    let merge_disjoint_exn x = simplify_accessor merge_disjoint_exn x
    let merge_skewed x = simplify_accessor merge_skewed x
    let split x = simplify_accessor split x
    let split_le_gt x = simplify_accessor split_le_gt x
    let split_lt_ge x = simplify_accessor split_lt_ge x
    let subrange x = simplify_accessor subrange x
    let fold_range_inclusive x = simplify_accessor fold_range_inclusive x
    let range_to_alist x = simplify_accessor range_to_alist x
    let closest_key x = simplify_accessor closest_key x
    let rank x = simplify_accessor rank x
    let quickcheck_shrinker x = simplify_accessor quickcheck_shrinker x
    let key_set x = simplify_accessor key_set x

    let to_sequence ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to x =
      simplify_accessor
        to_sequence
        ?order
        ?keys_greater_or_equal_to
        ?keys_less_or_equal_to
        x
    ;;

    let binary_search t ~compare how v = simplify_accessor binary_search t ~compare how v

    let binary_search_segmented t ~segment_of how =
      simplify_accessor binary_search_segmented t ~segment_of how
    ;;

    let binary_search_subrange t ~compare ~lower_bound ~upper_bound =
      simplify_accessor binary_search_subrange t ~compare ~lower_bound ~upper_bound
    ;;

    let append ~lower_part ~upper_part = simplify_accessor append ~lower_part ~upper_part
    let empty () = simplify_creator empty
    let singleton x = simplify_creator singleton x
    let of_sorted_array_unchecked x = simplify_creator of_sorted_array_unchecked x
    let of_sorted_array x = simplify_creator of_sorted_array x

    let of_increasing_iterator_unchecked ~len ~f =
      simplify_creator of_increasing_iterator_unchecked ~len ~f
    ;;

    let of_increasing_sequence seq = simplify_creator of_increasing_sequence seq
    let of_alist x = simplify_creator of_alist x
    let of_alist_or_error x = simplify_creator of_alist_or_error x
    let of_alist_exn x = simplify_creator of_alist_exn x
    let of_hashtbl_exn x = simplify_creator of_hashtbl_exn x
    let of_alist_multi x = simplify_creator of_alist_multi x
    let of_alist_fold x = simplify_creator of_alist_fold x
    let of_alist_reduce x = simplify_creator of_alist_reduce x
    let of_iteri ~iteri = simplify_creator of_iteri ~iteri
    let of_iteri_exn ~iteri = simplify_creator of_iteri_exn ~iteri
    let of_tree x = simplify_creator of_tree x
    let of_sequence x = simplify_creator of_sequence x
    let of_sequence_or_error x = simplify_creator of_sequence_or_error x
    let of_sequence_exn x = simplify_creator of_sequence_exn x
    let of_sequence_multi x = simplify_creator of_sequence_multi x
    let of_sequence_fold x = simplify_creator of_sequence_fold x
    let of_sequence_reduce x = simplify_creator of_sequence_reduce x
    let of_list_with_key x = simplify_creator of_list_with_key x
    let of_list_with_key_or_error x = simplify_creator of_list_with_key_or_error x
    let of_list_with_key_exn x = simplify_creator of_list_with_key_exn x
    let of_list_with_key_multi x = simplify_creator of_list_with_key_multi x
    let of_list_with_key_fold x = simplify_creator of_list_with_key_fold x
    let of_list_with_key_reduce x = simplify_creator of_list_with_key_reduce x
    let map_keys_exn x = simplify_creator map_keys_exn x
    let transpose_keys x = simplify_creator (simplify_accessor transpose_keys) x
    let quickcheck_generator x = simplify_creator quickcheck_generator x

    type ('a, 'b) t = ('a, 'b, Int.comparator_witness) t_

    let sexp_of_t sexp_of_a sexp_of_b t = [%sexp (to_alist t : (a Key.t * b) list)]

    let compare (type a b) compare_a compare_b t1 t2 =
      [%compare: (a Key.t * b) list] (to_alist t1) (to_alist t2)
    ;;
  end

  type ('a, 'b, 'c) t = Unit_test_follows
  type ('a, 'b, 'c) tree = ('a, 'b, 'c) t
  type 'a key
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) Without_comparator.t
  type ('a, 'b, 'c) access_options = ('a, 'b, 'c) Without_comparator.t
  type 'cmp cmp

  module Key = struct
    open Key

    let of_int = of_int
    let to_int = to_int

    module T = struct
      type t = int Key.t [@@deriving sexp, hash]

      let compare t t' = Poly.compare (to_int t) (to_int t')
    end

    include T
    include Comparable.Make (T)

    let to_string t = Sexp.to_string (sexp_of_t t)
    let sample = of_int 0

    let samples =
      List.init 10 ~f:(fun i -> of_int (i + 1))
      |> List.dedup_and_sort ~compare
      |> List.sort ~compare
    ;;

    let min = List.hd_exn samples
    let max = List.hd_exn (List.rev samples)
    let mid = List.nth_exn samples (List.length samples / 2)
    let pred t = of_int (to_int t - 1)
    let succ t = of_int (to_int t + 1)
    let quickcheck_generator = Quickcheck.Generator.map Int.quickcheck_generator ~f:of_int
    let quickcheck_observer = Quickcheck.Observer.unmap Int.quickcheck_observer ~f:to_int
  end

  module Caml_map = Caml_map.Make (Key)

  let random_alist keys =
    List.rev (List.fold keys ~init:[] ~f:(fun l key -> (key, Random.int 1000) :: l))
  ;;

  let random_map keys =
    List.fold (random_alist keys) ~init:(Map.empty ()) ~f:(fun map (key, data) ->
      Map.set ~key ~data map)
  ;;

  let caml_map_of_alist alist =
    List.fold alist ~init:Caml_map.empty ~f:(fun map (key, data) ->
      Caml_map.add key data map)
  ;;

  let alist_equal ~data_equal l1 l2 =
    List.equal (fun (k1, d1) (k2, d2) -> Key.equal k1 k2 && data_equal d1 d2) l1 l2
  ;;

  let caml_map_to_alist map =
    List.rev (Caml_map.fold (fun key data l -> (key, data) :: l) map [])
  ;;

  (* relies on correctness of Map.to_alist *)
  let equal_maps ~data_equal ~caml_map map =
    Map.length map = Caml_map.cardinal caml_map
    && alist_equal ~data_equal (Map.to_alist map) (caml_map_to_alist caml_map)
  ;;

  let add _ = assert false
  let add_exn _ = assert false
  let set _ = assert false
  let remove _ = assert false
  let find _ = assert false
  let mem _ = assert false
  let iter _ = assert false
  let iteri _ = assert false
  let iter_keys _ = assert false
  let map _ = assert false
  let mapi _ = assert false
  let fold _ = assert false
  let equal _ = assert false
  let compare_direct _ = assert false

  (* runs a series of random tests on a map of the input type and a Caml map to see if
     they have the same behavior *)
  let%test _ =
    let rec loop n ~prev_caml_map ~caml_map ~prev_core_map ~core_map =
      if n = 0
      then equal_maps ~data_equal:( = ) ~caml_map core_map
      else (
        let remove key = Caml_map.remove key caml_map, Map.remove core_map key in
        let add key =
          let data = Random.int 1000 in
          Caml_map.add key data caml_map, Map.set ~key ~data core_map
        in
        let caml_choose caml_map =
          try Some (Caml_map.choose caml_map) with
          | _ -> None
        in
        let add_or_remove ~prefer =
          match caml_choose caml_map, prefer with
          | None, _ -> add Key.sample
          | Some (key, _), `Remove -> remove key
          | Some (key, _), `Add ->
            (match
               List.find Key.samples ~f:(fun key -> not (Caml_map.mem key caml_map))
             with
             | Some key -> add key
             | None -> remove key)
        in
        let old_values = caml_map, core_map in
        let new_caml_map, new_core_map =
          match Random.int 8 with
          | 0 -> add_or_remove ~prefer:`Add
          | 1 -> add_or_remove ~prefer:`Remove
          | 2 ->
            (match caml_choose caml_map with
             | None ->
               assert (Map.is_empty core_map);
               assert (Map.length core_map = 0)
             | Some (key, data) ->
               assert (Caml_map.find key caml_map = data);
               assert (not (Map.is_empty core_map));
               assert (Map.length core_map = Caml_map.cardinal caml_map);
               assert (Map.mem core_map key);
               assert ([%equal: int option] (Map.find core_map key) (Some data)));
            old_values
          | 3 ->
            let target =
              let sum = ref 0 in
              Caml_map.iter (fun _ data -> sum := !sum + data) caml_map;
              !sum
            in
            let actual =
              let sum = ref 0 in
              Map.iter ~f:(fun data -> sum := !sum + data) core_map;
              !sum
            in
            let actual2 =
              let sum = ref 0 in
              Map.iteri ~f:(fun ~key:_ ~data -> sum := !sum + data) core_map;
              !sum
            in
            assert (target = actual && actual = actual2);
            old_values
          | 4 ->
            let target =
              let sum = ref 0 in
              Caml_map.iter (fun key _ -> sum := !sum + Key.to_int key) caml_map;
              !sum
            in
            let actual =
              let sum = ref 0 in
              Map.iter_keys ~f:(fun key -> sum := !sum + Key.to_int key) core_map;
              !sum
            in
            assert (target = actual);
            old_values
          | 5 ->
            let caml_map = Caml_map.mapi (fun key data -> key, data) caml_map in
            let core_map = Map.mapi ~f:(fun ~key ~data -> key, data) core_map in
            let increment = Random.int 1000 in
            let caml_map = Caml_map.map (fun (_, n) -> n + increment) caml_map in
            let core_map = Map.map ~f:(fun (_, n) -> n + increment) core_map in
            assert (equal_maps ~data_equal:( = ) ~caml_map core_map);
            old_values
          | 6 ->
            let caml_alist =
              Caml_map.fold (fun key data acc -> (key, data) :: acc) caml_map []
            in
            let core_alist =
              Map.fold ~f:(fun ~key ~data acc -> (key, data) :: acc) core_map ~init:[]
            in
            assert (alist_equal ~data_equal:( = ) caml_alist core_alist);
            old_values
          | 7 ->
            let unchanged = Caml_map.equal ( = ) prev_caml_map caml_map in
            assert ([%equal: bool] unchanged (Map.equal ( = ) prev_core_map core_map));
            assert (
              [%equal: bool]
                unchanged
                (Map.compare_direct Int.compare prev_core_map core_map = 0));
            old_values
          | _ -> assert false
        in
        loop
          (n - 1)
          ~prev_caml_map:caml_map
          ~caml_map:new_caml_map
          ~prev_core_map:core_map
          ~core_map:new_core_map)
    in
    loop
      10000
      ~prev_caml_map:Caml_map.empty
      ~caml_map:Caml_map.empty
      ~prev_core_map:(Map.empty ())
      ~core_map:(Map.empty ())
  ;;

  let iter2 _ = assert false

  let%test_unit _ =
    let test l1 l2 expected =
      let map_of_alist l =
        Map.of_alist_exn (List.map l ~f:(fun (k, v) -> Key.of_int k, v))
      in
      let result = ref [] in
      Map.iter2 (map_of_alist l1) (map_of_alist l2) ~f:(fun ~key ~data ->
        result := (key, data) :: !result);
      let result = List.rev_map !result ~f:(fun (k, v) -> Key.to_int k, v) in
      assert (
        [%equal: (int * [ `Both of int * int | `Left of int | `Right of int ]) list]
          result
          expected)
    in
    test [] [] [];
    test [ 0, 10 ] [] [ 0, `Left 10 ];
    test [] [ 0, 10 ] [ 0, `Right 10 ];
    test [ 0, 10 ] [ 0, 11 ] [ 0, `Both (10, 11) ];
    test
      [ 0, 10; 3, 13; 4, 14; 6, 16 ]
      [ 1, 11; 3, 13; 4, 14; 5, 15 ]
      [ 0, `Left 10
      ; 1, `Right 11
      ; 3, `Both (13, 13)
      ; 4, `Both (14, 14)
      ; 5, `Right 15
      ; 6, `Left 16
      ]
  ;;

  let fold2 _ = assert false

  let%test_unit _ =
    let test l1 l2 expected =
      let map_of_alist l =
        Map.of_alist_exn (List.map l ~f:(fun (k, v) -> Key.of_int k, v))
      in
      let result =
        Map.fold2 (map_of_alist l1) (map_of_alist l2) ~init:[] ~f:(fun ~key ~data acc ->
          (key, data) :: acc)
      in
      let result = List.rev_map result ~f:(fun (k, v) -> Key.to_int k, v) in
      assert (
        [%equal: (int * [ `Both of int * int | `Left of int | `Right of int ]) list]
          result
          expected)
    in
    test [] [] [];
    test [ 0, 10 ] [] [ 0, `Left 10 ];
    test [] [ 0, 10 ] [ 0, `Right 10 ];
    test [ 0, 10 ] [ 0, 11 ] [ 0, `Both (10, 11) ];
    test
      [ 0, 10; 3, 13; 4, 14; 6, 16 ]
      [ 1, 11; 3, 13; 4, 14; 5, 15 ]
      [ 0, `Left 10
      ; 1, `Right 11
      ; 3, `Both (13, 13)
      ; 4, `Both (14, 14)
      ; 5, `Right 15
      ; 6, `Left 16
      ]
  ;;

  let empty = Unit_test_follows
  let%test _ = equal_maps ~data_equal:( = ) ~caml_map:Caml_map.empty (Map.empty ())
  let singleton _ = assert false

  let%test _ =
    equal_maps
      ~data_equal:( = )
      ~caml_map:(Caml_map.add Key.sample 0 Caml_map.empty)
      (Map.singleton Key.sample 0)
  ;;

  let of_sorted_array _ = assert false

  (* test detection of invalid input *)
  let%test _ =
    Map.of_sorted_array [| Key.of_int 0, 0; Key.of_int 0, 0 |] |> Result.is_error
  ;;

  let%test _ =
    Map.of_sorted_array [| Key.of_int 1, 0; Key.of_int 0, 0; Key.of_int 1, 0 |]
    |> Result.is_error
  ;;

  let of_sorted_array_unchecked _ = assert false

  (* test it gets same result as [Map.of_alist] *)
  let%test _ =
    let alist =
      List.sort (random_alist Key.samples) ~compare:(fun (k1, _) (k2, _) ->
        Key.compare k1 k2)
    in
    let array = Array.of_list alist in
    let array_rev = Array.of_list (List.rev alist) in
    let map_of_alist = Map.of_alist_exn alist in
    let map_of_array = Map.of_sorted_array_unchecked array in
    let map_of_rev_array = Map.of_sorted_array_unchecked array_rev in
    let map_of_sequence =
      Sequence.of_list alist |> Map.of_increasing_sequence |> Or_error.ok_exn
    in
    let map_equal = Map.equal Int.equal in
    map_equal map_of_alist map_of_array
    && map_equal map_of_alist map_of_rev_array
    && map_equal map_of_alist map_of_sequence
  ;;

  let invariants _ = assert false

  (* Test constructed AVL tree is valid *)
  let%test_unit _ =
    for n = 0 to 100 do
      let alist = List.init n ~f:(fun i -> Key.of_int i, i) in
      assert (List.permute alist |> Map.of_alist_exn |> Map.invariants);
      assert (Array.of_list alist |> Map.of_sorted_array_unchecked |> Map.invariants);
      assert (
        List.rev alist |> Array.of_list |> Map.of_sorted_array_unchecked |> Map.invariants);
      assert (
        Sequence.of_list alist
        |> Map.of_increasing_sequence
        |> Or_error.ok_exn
        |> Map.invariants)
    done
  ;;

  let of_increasing_iterator_unchecked ~len:_ = assert false
  let _ = Map.of_increasing_iterator_unchecked

  (* already tested in the array functions *)

  let of_alist _ = assert false

  let%test _ =
    let alist = random_alist Key.samples in
    match Map.of_alist alist with
    | `Duplicate_key _ -> false
    | `Ok map -> alist_equal ~data_equal:( = ) (Map.to_alist map) alist
  ;;

  let%test _ =
    match Map.of_alist [] with
    | `Ok map -> [%equal: (Key.t * _) list] (Map.to_alist map) []
    | `Duplicate_key _ -> false
  ;;

  let%test _ =
    match Map.of_alist [ Key.sample, 0; Key.sample, 1 ] with
    | `Ok _ -> false
    | `Duplicate_key key -> Key.equal key Key.sample
  ;;

  let of_alist_or_error _ = assert false
  let%test _ = Result.is_error (Map.of_alist_or_error [ Key.sample, 0; Key.sample, 1 ])

  let%test _ =
    Result.is_ok
      (Map.of_alist_or_error (List.map Key.samples ~f:(fun key -> key, Key.to_int key)))
  ;;

  let of_alist_exn _ = assert false

  let%expect_test _ =
    (* Can't use require_does_raise because the exceptions differ on different key
       types. *)
    require
      [%here]
      (Exn.does_raise (fun () -> Map.of_alist_exn [ Key.sample, 0; Key.sample, 1 ]))
  ;;

  let%expect_test _ =
    require_does_not_raise [%here] (fun () ->
      ignore
        (Map.of_hashtbl_exn
           (List.map Key.samples ~f:(fun key -> key, Key.to_int key)
            |> Hashtbl.Poly.of_alist_exn)
          : _ Map.t_))
  ;;

  let%expect_test _ =
    let hashtbl_with_dup =
      let bogus_hashable : Key.t Hashtbl.Hashable.t =
        let i = ref 0 in
        { hash =
            (fun _ ->
              incr i;
              !i)
        ; compare = (fun _ _ -> 1)
        ; sexp_of_t = Key.sexp_of_t
        }
      in
      Hashtbl.Using_hashable.of_alist_exn
        ~hashable:bogus_hashable
        [ Key.sample, 0; Key.sample, 1 ]
    in
    (* Can't use require_does_raise because the exceptions differ on different key
       types. *)
    require [%here] (Exn.does_raise (fun () -> Map.of_hashtbl_exn hashtbl_with_dup))
  ;;

  let of_hashtbl_exn _ = assert false
  let of_alist_fold _ = assert false
  let of_alist_reduce _ = assert false

  let%test_unit _ =
    let filtered =
      List.filter Key.samples ~f:(fun key -> not (Key.equal key Key.sample))
    in
    let alist = random_alist filtered in
    let caml_map = Caml_map.add Key.sample 6 (caml_map_of_alist alist) in
    let alist' = ((Key.sample, 1) :: (Key.sample, 2) :: alist) @ [ Key.sample, 3 ] in
    let core_map_fold = Map.of_alist_fold ~init:0 ~f:( + ) alist' in
    let core_map_reduce = Map.of_alist_reduce ~f:( + ) alist' in
    assert (equal_maps ~data_equal:( = ) ~caml_map core_map_fold);
    assert (equal_maps ~data_equal:( = ) ~caml_map core_map_reduce)
  ;;

  let of_alist_multi _ = assert false

  let%test _ =
    equal_maps
      ~data_equal:[%equal: int list]
      ~caml_map:(Caml_map.add Key.sample [ 0; 1 ] Caml_map.empty)
      (Map.of_alist_multi [ Key.sample, 0; Key.sample, 1 ])
  ;;

  let of_iteri ~iteri:_ = assert false
  let of_iteri_exn ~iteri:_ = assert false

  let alist_iteri alist ~f =
    List.iter alist ~f:(fun (key, data) -> f ~key ~data) [@nontail]
  ;;

  let%test _ =
    let alist = random_alist Key.samples in
    match Map.of_iteri ~iteri:(alist_iteri alist) with
    | `Duplicate_key _ -> false
    | `Ok map -> alist_equal ~data_equal:( = ) (Map.to_alist map) alist
  ;;

  let%test _ =
    match Map.of_iteri ~iteri:(alist_iteri [ Key.sample, 0; Key.sample, 1 ]) with
    | `Ok _ -> false
    | `Duplicate_key _ -> true
  ;;

  let%test_unit _ =
    let alist = random_alist Key.samples in
    ignore (Map.of_iteri_exn ~iteri:(alist_iteri alist) : _ Map.t_)
  ;;

  let%expect_test _ =
    (* Can't use require_does_raise because the exceptions differ on different key
       types. *)
    require
      [%here]
      (Exn.does_raise (fun () ->
         Map.of_iteri_exn ~iteri:(alist_iteri [ Key.sample, 0; Key.sample, 1 ])))
  ;;

  let of_increasing_sequence _ = assert false

  let%test "of_increasing_sequence: vs of_alist_or_error" =
    let alist = random_alist Key.samples in
    let increasing_alist =
      List.sort alist ~compare:(fun a b -> Comparable.lift ~f:fst Key.compare a b)
      |> List.remove_consecutive_duplicates ~equal:(fun a b ->
           Comparable.lift ~f:fst Key.equal a b)
    in
    let duplicates_alist =
      match increasing_alist with
      | [] -> failwith "empty alist in test"
      | x :: xs -> x :: x :: xs
    in
    let decreasing_alist = List.rev increasing_alist in
    (match Map.of_increasing_sequence (Sequence.of_list duplicates_alist) with
     | Error _ -> ()
     | Ok _ -> failwith "of_increasing_sequence: expected to fail with duplicate key");
    (match Map.of_increasing_sequence (Sequence.of_list decreasing_alist) with
     | Error _ -> ()
     | Ok _ -> failwith "of_increasing_sequence: expected to fail with non-increasing key");
    match Map.of_increasing_sequence (Sequence.of_list increasing_alist) with
    | Error _ -> failwith "of_increasing_sequence: expected to get a map back"
    | Ok map -> Map.equal Int.equal map (Map.of_alist_exn increasing_alist)
  ;;

  (* Sequence constructors check against the alist ones *)
  let of_sequence_fold _ = assert false
  let of_sequence_reduce _ = assert false

  let%test_unit _ =
    let filtered =
      List.filter Key.samples ~f:(fun key -> not (Key.equal key Key.sample))
    in
    let alist =
      [ Key.sample, 1; Key.sample, 2 ] @ random_alist filtered @ [ Key.sample, 3 ]
    in
    let alist_fold = Map.of_alist_fold ~init:0 ~f:( + ) alist in
    let alist_reduce = Map.of_alist_reduce ~f:( + ) alist in
    let sequence_fold = Map.of_sequence_fold ~init:0 ~f:( + ) (Sequence.of_list alist) in
    let sequence_reduce = Map.of_sequence_reduce ~f:( + ) (Sequence.of_list alist) in
    assert (Map.equal Int.equal alist_fold sequence_fold);
    assert (Map.equal Int.equal alist_reduce sequence_reduce)
  ;;

  let of_sequence _ = assert false

  let%test _ =
    let input = random_alist Key.samples in
    let alist = Map.of_alist input in
    let sequence = Map.of_sequence (Sequence.of_list input) in
    match alist, sequence with
    | `Duplicate_key alist_key, `Duplicate_key sequence_key ->
      Key.equal alist_key sequence_key
    | `Ok alist_map, `Ok sequence_map -> Map.equal Int.equal alist_map sequence_map
    | _, _ -> false
  ;;

  let of_sequence_or_error _ = assert false
  let of_sequence_exn _ = assert false

  let%test_unit _ =
    let input = random_alist Key.samples in
    let alist_exn = Option.try_with (fun () -> Map.of_alist_exn input) in
    let alist_or_error = Result.ok (Map.of_alist_or_error input) in
    let sequence_exn =
      Option.try_with (fun () -> Map.of_sequence_exn (Sequence.of_list input))
    in
    let sequence_or_error =
      Result.ok (Map.of_sequence_or_error (Sequence.of_list input))
    in
    assert (Option.equal (Map.equal Int.equal) alist_exn sequence_exn);
    assert (Option.equal (Map.equal Int.equal) alist_or_error sequence_or_error)
  ;;

  let of_sequence_multi _ = assert false

  let%test _ =
    let filtered =
      List.filter Key.samples ~f:(fun key -> not (Key.equal key Key.sample))
    in
    let input =
      ((Key.sample, 1) :: (Key.sample, 2) :: random_alist filtered) @ [ Key.sample, 3 ]
    in
    let alist = Map.of_alist_multi input in
    let sequence = Map.of_sequence_multi (Sequence.of_list input) in
    Map.equal (List.equal Int.equal) alist sequence
  ;;

  let of_list_with_key _ = assert false
  let of_list_with_key_or_error _ = assert false
  let of_list_with_key_exn _ = assert false

  let%expect_test _ =
    let input = random_alist Key.samples in
    let list =
      match Map.of_list_with_key input ~get_key:fst with
      | `Ok x -> Some x
      | `Duplicate_key _ -> None
    in
    let list_exn =
      Option.try_with (fun () -> Map.of_list_with_key_exn input ~get_key:fst)
    in
    let list_or_error = Result.ok (Map.of_list_with_key_or_error input ~get_key:fst) in
    let expect =
      Result.ok
        (Map.of_alist_or_error (List.map input ~f:(fun (key, data) -> key, (key, data))))
    in
    let module T = struct
      type t = (int, Key.t * int) Map.t option

      let equal = Option.equal (Map.equal [%equal: Key.t * int])

      let sexp_of_t =
        Option.sexp_of_t (Map.sexp_of_t [%sexp_of: int] [%sexp_of: Key.t * int])
      ;;
    end
    in
    Expect_test_helpers_core.require_equal [%here] (module T) list expect;
    Expect_test_helpers_core.require_equal [%here] (module T) list_exn expect;
    Expect_test_helpers_core.require_equal [%here] (module T) list_or_error expect
  ;;

  let of_list_with_key_multi _ = assert false
  let of_list_with_key_fold _ = assert false
  let of_list_with_key_reduce _ = assert false

  let%expect_test _ =
    let input = random_alist Key.samples in
    let list_multi = Map.of_list_with_key_multi input ~get_key:fst in
    let list_fold =
      Map.of_list_with_key_fold input ~get_key:fst ~init:[] ~f:(fun acc x -> x :: acc)
      |> Map.map ~f:List.rev
    in
    let list_reduce =
      Map.of_list_with_key_reduce
        (List.map input ~f:List.return)
        ~get_key:(fun x -> x |> List.hd_exn |> fst)
        ~f:(fun x y -> x @ y)
    in
    let expect =
      Map.of_alist_multi (List.map input ~f:(fun (key, data) -> key, (key, data)))
    in
    let module T = struct
      type t = (int, (Key.t * int) list) Map.t

      let equal = Map.equal [%equal: (Key.t * int) list]
      let sexp_of_t = Map.sexp_of_t [%sexp_of: int] [%sexp_of: (Key.t * int) list]
    end
    in
    Expect_test_helpers_core.require_equal [%here] (module T) list_multi expect;
    Expect_test_helpers_core.require_equal [%here] (module T) list_reduce expect;
    Expect_test_helpers_core.require_equal [%here] (module T) list_fold expect
  ;;

  let is_empty _ = assert false
  let%test _ = Map.is_empty (Map.empty ())
  let%test _ = not (Map.is_empty (Map.singleton Key.sample 0))
  let%test _ = not (Map.is_empty (random_map Key.samples))
  let of_tree _ = assert false
  let to_tree _ = assert false
  let%test _ = Map.is_empty (Map.of_tree (Map.to_tree (Map.empty ())))

  let%test _ =
    let map = random_map Key.samples in
    alist_equal
      ~data_equal:( = )
      (Map.to_alist map)
      (Map.to_alist (Map.of_tree (Map.to_tree map)))
  ;;

  let add_multi _ = assert false

  let%test _ =
    let m1 = Map.add_multi (Map.empty ()) ~key:Key.sample ~data:0 in
    let m2 = Map.add_multi m1 ~key:Key.sample ~data:1 in
    equal_maps
      ~data_equal:[%equal: int list]
      m2
      ~caml_map:(Caml_map.add Key.sample [ 1; 0 ] Caml_map.empty)
  ;;

  let find_multi _ = assert false

  let%test _ =
    let m1 = Map.add_multi (Map.empty ()) ~key:Key.sample ~data:0 in
    let m2 = Map.add_multi m1 ~key:Key.sample ~data:1 in
    List.equal
      Int.equal
      (Map.find_multi m2 Key.sample)
      (Caml_map.find Key.sample (Caml_map.add Key.sample [ 1; 0 ] Caml_map.empty))
  ;;

  let remove_multi _ = assert false

  let%test _ =
    let m1 = Map.set (Map.empty ()) ~key:Key.sample ~data:[ 1; 2; 3 ] in
    let m2 = Map.remove_multi m1 Key.sample in
    let m3 = Map.remove_multi m2 Key.sample in
    let m4 = Map.remove_multi m3 Key.sample in
    let m5 = Map.remove_multi m4 Key.sample in
    let equal_maps = equal_maps ~data_equal:[%equal: int list] in
    equal_maps m2 ~caml_map:(Caml_map.add Key.sample [ 2; 3 ] Caml_map.empty)
    && equal_maps m3 ~caml_map:(Caml_map.add Key.sample [ 3 ] Caml_map.empty)
    && equal_maps m4 ~caml_map:Caml_map.empty
    && equal_maps m5 ~caml_map:Caml_map.empty
  ;;

  let change _ = assert false
  let update _ = assert false

  let%test _ =
    let m1 = Map.remove (random_map Key.samples) Key.sample in
    let f = function
      | Some x -> Some (x + 1)
      | None -> Some 0
    in
    let m2 = Map.change m1 Key.sample ~f in
    let m3 = Map.change m2 Key.sample ~f in
    match Map.find m3 Key.sample with
    | Some 1 -> true
    | _ -> false
  ;;

  let%test _ =
    let m1 = Map.remove (random_map Key.samples) Key.sample in
    let f = function
      | Some x -> x + 1
      | None -> 0
    in
    let m2 = Map.update m1 Key.sample ~f in
    let m3 = Map.update m2 Key.sample ~f in
    match Map.find m3 Key.sample with
    | Some 1 -> true
    | _ -> false
  ;;

  let%test _ =
    let m1 = Map.set (random_map Key.samples) ~key:Key.sample ~data:0 in
    let m2 =
      Map.change m1 Key.sample ~f:(function
        | Some _ -> None
        | None -> Some 0)
    in
    match Map.find m2 Key.sample with
    | None -> true
    | Some _ -> false
  ;;

  let find_exn _ = assert false

  let%expect_test _ =
    (* Can't use require_does_raise because the exceptions differ on different key
       types. *)
    match Map.find_exn (Map.empty ()) Key.sample with
    | exception (Not_found_s _ | Stdlib.Not_found) -> ()
    | _ -> print_cr [%here] [%message "didn't raise"]
  ;;

  let%test _ = [%equal: int list] (Map.find_multi (Map.empty ()) Key.sample) []
  let fold_right _ = assert false

  let%test _ =
    let f ~key ~data acc = (key, data) :: acc in
    let map = random_map Key.samples in
    let alist = Map.fold map ~init:[] ~f in
    let right_alist = List.rev (Map.fold_right map ~init:[] ~f) in
    alist_equal ~data_equal:( = ) right_alist alist
  ;;

  let filter _ = assert false

  let%test _ =
    let caml_map = Caml_map.add Key.sample (-1) Caml_map.empty in
    let core_map =
      Map.filter
        (Map.set (random_map Key.samples) ~key:Key.sample ~data:(-1))
        ~f:(fun data -> data = -1)
    in
    equal_maps ~data_equal:( = ) ~caml_map core_map
  ;;

  let filteri _ = assert false

  let%test _ =
    let caml_map = Caml_map.add Key.sample 0 Caml_map.empty in
    let core_map =
      Map.filteri
        (Map.set (random_map Key.samples) ~key:Key.sample ~data:0)
        ~f:(fun ~key ~data -> Key.equal key Key.sample && data = 0)
    in
    equal_maps ~data_equal:( = ) ~caml_map core_map
  ;;

  let filter_keys _ = assert false

  let%test _ =
    let caml_map = Caml_map.add Key.sample 0 Caml_map.empty in
    let core_map =
      Map.filter_keys
        (Map.set (random_map Key.samples) ~key:Key.sample ~data:0)
        ~f:(fun key -> Key.equal key Key.sample)
    in
    equal_maps ~data_equal:( = ) ~caml_map core_map
  ;;

  let filter_map _ = assert false

  let%test _ =
    let alist = random_alist Key.samples in
    let core_map = Map.set (Map.of_alist_exn alist) ~key:Key.sample ~data:(-1) in
    let core_map =
      Map.filter_map core_map ~f:(fun x -> if x >= 0 then Some (x + 1) else None)
    in
    let caml_map = Caml_map.remove Key.sample (caml_map_of_alist alist) in
    let caml_map = Caml_map.map (fun x -> x + 1) caml_map in
    equal_maps ~data_equal:( = ) ~caml_map core_map
  ;;

  let filter_mapi _ = assert false

  let%test _ =
    let base_map = Map.set (random_map Key.samples) ~key:Key.sample ~data:0 in
    let m1 =
      Map.filter_mapi base_map ~f:(fun ~key ~data ->
        if Key.equal key Key.sample && data = 0 then None else Some (data + 1))
    in
    let m2 = Map.map (Map.remove base_map Key.sample) ~f:(fun x -> x + 1) in
    Map.equal ( = ) m1 m2
  ;;

  let partition_mapi _ = assert false

  module Some_keys = struct
    let k1 = Key.of_int 1
    let k2 = Key.of_int 2
    let k3 = Key.of_int 3
    let k4 = Key.of_int 4
  end

  let%test _ =
    let open Some_keys in
    let m0 = Map.of_alist_exn [ k1, "a"; k2, "2"; k3, "d"; k4, "5" ] in
    let m1, m2 =
      Map.partition_mapi m0 ~f:(fun ~key ~data ->
        match Int.of_string data with
        | n -> First n
        | exception _ -> Second (data ^ Int.to_string (Key.to_int key)))
    in
    Map.equal Int.equal m1 (Map.of_alist_exn [ k2, 2; k4, 5 ])
    && Map.equal String.equal m2 (Map.of_alist_exn [ k1, "a1"; k3, "d3" ])
  ;;

  let partition_map _ = assert false

  let%test _ =
    let open Some_keys in
    let m0 = Map.of_alist_exn [ k1, "a"; k2, "2"; k3, "d"; k4, "5" ] in
    let m1, m2 =
      Map.partition_map m0 ~f:(fun data ->
        match Int.of_string data with
        | n -> First n
        | exception _ -> Second data)
    in
    Map.equal Int.equal m1 (Map.of_alist_exn [ k2, 2; k4, 5 ])
    && Map.equal String.equal m2 (Map.of_alist_exn [ k1, "a"; k3, "d" ])
  ;;

  let partitioni_tf _ = assert false

  let%test _ =
    let open Some_keys in
    let m0 = Map.of_alist_exn [ k1, "a"; k2, "2"; k3, "d"; k4, "5" ] in
    let m1, m2 =
      Map.partitioni_tf m0 ~f:(fun ~key ~data ->
        match Int.of_string data with
        | n -> n = Key.to_int key
        | exception _ -> false)
    in
    Map.equal String.equal m1 (Map.of_alist_exn [ k2, "2" ])
    && Map.equal String.equal m2 (Map.of_alist_exn [ k1, "a"; k3, "d"; k4, "5" ])
  ;;

  let partition_tf _ = assert false

  let%test _ =
    let open Some_keys in
    let m0 = Map.of_alist_exn [ k1, "a"; k2, "2"; k3, "d"; k4, "5" ] in
    let m1, m2 =
      Map.partition_tf m0 ~f:(fun data ->
        match Int.of_string data with
        | _ -> true
        | exception _ -> false)
    in
    Map.equal String.equal m1 (Map.of_alist_exn [ k2, "2"; k4, "5" ])
    && Map.equal String.equal m2 (Map.of_alist_exn [ k1, "a"; k3, "d" ])
  ;;

  let combine_errors _ = assert false

  let%test _ =
    let open Some_keys in
    let m_ok = Map.of_alist_exn [ k1, Ok "a"; k2, Ok "2"; k3, Ok "d"; k4, Ok "5" ] in
    let ok_m = Map.combine_errors m_ok in
    let m_err =
      Map.of_alist_exn
        [ k1, Ok "a"; k2, error_s (Atom "2"); k3, Ok "d"; k4, error_s (Atom "5") ]
    in
    let err_m = Map.combine_errors m_err in
    Or_error.equal
      (Map.equal String.equal)
      ok_m
      (Ok (Map.of_alist_exn [ k1, "a"; k2, "2"; k3, "d"; k4, "5" ]))
    && Or_error.is_error err_m
  ;;

  let unzip _ = assert false

  let%test _ =
    let open Some_keys in
    let m0 =
      Map.of_alist_exn [ k1, ("a", 2); k2, ("1", 0); k3, ("d", 7); k4, ("5", 3) ]
    in
    let m1, m2 = Map.unzip m0 in
    Map.equal String.equal m1 (Map.of_alist_exn [ k1, "a"; k2, "1"; k3, "d"; k4, "5" ])
    && Map.equal Int.equal m2 (Map.of_alist_exn [ k1, 2; k2, 0; k3, 7; k4, 3 ])
  ;;

  let keys _ = assert false
  let data _ = assert false
  let to_alist ?key_order:_ _ = assert false

  let%test _ =
    let map = Map.of_alist_exn (random_alist Key.samples) in
    let map_keys = Map.keys map in
    let sorted_keys = List.sort map_keys ~compare:Key.compare in
    List.equal Key.equal map_keys sorted_keys
  ;;

  let%test_unit _ =
    let base_alist = random_alist Key.samples in
    let map = Map.of_alist_exn base_alist in
    let map_keys = Map.keys map in
    let all_keys = List.sort ~compare:Key.compare Key.samples in
    let map_data = Map.data map in
    let map_alist = Map.to_alist map in
    assert (List.equal Key.equal map_keys all_keys);
    assert (alist_equal ~data_equal:( = ) map_alist base_alist);
    assert (alist_equal ~data_equal:( = ) (List.zip_exn map_keys map_data) base_alist);
    let map_alist_increasing = Map.to_alist ~key_order:`Increasing map in
    let map_alist_decreasing = Map.to_alist ~key_order:`Decreasing map in
    assert (alist_equal ~data_equal:( = ) map_alist map_alist_increasing);
    assert (alist_equal ~data_equal:( = ) (List.rev map_alist) map_alist_decreasing)
  ;;

  let symmetric_diff _ = assert false
  let fold_symmetric_diff _ = assert false

  type sym_diffs_t = (Key.t, int) Base.Map.Symmetric_diff_element.t list
  [@@deriving compare, sexp_of]

  let symmetric_diff_to_list m1 m2 =
    Sequence.to_list (Map.symmetric_diff m1 m2 ~data_equal:( = ))
  ;;

  let fold_symmetric_diff_to_list m1 m2 =
    Map.fold_symmetric_diff m1 m2 ~data_equal:( = ) ~init:[] ~f:(fun acc d -> d :: acc)
    |> List.rev
  ;;

  let test_symmetric_diffs here m1 m2 expect =
    [%test_result: sym_diffs_t]
      (symmetric_diff_to_list m1 m2)
      ~expect
      ~here:[ [%here]; here ];
    [%test_result: sym_diffs_t]
      (fold_symmetric_diff_to_list m1 m2)
      ~expect
      ~here:[ [%here]; here ];
    true
  ;;

  let%test _ =
    let m1 = random_map Key.samples in
    test_symmetric_diffs [%here] m1 m1 []
  ;;

  let%test _ =
    let key = Key.of_int 7 in
    let m1 = Map.empty () in
    let m1 = Map.set m1 ~key:(Key.of_int 1) ~data:1 in
    let m2 = Map.set m1 ~key ~data:2_000 in
    test_symmetric_diffs [%here] m1 m2 [ key, `Right 2_000 ]
  ;;

  let%test _ =
    let m1 = random_map Key.samples in
    let m2 =
      List.fold (Map.to_alist m1) ~init:(Map.empty ()) ~f:(fun m (k, d) ->
        Map.set m ~key:k ~data:d)
    in
    test_symmetric_diffs [%here] m1 m2 []
  ;;

  let%test _ =
    let key = Key.of_int 20 in
    let m1 = random_map Key.samples in
    let m2 = Map.set m1 ~key ~data:2_000 in
    test_symmetric_diffs [%here] m1 m2 [ key, `Right 2_000 ]
  ;;

  let%test _ =
    let key = Key.of_int 5 in
    let m1 = random_map Key.samples in
    let m2 = Map.remove m1 key in
    test_symmetric_diffs [%here] m1 m2 [ key, `Left (Map.find_exn m1 key) ]
  ;;

  let assert_change map key to_ =
    Map.update map key ~f:(function
      | None -> assert false
      | Some v ->
        assert (v <> to_);
        to_)
  ;;

  let%test _ =
    let key = Key.of_int 7 in
    let m1 = random_map Key.samples in
    let m2 = assert_change m1 key 2_000 in
    test_symmetric_diffs [%here] m1 m2 [ key, `Unequal (Map.find_exn m1 key, 2000) ]
  ;;

  let%test _ =
    let key = Key.of_int 3 in
    let key' = Key.of_int 4 in
    let m1 = random_map Key.samples in
    let m2 = assert_change m1 key 1_000 in
    let m2 = assert_change m2 key' 2_000 in
    test_symmetric_diffs
      [%here]
      m1
      m2
      [ key, `Unequal (Map.find_exn m1 key, 1000)
      ; key', `Unequal (Map.find_exn m1 key', 2000)
      ]
  ;;

  let int_sym_diff_len m1 m2 =
    Core.Map.symmetric_diff m1 m2 ~data_equal:Int.equal |> Sequence.length
  ;;

  let fold_sym_diff_len m1 m2 =
    Core.Map.fold_symmetric_diff m1 m2 ~data_equal:Int.equal ~init:0 ~f:(fun acc _ ->
      Int.succ acc)
  ;;

  let%test _ =
    let map1 = Int.Map.empty in
    let map2 = Int.Map.of_alist_exn [ 1, 1; 2, 2; 3, 3; 4, 4; 5, 5 ] in
    int_sym_diff_len map1 map2 = 5 && fold_sym_diff_len map1 map2 = 5
  ;;

  let%test _ =
    let map1 = Int.Map.of_alist_exn [ 1, 1; 2, 2; 3, 3; 4, 4; 5, 5 ] in
    let map2 = Int.Map.empty in
    int_sym_diff_len map1 map2 = 5 && fold_sym_diff_len map1 map2 = 5
  ;;

  let%test _ =
    let map1 = Int.Map.of_alist_exn [ 1, 1; 2, 2 ] in
    let map2 =
      List.fold
        [ 3, 3; 4, 4; 5, 5 ]
        ~init:map1
        ~f:(fun acc (key, data) -> Core.Map.set acc ~key ~data)
    in
    int_sym_diff_len map1 map2 = 3 && fold_sym_diff_len map1 map2 = 3
  ;;

  let%test _ =
    let map2 = Int.Map.of_alist_exn [ 1, 1; 2, 2 ] in
    let map1 =
      List.fold
        [ 3, 3; 4, 4; 5, 5 ]
        ~init:map2
        ~f:(fun acc (key, data) -> Core.Map.set acc ~key ~data)
    in
    int_sym_diff_len map1 map2 = 3 && fold_sym_diff_len map1 map2 = 3
  ;;

  let merge _ = assert false

  let%test _ =
    let map = random_map Key.samples in
    let added_to_self =
      Map.merge map map ~f:(fun ~key:_ -> function
        | `Left _ | `Right _ -> assert false
        | `Both (x1, x2) -> Some (x1 + x2))
    in
    let doubled = Map.map map ~f:(fun x -> x * 2) in
    Map.equal ( = ) added_to_self doubled
  ;;

  let%test _ =
    let map = random_map Key.samples in
    let map' =
      Map.merge map (Map.empty ()) ~f:(fun ~key:_ x ->
        match x with
        | `Right _ | `Both _ -> assert false
        | `Left x -> Some x)
    in
    Map.equal ( = ) map map'
  ;;

  let%test _ =
    let map = random_map Key.samples in
    let map' =
      Map.merge (Map.empty ()) map ~f:(fun ~key:_ x ->
        match x with
        | `Left _ | `Both _ -> assert false
        | `Right x -> Some x)
    in
    Map.equal ( = ) map map'
  ;;

  let%test _ =
    let map = random_map Key.samples in
    let map' =
      Map.merge map map ~f:(fun ~key:_ x ->
        match x with
        | `Left _ | `Right _ -> assert false
        | `Both _ -> None)
    in
    Map.is_empty map'
  ;;

  let%test_unit _ =
    let test l1 l2 expect =
      let map_of_alist l =
        Map.of_alist_exn (List.map l ~f:(fun (k, v) -> Key.of_int k, v))
      in
      let result =
        Map.merge (map_of_alist l1) (map_of_alist l2) ~f:(fun ~key data ->
          let _key = key in
          match data with
          | `Left l -> if l < 0 then None else Some data
          | `Right r -> if r < 0 then None else Some data
          | `Both (l, r) -> if l + r < 0 then None else Some data)
      in
      let result = Map.to_alist result in
      let result = List.map result ~f:(fun (k, v) -> Key.to_int k, v) in
      [%test_result: (int * [ `Both of int * int | `Left of int | `Right of int ]) list]
        result
        ~expect
    in
    test [] [] [];
    test [ 0, 10 ] [] [ 0, `Left 10 ];
    test [] [ 0, 10 ] [ 0, `Right 10 ];
    test [ 0, 10 ] [ 0, 11 ] [ 0, `Both (10, 11) ];
    test
      [ 0, 10; 3, 13; 4, 14; 6, 16 ]
      [ 1, 11; 3, 13; 4, 14; 5, 15 ]
      [ 0, `Left 10
      ; 1, `Right 11
      ; 3, `Both (13, 13)
      ; 4, `Both (14, 14)
      ; 5, `Right 15
      ; 6, `Left 16
      ];
    test
      [ 0, -10; 3, 13; 4, 14; 6, 16 ]
      [ 1, 11; 3, 13; 4, -15; 5, -15 ]
      [ 1, `Right 11; 3, `Both (13, 13); 6, `Left 16 ]
  ;;

  let merge_disjoint_exn _ = assert false

  let%test_unit _ =
    let to_alist list = List.map list ~f:(fun x -> Key.of_int x, x) in
    let test_ok a b =
      [%test_result: (Key.t * int) list]
        (Map.to_alist
           (Map.merge_disjoint_exn
              (Map.of_alist_exn (to_alist a))
              (Map.of_alist_exn (to_alist b))))
        ~expect:(to_alist (List.sort ~compare (a @ b)))
    in
    test_ok [] [];
    test_ok [ 1 ] [ 2 ];
    test_ok [ 2 ] [ 1 ];
    test_ok [ 1; 3 ] [ 2; 4 ];
    test_ok [ 2; 4; 6; 8 ] [ 1; 3; 5; 7; 9 ];
    let test_error a b =
      assert (
        Exn.does_raise (fun () ->
          Map.merge_disjoint_exn
            (Map.of_alist_exn (to_alist a))
            (Map.of_alist_exn (to_alist b))))
    in
    test_error [ 1 ] [ 1 ];
    test_error [ 1; 2 ] [ 2; 3 ];
    test_error [ 1; 3; 5; 7; 9 ] [ 2; 5; 8 ]
  ;;

  let merge_skewed _ = assert false

  let%test_module _ =
    (module struct
      let make_alist = List.map ~f:(fun (k, v) -> Key.of_int k, v)

      let%test_unit _ =
        let test l1 l2 expect =
          let map_of_alist l = Map.of_alist_exn (make_alist l) in
          let result =
            Map.merge_skewed
              (map_of_alist l1)
              (map_of_alist l2)
              ~combine:(fun ~key:_ x y -> Int.max x y)
            |> Map.to_alist ~key_order:`Increasing
          in
          [%test_result: (Key.t * int) list] result ~expect:(make_alist expect)
        in
        test [] [] [];
        test [ 0, 10 ] [] [ 0, 10 ];
        test [] [ 0, 10 ] [ 0, 10 ];
        test [ 0, 10 ] [ 0, 11 ] [ 0, 11 ];
        test [ 0, 11 ] [ 0, 10 ] [ 0, 11 ];
        test
          [ 0, 10; 3, 13; 4, 14; 6, 16 ]
          [ 1, 11; 3, 13; 4, 14; 5, 15 ]
          [ 0, 10; 1, 11; 3, 13; 4, 14; 5, 15; 6, 16 ]
      ;;

      let%test_unit _ =
        let test l1 l2 expect =
          let map_of_alist l = Map.of_alist_exn (make_alist l) in
          let result =
            Map.merge_skewed
              (map_of_alist l1)
              (map_of_alist l2)
              ~combine:(fun ~key:_ n1 n2 -> n1 - n2)
            |> Map.to_alist ~key_order:`Increasing
          in
          [%test_result: (Key.t * int) list] result ~expect:(make_alist expect)
        in
        test [ 1, 2 ] [ 1, 1 ] [ 1, 1 ];
        test [ 1, 1 ] [ 1, 2 ] [ 1, -1 ];
        (* test arguments that differ in length *)
        test [ 1, 1; 2, 2 ] [ 1, 0 ] [ 1, 1; 2, 2 ];
        test [ 1, 0 ] [ 1, 1; 2, 2 ] [ 1, -1; 2, 2 ]
      ;;
    end)
  ;;

  let min_and_max_keys ~init keys =
    List.fold keys ~init:(init, init) ~f:(fun (min, max) key ->
      ( (if Key.compare key min < 0 then key else min)
      , if Key.compare key max > 0 then key else max ))
  ;;

  let min_elt _ = assert false
  let min_elt_exn _ = assert false
  let max_elt _ = assert false
  let max_elt_exn _ = assert false

  let%test_unit _ =
    let min_key, max_key = min_and_max_keys ~init:Key.sample Key.samples in
    let map = random_map (Key.sample :: Key.samples) in
    let min_key_element = Map.find_exn map min_key in
    let max_key_element = Map.find_exn map max_key in
    assert ([%equal: Key.t * int] (Map.max_elt_exn map) (max_key, max_key_element));
    assert (
      [%equal: (Key.t * int) option] (Map.max_elt map) (Some (max_key, max_key_element)));
    assert ([%equal: Key.t * int] (Map.min_elt_exn map) (min_key, min_key_element));
    assert (
      [%equal: (Key.t * int) option] (Map.min_elt map) (Some (min_key, min_key_element)))
  ;;

  let%test _ = [%equal: (Key.t * int) option] (Map.min_elt (Map.empty ())) None
  let%test _ = [%equal: (Key.t * int) option] (Map.max_elt (Map.empty ())) None

  let%expect_test _ =
    require_does_raise [%here] (fun () -> Map.min_elt_exn (Map.empty ()));
    [%expect {| map.ml.Tree0.Map_min_elt_exn_of_empty_map |}]
  ;;

  let%expect_test _ =
    require_does_raise [%here] (fun () -> Map.max_elt_exn (Map.empty ()));
    [%expect {| map.ml.Tree0.Map_max_elt_exn_of_empty_map |}]
  ;;

  let for_all _ = assert false
  let exists _ = assert false
  let count _ = assert false
  let sum _ = assert false
  let sumi _ = assert false
  let for_alli _ = assert false
  let existsi _ = assert false
  let counti _ = assert false
  let%test _ = Map.for_all (Map.empty ()) ~f:(fun _ -> assert false)
  let%test _ = not (Map.exists (Map.empty ()) ~f:(fun _ -> assert false))
  let%test _ = Map.count (Map.empty ()) ~f:(fun _ -> assert false) = 0
  let%test _ = Map.for_alli (Map.empty ()) ~f:(fun ~key:_ ~data:_ -> assert false)
  let%test _ = not (Map.existsi (Map.empty ()) ~f:(fun ~key:_ ~data:_ -> assert false))
  let%test _ = Map.counti (Map.empty ()) ~f:(fun ~key:_ ~data:_ -> assert false) = 0

  let%test_unit _ =
    let pos x = x >= 0 in
    let neg x = x < 0 in
    let poskd ~key:_ ~data:x = x >= 0 in
    let negkd ~key:_ ~data:x = x < 0 in
    let base_map = random_map Key.samples in
    let with_negative = Map.set base_map ~key:Key.sample ~data:(-1) in
    assert (Map.for_all base_map ~f:pos);
    assert (not (Map.for_all with_negative ~f:pos));
    assert (not (Map.exists base_map ~f:neg));
    assert (Map.exists with_negative ~f:neg);
    assert (Map.count base_map ~f:pos = Map.length base_map);
    assert (Map.count with_negative ~f:pos = Map.length base_map);
    assert (Map.count base_map ~f:neg = 0);
    assert (Map.count with_negative ~f:neg = 1);
    assert (Map.for_alli base_map ~f:poskd);
    assert (not (Map.for_alli with_negative ~f:poskd));
    assert (not (Map.existsi base_map ~f:negkd));
    assert (Map.existsi with_negative ~f:negkd);
    assert (Map.counti base_map ~f:poskd = Map.length base_map);
    assert (Map.counti with_negative ~f:poskd = Map.length base_map);
    assert (Map.counti base_map ~f:negkd = 0);
    assert (Map.counti with_negative ~f:negkd = 1)
  ;;

  let%expect_test "for_all and exists are short-circuiting" =
    let num_calls = ref 0 in
    let const bool _ =
      incr num_calls;
      bool
    in
    let const_kd bool ~key:_ ~data:_ =
      incr num_calls;
      bool
    in
    let require_num_calls expect =
      require_equal [%here] (module Int) !num_calls expect;
      num_calls := 0
    in
    require [%here] (Map.for_all (Map.empty ()) ~f:(const true));
    require [%here] (Map.for_alli (Map.empty ()) ~f:(const_kd true));
    require [%here] (not (Map.exists (Map.empty ()) ~f:(const true)));
    require [%here] (not (Map.existsi (Map.empty ()) ~f:(const_kd true)));
    require_num_calls 0;
    let nonempty_map = random_map Key.samples in
    require [%here] (not (Map.for_all nonempty_map ~f:(const false)));
    require_num_calls 1;
    require [%here] (not (Map.for_alli nonempty_map ~f:(const_kd false)));
    require_num_calls 1;
    require [%here] (Map.exists nonempty_map ~f:(const true));
    require_num_calls 1;
    require [%here] (Map.existsi nonempty_map ~f:(const_kd true));
    require_num_calls 1
  ;;

  let iteri_until _ = assert false

  let%expect_test "iteri_until" =
    let test list =
      let map = list |> List.mapi ~f:(fun i n -> Key.of_int i, n) |> Map.of_alist_exn in
      Map.iteri_until map ~f:(fun ~key ~data ->
        let key = Key.to_int key in
        print_s [%message (key : int) (data : int)];
        if key > data then Stop else Continue)
      |> [%sexp_of: Core.Map.Finished_or_unfinished.t]
      |> print_s
    in
    test [ 3; 1; 4; 1; 5; 9; 2; 6; 5 ];
    [%expect
      {|
      ((key  0)
       (data 3))
      ((key  1)
       (data 1))
      ((key  2)
       (data 4))
      ((key  3)
       (data 1))
      Unfinished
      |}];
    test [ 1; 4; 9; 16; 25; 36; 49; 64 ];
    [%expect
      {|
      ((key  0)
       (data 1))
      ((key  1)
       (data 4))
      ((key  2)
       (data 9))
      ((key  3)
       (data 16))
      ((key  4)
       (data 25))
      ((key  5)
       (data 36))
      ((key  6)
       (data 49))
      ((key  7)
       (data 64))
      Finished
      |}]
  ;;

  let fold_until _ = assert false

  let%expect_test "fold_until" =
    let test list =
      let map = List.mapi list ~f:(fun i n -> Key.of_int i, n) |> Map.of_alist_exn in
      Map.fold_until
        map
        ~init:0
        ~f:(fun ~key ~data acc ->
          let key = Key.to_int key in
          print_s [%message (key : int) (data : int)];
          if key > data then Stop acc else Continue (acc + key))
        ~finish:Fn.id
      |> [%sexp_of: int]
      |> print_s
    in
    test [ 3; 1; 4; 1; 5; 9; 2; 6; 5 ];
    [%expect
      {|
      ((key  0)
       (data 3))
      ((key  1)
       (data 1))
      ((key  2)
       (data 4))
      ((key  3)
       (data 1))
      3
      |}];
    test [ 1; 4; 9; 16; 25; 36; 49; 64 ];
    [%expect
      {|
      ((key  0)
       (data 1))
      ((key  1)
       (data 4))
      ((key  2)
       (data 9))
      ((key  3)
       (data 16))
      ((key  4)
       (data 25))
      ((key  5)
       (data 36))
      ((key  6)
       (data 49))
      ((key  7)
       (data 64))
      28
      |}]
  ;;

  let to_sequence ?order:_ ?keys_greater_or_equal_to:_ ?keys_less_or_equal_to:_ _ =
    assert false
  ;;

  let%test_module "to_sequence" =
    (module struct
      let m = random_map Key.samples

      let ( <=> ) observed expected =
        [%test_eq: (Key.t * int) list] (Sequence.to_list observed) expected
      ;;

      let limit_keys min max =
        List.filter ~f:(fun (key, _) -> Key.( >= ) key min && Key.( <= ) key max)
      ;;

      let%test_unit _ = Map.to_sequence ~order:`Increasing_key m <=> Map.to_alist m

      let%test_unit _ =
        Map.to_sequence ~order:`Decreasing_key m <=> List.rev (Map.to_alist m)
      ;;

      let%test_unit _ =
        Map.to_sequence ~order:`Increasing_key ~keys_greater_or_equal_to:Key.mid m
        <=> limit_keys Key.mid Key.max (Map.to_alist m)
      ;;

      let%test_unit _ =
        let keys_greater_or_equal_to, keys_less_or_equal_to = Key.mid, Key.pred Key.max in
        Map.to_sequence
          m
          ~order:`Increasing_key
          ~keys_greater_or_equal_to
          ~keys_less_or_equal_to
        <=> limit_keys keys_greater_or_equal_to keys_less_or_equal_to (Map.to_alist m)
      ;;

      let%test_unit _ =
        Map.to_sequence m ~order:`Increasing_key ~keys_less_or_equal_to:Key.mid
        <=> limit_keys Key.min Key.mid (Map.to_alist m)
      ;;

      let%test_unit _ =
        Map.to_sequence ~order:`Decreasing_key ~keys_less_or_equal_to:Key.mid m
        <=> limit_keys Key.min Key.mid (List.rev (Map.to_alist m))
      ;;

      let%test_unit _ =
        let keys_greater_or_equal_to, keys_less_or_equal_to = Key.succ Key.min, Key.mid in
        Map.to_sequence
          m
          ~order:`Decreasing_key
          ~keys_greater_or_equal_to
          ~keys_less_or_equal_to
        <=> limit_keys
              keys_greater_or_equal_to
              keys_less_or_equal_to
              (List.rev (Map.to_alist m))
      ;;

      let%test_unit _ =
        Map.to_sequence m ~order:`Decreasing_key ~keys_greater_or_equal_to:Key.mid
        <=> limit_keys Key.mid Key.max (List.rev (Map.to_alist m))
      ;;

      let%test_unit _ = Map.to_sequence ~order:`Increasing_key (Map.empty ()) <=> []
      let%test_unit _ = Map.to_sequence ~order:`Decreasing_key (Map.empty ()) <=> []

      let%test_unit _ =
        Map.to_sequence
          ~order:`Increasing_key
          ~keys_greater_or_equal_to:(Key.succ Key.max)
          m
        <=> []
      ;;

      let%test_unit _ =
        Map.to_sequence ~order:`Decreasing_key ~keys_less_or_equal_to:(Key.pred Key.min) m
        <=> []
      ;;

      let%test_unit _ =
        Map.to_sequence
          ~order:`Increasing_key
          ~keys_less_or_equal_to:Key.min
          ~keys_greater_or_equal_to:Key.max
          m
        <=> []
      ;;
    end)
  ;;

  let map_keys _ = assert false
  let map_keys_exn _ = assert false

  let%test_module "map_keys" =
    (module struct
      let to_alist map = List.map (Map.to_alist map) ~f:(fun (x, y) -> Key.to_int x, y)

      let of_alist alist =
        Map.of_alist_exn (List.map alist ~f:(fun (x, y) -> Key.of_int x, y))
      ;;

      let map012 = of_alist [ 0, "one"; 1, "two"; 2, "three" ]
      let map123 = of_alist [ 1, "one"; 2, "two"; 3, "three" ]

      let ( <=> ) actual expect =
        [%test_result: (int * string) list] (to_alist actual) ~expect:(to_alist expect)
      ;;

      let%test_unit _ = Map.map_keys_exn map012 ~f:Key.succ <=> map123
      let%test _ = does_raise (fun () -> Map.map_keys_exn map123 ~f:(fun _ -> Key.sample))
    end)
  ;;

  let binary_search_subrange _ ~compare:_ ~lower_bound:_ ~upper_bound:_ = assert false

  let%test_module "binary_search_subrange" =
    (module struct
      (* Strategy is to generate maps keyed by integer, and compare various prefixes of
         the binary representation. *)

      module Key_int = struct
        type t = int [@@deriving quickcheck, sexp_of]

        let length = 8 (* small but allows a few lengths of prefix *)
        let quickcheck_generator = Int.gen_uniform_incl 0 ((1 lsl length) - 1)
      end

      module Prefix_length = struct
        type t = int [@@deriving quickcheck, sexp]

        let quickcheck_generator = Int.gen_uniform_incl 0 Key_int.length
      end

      module Key = struct
        include Key

        let quickcheck_generator =
          Quickcheck.Generator.map Key_int.quickcheck_generator ~f:of_int
        ;;

        let quickcheck_observer =
          Quickcheck.Observer.unmap Key_int.quickcheck_observer ~f:to_int
        ;;

        let quickcheck_shrinker =
          Quickcheck.Shrinker.map Key_int.quickcheck_shrinker ~f:of_int ~f_inverse:to_int
        ;;
      end

      module Int_map = struct
        type t = (int, int) Map.t [@@deriving sexp_of]

        let quickcheck_generator =
          Map.quickcheck_generator Key.quickcheck_generator Int.quickcheck_generator
        ;;

        let quickcheck_observer =
          Map.quickcheck_observer Key.quickcheck_observer Int.quickcheck_observer
        ;;

        let quickcheck_shrinker =
          Map.quickcheck_shrinker Key.quickcheck_shrinker Int.quickcheck_shrinker
        ;;
      end

      type t =
        { map : Int_map.t
        ; prefix_length : Prefix_length.t
        ; lower_bound : Key_int.t Maybe_bound.t
        ; upper_bound : Key_int.t Maybe_bound.t
        }
      [@@deriving quickcheck, sexp_of]

      let suffix_length_of ~prefix_length = Key_int.length - prefix_length

      let compare_prefix ~prefix_length ~key ~data:_ bound =
        let prefix_of x = x asr suffix_length_of ~prefix_length in
        Comparable.lift Int.compare ~f:prefix_of (Key.to_int key) bound
      ;;

      let f { map; prefix_length; lower_bound; upper_bound } =
        Map.binary_search_subrange
          map
          ~compare:(compare_prefix ~prefix_length)
          ~lower_bound
          ~upper_bound
        |> Map.to_alist
      ;;

      let reference_f { map; prefix_length; lower_bound; upper_bound } =
        let satisfies_lower_bound (key, data) =
          match lower_bound with
          | Unbounded -> true
          | Incl bound -> compare_prefix ~prefix_length ~key ~data bound >= 0
          | Excl bound -> compare_prefix ~prefix_length ~key ~data bound > 0
        in
        let satisfies_upper_bound (key, data) =
          match upper_bound with
          | Unbounded -> true
          | Incl bound -> compare_prefix ~prefix_length ~key ~data bound <= 0
          | Excl bound -> compare_prefix ~prefix_length ~key ~data bound < 0
        in
        Map.to_sequence map
        |> Sequence.filter ~f:satisfies_lower_bound
        |> Sequence.filter ~f:satisfies_upper_bound
        |> Sequence.to_list
      ;;

      let%expect_test _ =
        Expect_test_helpers_base.quickcheck
          [%here]
          quickcheck_generator
          ~f:(fun input ->
            Expect_test_helpers_base.require_equal
              [%here]
              (module struct
                type t = (Key.t * int) list [@@deriving equal, sexp_of]
              end)
              (f input)
              (reference_f input))
          ~sexp_of:sexp_of_t
          ~shrinker:quickcheck_shrinker;
        [%expect {| |}]
      ;;
    end)
  ;;

  let length _ = assert false

  (* Length has to be updated correctly by many operations, which should be tested here.
     Some basic operations are already tested above. *)
  let%test_module "length" =
    (module struct
      let sample_map = random_map Key.samples
      let k1 = List.nth_exn (Map.keys sample_map) 0
      let k2 = List.nth_exn (Map.keys sample_map) 1
      let k3 = List.nth_exn (Map.keys sample_map) 2
      let k4 = List.nth_exn (Map.keys sample_map) 3

      let%test "change" =
        let m = Map.set ~key:Key.sample ~data:1 (Map.empty ()) in
        assert (Map.length m = 1);
        let m = Map.change m Key.sample ~f:Fn.id in
        let caml_map = Caml_map.add Key.sample 1 Caml_map.empty in
        assert (equal_maps ~data_equal:( = ) m ~caml_map);
        let m = Map.change (Map.empty ()) Key.sample ~f:(Fn.const (Some 1)) in
        assert (Map.length m = 1);
        let m = Map.change m Key.sample ~f:(Fn.const (Some 1)) in
        assert (Map.length m = 1);
        let m = Map.change m Key.sample ~f:(Fn.const None) in
        Map.length m = 0
      ;;

      let%test "update" =
        let m = Map.set ~key:Key.sample ~data:1 (Map.empty ()) in
        assert (Map.length m = 1);
        let m = Map.update (Map.empty ()) Key.sample ~f:(Fn.const 1) in
        assert (Map.length m = 1);
        let m = Map.update m Key.sample ~f:(Fn.const 1) in
        Map.length m = 1
      ;;

      let%test "filteri" =
        let m' =
          Map.filteri sample_map ~f:(fun ~key:x ~data:_ ->
            Key.( <> ) x k1 && Key.( <> ) x k2)
        in
        let m'' = Map.remove (Map.remove sample_map k1) k2 in
        assert (Map.length m' = Map.length m'');
        Map.length m' = Map.length sample_map - 2
      ;;

      let%test "of_alist_exn and of_alist_fold" =
        let expected_length = List.length Key.samples in
        let dup x = x, x in
        let m = Map.of_alist_exn (List.map Key.samples ~f:dup) in
        assert (Map.length m = List.length Key.samples);
        let alist = List.map (Key.samples @ Key.samples) ~f:dup in
        let m = Map.of_alist_fold alist ~init:Key.sample ~f:(fun x _ -> x) in
        Map.length m = expected_length
      ;;

      let%test "merge" =
        let m1 = Map.of_alist_exn [ k1, 1 ] in
        let m2 = Map.of_alist_exn [ k2, 2 ] in
        let m' =
          Map.merge m1 m2 ~f:(fun ~key:_ -> function
            | `Both _ -> assert false
            | `Left x | `Right x -> Some x)
        in
        assert (Map.length m' = 2);
        let m3 = Map.of_alist_exn [ k1, 2 ] in
        let m' =
          Map.merge m1 m3 ~f:(fun ~key:_ -> function
            | `Both (x, _) -> Some x
            | `Left _ | `Right _ -> assert false)
        in
        assert (Map.length m' = 1);
        let m' =
          Map.merge m1 m3 ~f:(fun ~key:_ -> function
            | `Both (_, _) -> None
            | `Left _ | `Right _ -> assert false)
        in
        assert (Map.length m' = 0);
        let m4 = Map.of_alist_exn [ k1, 1; k2, 2; k3, 3 ] in
        let m5 = Map.of_alist_exn [ k3, 99; k4, 4 ] in
        let m' =
          Map.merge m4 m5 ~f:(fun ~key:_ -> function
            | `Both (x, _) -> Some x
            | `Left x | `Right x -> Some x)
        in
        Map.length m' = 4
      ;;
    end)
  ;;

  let fold_range_inclusive _ = assert false
  let range_to_alist _ = assert false
  let closest_key _ = assert false
  let nth _ = assert false
  let nth_exn _ = assert false
  let rank _ = assert false

  let%test_unit _ =
    let map = random_map (Key.sample :: Key.samples) in
    let min_key, max_key = min_and_max_keys ~init:Key.sample Key.samples in
    let after_min, before_max =
      List.fold Key.samples ~init:(max_key, min_key) ~f:(fun (near_min, near_max) key ->
        ( (if Key.compare key min_key > 0 && Key.compare key near_min < 0
           then key
           else near_min)
        , if Key.compare key max_key < 0 && Key.compare key near_max > 0
          then key
          else near_max ))
    in
    let keys_between ~min ~max =
      Map.fold_range_inclusive map ~min ~max ~f:(fun ~key:_ ~data:_ n -> n + 1) ~init:0
    in
    let length = Map.length map in
    (* fold_range_inclusive *)
    (* The two [Key.(>)] assertions below verify that the intervals in the
       two assertions following them are indeed empty.*)
    assert (Key.( > ) before_max min_key);
    assert (Key.( > ) max_key min_key);
    assert (keys_between ~min:before_max ~max:min_key = 0);
    assert (keys_between ~min:max_key ~max:min_key = 0);
    assert (keys_between ~min:min_key ~max:max_key = length);
    assert (keys_between ~min:after_min ~max:before_max = length - 2);
    (* closest_key *)
    let prev_key t k = Map.closest_key t `Less_than k in
    let next_key t k = Map.closest_key t `Greater_than k in
    assert ([%equal: (Key.t * int) option] (prev_key map min_key) None);
    assert ([%equal: (Key.t * int) option] (next_key map max_key) None);
    let optional_key_equal key = function
      | None -> false
      | Some (key', _) -> Key.equal key key'
    in
    assert (optional_key_equal min_key (prev_key map after_min));
    assert (optional_key_equal max_key (next_key map before_max));
    assert (optional_key_equal min_key (Map.closest_key map `Less_or_equal_to min_key));
    assert (optional_key_equal min_key (Map.closest_key map `Greater_or_equal_to min_key));
    assert (optional_key_equal max_key (Map.closest_key map `Less_or_equal_to max_key));
    assert (optional_key_equal max_key (Map.closest_key map `Greater_or_equal_to max_key));
    assert (optional_key_equal after_min (Map.closest_key map `Less_or_equal_to after_min));
    assert (
      optional_key_equal after_min (Map.closest_key map `Greater_or_equal_to after_min));
    assert (
      optional_key_equal before_max (Map.closest_key map `Less_or_equal_to before_max));
    assert (
      optional_key_equal before_max (Map.closest_key map `Greater_or_equal_to before_max));
    (let map_with_hole_after_min = Map.remove map after_min in
     assert (optional_key_equal min_key (prev_key map_with_hole_after_min after_min));
     assert (
       optional_key_equal
         min_key
         (Map.closest_key map_with_hole_after_min `Less_or_equal_to after_min));
     let map_with_hole_before_max = Map.remove map before_max in
     assert (optional_key_equal max_key (next_key map_with_hole_before_max before_max));
     assert (
       optional_key_equal
         max_key
         (Map.closest_key map_with_hole_before_max `Greater_or_equal_to before_max)));
    (* range_to_alist *)
    assert (
      alist_equal
        ~data_equal:( = )
        (Map.range_to_alist ~min:min_key ~max:max_key map)
        (Map.to_alist map));
    assert (
      alist_equal
        ~data_equal:( = )
        (Map.range_to_alist ~min:after_min ~max:before_max map)
        (Map.to_alist (Map.remove (Map.remove map min_key) max_key)));
    (* rank *)
    assert ([%equal: int option] (Map.rank map min_key) (Some 0));
    assert ([%equal: int option] (Map.rank map after_min) (Some 1));
    assert ([%equal: int option] (Map.rank map before_max) (Some (length - 2)));
    assert ([%equal: int option] (Map.rank map max_key) (Some (length - 1)));
    assert ([%equal: int option] (Map.rank (Map.remove map Key.sample) Key.sample) None);
    (* nth *)
    assert (
      alist_equal
        ~data_equal:( = )
        (Map.to_alist map)
        (List.init (Map.length map) ~f:(Map.nth map) |> List.filter_opt));
    assert (Option.is_none (Map.nth map (-1)));
    assert (Option.is_none (Map.nth map (Map.length map)))
  ;;

  let _ = Map.nth_exn

  (* same testing as nth *)
  let split _ = assert false
  let split_le_gt _ = assert false
  let split_lt_ge _ = assert false

  let%test_unit _ =
    let check here map pivot =
      let () =
        let l, maybe, r = Map.split map pivot in
        assert (Map.invariants l);
        assert (Map.invariants r);
        Map.iteri l ~f:(fun ~key ~data:_ -> assert (Key.( < ) key pivot));
        Map.iteri r ~f:(fun ~key ~data:_ -> assert (Key.( > ) key pivot));
        [%test_eq: (Key.t * int) option]
          ~here:[ here ]
          (Option.map ~f:(fun d -> pivot, d) (Map.find map pivot))
          maybe;
        [%test_eq: int]
          ~here:[ here ]
          (Map.length map)
          (Map.length l + Map.length r + Option.length maybe)
      in
      let () =
        let l, r = Map.split_le_gt map pivot in
        assert (Map.invariants l);
        assert (Map.invariants r);
        Map.iteri l ~f:(fun ~key ~data:_ -> assert (Key.( <= ) key pivot));
        Map.iteri r ~f:(fun ~key ~data:_ -> assert (Key.( > ) key pivot));
        [%test_eq: int] ~here:[ here ] (Map.length map) (Map.length l + Map.length r)
      in
      let () =
        let l, r = Map.split_lt_ge map pivot in
        assert (Map.invariants l);
        assert (Map.invariants r);
        Map.iteri l ~f:(fun ~key ~data:_ -> assert (Key.( < ) key pivot));
        Map.iteri r ~f:(fun ~key ~data:_ -> assert (Key.( >= ) key pivot));
        [%test_eq: int] ~here:[ here ] (Map.length map) (Map.length l + Map.length r)
      in
      ()
    in
    let map = random_map Key.samples in
    check [%here] map Key.min;
    check [%here] map Key.max;
    check [%here] map Key.mid;
    let map = Map.remove map Key.mid in
    check [%here] map Key.mid
  ;;

  let subrange _ = assert false

  let%test_unit "subrange" =
    let check here map ~lower_bound ~upper_bound =
      let res = Map.subrange map ~lower_bound ~upper_bound in
      assert (Map.invariants res);
      let subrange_computed_naively =
        try
          Map.fold map ~init:(Map.empty ()) ~f:(fun ~key ~data acc ->
            if Maybe_bound.interval_contains_exn
                 key
                 ~lower:lower_bound
                 ~upper:upper_bound
                 ~compare:Key.compare
            then Map.set acc ~key ~data
            else acc)
        with
        | _ -> Map.empty ()
      in
      [%test_result: (int, int) Map.t]
        ~here:[ here; [%here] ]
        ~expect:subrange_computed_naively
        res
    in
    let map = random_map Key.samples in
    check [%here] map ~lower_bound:Unbounded ~upper_bound:Unbounded;
    check [%here] map ~lower_bound:(Incl Key.min) ~upper_bound:(Incl Key.max);
    check [%here] map ~lower_bound:(Excl Key.min) ~upper_bound:(Incl Key.max);
    check [%here] map ~lower_bound:(Incl Key.min) ~upper_bound:(Excl Key.max);
    check [%here] map ~lower_bound:(Excl Key.min) ~upper_bound:(Excl Key.max);
    check [%here] map ~lower_bound:(Excl Key.min) ~upper_bound:Unbounded;
    check [%here] map ~lower_bound:(Incl Key.max) ~upper_bound:(Excl Key.min);
    check [%here] map ~lower_bound:(Incl Key.min) ~upper_bound:(Excl Key.mid);
    check [%here] map ~lower_bound:(Incl Key.min) ~upper_bound:(Incl Key.mid);
    let map = Map.remove map Key.mid in
    check [%here] map ~lower_bound:(Incl Key.min) ~upper_bound:(Excl Key.mid);
    check [%here] map ~lower_bound:(Incl Key.min) ~upper_bound:(Incl Key.mid);
    let lmid = Key.(of_int ((to_int min + to_int mid) / 2))
    and rmid = Key.(of_int ((to_int mid + to_int max) / 2)) in
    check [%here] map ~lower_bound:(Incl lmid) ~upper_bound:(Incl rmid);
    check [%here] map ~lower_bound:(Incl lmid) ~upper_bound:(Excl rmid);
    check [%here] map ~lower_bound:(Excl lmid) ~upper_bound:(Incl rmid);
    check [%here] map ~lower_bound:(Excl lmid) ~upper_bound:(Excl rmid)
  ;;

  let append ~lower_part:_ ~upper_part:_ = assert false

  let%test_unit "append" =
    let check whole lower_part upper_part =
      match Map.append ~lower_part ~upper_part with
      | `Ok whole2 ->
        assert (Map.invariants whole2);
        assert (
          0
          = Map.compare
              Int.compare
              (fun x y -> if phys_equal x y then 0 else 1)
              whole2
              whole)
      | `Overlapping_key_ranges -> failwith "expected Ok"
    in
    let check_fail lower_part upper_part =
      match Map.append ~lower_part ~upper_part with
      | `Ok _ -> failwith "expected Error"
      | `Overlapping_key_ranges -> ()
    in
    let map = random_map Key.samples in
    let mid_value = Map.find map Key.mid |> Option.value_exn in
    let map_without_mid = Map.remove map Key.mid in
    let lower_part, _, upper_part = Map.split map_without_mid Key.mid in
    check map_without_mid lower_part upper_part;
    check_fail upper_part lower_part;
    check_fail
      (Map.set lower_part ~key:Key.mid ~data:mid_value)
      (Map.set upper_part ~key:Key.mid ~data:mid_value);
    let check_via_alist lower_part upper_part =
      match Map.append ~lower_part ~upper_part with
      | `Ok whole_map ->
        assert (Map.invariants whole_map);
        let expected_whole_map =
          Map.of_alist_exn
            (List.append (Map.to_alist lower_part) (Map.to_alist upper_part))
        and compare =
          Map.compare Int.compare (fun x y -> if phys_equal x y then 0 else 1)
        in
        assert (compare expected_whole_map whole_map = 0)
      | `Overlapping_key_ranges ->
        let lower_part_max = Key.to_int (fst (Map.max_elt_exn lower_part))
        and upper_part_min = Key.to_int (fst (Map.min_elt_exn upper_part)) in
        assert (Int.compare lower_part_max upper_part_min >= 0)
    in
    let examples =
      List.map
        ~f:(fun l -> Map.of_alist_exn (List.map ~f:(Tuple2.map_fst ~f:Key.of_int) l))
        [ [ 1, 1; 2, 2; 3, 3 ]
        ; [ 3, 3; 4, 4; 5, 5 ]
        ; [ 6, 6 ]
        ; [ 1, 1; 6, 6 ]
        ; [ 1, 1; 2, 2 ]
        ]
    in
    List.Let_syntax.(
      let%map l = examples
      and r = examples in
      l, r)
    |> List.iter ~f:(fun (l, r) -> check_via_alist l r)
  ;;

  let%test _ =
    [%equal: (Key.t * int) option]
      (Map.closest_key (Map.empty ()) `Greater_or_equal_to Key.sample)
      None
  ;;

  let%test _ =
    [%equal: (Key.t * int) option]
      (Map.closest_key (Map.empty ()) `Greater_than Key.sample)
      None
  ;;

  let%test _ =
    [%equal: (Key.t * int) option]
      (Map.closest_key (Map.empty ()) `Less_or_equal_to Key.sample)
      None
  ;;

  let%test _ =
    [%equal: (Key.t * int) option]
      (Map.closest_key (Map.empty ()) `Less_than Key.sample)
      None
  ;;

  let binary_search _ = assert false
  let binary_search_segmented _ = assert false

  let%test_module "binary_search" =
    (module struct
      let small_map =
        Map.of_alist_exn (List.map ~f:(Tuple2.map_fst ~f:Key.of_int) [ 1, 1; 2, 2; 3, 3 ])
      ;;

      let compare_key ~key ~data:_ k = Int.compare (Key.to_int key) k

      let%test _ =
        [%equal: (Key.t * int) option]
          (Map.binary_search (Map.empty ()) ~compare:compare_key `First_equal_to 1)
          None
      ;;

      let%test _ =
        [%equal: (Key.t * int) option]
          (Map.binary_search small_map ~compare:compare_key `First_equal_to 2)
          (Some (Key.of_int 2, 2))
      ;;

      let%test _ =
        [%equal: (Key.t * int) option]
          (Map.binary_search
             small_map
             ~compare:compare_key
             `First_greater_than_or_equal_to
             2)
          (Some (Key.of_int 2, 2))
      ;;

      let%test _ =
        [%equal: (Key.t * int) option]
          (Map.binary_search
             small_map
             ~compare:compare_key
             `First_strictly_greater_than
             3)
          None
      ;;

      let%test _ =
        [%equal: (Key.t * int) option]
          (Map.binary_search_segmented
             (Map.empty ())
             ~segment_of:(fun ~key:_ ~data:_ -> assert false)
             `First_on_right)
          None
      ;;

      let%test _ =
        [%equal: (Key.t * int) option]
          (Map.binary_search_segmented
             small_map
             ~segment_of:(fun ~key ~data:_ ->
               if Key.to_int key < 3 then `Left else `Right)
             `First_on_right)
          (Some (Key.of_int 3, 3))
      ;;

      let%test _ =
        [%equal: (Key.t * int) option]
          (Map.binary_search_segmented
             small_map
             ~segment_of:(fun ~key ~data:_ ->
               if Key.to_int key < 3 then `Left else `Right)
             `Last_on_left)
          (Some (Key.of_int 2, 2))
      ;;

      let%test _ =
        [%equal: (Key.t * int) option]
          (Map.binary_search_segmented
             small_map
             ~segment_of:(fun ~key ~data:_ ->
               if Key.to_int key > 3 then `Right else `Left)
             `First_on_right)
          None
      ;;
    end)
  ;;

  module Make_applicative_traversals (_ : Applicative.Lazy_applicative) = struct
    let mapi _ = assert false
    let filter_mapi _ = assert false
  end

  let validate ~name:_ _ = assert false

  let%test_unit _ =
    let validate expect map =
      expect
        (Validate.result
           (Map.validate
              ~name:Key.to_string
              (Validate.of_error (fun i ->
                 if i mod 2 = 0 then Ok () else error "must be even" i [%sexp_of: int]))
              map))
    in
    let is_ok = Result.is_ok in
    let is_error = Result.is_error in
    assert (validate is_ok (Map.empty ()));
    assert (validate is_ok (Map.of_alist_exn [ Key.of_int 0, 0 ]));
    assert (validate is_error (Map.of_alist_exn [ Key.of_int 0, 1 ]));
    assert (validate is_ok (Map.of_alist_exn [ Key.of_int 0, 0; Key.of_int 1, 0 ]));
    assert (validate is_error (Map.of_alist_exn [ Key.of_int 0, 0; Key.of_int 1, 1 ]))
  ;;

  let validatei ~name:_ _ = assert false

  let%test_unit _ =
    let checki (key, data) =
      if Key.to_int key mod 2 = 0 && data mod 2 = 0
      then Ok ()
      else error_s [%message "key and data must be even" (key : Key.t) (data : int)]
    in
    let validatei expect map =
      expect
        (Validate.result
           (Map.validatei ~name:Key.to_string (Validate.of_error checki) map))
    in
    let is_ok = Result.is_ok in
    let is_error = Result.is_error in
    assert (validatei is_ok (Map.empty ()));
    assert (validatei is_ok (Map.of_alist_exn [ Key.of_int 0, 0 ]));
    assert (validatei is_error (Map.of_alist_exn [ Key.of_int 0, 1 ]));
    assert (validatei is_error (Map.of_alist_exn [ Key.of_int 1, 0 ]));
    assert (validatei is_ok (Map.of_alist_exn [ Key.of_int 0, 0; Key.of_int 2, 0 ]));
    assert (validatei is_error (Map.of_alist_exn [ Key.of_int 0, 0; Key.of_int 1, 1 ]));
    assert (validatei is_error (Map.of_alist_exn [ Key.of_int 0, 0; Key.of_int 2, 1 ]));
    assert (validatei is_error (Map.of_alist_exn [ Key.of_int 0, 0; Key.of_int 1, 2 ]))
  ;;

  (* Ensure polymorphic equality raises for maps. *)
  let%test_unit _ =
    match Map.kind with
    | `Tree -> ()
    | `Map ->
      let ts = [ Map.empty (); Map.of_alist_exn [ Key.sample, 13 ] ] in
      List.iter ts ~f:(fun t1 ->
        List.iter ts ~f:(fun t2 -> assert (Exn.does_raise (fun () -> Poly.equal t1 t2))))
  ;;

  let key_set _ = assert false
  let%test_unit _ = assert (Set.is_empty (Map.key_set (Map.empty ())))

  let%test_unit _ =
    let m =
      Map.of_alist_exn (List.map ~f:(Tuple2.map_fst ~f:Key.of_int) [ 1, 1; 2, 2; 3, 3 ])
    in
    let s = Map.key_set m in
    assert (Set.length s = Map.length m);
    Map.iteri m ~f:(fun ~key ~data -> assert (Key.to_int key = data));
    assert ([%equal: Key.t list] (Set.to_list s) (Map.keys m))
  ;;

  let of_key_set _ = assert false

  let%test_unit _ =
    assert (Map.is_empty (Map.of_key_set (Map.key_set (Map.empty ())) ~f:Key.to_int))
  ;;

  let%test_unit _ =
    let m =
      Map.of_alist_exn (List.map ~f:(Tuple2.map_fst ~f:Key.of_int) [ 1, 1; 2, 2; 3, 3 ])
    in
    let key_set = Map.key_set m in
    assert (Map.equal Int.equal (Map.of_key_set key_set ~f:Key.to_int) m)
  ;;

  let transpose_keys _ = assert false

  let%expect_test _ =
    let make alist =
      Map.of_iteri_exn ~iteri:(fun ~f ->
        List.iter alist ~f:(fun (key, data) -> f ~key:(Key.of_int key) ~data) [@nontail])
    in
    let test alist =
      let original = make alist |> Map.map ~f:make in
      let transposed = Map.transpose_keys original in
      print_s [%sexp (transposed : (int, (int, int) Map.t) Map.t)]
    in
    test [];
    [%expect {| () |}];
    test [ 0, [] ];
    [%expect {| () |}];
    test [ 0, []; 1, [ 1, 11 ] ];
    [%expect {| ((1 ((1 11)))) |}];
    test [ 0, []; 1, [ 1, 11 ]; 2, [ 1, 21; 2, 22 ] ];
    [%expect
      {|
      ((1 (
         (1 11)
         (2 21)))
       (2 ((2 22))))
      |}];
    test [ 0, []; 1, [ 1, 11 ]; 2, [ 1, 21; 2, 22 ]; 3, [ 1, 31; 2, 32; 3, 33 ] ];
    [%expect
      {|
      ((1 (
         (1 11)
         (2 21)
         (3 31)))
       (2 (
         (2 22)
         (3 32)))
       (3 ((3 33))))
      |}]
  ;;

  let quickcheck_generator _ _ = assert false

  let%test_module _ =
    (module struct
      open Quickcheck

      let sexp_of = [%sexp_of: (int, char) Map.t]
      let compare = Map.compare_direct Char.compare

      let quickcheck_generator =
        Map.quickcheck_generator Key.quickcheck_generator Char.quickcheck_generator
      ;;

      let can_generate f = test_can_generate quickcheck_generator ~sexp_of ~f

      let%test_unit _ =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ = can_generate (fun t -> Map.is_empty t)
      let%test_unit _ = can_generate (fun t -> Map.length t = 1)
      let%test_unit _ = can_generate (fun t -> Map.length t = 2)
      let%test_unit _ = can_generate (fun t -> Map.length t >= 3)

      let%test_unit _ =
        can_generate (fun t -> Map.existsi t ~f:(fun ~key ~data:_ -> Key.to_int key >= 0))
      ;;

      let%test_unit _ =
        can_generate (fun t -> Map.existsi t ~f:(fun ~key ~data:_ -> Key.to_int key < 0))
      ;;
    end)
  ;;

  let quickcheck_observer _ _ = assert false

  let%test_module _ =
    (module struct
      open Quickcheck

      module Int_char_map = struct
        type t = (int, char) Map.t [@@deriving sexp_of, compare]

        let hash_fold_t hash t = [%hash_fold: (Key.t * char) list] hash (Map.to_alist t)
        let hash t = hash_fold_t (Hash.alloc ()) t |> Hash.get_hash_value
      end

      module F =
        Fn_for_testing.Make (Int_char_map) (Int)
          (struct
            let examples =
              []
              :: List.mapi Key.samples ~f:(fun i key -> key, Char.of_int_exn i)
              :: List.mapi Key.samples ~f:(fun i key -> [ key, Char.of_int_exn i ])
              |> List.map ~f:Map.of_alist_exn
            ;;
          end)

      let sexp_of = [%sexp_of: F.t]
      let compare = [%compare: F.t]

      let quickcheck_generator =
        (* memoizing these functions makes [test_no_duplicates] run much faster *)
        let hashable = Hashtbl_intf.Hashable.of_key (module Int_char_map) in
        Generator.(
          fn
            (Map.quickcheck_observer Key.quickcheck_observer Char.quickcheck_observer)
            Int.quickcheck_generator
          |> map ~f:(fun f -> Memo.general f ~hashable))
      ;;

      let can_generate ?trials f =
        test_can_generate quickcheck_generator ?trials ~sexp_of ~f
      ;;

      let%test_unit (_ [@tags "no-js"]) =
        test_distinct_values
          quickcheck_generator
          ~sexp_of
          ~compare
          ~trials:1_000
          ~distinct_values:500
      ;;

      let%test_unit _ =
        can_generate (fun f -> f (Map.singleton (Key.of_int 0) 'a') <> f (Map.empty ()))
      ;;

      let%test_unit _ =
        can_generate (fun f ->
          f (Map.singleton (Key.of_int 0) 'a') <> f (Map.singleton (Key.of_int 1) 'a'))
      ;;

      let%test_unit _ =
        can_generate (fun f ->
          f (Map.singleton (Key.of_int 0) 'a') <> f (Map.singleton (Key.of_int 0) 'b'))
      ;;
    end)
  ;;

  let quickcheck_shrinker _ _ = assert false

  let%test_module _ =
    (module struct
      open Quickcheck

      let key_shrinker =
        Shrinker.create (fun t ->
          let n = Key.to_int t in
          if Int.( <= ) n 0
          then Sequence.empty
          else Sequence.singleton (Key.of_int (Int.pred n)))
      ;;

      let quickcheck_shrinker =
        Map.quickcheck_shrinker key_shrinker String.quickcheck_shrinker
      ;;

      let normalize_alist alist =
        List.sort alist ~compare:(fun (k1, _) (k2, _) -> Int.compare k1 k2)
      ;;

      let normalize_alists alists =
        List.map alists ~f:normalize_alist
        |> List.sort ~compare:[%compare: (int * string) list]
      ;;

      let map_of_alist alist =
        List.map alist ~f:(fun (k, v) -> Key.of_int k, v) |> Map.of_alist_exn
      ;;

      let alist_of_map t = Map.to_alist t |> List.map ~f:(fun (k, v) -> Key.to_int k, v)

      let test alist shrunk_alists =
        [%test_result: (int * string) list list]
          (map_of_alist alist
           |> Shrinker.shrink quickcheck_shrinker
           |> Sequence.to_list
           |> List.map ~f:alist_of_map
           |> normalize_alists)
          ~expect:(normalize_alists shrunk_alists)
      ;;

      let%test_unit _ = test [] []
      let%test_unit _ = test [ 0, "" ] [ [] ]
      let%test_unit _ = test [ 1, "" ] [ []; [ 0, "" ] ]
      let%test_unit _ = test [ 0, "a" ] [ []; [ 0, "" ] ]
      let%test_unit _ = test [ 1, "a" ] [ []; [ 0, "a" ]; [ 1, "" ] ]

      let%test_unit _ =
        test
          [ 0, "a"; 1, "b" ]
          [ [ 0, "a" ]; [ 1, "b" ]; [ 0, ""; 1, "b" ]; [ 0, "a"; 1, "" ] ]
      ;;
    end)
  ;;
end

module Key_int = struct
  type 'a t = int [@@deriving sexp, compare, hash]

  let of_int = Fn.id
  let to_int = Fn.id
end

module Key_poly = struct
  type 'a t = 'a [@@deriving sexp, compare, hash]

  let of_int = Fn.id
  let to_int = Fn.id
end

module Create_options_with_comparator = struct
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) With_comparator.t

  let simplify_creator f = f ~comparator:Int.comparator
end

module Create_options_without_comparator = struct
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) Without_comparator.t

  let simplify_creator = Fn.id
end

module Create_options_with_first_class_module = struct
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) With_first_class_module.t

  let simplify_creator f =
    f (module Int : Comparator.S with type t = _ and type comparator_witness = _)
  ;;
end

module Access_options_without_comparator = struct
  type ('a, 'b, 'c) access_options = ('a, 'b, 'c) Without_comparator.t

  let simplify_accessor = Fn.id
end

module Access_options_with_comparator = struct
  type ('a, 'b, 'c) access_options = ('a, 'b, 'c) With_comparator.t

  let simplify_accessor f = f ~comparator:Int.comparator
end

let%test_module "Map" =
  (module Unit_tests
            (Key_poly)
            (struct
              include Map

              type ('a, 'b, 'c) t_ = ('a, 'b, 'c) t
              type ('a, 'b, 'c) tree = ('a, 'b, 'c) Tree.t
              type 'cmp cmp = 'cmp

              include Create_options_with_first_class_module
              include Access_options_without_comparator

              let kind = `Map
            end))
;;

let%test_module "Map.Poly" =
  (module Unit_tests
            (Key_poly)
            (struct
              include Map.Poly

              type ('a, 'b, 'c) t_ = ('a, 'b) t
              type ('a, 'b, 'c) tree = ('a, 'b) Tree.t
              type 'cmp cmp = comparator_witness

              include Create_options_without_comparator
              include Access_options_without_comparator

              let kind = `Map
            end))
;;

let%test_module "Int.Map" =
  (module Unit_tests
            (Key_int)
            (struct
              include Int.Map
              module Tree = Map.Make_tree (Int.Map.Key)

              type ('a, 'b, 'c) t_ = 'b t
              type ('a, 'b, 'c) tree = 'b Tree.t
              type 'cmp cmp = Int.comparator_witness

              include Create_options_without_comparator
              include Access_options_without_comparator

              let kind = `Map

              include (
                Core.Map :
                  Map_intf.Accessors_generic
                    with type ('k, 'v, 'c) t := 'v t
                    with type ('k, 'v, 'c) tree := 'v Tree.t
                    with type 'k key := int
                    with type 'c cmp := Int.comparator_witness
                    with type ('k, 'v, 'c) access_options :=
                      ('k, 'v, 'c) Without_comparator.t)
            end))
;;

let%test_module "Map.Tree" =
  (module Unit_tests
            (Key_poly)
            (struct
              include Map.Tree

              type ('a, 'b, 'c) t_ = ('a, 'b, 'c) t
              type ('a, 'b, 'c) tree = ('a, 'b, 'c) t
              type 'cmp cmp = 'cmp

              include Create_options_with_comparator
              include Access_options_with_comparator

              let kind = `Tree
            end))
;;

let%test_module "Map.Poly.Tree" =
  (module Unit_tests
            (Key_poly)
            (struct
              include Map.Poly.Tree

              type ('a, 'b, 'c) t_ = ('a, 'b) t
              type ('a, 'b, 'c) tree = ('a, 'b) t
              type 'cmp cmp = comparator_witness

              include Create_options_without_comparator
              include Access_options_without_comparator

              let kind = `Tree
            end))
;;

let%test_module "Int.Map.Tree" =
  (module Unit_tests
            (Key_int)
            (struct
              include Map.Make_tree (Int.Map.Key)

              type ('a, 'b, 'c) t_ = 'b t
              type ('a, 'b, 'c) tree = 'b t
              type 'cmp cmp = Int.comparator_witness

              include Create_options_without_comparator
              include Access_options_without_comparator

              let kind = `Tree
            end))
;;

let%test_unit _ = Core.Map.find_or_error (Int.Map.singleton 1 ()) 1 |> Or_error.ok_exn

let%expect_test _ =
  Core.Map.find_or_error Int.Map.empty 1 |> printf !"%{sexp: unit Or_error.t}";
  [%expect {| (Error ("key not found" 1)) |}]
;;

let%expect_test _ =
  Core.Map.find_or_error Map.Poly.empty 1 |> printf !"%{sexp: unit Or_error.t}";
  [%expect {| (Error ("key not found" _)) |}]
;;
