(* This module defines a functor, [Unit_tests], that does unit tests on a generic map,
   and then instantiates that functor to create unit tests for [Map], [Map.Poly], and
   [Int.Map]. *)




module Caml_map = Map
open Std

open Core_map_intf

module Unit_tests
  (Key : sig
    type 'a t with sexp

    val of_int : int -> int t
    val to_int : int t -> int
  end)
  (Map : sig
    type ('a, 'b, 'c) t_
    type ('a, 'b, 'c) tree

    type ('a, 'b, 'c) create_options

    include Creators_generic
      with type ('a, 'b, 'c) t       := ('a, 'b, 'c) t_
      with type ('a, 'b, 'c) tree    := ('a, 'b, 'c) tree
      with type 'a key               := 'a Key.t
      with type ('a, 'b, 'c) options := ('a, 'b, 'c) create_options

    val simplify_creator : (int, Int.comparator, 'c) create_options -> 'c

    type ('a, 'b, 'c) access_options

    include Accessors_generic
      with type ('a, 'b, 'c) t       := ('a, 'b, 'c) t_
      with type ('a, 'b, 'c) tree    := ('a, 'b, 'c) tree
      with type 'a key               := 'a Key.t
      with type ('a, 'b, 'c) options := ('a, 'b, 'c) access_options

    val simplify_accessor : (int, Int.comparator, 'c) access_options -> 'c

    val kind : [ `Map | `Tree ]
  end)
  (* The result signature doesn't actually mean anything -- the values are required so
     that implementors are reminded to add a unit test for each one. *)
  : Creators_and_accessors_generic = struct
  module Map = struct
    include Map
    let add                  = simplify_accessor add
    let add_multi            = simplify_accessor add_multi
    let change               = simplify_accessor change
    let find                 = simplify_accessor find
    let find_exn             = simplify_accessor find_exn
    let invariants           = simplify_accessor invariants
    let remove               = simplify_accessor remove
    let mem                  = simplify_accessor mem
    let filter               = simplify_accessor filter
    let filter_map           = simplify_accessor filter_map
    let filter_mapi          = simplify_accessor filter_mapi
    let compare_direct       = simplify_accessor compare_direct
    let equal                = simplify_accessor equal
    let iter2                = simplify_accessor iter2
    let symmetric_diff       = simplify_accessor symmetric_diff
    let merge                = simplify_accessor merge
    let fold_range_inclusive = simplify_accessor fold_range_inclusive
    let range_to_alist       = simplify_accessor range_to_alist
    let prev_key             = simplify_accessor prev_key
    let next_key             = simplify_accessor next_key
    let rank                 = simplify_accessor rank

    let empty ()                  = simplify_creator empty
    let singleton                 = simplify_creator singleton
    let of_sorted_array_unchecked = simplify_creator of_sorted_array_unchecked
    let of_sorted_array           = simplify_creator of_sorted_array
    let of_alist                  = simplify_creator of_alist
    let of_alist_exn              = simplify_creator of_alist_exn
    let of_alist_multi            = simplify_creator of_alist_multi
    let of_alist_fold             = simplify_creator of_alist_fold
    let of_tree                   = simplify_creator of_tree
  end

  type ('a, 'b, 'c) t = Unit_test_follows
  type ('a, 'b, 'c) tree = ('a, 'b, 'c) t
  type 'a key
  type ('a, 'b, 'c) options = ('a, 'b, 'c) Without_comparator.t

  module Key = struct
    open Key
    let of_int = of_int
    let to_int = to_int

    module T = struct
      type t = int Key.t with sexp
      let compare t t' = Pervasives.compare (to_int t) (to_int t')
      let equal t t' = compare t t' = 0
    end
    include T

    let sample = of_int 0
    let samples = List.dedup ~compare (List.init 10 ~f:(fun i -> of_int (i + 1)))
  end

  module Caml_map = Caml_map.Make (Key)

  let random_alist keys =
    List.rev (List.fold keys ~init:[] ~f:(fun l key -> (key, Random.int 1000) :: l))

  let random_map keys =
    List.fold (random_alist keys) ~init:(Map.empty ())
      ~f:(fun map (key, data) -> Map.add ~key ~data map)

  let caml_map_of_alist alist =
    List.fold alist ~init:Caml_map.empty
      ~f:(fun map (key, data) -> Caml_map.add key data map)

  let alist_equal l1 l2 =
    List.equal l1 l2 ~equal:(fun (k1, d1) (k2, d2) -> Key.equal k1 k2 && d1 = d2)

  let caml_map_to_alist map =
    List.rev (Caml_map.fold (fun key data l -> (key, data) :: l) map [])

  (* relies on correctness of Map.to_alist *)
  let equal_maps ~caml_map map =
    alist_equal (Map.to_alist map) (caml_map_to_alist caml_map)

  let add     _ = assert false
  let remove  _ = assert false
  let find    _ = assert false
  let mem     _ = assert false
  let iter    _ = assert false
  let length  _ = assert false
  let map     _ = assert false
  let mapi    _ = assert false
  let fold    _ = assert false
  let equal   _ = assert false
  let compare_direct _ = assert false

  (* runs a series of random tests on a map of the input type and a Caml map to see if
     they have the same behavior *)
  TEST =
    let rec loop n ~prev_caml_map ~caml_map ~prev_core_map ~core_map =
      if n = 0 then equal_maps ~caml_map core_map else
      let remove key =
        Caml_map.remove key caml_map,
        Map.remove core_map key
      in
      let add key =
        let data = Random.int 1000 in
        Caml_map.add key data caml_map,
        Map.add ~key ~data core_map
      in
      let caml_choose caml_map =
        try Some (Caml_map.choose caml_map)
        with _ -> None
      in
      let add_or_remove ~prefer =
        match caml_choose caml_map, prefer with
        | None, _ -> add Key.sample
        | Some (key, _), `Remove -> remove key
        | Some (key, _), `Add ->
          match List.find Key.samples ~f:(fun key -> not (Caml_map.mem key caml_map)) with
          | Some key -> add key
          | None -> remove key
      in
      let old_values = caml_map, core_map in
      let new_caml_map, new_core_map =
        match Random.int 7 with
        | 0 -> add_or_remove ~prefer:`Add
        | 1 -> add_or_remove ~prefer:`Remove
        | 2 ->
          begin match caml_choose caml_map with
          | None ->
            assert (Map.is_empty core_map);
            assert (Map.length core_map = 0)
          | Some (key, data) ->
            assert (Caml_map.find key caml_map = data);
            assert (not (Map.is_empty core_map));
            assert (Map.length core_map = Caml_map.cardinal caml_map);
            assert (Map.mem core_map key);
            assert (Map.find core_map key = Some data)
          end;
          old_values
        | 3 ->
          let target =
            let sum = ref 0 in
            Caml_map.iter (fun _ data -> sum := !sum + data) caml_map;
            !sum
          in
          let actual =
            let sum = ref 0 in
            Map.iter ~f:(fun ~key:_ ~data -> sum := !sum + data) core_map;
            !sum
          in
          assert (target = actual);
          old_values
        | 4 ->
          let caml_map = Caml_map.mapi (fun key data -> (key, data)) caml_map in
          let core_map = Map.mapi ~f:(fun ~key ~data -> (key, data)) core_map in
          let increment = Random.int 1000 in
          let caml_map = Caml_map.map (fun (_, n) -> n + increment) caml_map in
          let core_map = Map.map ~f:(fun (_, n) -> n + increment) core_map in
          assert (equal_maps ~caml_map core_map);
          old_values
        | 5 ->
          let caml_alist =
            Caml_map.fold (fun key data acc -> (key, data) :: acc) caml_map []
          in
          let core_alist =
            Map.fold ~f:(fun ~key ~data acc -> (key, data) :: acc) core_map ~init:[]
          in
          assert (alist_equal caml_alist core_alist);
          old_values
        | 6 ->
          let unchanged = Caml_map.equal (=) prev_caml_map caml_map in
          assert (unchanged = Map.equal (=) prev_core_map core_map);
          assert (unchanged = (Map.compare_direct Int.compare prev_core_map core_map = 0));
          old_values
        | _ -> assert false
      in
      loop (n - 1)
        ~prev_caml_map:caml_map ~caml_map:new_caml_map
        ~prev_core_map:core_map ~core_map:new_core_map
    in
    loop 10000
      ~prev_caml_map:Caml_map.empty ~caml_map:Caml_map.empty
      ~prev_core_map:(Map.empty ()) ~core_map:(Map.empty ())
  ;;

  let iter2 _ = assert false

  TEST_UNIT =
    let test l1 l2 expected =
      let map_of_alist l =
        Map.of_alist_exn (List.map l ~f:(fun (k, v) -> Key.of_int k, v))
      in
      let result = ref [] in
      Map.iter2 (map_of_alist l1) (map_of_alist l2)
        ~f:(fun ~key ~data -> result := (key, data) :: !result);
      let result =
        List.rev_map !result ~f:(fun (k, v) -> Key.to_int k, v)
      in
      assert (result = expected)
    in
    test [] [] [];
    test [0, 10] [] [0, `Left 10];
    test [] [0, 10] [0, `Right 10];
    test [0, 10] [0, 11] [0, `Both (10, 11)];
    test
      [0, 10; 3, 13; 4, 14; 6, 16]
      [1, 11; 3, 13; 4, 14; 5, 15]
      [ 0, `Left 10
      ; 1, `Right 11
      ; 3, `Both (13, 13)
      ; 4, `Both (14, 14)
      ; 5, `Right 15
      ; 6, `Left 16
      ];
  ;;

  let empty = Unit_test_follows

  TEST = equal_maps ~caml_map:Caml_map.empty (Map.empty ())

  let singleton _ = assert false

  TEST =
    equal_maps
      ~caml_map:(Caml_map.add Key.sample 0 Caml_map.empty) (Map.singleton Key.sample 0)
  ;;

  let of_sorted_array _ = assert false

  (* test detection of invalid input *)
  TEST = Map.of_sorted_array [|Key.of_int 0, 0; Key.of_int 0, 0|] |! Result.is_error
  TEST = Map.of_sorted_array [|Key.of_int 1, 0
                         ; Key.of_int 0, 0
                         ; Key.of_int 1, 0|] |! Result.is_error

  let of_sorted_array_unchecked _ = assert false

  (* test it gets same result as [Map.of_alist] *)
  TEST =
    let alist =
      List.sort (random_alist Key.samples) ~cmp:(fun (k1, _) (k2, _) -> Key.compare k1 k2)
    in
    let array = Array.of_list alist in
    let array_rev = Array.of_list (List.rev alist) in
    let map_of_alist = Map.of_alist_exn alist in
    let map_of_array = Map.of_sorted_array_unchecked array in
    let map_of_rev_array = Map.of_sorted_array_unchecked array_rev in
    let map_equal = Map.equal Int.equal in
    map_equal map_of_alist map_of_array &&
      map_equal map_of_alist map_of_rev_array
  ;;

  let invariants _ = assert false

  (* Test constructed AVL tree is valid *)
  TEST_UNIT =
    for n = 0 to 100 do
      let alist = List.init n ~f:(fun i -> Key.of_int i, i) in
      assert (List.permute alist |! Map.of_alist_exn |! Map.invariants);
      assert (Array.of_list alist |! Map.of_sorted_array_unchecked |! Map.invariants);
      assert (List.rev alist |! Array.of_list |! Map.of_sorted_array_unchecked |! Map.invariants);
    done
  ;;

  let of_alist _ = assert false

  TEST =
    let alist = random_alist Key.samples in
    match Map.of_alist alist with
    | `Duplicate_key _ -> false
    | `Ok map -> alist_equal (Map.to_alist map) alist
  ;;

  TEST =
    match Map.of_alist [] with
    | `Ok map -> Map.to_alist map = []
    | `Duplicate_key _ -> false
  ;;

  TEST =
    match Map.of_alist [Key.sample, 0; Key.sample, 1] with
    | `Ok _ -> false
    | `Duplicate_key _ -> true
  ;;

  let of_alist_exn _ = assert false

  TEST =
    try ignore (Map.of_alist_exn [Key.sample, 0; Key.sample, 1]); false
    with _ -> true
  ;;

  let of_alist_fold _ = assert false

  TEST =
    let filtered =
      List.filter Key.samples ~f:(fun key -> not (Key.equal key Key.sample))
    in
    let alist = random_alist filtered in
    let caml_map = Caml_map.add Key.sample 6 (caml_map_of_alist alist) in
    let alist' =
      (Key.sample, 1) :: (Key.sample, 2) :: alist @ [Key.sample, 3]
    in
    let core_map = Map.of_alist_fold ~init:0 ~f:(+) alist' in
    equal_maps ~caml_map core_map
  ;;

  let of_alist_multi _ = assert false

  TEST =
    equal_maps ~caml_map:(Caml_map.add Key.sample [0; 1] Caml_map.empty)
      (Map.of_alist_multi [Key.sample, 0; Key.sample, 1])
  ;;

  let is_empty _ = assert false

  TEST = Map.is_empty (Map.empty ())
  TEST = not (Map.is_empty (Map.singleton Key.sample 0))
  TEST = not (Map.is_empty (random_map Key.samples))

  let of_tree _ = assert false
  let to_tree _ = assert false

  TEST = Map.is_empty (Map.of_tree (Map.to_tree (Map.empty ())))
  TEST =
    let map = random_map Key.samples in
    alist_equal (Map.to_alist map)
      (Map.to_alist (Map.of_tree (Map.to_tree map)))
  ;;

  let add_multi _ = assert false

  TEST =
    let m1 = Map.add_multi (Map.empty ()) ~key:Key.sample ~data:0 in
    let m2 = Map.add_multi m1 ~key:Key.sample ~data:1 in
    equal_maps m2 ~caml_map:(Caml_map.add Key.sample [1; 0] Caml_map.empty)
  ;;

  let change _ = assert false

  TEST =
    let m1 = Map.remove (random_map Key.samples) Key.sample in
    let f = function Some x -> Some (x + 1) | None -> Some 0 in
    let m2 = Map.change m1 Key.sample f in
    let m3 = Map.change m2 Key.sample f in
    match Map.find m3 Key.sample with
    | Some 1 -> true
    | _ -> false
  ;;

  TEST =
    let m1 = Map.add (random_map Key.samples) ~key:Key.sample ~data:0 in
    let m2 = Map.change m1 Key.sample (function
      | Some _ -> None
      | None -> Some 0)
    in
    match Map.find m2 Key.sample with
    | None -> true
    | Some _ -> false
  ;;

  let find_exn _ = assert false

  TEST =
    try ignore (Map.find_exn (Map.empty ()) Key.sample); false
    with Not_found -> true
  ;;

  let fold_right _ = assert false

  TEST =
    let f ~key ~data acc = (key, data) :: acc in
    let map = random_map Key.samples in
    let alist = Map.fold map ~init:[] ~f in
    let right_alist = List.rev (Map.fold_right map ~init:[] ~f) in
    alist_equal right_alist alist
  ;;

  let filter _ = assert false

  TEST =
    let caml_map = Caml_map.add Key.sample 0 Caml_map.empty in
    let core_map =
      Map.filter (Map.add (random_map Key.samples) ~key:Key.sample ~data:0)
        ~f:(fun ~key ~data -> Key.equal key Key.sample && data = 0)
    in
    equal_maps ~caml_map core_map
  ;;

  let filter_map _ = assert false

  TEST =
    let alist = random_alist Key.samples in
    let core_map = Map.add (Map.of_alist_exn alist) ~key:Key.sample ~data:(-1) in
    let core_map =
      Map.filter_map core_map ~f:(fun x -> if x >= 0 then Some (x + 1) else None)
    in
    let caml_map = Caml_map.remove Key.sample (caml_map_of_alist alist) in
    let caml_map = Caml_map.map (fun x -> x + 1) caml_map in
    equal_maps ~caml_map core_map
  ;;

  let filter_mapi _ = assert false

  TEST =
    let base_map = Map.add (random_map Key.samples) ~key:Key.sample ~data:0 in
    let m1 =
      Map.filter_mapi base_map ~f:(fun ~key ~data ->
        if Key.equal key Key.sample && data = 0 then None else Some (data + 1))
    in
    let m2 = Map.map (Map.remove base_map Key.sample) ~f:(fun x -> x + 1) in
    Map.equal (=) m1 m2
  ;;

  let keys     _ = assert false
  let data     _ = assert false
  let to_alist _ = assert false

  TEST =
    let map = Map.of_alist_exn (random_alist Key.samples) in
    let map_keys = Map.keys map in
    let sorted_keys = List.sort map_keys ~cmp:Key.compare in
    List.equal map_keys sorted_keys ~equal:Key.equal
  ;;

  TEST =
    let base_alist = random_alist Key.samples in
    let map = Map.of_alist_exn base_alist in
    let map_keys = Map.keys map in
    let all_keys = List.sort ~cmp:Key.compare Key.samples in
    let map_data = Map.data map in
    let map_alist = Map.to_alist map in
    assert (List.equal map_keys all_keys ~equal:Key.equal);
    assert (alist_equal map_alist base_alist);
    assert (alist_equal (List.zip_exn map_keys map_data) base_alist);
    true
  ;;

  let symmetric_diff _ = assert false

  TEST =
    let m1 = random_map Key.samples in
    Map.symmetric_diff m1 m1 ~data_equal:(=) = []
  ;;

  TEST =
    let key = Key.of_int 7 in
    let m1 = Map.empty () in
    let m1 = Map.add m1 ~key:(Key.of_int 1) ~data:1 in
    (* data must be out of the range of random_map to be a good test *)
    let m2 = Map.add m1 ~key:key ~data:2_000 in
    Map.symmetric_diff m1 m2 ~data_equal:(=) = [(key, `Right 2_000)]
  ;;

  TEST =
    let m1 = random_map Key.samples in
    let m2 =
      List.fold (Map.to_alist m1) ~init:(Map.empty ()) ~f:(fun m (k,d) ->
        Map.add m ~key:k ~data:d)
    in
    Map.symmetric_diff m1 m2 ~data_equal:(=) = []
  ;;


  TEST =
    let key = Key.of_int 20 in
    let m1 = random_map Key.samples in
    (* data must be out of the range of random_map to be a good test *)
    let m2 = Map.add m1 ~key:key ~data:2_000 in
    Map.symmetric_diff m1 m2 ~data_equal:(=) = [(key, `Right 2_000)]
  ;;

  TEST =
    let key = Key.of_int 5 in
    let m1 = random_map Key.samples in
    let m2 = Map.remove m1 key in
    Map.symmetric_diff m1 m2 ~data_equal:(=) = [(key, `Left (Map.find_exn m1 key))]
  ;;

  TEST =
    let key = Key.of_int 7 in
    let m1 = random_map Key.samples in
    let m2 =
      Map.change m1 key (function
        | None -> assert false
        | Some v ->
          assert (v <> 2_000);
          Some 2_000)
    in
    Map.symmetric_diff m1 m2 ~data_equal:(=) =
        [(key, `Unequal (Map.find_exn m1 key, 2000))]
  ;;

  let merge _ = assert false

  TEST =
    let map = random_map Key.samples in
    let added_to_self = Map.merge map map ~f:(fun ~key:_ -> function
      | `Left _ | `Right _ -> assert false
      | `Both (x1, x2) -> Some (x1 + x2))
    in
    let doubled = Map.map map ~f:(fun x -> x * 2) in
    Map.equal (=) added_to_self doubled
  ;;

  TEST =
    let map = random_map Key.samples in
    let map' =
      Map.merge map (Map.empty ()) ~f:(fun ~key:_ x ->
        match x with
        | `Right _ | `Both _ -> assert false
        | `Left x -> Some x)
    in
    Map.equal (=) map map'
  ;;

  TEST =
    let map = random_map Key.samples in
    let map' =
      Map.merge (Map.empty ()) map ~f:(fun ~key:_ x ->
        match x with
        | `Left _ | `Both _ -> assert false
        | `Right x -> Some x)
    in
    Map.equal (=) map map'
  ;;

  TEST =
    let map = random_map Key.samples in
    let map' =
      Map.merge map map ~f:(fun ~key:_ x ->
        match x with
        | `Left _ | `Right _ -> assert false
        | `Both _ -> None)
    in
    Map.is_empty map'
  ;;

  let min_and_max_keys ~init keys =
    List.fold keys ~init:(init, init)
      ~f:(fun (min, max) key ->
        ((if Key.compare key min < 0 then key else min),
         (if Key.compare key max > 0 then key else max)))

  let min_elt     _ = assert false
  let min_elt_exn _ = assert false
  let max_elt     _ = assert false
  let max_elt_exn _ = assert false

  TEST =
    let min_key, max_key = min_and_max_keys ~init:Key.sample Key.samples in
    let map = random_map (Key.sample :: Key.samples) in
    let min_key_element = Map.find_exn map min_key in
    let max_key_element = Map.find_exn map max_key in
    assert (Map.max_elt_exn map =      (max_key, max_key_element));
    assert (Map.max_elt     map = Some (max_key, max_key_element));
    assert (Map.min_elt_exn map =      (min_key, min_key_element));
    assert (Map.min_elt     map = Some (min_key, min_key_element));
    true
  ;;

  TEST = Map.min_elt (Map.empty ()) = None
  TEST = Map.max_elt (Map.empty ()) = None
  TEST = try ignore (Map.min_elt_exn (Map.empty ())); false with _ -> true
  TEST = try ignore (Map.max_elt_exn (Map.empty ())); false with _ -> true

  let for_all _ = assert false
  let exists  _ = assert false

  TEST = Map.for_all (Map.empty ()) ~f:(fun _ -> assert false)
  TEST = not (Map.exists (Map.empty ()) ~f:(fun _ -> assert false))

  TEST =
    let pos x = x >= 0 in
    let neg x = x <  0 in
    let base_map = random_map Key.samples in
    let with_negative = Map.add base_map ~key:Key.sample ~data:(-1) in
    assert (Map.for_all base_map ~f:pos);
    assert (not (Map.for_all with_negative ~f:pos));
    assert (not (Map.exists base_map ~f:neg));
    assert (Map.exists with_negative ~f:neg);
    true
  ;;

  let fold_range_inclusive _ = assert false
  let range_to_alist       _ = assert false
  let prev_key             _ = assert false
  let next_key             _ = assert false
  let rank                 _ = assert false

  TEST =
    let map = random_map (Key.sample :: Key.samples) in
    let min_key, max_key = min_and_max_keys ~init:Key.sample Key.samples in
    let after_min, before_max =
      List.fold Key.samples ~init:(max_key, min_key)
        ~f:(fun (near_min, near_max) key ->
          ((if Key.compare key min_key  > 0
            && Key.compare key near_min < 0 then key else near_min),
           (if Key.compare key max_key  < 0
            && Key.compare key near_max > 0 then key else near_max)))
    in
    let keys_between ~min ~max =
      Map.fold_range_inclusive map ~min ~max ~f:(fun ~key:_ ~data:_ n -> n + 1) ~init:0
    in
    let length = Map.length map in
    (* fold_range_inclusive *)
    assert (keys_between ~min:min_key ~max:max_key = length);
    assert (keys_between ~min:after_min ~max:before_max = length - 2);
    (* prev_key / next_key *)
    assert (Map.prev_key map min_key = None);
    assert (Map.next_key map max_key = None);
    let optional_key_equal key = function
      | None -> false
      | Some (key', _) -> Key.equal key key'
    in
    assert (optional_key_equal min_key (Map.prev_key map after_min));
    assert (optional_key_equal max_key (Map.next_key map before_max));
    (* range_to_alist *)
    assert (alist_equal (Map.range_to_alist ~min:min_key ~max:max_key map)
              (Map.to_alist map));
    assert (alist_equal
      (Map.range_to_alist ~min:after_min ~max:before_max map)
      (Map.to_alist (Map.remove (Map.remove map min_key) max_key)));
    (* rank *)
    assert (Map.rank map min_key    = Some 0);
    assert (Map.rank map after_min  = Some 1);
    assert (Map.rank map before_max = Some (length - 2));
    assert (Map.rank map max_key    = Some (length - 1));
    assert (Map.rank (Map.remove map Key.sample) Key.sample = None);
    true
  ;;

  TEST = Map.prev_key (Map.empty ()) Key.sample = None
  TEST = Map.next_key (Map.empty ()) Key.sample = None

  let validate ~name:_ _ = assert false

  TEST_UNIT =
    let validate expect map =
      expect
        (Validate.result
           (Map.validate
              ~name:(fun key -> Sexp.to_string (<:sexp_of< Key.t >> key))
              (Validate.of_error
                 (fun i ->
                   if i mod 2 = 0 then
                     Ok ()
                   else
                     error "must be even" i <:sexp_of< int >>))
              map))
    in
    let is_ok = Result.is_ok in
    let is_error = Result.is_error in
    assert (validate is_ok    (Map.empty ()));
    assert (validate is_ok    (Map.of_alist_exn [ Key.of_int 0, 0                  ]));
    assert (validate is_error (Map.of_alist_exn [ Key.of_int 0, 1                  ]));
    assert (validate is_ok    (Map.of_alist_exn [ Key.of_int 0, 0; Key.of_int 1, 0 ]));
    assert (validate is_error (Map.of_alist_exn [ Key.of_int 0, 0; Key.of_int 1, 1 ]));
  ;;

  (* Ensure polymorphic equality raises for maps. *)
  TEST_UNIT =
    match Map.kind with
    | `Tree -> ()
    | `Map ->
      let ts = [ Map.empty ();  Map.of_alist_exn [ Key.sample, 13 ] ] in
      List.iter ts ~f:(fun t1 ->
        List.iter ts ~f:(fun t2 ->
          assert (Result.is_error (Result.try_with (fun () ->
            Polymorphic_compare.equal t1 t2)))));
  ;;
end

module Key_int = struct
  type 'a t = int with sexp
  let of_int = Fn.id
  let to_int = Fn.id
end

module Key_poly = struct
  type 'a t = 'a with sexp
  let of_int = Fn.id
  let to_int = Fn.id
end

module Create_options_with_comparator = struct
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) With_comparator.t
  let simplify_creator f = f ~comparator:Core_int.comparator
end

module Create_options_without_comparator = struct
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) Without_comparator.t
  let simplify_creator  = Fn.id
end

module Access_options_without_comparator = struct
  type ('a, 'b, 'c) access_options = ('a, 'b, 'c) Without_comparator.t
  let simplify_accessor = Fn.id
end

module Access_options_with_comparator = struct
  type ('a, 'b, 'c) access_options = ('a, 'b, 'c) With_comparator.t
  let simplify_accessor f = f ~comparator:Core_int.comparator
end

TEST_MODULE "Map" = Unit_tests (Key_poly) (struct
  include Map
  type ('a, 'b, 'c) t_   = ('a, 'b, 'c) t
  type ('a, 'b, 'c) tree = ('a, 'b, 'c) Tree.t
  include Create_options_with_comparator
  include Access_options_without_comparator
  let kind = `Map
end)

TEST_MODULE "Map.Poly" = Unit_tests (Key_poly) (struct
  include Map.Poly
  type ('a, 'b, 'c) t_   = ('a, 'b) t
  type ('a, 'b, 'c) tree = ('a, 'b) Tree.t
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Map
end)

TEST_MODULE "Int.Map" = Unit_tests (Key_int) (struct
  include Int.Map
  type ('a, 'b, 'c) t_   = 'b t
  type ('a, 'b, 'c) tree = 'b Tree.t
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Map
end)

TEST_MODULE "Map.Tree" = Unit_tests (Key_poly) (struct
  include Map.Tree
  type ('a, 'b, 'c) t_   = ('a, 'b, 'c) t
  type ('a, 'b, 'c) tree = ('a, 'b, 'c) t
  include Create_options_with_comparator
  include Access_options_with_comparator
  let kind = `Tree
end)

TEST_MODULE "Map.Poly.Tree" = Unit_tests (Key_poly) (struct
  include Map.Poly.Tree
  type ('a, 'b, 'c) t_   = ('a, 'b) t
  type ('a, 'b, 'c) tree = ('a, 'b) t
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Tree
end)

TEST_MODULE "Int.Map.Tree" = Unit_tests (Key_int) (struct
  include Int.Map.Tree
  type ('a, 'b, 'c) t_   = 'b t
  type ('a, 'b, 'c) tree = 'b t
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Tree
end)
