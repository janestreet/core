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

    include Creators
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Map.tree
      with type 'a key := 'a Key.t

    val simplify_creator : (int, Int.comparator, 'c) create_options -> 'c

    include Accessors
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Map.tree
      with type 'a key := 'a Key.t
  end) : sig
  (* The result signature doesn't actually mean anything -- the values are required so
     that implementors are reminded to add a unit test for each one. *)
  type ('a, 'b, 'c) t

  include Creators
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
  include Accessors
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
    with type 'a key := 'a key
end = struct
  module Map = struct
    include Map
    let empty ()       = simplify_creator empty
    let singleton      = simplify_creator singleton
    let of_alist       = simplify_creator of_alist
    let of_alist_exn   = simplify_creator of_alist_exn
    let of_alist_multi = simplify_creator of_alist_multi
    let of_alist_fold  = simplify_creator of_alist_fold
    let of_tree        = simplify_creator of_tree
  end

  type ('a, 'b, 'c) t = Unit_test_follows
  type ('a, 'b, 'c) tree = ('a, 'b, 'c) t
  type 'a key
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) create_options_without_comparator

  module Key = struct
    open Key
    module T = struct
      type t = int Key.t with sexp
      let compare t t' = Pervasives.compare (to_int t) (to_int t')
    end
    include T

    let sample = of_int 0
    let samples = List.init 10 ~f:(fun i -> of_int (i + 1))

    module Set = Set.Make (T)
  end

  module Caml_map = Caml_map.Make (Key)

  let sample_keys =
    List.fold Key.samples ~init:Key.Set.empty ~f:(fun set key -> Set.add set key)
  ;;

  let sample_alist =
    List.rev (List.fold Key.samples ~init:[]
                ~f:(fun l key -> (key, Random.int 1000) :: l))
  ;;

  let sample_map =
    List.fold sample_alist ~init:(Map.empty ())
      ~f:(fun map (key, data) -> Map.add ~key ~data map)
  ;;

  let sample_caml_map =
    List.fold sample_alist ~init:Caml_map.empty
      ~f:(fun map (key, data) -> Caml_map.add key data map)
  ;;

  let alist_equal l1 l2 =
    List.equal l1 l2 ~equal:(fun (k1, d1) (k2, d2) -> Key.compare k1 k2 = 0 && d1 = d2)
  ;;

  let caml_to_alist map =
    List.rev (Caml_map.fold (fun key data l -> (key, data) :: l) map [])
  ;;

  (* relies on correctness of Map.to_alist *)
  let equal_maps ~caml_map map = alist_equal (Map.to_alist map) (caml_to_alist caml_map)

  let add     _ = assert false
  let remove  _ = assert false
  let find    _ = assert false
  let mem     _ = assert false
  let iter    _ = assert false
  let map     _ = assert false
  let mapi    _ = assert false
  let fold    _ = assert false
  let equal   _ = assert false
  let compare _ = assert false

  (* runs a series of random tests on a map of the input type and a Caml map to see if
     they have the same behavior *)
  TEST =
    let rec loop n (old_caml, caml_map) (old_other, other_map) ~included ~excluded =
      if n = 0 then equal_maps ~caml_map other_map else
      let remove key =
        Caml_map.remove key caml_map,
        Map.remove other_map key,
        Set.remove included key,
        Set.add excluded key
      in
      let add key =
        let data = Random.int 1000 in
        Caml_map.add key data caml_map,
        Map.add ~key ~data other_map,
        Set.add included key,
        Set.remove excluded key
      in
      let old_values = caml_map, other_map, included, excluded in
      let if_not_empty set action =
        if not (Set.is_empty set) then
          action (Set.choose_exn set)
        else
          old_values
      in
      let new_caml_map, new_other_map, included, excluded =
        match Random.int 10 with
        | 0 -> if_not_empty excluded add
        | 1 -> if_not_empty excluded remove
        | 2 -> if_not_empty included add
        | 3 -> if_not_empty included remove
        | 4 ->
          if not (Set.is_empty included) then begin
            let key = Set.choose_exn included in
            assert (Map.mem other_map key);
            assert (Caml_map.find key caml_map = Map.find_exn other_map key)
          end;
          old_values
        | 5 ->
          if not (Set.is_empty excluded) then begin
            let key = Set.choose_exn excluded in
            assert (Map.find other_map key = None);
            assert (not (Caml_map.mem key caml_map) && not (Map.mem other_map key))
          end;
          old_values
        | 6 ->
          let target =
            let sum = ref 0 in
            Caml_map.iter (fun _ data -> sum := !sum + data) caml_map;
            !sum
          in
          let actual =
            let sum = ref 0 in
            Map.iter ~f:(fun ~key:_ ~data -> sum := !sum + data) other_map;
            !sum
          in
          assert (target = actual);
          old_values
        | 7 ->
          let caml_map  = Caml_map.mapi (fun key data -> (key, data)) caml_map in
          let other_map = Map.mapi ~f:(fun ~key ~data -> (key, data)) other_map in
          let increment = Random.int 1000 in
          let caml_map  = Caml_map.map (fun (_, n) -> n + increment) caml_map in
          let other_map = Map.map ~f:(fun (_, n) -> n + increment) other_map in
          assert (equal_maps ~caml_map other_map);
          caml_map, other_map, included, excluded
        | 8 ->
          let caml_alist =
            Caml_map.fold (fun key data acc -> (key, data) :: acc) caml_map []
          in
          let other_alist =
            Map.fold ~f:(fun ~key ~data acc -> (key, data) :: acc) other_map ~init:[]
          in
          assert (alist_equal caml_alist other_alist);
          old_values
        | 9 ->
          let unchanged = Caml_map.equal (=) old_caml caml_map in
          assert (unchanged = Map.equal (=) old_other other_map);
          assert (unchanged = (Map.compare Int.compare old_other other_map = 0));
          old_values
        | _ -> assert false
      in
      loop (n - 1) (caml_map, new_caml_map) (other_map, new_other_map) ~included ~excluded
    in
    loop 10000 (Caml_map.empty, Caml_map.empty) (Map.empty (), Map.empty ())
      ~included:Key.Set.empty ~excluded:sample_keys
  ;;

  let empty = Unit_test_follows

  TEST = equal_maps ~caml_map:Caml_map.empty (Map.empty ())

  let singleton _ = assert false

  TEST =
    equal_maps
      ~caml_map:(Caml_map.add Key.sample 0 Caml_map.empty) (Map.singleton Key.sample 0)
  ;;

  let of_alist _ = assert false

  TEST =
    let alist =
      Set.fold sample_keys ~init:[] ~f:(fun l key -> l @ [key, Random.int 1000])
    in
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
    let caml_map = Caml_map.add Key.sample 6 sample_caml_map in
    let alist =
      (Key.sample, 1) :: (Key.sample, 2)
      :: Map.to_alist (Map.remove sample_map Key.sample)
      @ [Key.sample, 3]
    in
    let other_map = Map.of_alist_fold ~init:0 ~f:(+) alist in
    equal_maps ~caml_map other_map
  ;;

  let of_alist_multi _ = assert false

  TEST =
    equal_maps ~caml_map:(Caml_map.add Key.sample [0; 1] Caml_map.empty)
      (Map.of_alist_multi [Key.sample, 0; Key.sample, 1])
  ;;

  let is_empty _ = assert false

  TEST = Map.is_empty (Map.empty ())
  TEST = not (Map.is_empty (Map.singleton Key.sample 0))
  TEST = not (Map.is_empty sample_map)

  let of_tree _ = assert false
  let to_tree _ = assert false

  TEST = Map.is_empty (Map.of_tree (Map.to_tree (Map.empty ())))

  TEST =
    alist_equal
      (Map.to_alist sample_map)
      (Map.to_alist (Map.of_tree (Map.to_tree sample_map)))
  ;;

  let length _ = assert false

  TEST = Map.length (Map.empty ()) = 0
  TEST = Map.length (Map.singleton Key.sample 0) = 1
  TEST = Map.length sample_map = Set.length sample_keys

  let add_multi _ = assert false

  TEST =
    let m1 = Map.add_multi (Map.empty ()) ~key:Key.sample ~data:0 in
    let m2 = Map.add_multi m1 ~key:Key.sample ~data:1 in
    equal_maps m2 ~caml_map:(Caml_map.add Key.sample [1; 0] Caml_map.empty)
  ;;

  let change _ = assert false

  TEST =
    let m1 = Map.remove sample_map Key.sample in
    let f = function Some x -> Some (x + 1) | None -> Some 0 in
    let m2 = Map.change m1 Key.sample f in
    let m3 = Map.change m2 Key.sample f in
    match Map.find m3 Key.sample with
    | Some 1 -> true
    | _ -> false
  ;;

  TEST =
    let m1 = Map.add sample_map ~key:Key.sample ~data:0 in
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
    let alist = Map.fold sample_map ~init:[] ~f in
    let right_alist = List.rev (Map.fold_right sample_map ~init:[] ~f) in
    alist_equal right_alist alist
  ;;

  let filter _ = assert false

  TEST =
    equal_maps ~caml_map:(Caml_map.add Key.sample 0 Caml_map.empty)
      (Map.filter (Map.add sample_map ~key:Key.sample ~data:0)
        ~f:(fun ~key ~data -> Key.compare key Key.sample = 0 && data = 0))
  ;;

  let filter_map _ = assert false

  TEST =
    let m = Map.add sample_map ~key:Key.sample ~data:(-1) in
    let m' = Map.filter_map m ~f:(fun x -> if x >= 0 then Some (x + 1) else None) in
    let caml_map =
      Caml_map.map (fun x -> x + 1) (Caml_map.remove Key.sample sample_caml_map)
    in
    equal_maps ~caml_map m'
  ;;

  let filter_mapi _ = assert false

  TEST =
    let m = Map.add sample_map ~key:Key.sample ~data:0 in
    let m1 = Map.filter_mapi m ~f:(fun ~key ~data ->
      if key = Key.sample && data = 0 then None else Some (data + 1))
    in
    let m2 = Map.map (Map.remove sample_map Key.sample) ~f:(fun x -> x + 1) in
    Map.equal Pervasives.(=) m1 m2
  ;;

  let keys     _ = assert false
  let data     _ = assert false
  let to_alist _ = assert false

  TEST =
    let map_keys  = Map.keys sample_map in
    let all_keys  = Set.elements sample_keys in
    let map_data  = Map.data sample_map in
    let map_alist = Map.to_alist sample_map in
    List.equal map_keys all_keys ~equal:(fun k1 k2 -> Key.compare k1 k2 = 0)
    && alist_equal map_alist sample_alist
    && alist_equal (List.zip_exn map_keys map_data) sample_alist
  ;;

  let merge _ = assert false

  TEST =
    let added_to_self = Map.merge sample_map sample_map ~f:(fun ~key:_ -> function
      | `Left _ | `Right _ -> assert false
      | `Both (x1, x2) -> Some (x1 + x2))
    in
    let doubled = Map.map sample_map ~f:(fun x -> x * 2) in
    Map.equal (=) added_to_self doubled
  ;;

  let sample_map_with_sample_key =
    Map.add sample_map ~key:Key.sample ~data:(Random.int 1000)

  (* This computes {min,max}_key for Key.samples with Key.sample added to it. *)
  let min_key, max_key =
    List.fold Key.samples ~init:(Key.sample, Key.sample)
      ~f:(fun (min, max) key ->
        ((if Key.compare key min < 0 then key else min),
         (if Key.compare key max > 0 then key else max)))

  let min_elt     _ = assert false
  let min_elt_exn _ = assert false
  let max_elt     _ = assert false
  let max_elt_exn _ = assert false

  TEST =
    let map = sample_map_with_sample_key in
    let min_key_element = Map.find_exn map min_key in
    let max_key_element = Map.find_exn map max_key in
       Map.max_elt_exn map =      (max_key, max_key_element)
    && Map.max_elt     map = Some (max_key, max_key_element)
    && Map.min_elt_exn map =      (min_key, min_key_element)
    && Map.min_elt     map = Some (min_key, min_key_element)
  ;;

  TEST =
    Map.min_elt (Map.empty ()) = None && Map.max_elt (Map.empty ()) = None
    && (try ignore (Map.min_elt_exn (Map.empty ())); false with _ -> true)
    && (try ignore (Map.max_elt_exn (Map.empty ())); false with _ -> true)
  ;;

  let for_all _ = assert false
  let exists  _ = assert false

  TEST = Map.for_all (Map.empty ()) ~f:(fun _ -> assert false)

  TEST = not (Map.exists (Map.empty ()) ~f:(fun _ -> assert false))

  TEST =
    let pos x = x >= 0 in
    let neg x = x <  0 in
    let with_negative = Map.add sample_map ~key:Key.sample ~data:(-1) in
    Map.for_all sample_map ~f:pos
    && not (Map.for_all with_negative ~f:pos)
    && Map.for_all (Map.empty ()) ~f:(fun _ -> false)
    && not (Map.exists sample_map ~f:neg)
    && Map.exists with_negative ~f:neg
    && not (Map.exists (Map.empty ()) ~f:(fun _ -> true))
  ;;

  let fold_range_inclusive _ = assert false
  let range_to_alist       _ = assert false
  let prev_key             _ = assert false
  let next_key             _ = assert false
  let rank                 _ = assert false

  TEST =
    let map = sample_map_with_sample_key in
    let after_min, before_max =
      List.fold Key.samples ~init:(max_key, min_key)
        ~f:(fun (near_min, near_max) key ->
          ((if Key.compare key min_key  > 0
            && Key.compare key near_min < 0 then key else near_min),
           (if Key.compare key max_key  < 0
            && Key.compare key near_max > 0 then key else near_max)))
    in
    let optional_key_equal key = function
      | None -> false
      | Some (k, _) -> Key.compare key k = 0
    in
    let keys_between ~min ~max =
      Map.fold_range_inclusive map ~min ~max ~f:(fun ~key:_ ~data:_ n -> n + 1) ~init:0
    in
    let length = Map.length map in
    (* fold_range_inclusive *)
    keys_between ~min:min_key ~max:max_key = length
    && keys_between ~min:after_min ~max:before_max = length - 2
    (* prev_key / next_key *)
    && Map.prev_key map min_key = None
    && Map.next_key map max_key = None
    && optional_key_equal min_key (Map.prev_key map after_min)
    && optional_key_equal max_key (Map.next_key map before_max)
    (* range_to_alist *)
    && alist_equal (Map.range_to_alist ~min:min_key ~max:max_key map) (Map.to_alist map)
    && alist_equal
      (Map.range_to_alist ~min:after_min ~max:before_max map)
      (Map.to_alist (Map.remove (Map.remove map min_key) max_key))
    (* rank *)
    && Map.rank map min_key    = Some 0
    && Map.rank map after_min  = Some 1
    && Map.rank map before_max = Some (length - 2)
    && Map.rank map max_key    = Some (length - 1)
    && Map.rank (Map.remove map Key.sample) Key.sample = None
  ;;

  TEST =
       Map.prev_key (Map.empty ()) Key.sample = None
    && Map.next_key (Map.empty ()) Key.sample = None
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

TEST_MODULE "Map" = Unit_tests (Key_poly) (struct
  include Map

  type ('a, 'b, 'c) t_ = ('a, 'b, 'c) t

  let simplify_creator f = f ~comparator:Core_int.comparator
end)

TEST_MODULE "Map.Poly" = Unit_tests (Key_poly) (struct
  include Map.Poly

  type ('a, 'b, 'c) tree = ('a, 'b, 'c) Map.tree

  let simplify_creator = Fn.id
end)

TEST_MODULE "Int.Map" = Unit_tests (Key_int) (struct
  include Int.Map
  let simplify_creator = Fn.id
end)
