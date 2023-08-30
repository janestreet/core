open! Core
include Hashtbl_unit_tests_intf

(* Demonstrate that [Hashtbl_for_testing] contains all the parts of [Hashtbl_intf.Hashtbl]
   that need to be tested. *)
module _ (Hashtbl : Hashtbl_intf.Hashtbl) = struct
  let%test_module "tests are exhaustive" =
    (module (
               struct
                 (* Include [Hashtbl_for_testing]. *)
                 include (
                   Hashtbl : Hashtbl_for_testing with type ('a, 'b) t = ('a, 'b) Hashtbl.t)

                 (* Now add what remains from [Hashtbl_intf.Hashtbl]. *)

                 (* type definition *)
                 type 'a key = 'a

                 (* module type aliases *)
                 module type For_deriving = Hashtbl.For_deriving
                 module type Key = Hashtbl.Key
                 module type Key_binable = Hashtbl.Key_binable
                 module type Key_stable = Hashtbl.Key_stable
                 module type Key_plain = Hashtbl.Key_plain
                 module type S = Hashtbl.S
                 module type S_binable = Hashtbl.S_binable
                 module type S_stable = Hashtbl.S_stable
                 module type S_plain = Hashtbl.S_plain

                 (* module aliases *)
                 module Hashable = Hashtbl.Hashable
                 module M = Hashtbl.M
                 module Make = Hashtbl.Make
                 module Make_binable = Hashtbl.Make_binable
                 module Make_binable_with_hashable = Hashtbl.Make_binable_with_hashable
                 module Make_stable = Hashtbl.Make_stable
                 module Make_stable_with_hashable = Hashtbl.Make_stable_with_hashable
                 module Make_plain = Hashtbl.Make_plain
                 module Make_plain_with_hashable = Hashtbl.Make_plain_with_hashable
                 module Make_with_hashable = Hashtbl.Make_with_hashable
                 module Merge_into_action = Hashtbl.Merge_into_action
                 module Poly = Hashtbl.Poly
                 module Using_hashable = Hashtbl.Using_hashable

                 (* exports for deriving via [Hashtbl.M(_)] *)
                 include (Hashtbl : For_deriving with type ('a, 'b) t := ('a, 'b) t)

                 (* hash function accessors, not interesting to test *)
                 let hash = Hashtbl.hash
                 let hash_param = Hashtbl.hash_param
                 let hashable = Hashtbl.hashable
                 let hashable_s = Hashtbl.hashable_s
               end :
                 Hashtbl_intf.Hashtbl))
  ;;
end

module Make_quickcheck_comparison_to_Map (Hashtbl : Hashtbl_for_testing) = struct
  let%test_module "quickcheck comparison to Map" =
    (module (
               struct
                 module Qc = Quickcheck
                 module Gen = Qc.Generator
                 open Gen.Monad_infix

                 module type Creators_with_quickcheck_generators = sig
                   include
                     Hashtbl_intf.Creators
                       with type ('a, 'b) t := ('a, 'b) Hashtbl.t
                       with type 'a key := 'a
                       with type ('a, 'b, 'c) create_options :=
                         ('a, 'b, 'c) Hashtbl_intf.create_options_with_first_class_module

                   module For_tests : sig
                     type ('key, 'data) constructor [@@deriving sexp_of]
                     type ('key, 'data) multi_constructor [@@deriving sexp_of]

                     val empty_constructor : (_, _) constructor

                     val constructor_gen
                       :  'key Gen.t
                       -> 'data Gen.t
                       -> compare_keys:('key -> 'key -> int)
                       -> ('key, 'data) constructor Gen.t

                     val multi_constructor_gen
                       :  'key Gen.t
                       -> 'data Gen.t
                       -> compare_keys:('key -> 'key -> int)
                       -> ('key, 'data) multi_constructor Gen.t

                     val map_and_table
                       :  'key Base.Hashtbl.Key.t
                       -> (module Comparator.S
                             with type t = 'key
                              and type comparator_witness = 'cmp)
                       -> ('key, 'data) constructor
                       -> ('key, 'data, 'cmp) Map.t * ('key, 'data) Hashtbl.t

                     val map_and_table_multi
                       :  'key Base.Hashtbl.Key.t
                       -> (module Comparator.S
                             with type t = 'key
                              and type comparator_witness = 'cmp)
                       -> ('key, 'data) multi_constructor
                       -> ('key, 'data list, 'cmp) Map.t * ('key, 'data list) Hashtbl.t
                   end
                 end

                 module type Accessors_with_unit_tests = sig
                   include
                     Hashtbl_intf.Accessors
                       with type ('a, 'b) t := ('a, 'b) Hashtbl.t
                       with type 'a key := 'a

                   include
                     Hashtbl_intf.Multi
                       with type ('a, 'b) t := ('a, 'b) Hashtbl.t
                       with type 'a key := 'a

                   val invariant
                     :  'a Invariant.t
                     -> 'b Invariant.t
                     -> ('a, 'b) Hashtbl.t Invariant.t
                 end

                 include (
                   struct
                     let alist_gen key_gen data_gen ~unique_keys =
                       match unique_keys with
                       | None -> List.quickcheck_generator (Gen.tuple2 key_gen data_gen)
                       | Some compare ->
                         List.quickcheck_generator key_gen
                         >>= fun keys ->
                         let keys = List.dedup_and_sort keys ~compare in
                         List.gen_with_length (List.length keys) data_gen
                         >>| fun data -> List.zip_exn keys data
                     ;;

                     let create ?growth_allowed ?size m () =
                       Hashtbl.create ?growth_allowed ?size m
                     ;;

                     let create_gen () = Gen.singleton `Create
                     let of_alist = Hashtbl.of_alist
                     let of_alist_exn = Hashtbl.of_alist_exn
                     let of_alist_or_error = Hashtbl.of_alist_or_error
                     let of_alist_report_all_dups = Hashtbl.of_alist_report_all_dups

                     let of_alist_gen key_gen data_gen ~compare_keys =
                       alist_gen key_gen data_gen ~unique_keys:(Some compare_keys)
                       >>| fun alist -> `Of_alist alist
                     ;;

                     let of_alist_multi = Hashtbl.of_alist_multi

                     let of_alist_multi_gen key_gen data_gen =
                       alist_gen key_gen data_gen ~unique_keys:None
                       >>| fun alist -> `Of_alist_multi alist
                     ;;

                     let create_with_key = Hashtbl.create_with_key
                     let create_with_key_exn = Hashtbl.create_with_key_exn
                     let create_with_key_or_error = Hashtbl.create_with_key_or_error

                     let create_with_key_gen key_gen data_gen ~compare_keys =
                       alist_gen key_gen data_gen ~unique_keys:(Some compare_keys)
                       >>| fun alist -> `Create_with_key alist
                     ;;

                     let create_mapped = Hashtbl.create_mapped

                     let create_mapped_gen key_gen data_gen ~compare_keys =
                       alist_gen key_gen data_gen ~unique_keys:(Some compare_keys)
                       >>| fun alist -> `Create_mapped alist
                     ;;

                     let group = Hashtbl.group

                     let group_gen key_gen data_gen =
                       alist_gen key_gen data_gen ~unique_keys:None
                       >>| fun alist -> `Group alist
                     ;;

                     module For_tests = struct
                       type ('key, 'data) constructor =
                         [ `Create
                         | `Of_alist of ('key * 'data) list
                         | `Create_mapped of ('key * 'data) list
                         | `Create_with_key of ('key * 'data) list
                         ]
                       [@@deriving sexp_of]

                       type ('key, 'data) multi_constructor =
                         [ ('key, 'data list) constructor
                         | `Of_alist_multi of ('key * 'data) list
                         | `Group of ('key * 'data) list
                         ]
                       [@@deriving sexp_of]

                       let empty_constructor = `Create

                       let constructor_gen key_gen data_gen ~compare_keys =
                         Gen.union
                           [ create_gen ()
                           ; of_alist_gen key_gen data_gen ~compare_keys
                           ; create_mapped_gen key_gen data_gen ~compare_keys
                           ; create_with_key_gen key_gen data_gen ~compare_keys
                           ]
                       ;;

                       let multi_constructor_gen key_gen data_gen ~compare_keys =
                         Gen.union
                           [ constructor_gen
                               key_gen
                               (List.quickcheck_generator data_gen)
                               ~compare_keys
                           ; of_alist_multi_gen key_gen data_gen
                           ; group_gen key_gen data_gen
                           ]
                       ;;

                       let map_and_table m_hash m_map = function
                         | `Create -> Map.empty m_map, Hashtbl.create m_hash
                         | `Of_alist alist ->
                           Map.of_alist_exn m_map alist, Hashtbl.of_alist_exn m_hash alist
                         | `Create_with_key alist ->
                           ( Map.of_alist_exn m_map alist
                           , Hashtbl.create_with_key_exn m_hash alist ~get_key:fst
                             |> Hashtbl.map ~f:snd )
                         | `Create_mapped alist ->
                           ( Map.of_alist_exn m_map alist
                           , Hashtbl.create_mapped m_hash alist ~get_key:fst ~get_data:snd
                             |> (function
                             | `Ok table -> table
                             | `Duplicate_keys _ -> assert false) )
                       ;;

                       let map_and_table_multi m_hash m_map = function
                         | `Of_alist_multi alist ->
                           ( Map.of_alist_multi m_map (List.rev alist)
                           , Hashtbl.of_alist_multi m_hash alist )
                         | `Group alist ->
                           ( Map.of_alist_reduce
                               m_map
                               ~f:( @ )
                               (List.map alist ~f:(fun (key, data) -> key, [ data ]))
                           , Hashtbl.group
                               m_hash
                               alist
                               ~combine:( @ )
                               ~get_key:fst
                               ~get_data:(Fn.compose List.return snd) )
                         | (`Create | `Of_alist _ | `Create_with_key _ | `Create_mapped _)
                           as c -> map_and_table m_hash m_map c
                       ;;
                     end
                   end :
                     Creators_with_quickcheck_generators)

                 let create ?growth_allowed ?size m = create ?growth_allowed ?size m ()

                 include (
                   struct
                     module For_tests = struct
                       open For_tests

                       let is_even n = n land 1 = 0

                       module Key = struct
                         include String

                         let quickcheck_generator =
                           Gen.small_non_negative_int >>| sprintf "s%d"
                         ;;

                         let to_bool t =
                           is_empty t || is_even (Char.to_int (get t (length t - 1)))
                         ;;
                       end

                       module Data = struct
                         include Int

                         let quickcheck_generator = Gen.small_non_negative_int
                         let to_bool = is_even

                         (* Here, [succ] is useful for testing [filter_map] *)
                         let to_option t = if is_even t then Some (succ t) else None

                         let to_partition t =
                           if is_even t then First (succ t) else Second (pred t)
                         ;;
                       end

                       module Key_and_data = struct
                         let to_bool ~key ~data =
                           Bool.equal (Key.to_bool key) (Data.to_bool data)
                         ;;

                         let to_data ~key ~data =
                           if Key.to_bool key then succ data else pred data
                         ;;

                         let to_data_option ~key ~data =
                           if Key.to_bool key then Some data else Data.to_option data
                         ;;

                         let to_data_partition ~key ~data =
                           if Key.to_bool key
                           then First (succ data)
                           else Second (pred data)
                         ;;

                         let merge ~key x =
                           if Key.to_bool key
                           then None
                           else (
                             match x with
                             | `Left data -> Some (Data.succ data)
                             | `Right data -> Some (Data.pred data)
                             | `Both (x, y) -> Some (x - y))
                         ;;
                       end

                       let comparator = Key.comparator
                       let compare_keys = Key.compare

                       type nonrec constructor = (Key.t, Data.t) constructor
                       [@@deriving sexp_of]

                       type nonrec multi_constructor = (Key.t, Data.t) multi_constructor
                       [@@deriving sexp_of]

                       let empty_constructor = For_tests.empty_constructor

                       let constructor_gen =
                         constructor_gen
                           Key.quickcheck_generator
                           Data.quickcheck_generator
                           ~compare_keys
                       ;;

                       let multi_constructor_gen =
                         multi_constructor_gen
                           Key.quickcheck_generator
                           Data.quickcheck_generator
                           ~compare_keys
                       ;;

                       let map_and_table = map_and_table (module Key) (module Key)

                       let map_and_table_multi =
                         map_and_table_multi (module Key) (module Key)
                       ;;

                       let to_map t =
                         Hashtbl.to_alist t
                         |> Map.Using_comparator.of_alist_exn ~comparator
                       ;;
                     end

                     open For_tests

                     let invariant = Hashtbl.invariant

                     (* This test is mostly validating constructors, making sure all of them satisfy
            [invariant] and that we have correctly constructed corresponding hash tables
            and maps. *)
                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         Hashtbl.invariant ignore ignore t;
                         [%test_result: Data.t Key.Map.t] (to_map t) ~expect:map)
                     ;;

                     let%test_unit _ =
                       Qc.test
                         multi_constructor_gen
                         ~sexp_of:[%sexp_of: multi_constructor]
                         ~f:(fun multi_constructor ->
                         let map, t = map_and_table_multi multi_constructor in
                         Hashtbl.invariant ignore ignore t;
                         [%test_result: Data.t list Key.Map.t] (to_map t) ~expect:map)
                     ;;

                     let sexp_of_key = Hashtbl.sexp_of_key

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple2 constructor_gen Key.quickcheck_generator)
                         ~sexp_of:[%sexp_of: constructor * Key.t]
                         ~f:(fun (constructor, key) ->
                         let _, t = map_and_table constructor in
                         [%test_result: Sexp.t]
                           (Hashtbl.sexp_of_key t key)
                           ~expect:([%sexp_of: Key.t] key))
                     ;;

                     let clear = Hashtbl.clear

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let _, t = map_and_table constructor in
                         Hashtbl.clear t;
                         Hashtbl.invariant ignore ignore t;
                         [%test_result: Data.t Key.Map.t] (to_map t) ~expect:Key.Map.empty)
                     ;;

                     let copy = Hashtbl.copy

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         let t_copy = Hashtbl.copy t in
                         Hashtbl.clear t;
                         Hashtbl.invariant ignore ignore t_copy;
                         [%test_result: Data.t Key.Map.t] (to_map t_copy) ~expect:map)
                     ;;

                     let fold = Hashtbl.fold

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t]
                           (Hashtbl.fold t ~init:Key.Map.empty ~f:(fun ~key ~data map ->
                              Map.set map ~key ~data))
                           ~expect:map)
                     ;;

                     let iter = Hashtbl.iter

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         let datas = ref [] in
                         Hashtbl.iter t ~f:(fun data -> datas := data :: !datas);
                         [%test_result: Data.t list]
                           (List.sort !datas ~compare:Data.compare)
                           ~expect:(List.sort (Map.data map) ~compare:Data.compare))
                     ;;

                     let iteri = Hashtbl.iteri

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         let t_copy = Hashtbl.create (module Key) in
                         Hashtbl.iteri t ~f:(fun ~key ~data ->
                           Hashtbl.add_exn t_copy ~key ~data);
                         [%test_result: Data.t Key.Map.t] (to_map t_copy) ~expect:map)
                     ;;

                     let iter_keys = Hashtbl.iter_keys

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         let keys = ref [] in
                         Hashtbl.iter_keys t ~f:(fun key -> keys := key :: !keys);
                         [%test_result: Key.t list]
                           (List.sort !keys ~compare:Key.compare)
                           ~expect:(List.sort (Map.keys map) ~compare:Key.compare))
                     ;;

                     let exists = Hashtbl.exists

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: bool]
                           (Hashtbl.exists t ~f:Data.to_bool)
                           ~expect:(Map.exists map ~f:Data.to_bool))
                     ;;

                     let existsi = Hashtbl.existsi

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: bool]
                           (Hashtbl.existsi t ~f:Key_and_data.to_bool)
                           ~expect:(Map.existsi map ~f:Key_and_data.to_bool))
                     ;;

                     let for_all = Hashtbl.for_all

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: bool]
                           (Hashtbl.for_all t ~f:Data.to_bool)
                           ~expect:(Map.for_all map ~f:Data.to_bool))
                     ;;

                     let for_alli = Hashtbl.for_alli

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: bool]
                           (Hashtbl.for_alli t ~f:Key_and_data.to_bool)
                           ~expect:(Map.for_alli map ~f:Key_and_data.to_bool))
                     ;;

                     let count = Hashtbl.count

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: int]
                           (Hashtbl.count t ~f:Data.to_bool)
                           ~expect:(Map.count map ~f:Data.to_bool))
                     ;;

                     let counti = Hashtbl.counti

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: int]
                           (Hashtbl.counti t ~f:Key_and_data.to_bool)
                           ~expect:(Map.counti map ~f:Key_and_data.to_bool))
                     ;;

                     let length = Hashtbl.length
                     let capacity = Hashtbl.capacity

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: int] (Hashtbl.length t) ~expect:(Map.length map);
                         assert (Hashtbl.capacity t >= Hashtbl.length t))
                     ;;

                     let is_empty = Hashtbl.is_empty

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~examples:[ empty_constructor ]
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: bool]
                           (Hashtbl.is_empty t)
                           ~expect:(Map.is_empty map))
                     ;;

                     let mem = Hashtbl.mem

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple2 constructor_gen Key.quickcheck_generator)
                         ~sexp_of:[%sexp_of: constructor * Key.t]
                         ~f:(fun (constructor, key) ->
                         let map, t = map_and_table constructor in
                         [%test_result: bool]
                           (Hashtbl.mem t key)
                           ~expect:(Map.mem map key))
                     ;;

                     let remove = Hashtbl.remove

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple2 constructor_gen Key.quickcheck_generator)
                         ~sexp_of:[%sexp_of: constructor * Key.t]
                         ~f:(fun (constructor, key) ->
                         let map, t = map_and_table constructor in
                         Hashtbl.remove t key;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:(Map.remove map key))
                     ;;

                     let set = Hashtbl.set

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple3
                            constructor_gen
                            Key.quickcheck_generator
                            Data.quickcheck_generator)
                         ~sexp_of:[%sexp_of: constructor * Key.t * Data.t]
                         ~f:(fun (constructor, key, data) ->
                         let map, t = map_and_table constructor in
                         Hashtbl.set t ~key ~data;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:(Map.set map ~key ~data))
                     ;;

                     let add = Hashtbl.add
                     let add_exn = Hashtbl.add_exn

                     let%test_unit _ =
                       let f (name, add) =
                         Qc.test
                           (Gen.tuple3
                              constructor_gen
                              Key.quickcheck_generator
                              Data.quickcheck_generator)
                           ~sexp_of:[%sexp_of: constructor * Key.t * Data.t]
                           ~f:(fun (constructor, key, data) ->
                           let map, t = map_and_table constructor in
                           let expect =
                             match add t ~key ~data with
                             | `Ok -> Map.set map ~key ~data
                             | `Duplicate -> map
                           in
                           [%test_result: Data.t Key.Map.t]
                             (to_map t)
                             ~expect
                             ~message:name)
                       in
                       let add_exn t ~key ~data =
                         match add_exn t ~key ~data with
                         | () -> `Ok
                         | exception _ -> `Duplicate
                       in
                       List.iter ~f [ "add", add; "add_exn", add_exn ]
                     ;;

                     let change = Hashtbl.change

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple3
                            constructor_gen
                            Key.quickcheck_generator
                            (Option.quickcheck_generator Data.quickcheck_generator))
                         ~sexp_of:[%sexp_of: constructor * Key.t * Data.t option]
                         ~f:(fun (constructor, key, data_opt) ->
                           let map, t = map_and_table constructor in
                           Hashtbl.change t key ~f:(fun original ->
                             [%test_result: Data.t option]
                               original
                               ~expect:(Map.find map key);
                             data_opt);
                           [%test_result: Data.t Key.Map.t]
                             (to_map t)
                             ~expect:(Map.change map key ~f:(fun _ -> data_opt)))
                     ;;

                     let choose = Hashtbl.choose
                     let choose_exn = Hashtbl.choose_exn
                     let choose_randomly = Hashtbl.choose_randomly
                     let choose_randomly_exn = Hashtbl.choose_randomly_exn

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: bool]
                           (is_some (choose t))
                           ~expect:(not (Map.is_empty map));
                         [%test_result: bool]
                           (is_some (choose_randomly t))
                           ~expect:(not (Map.is_empty map));
                         [%test_result: bool]
                           (Exn.does_raise (fun () -> choose_exn t))
                           ~expect:(Map.is_empty map);
                         [%test_result: bool]
                           (Exn.does_raise (fun () -> choose_randomly_exn t))
                           ~expect:(Map.is_empty map))
                     ;;

                     let update = Hashtbl.update

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple3
                            constructor_gen
                            Key.quickcheck_generator
                            Data.quickcheck_generator)
                         ~sexp_of:[%sexp_of: constructor * Key.t * Data.t]
                         ~f:(fun (constructor, key, data) ->
                         let map, t = map_and_table constructor in
                         Hashtbl.update t key ~f:(fun original ->
                           [%test_result: Data.t option]
                             original
                             ~expect:(Map.find map key);
                           data);
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:(Map.update map key ~f:(fun _ -> data)))
                     ;;

                     let update_and_return = Hashtbl.update_and_return

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple3
                            constructor_gen
                            Key.quickcheck_generator
                            Data.quickcheck_generator)
                         ~sexp_of:[%sexp_of: constructor * Key.t * Data.t]
                         ~f:(fun (constructor, key, data) ->
                         let map, t = map_and_table constructor in
                         let result =
                           Hashtbl.update_and_return t key ~f:(fun original ->
                             [%test_result: Data.t option]
                               original
                               ~expect:(Map.find map key);
                             data)
                         in
                         [%test_result: Data.t Key.Map.t * Data.t]
                           (to_map t, result)
                           ~expect:
                             (let map = Map.update map key ~f:(fun _ -> data) in
                              map, Map.find_exn map key))
                     ;;

                     let add_multi = Hashtbl.add_multi

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple3
                            multi_constructor_gen
                            Key.quickcheck_generator
                            Data.quickcheck_generator)
                         ~sexp_of:[%sexp_of: multi_constructor * Key.t * Data.t]
                         ~f:(fun (multi_constructor, key, data) ->
                         let map, t = map_and_table_multi multi_constructor in
                         Hashtbl.add_multi t ~key ~data;
                         [%test_result: Data.t list Key.Map.t]
                           (to_map t)
                           ~expect:(Map.add_multi map ~key ~data))
                     ;;

                     let remove_multi = Hashtbl.remove_multi

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple2 multi_constructor_gen Key.quickcheck_generator)
                         ~sexp_of:[%sexp_of: multi_constructor * Key.t]
                         ~f:(fun (multi_constructor, key) ->
                         let map, t = map_and_table_multi multi_constructor in
                         Hashtbl.remove_multi t key;
                         [%test_result: Data.t list Key.Map.t]
                           (to_map t)
                           ~expect:(Map.remove_multi map key))
                     ;;

                     let find_multi = Hashtbl.find_multi

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple2 multi_constructor_gen Key.quickcheck_generator)
                         ~sexp_of:[%sexp_of: multi_constructor * Key.t]
                         ~f:(fun (multi_constructor, key) ->
                         let map, t = map_and_table_multi multi_constructor in
                         [%test_result: Data.t list]
                           (Hashtbl.find_multi t key)
                           ~expect:(Map.find_multi map key))
                     ;;

                     let map = Hashtbl.map

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t]
                           (to_map (Hashtbl.map t ~f:Data.succ))
                           ~expect:(Map.map map ~f:Data.succ))
                     ;;

                     let mapi = Hashtbl.mapi

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t]
                           (to_map (Hashtbl.mapi t ~f:Key_and_data.to_data))
                           ~expect:(Map.mapi map ~f:Key_and_data.to_data))
                     ;;

                     let map_inplace = Hashtbl.map_inplace

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         Hashtbl.map_inplace t ~f:Data.succ;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:(Map.map map ~f:Data.succ))
                     ;;

                     let mapi_inplace = Hashtbl.mapi_inplace

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         Hashtbl.mapi_inplace t ~f:Key_and_data.to_data;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:(Map.mapi map ~f:Key_and_data.to_data))
                     ;;

                     let filter_keys = Hashtbl.filter_keys

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t]
                           (to_map (Hashtbl.filter_keys t ~f:Key.to_bool))
                           ~expect:(Map.filter_keys map ~f:Key.to_bool))
                     ;;

                     let filter = Hashtbl.filter

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t]
                           (to_map (Hashtbl.filter t ~f:Data.to_bool))
                           ~expect:(Map.filter map ~f:Data.to_bool))
                     ;;

                     let filteri = Hashtbl.filteri

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t]
                           (to_map (Hashtbl.filteri t ~f:Key_and_data.to_bool))
                           ~expect:(Map.filteri map ~f:Key_and_data.to_bool))
                     ;;

                     let filter_map = Hashtbl.filter_map

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t]
                           (to_map (Hashtbl.filter_map t ~f:Data.to_option))
                           ~expect:(Map.filter_map map ~f:Data.to_option))
                     ;;

                     let filter_mapi = Hashtbl.filter_mapi

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t]
                           (to_map (Hashtbl.filter_mapi t ~f:Key_and_data.to_data_option))
                           ~expect:(Map.filter_mapi map ~f:Key_and_data.to_data_option))
                     ;;

                     let filter_inplace = Hashtbl.filter_inplace

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         Hashtbl.filter_inplace t ~f:Data.to_bool;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:
                             (Map.filteri map ~f:(fun ~key:_ ~data -> Data.to_bool data)))
                     ;;

                     let filteri_inplace = Hashtbl.filteri_inplace

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         Hashtbl.filteri_inplace t ~f:Key_and_data.to_bool;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:(Map.filteri map ~f:Key_and_data.to_bool))
                     ;;

                     let filter_map_inplace = Hashtbl.filter_map_inplace

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         Hashtbl.filter_map_inplace t ~f:Data.to_option;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:(Map.filter_map map ~f:Data.to_option))
                     ;;

                     let filter_mapi_inplace = Hashtbl.filter_mapi_inplace

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         Hashtbl.filter_mapi_inplace t ~f:Key_and_data.to_data_option;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:(Map.filter_mapi map ~f:Key_and_data.to_data_option))
                     ;;

                     let filter_keys_inplace = Hashtbl.filter_keys_inplace

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         Hashtbl.filter_keys_inplace t ~f:Key.to_bool;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:
                             (Map.filteri map ~f:(fun ~key ~data:_ -> Key.to_bool key)))
                     ;;

                     let partition_map = Hashtbl.partition_map

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t * Data.t Key.Map.t]
                           (let a, b = Hashtbl.partition_map t ~f:Data.to_partition in
                            to_map a, to_map b)
                           ~expect:(Map.partition_map map ~f:Data.to_partition))
                     ;;

                     let partition_mapi = Hashtbl.partition_mapi

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t * Data.t Key.Map.t]
                           (let a, b =
                              Hashtbl.partition_mapi t ~f:Key_and_data.to_data_partition
                            in
                            to_map a, to_map b)
                           ~expect:
                             (Map.partition_mapi map ~f:Key_and_data.to_data_partition))
                     ;;

                     let partition_tf = Hashtbl.partition_tf

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t * Data.t Key.Map.t]
                           (let a, b = Hashtbl.partition_tf t ~f:Data.to_bool in
                            to_map a, to_map b)
                           ~expect:(Map.partition_tf map ~f:Data.to_bool))
                     ;;

                     let partitioni_tf = Hashtbl.partitioni_tf

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t Key.Map.t * Data.t Key.Map.t]
                           (let a, b = Hashtbl.partitioni_tf t ~f:Key_and_data.to_bool in
                            to_map a, to_map b)
                           ~expect:(Map.partitioni_tf map ~f:Key_and_data.to_bool))
                     ;;

                     let find_or_add = Hashtbl.find_or_add
                     let findi_or_add = Hashtbl.findi_or_add

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple3
                            constructor_gen
                            Key.quickcheck_generator
                            Data.quickcheck_generator)
                         ~sexp_of:[%sexp_of: constructor * Key.t * Data.t]
                         ~f:(fun (constructor, key, data) ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t]
                           (find_or_add t key ~default:(fun () -> data))
                           ~expect:(Map.find map key |> Option.value ~default:data);
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:
                             (if Map.mem map key then map else Map.set map ~key ~data))
                     ;;

                     let find = Hashtbl.find
                     let find_exn = Hashtbl.find_exn

                     let%test_unit _ =
                       let f (name, find) =
                         Qc.test
                           (Gen.tuple2 constructor_gen Key.quickcheck_generator)
                           ~sexp_of:[%sexp_of: constructor * Key.t]
                           ~f:(fun (constructor, key) ->
                           let map, t = map_and_table constructor in
                           [%test_result: Data.t option]
                             (find t key)
                             ~expect:(Map.find map key)
                             ~message:name)
                       in
                       let find_exn t key =
                         match find_exn t key with
                         | data -> Some data
                         | exception _ -> None
                       in
                       List.iter ~f [ "find", find; "find_exn", find_exn ]
                     ;;

                     let find_and_call = Hashtbl.find_and_call
                     let findi_and_call = Hashtbl.findi_and_call
                     let find_and_call1 = Hashtbl.find_and_call1
                     let findi_and_call1 = Hashtbl.findi_and_call1
                     let find_and_call2 = Hashtbl.find_and_call2
                     let findi_and_call2 = Hashtbl.findi_and_call2

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple4
                            constructor_gen
                            Key.quickcheck_generator
                            Int.quickcheck_generator
                            String.quickcheck_generator)
                         ~sexp_of:[%sexp_of: constructor * Key.t * int * string]
                         ~f:(fun (constructor, key, a, b) ->
                         let map, t = map_and_table constructor in
                         [%test_result: (Data.t, Key.t) Either.t]
                           (Hashtbl.find_and_call
                              t
                              key
                              ~if_found:Either.first
                              ~if_not_found:Either.second)
                           ~expect:
                             (match Map.find map key with
                              | Some data -> First data
                              | None -> Second key);
                         [%test_result: (Key.t * Data.t, Key.t) Either.t]
                           (Hashtbl.findi_and_call
                              t
                              key
                              ~if_found:(fun ~key ~data -> Either.first (key, data))
                              ~if_not_found:Either.second)
                           ~expect:
                             (match Map.find map key with
                              | Some data -> First (key, data)
                              | None -> Second key);
                         [%test_result: (Data.t, Key.t) Either.t * int]
                           (Hashtbl.find_and_call1
                              t
                              key
                              ~a
                              ~if_found:(fun data a -> Either.first data, a)
                              ~if_not_found:(fun key a -> Either.second key, a))
                           ~expect:
                             (match Map.find map key with
                              | Some data -> First data, a
                              | None -> Second key, a);
                         [%test_result: (Key.t * Data.t, Key.t) Either.t * int]
                           (Hashtbl.findi_and_call1
                              t
                              key
                              ~a
                              ~if_found:(fun ~key ~data a -> Either.first (key, data), a)
                              ~if_not_found:(fun key a -> Either.second key, a))
                           ~expect:
                             (match Map.find map key with
                              | Some data -> First (key, data), a
                              | None -> Second key, a);
                         [%test_result: (Data.t, Key.t) Either.t * int * string]
                           (Hashtbl.find_and_call2
                              t
                              key
                              ~a
                              ~b
                              ~if_found:(fun data a b -> Either.first data, a, b)
                              ~if_not_found:(fun key a b -> Either.second key, a, b))
                           ~expect:
                             (match Map.find map key with
                              | Some data -> First data, a, b
                              | None -> Second key, a, b);
                         [%test_result: (Key.t * Data.t, Key.t) Either.t * int * string]
                           (Hashtbl.findi_and_call2
                              t
                              key
                              ~a
                              ~b
                              ~if_found:(fun ~key ~data a b ->
                                Either.first (key, data), a, b)
                              ~if_not_found:(fun key a b -> Either.second key, a, b))
                           ~expect:
                             (match Map.find map key with
                              | Some data -> First (key, data), a, b
                              | None -> Second key, a, b))
                     ;;

                     let find_and_remove = Hashtbl.find_and_remove

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple2 constructor_gen Key.quickcheck_generator)
                         ~sexp_of:[%sexp_of: constructor * Key.t]
                         ~f:(fun (constructor, key) ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t option]
                           (Hashtbl.find_and_remove t key)
                           ~expect:(Map.find map key);
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:(Map.remove map key))
                     ;;

                     let merge = Hashtbl.merge

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple2 constructor_gen constructor_gen)
                         ~sexp_of:[%sexp_of: constructor * constructor]
                         ~f:(fun (constructor1, constructor2) ->
                         let map1, t1 = map_and_table constructor1 in
                         let map2, t2 = map_and_table constructor2 in
                         [%test_result: Data.t Key.Map.t]
                           (to_map (Hashtbl.merge t1 t2 ~f:Key_and_data.merge))
                           ~expect:(Map.merge map1 map2 ~f:Key_and_data.merge))
                     ;;

                     let merge_into = Hashtbl.merge_into

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple2 constructor_gen constructor_gen)
                         ~sexp_of:[%sexp_of: constructor * constructor]
                         ~f:(fun (constructor1, constructor2) ->
                         let map1, t1 = map_and_table constructor1 in
                         let map2, t2 = map_and_table constructor2 in
                         let f ~key data2 data1_opt : _ Hashtbl_intf.Merge_into_action.t =
                           match
                             Key_and_data.merge
                               ~key
                               (match data1_opt with
                                | Some data1 -> `Both (data1, data2)
                                | None -> `Right data2)
                           with
                           | None -> Remove
                           | Some x -> Set_to x
                         in
                         Hashtbl.merge_into ~dst:t1 ~src:t2 ~f;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t1)
                           ~expect:
                             (Map.fold map2 ~init:map1 ~f:(fun ~key ~data:data2 map1 ->
                                Map.change map1 key ~f:(fun data1_opt ->
                                  match f ~key data2 data1_opt with
                                  | Set_to x -> Some x
                                  | Remove -> None))))
                     ;;

                     let equal = Hashtbl.equal

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple2 constructor_gen constructor_gen)
                         ~sexp_of:[%sexp_of: constructor * constructor]
                         ~f:(fun (constructor1, constructor2) ->
                         let map1, t1 = map_and_table constructor1 in
                         let map2, t2 = map_and_table constructor2 in
                         [%test_result: bool]
                           (Hashtbl.equal Data.equal t1 t2)
                           ~expect:(Map.equal Data.equal map1 map2))
                     ;;

                     let similar = Hashtbl.similar

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple2 constructor_gen constructor_gen)
                         ~sexp_of:[%sexp_of: constructor * constructor]
                         ~f:(fun (constructor1, constructor2) ->
                         let map1, t1 = map_and_table constructor1 in
                         let map2, t2 = map_and_table constructor2 in
                         [%test_result: bool]
                           (Hashtbl.similar Data.equal t1 t2)
                           ~expect:(Map.equal Data.equal map1 map2))
                     ;;

                     let keys = Hashtbl.keys

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Key.t list]
                           (keys t |> List.sort ~compare:Key.compare)
                           ~expect:(Map.keys map |> List.sort ~compare:Key.compare))
                     ;;

                     let data = Hashtbl.data

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: Data.t list]
                           (data t |> List.sort ~compare:Data.compare)
                           ~expect:(Map.data map |> List.sort ~compare:Data.compare))
                     ;;

                     let to_alist = Hashtbl.to_alist

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         [%test_result: (Key.t * Data.t) list]
                           (to_alist t |> List.sort ~compare:[%compare: Key.t * Data.t])
                           ~expect:
                             (Map.to_alist map
                              |> List.sort ~compare:[%compare: Key.t * Data.t]))
                     ;;

                     let validate = Hashtbl.validate

                     let%test_unit _ =
                       Qc.test
                         constructor_gen
                         ~sexp_of:[%sexp_of: constructor]
                         ~f:(fun constructor ->
                         let map, t = map_and_table constructor in
                         let name = Key.to_string in
                         let check_data data =
                           if Data.to_bool data
                           then Validate.pass
                           else Validate.fail "data"
                         in
                         [%test_result: bool]
                           (Result.is_ok
                              (Validate.result (Hashtbl.validate ~name check_data t)))
                           ~expect:(Map.for_all map ~f:Data.to_bool))
                     ;;

                     let incr = Hashtbl.incr

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple3
                            constructor_gen
                            Key.quickcheck_generator
                            Data.quickcheck_generator)
                         ~sexp_of:[%sexp_of: constructor * Key.t * Data.t]
                         ~f:(fun (constructor, key, by) ->
                         let map, t = map_and_table constructor in
                         Hashtbl.incr t key ~by;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:
                             (Map.update map key ~f:(fun opt ->
                                Option.value opt ~default:0 + by)))
                     ;;

                     let decr = Hashtbl.decr

                     let%test_unit _ =
                       Qc.test
                         (Gen.tuple3
                            constructor_gen
                            Key.quickcheck_generator
                            Data.quickcheck_generator)
                         ~sexp_of:[%sexp_of: constructor * Key.t * Data.t]
                         ~f:(fun (constructor, key, by) ->
                         let map, t = map_and_table constructor in
                         Hashtbl.decr t key ~by;
                         [%test_result: Data.t Key.Map.t]
                           (to_map t)
                           ~expect:
                             (Map.update map key ~f:(fun opt ->
                                Option.value opt ~default:0 - by)))
                     ;;
                   end :
                     Accessors_with_unit_tests)

                 (* not particularly interesting to test *)
                 let sexp_of_t = Hashtbl.sexp_of_t
               end :
                 Hashtbl_for_testing with type ('a, 'b) t := ('a, 'b) Hashtbl.t))
  ;;
end

module Make_mutation_in_callbacks (Hashtbl : Hashtbl_for_testing) = struct
  let%test_module "mutation in callbacks" =
    (module (
               struct
                 module Test = struct
                   exception Note of (string, Sexp.t) List.Assoc.t * exn [@@deriving sexp]

                   let note str value sexp_of_value thunk =
                     try thunk () with
                     | Note (alist, exn) ->
                       raise (Note ((str, sexp_of_value value) :: alist, exn))
                     | exn -> raise (Note ([ str, sexp_of_value value ], exn))
                   ;;

                   let for_each str sexp_of_value values f =
                     List.iter values ~f:(fun value ->
                       note str value sexp_of_value (fun () -> f value))
                   ;;

                   let size = 16

                   let key_funs =
                     [ (fun i -> i); (fun i -> i + size); (fun i -> i * size) ]
                   ;;

                   let multi_funs =
                     [ (fun _ -> []); (fun i -> [ i ]); (fun i -> List.init i ~f:Fn.id) ]
                   ;;

                   let sample_keys =
                     List.concat_map key_funs ~f:(fun f -> [ f 1; f 2; f 3 ])
                   ;;

                   let sample_data = [ 100; 200; 300 ]
                   let sexp_of_key = [%sexp_of: int]
                   let sexp_of_data = [%sexp_of: int]

                   let make_table ~key_of_index ~data_of_index =
                     List.init size ~f:(fun i -> key_of_index i, data_of_index i)
                     |> Hashtbl.of_alist_exn (module Int)
                   ;;

                   let makers =
                     List.map key_funs ~f:(fun key_of_index ->
                       let maker () = make_table ~key_of_index ~data_of_index:Fn.id in
                       maker)
                   ;;

                   let multi_makers =
                     List.concat_map key_funs ~f:(fun key_of_index ->
                       List.concat_map multi_funs ~f:(fun data_of_index ->
                         [ (fun () -> make_table ~key_of_index ~data_of_index) ]))
                   ;;

                   let sexp_of_maker make =
                     make () |> Hashtbl.to_alist |> [%sexp_of: (int * int) list]
                   ;;

                   let sexp_of_multi_maker make =
                     make () |> Hashtbl.to_alist |> [%sexp_of: (int * int list) list]
                   ;;

                   let bools = [ true; false ]

                   let option values =
                     None :: List.map values ~f:(fun value -> Some value)
                   ;;

                   let first_or_second values =
                     List.concat_map values ~f:(fun value ->
                       [ First value; Second value ])
                   ;;

                   let default_mutate t = Hashtbl.clear t

                   let make_caller test_result =
                     let caller t f = Hashtbl.map t ~f |> Hashtbl.to_alist in
                     let callback x = x in
                     caller, callback, test_result
                   ;;

                   let mono_caller () = make_caller [%test_result: (int * int) list]
                   let multi_caller () = make_caller [%test_result: (int * int list) list]

                   let inside_iter t f =
                     with_return (fun r ->
                       Hashtbl.iteri t ~f:(fun ~key:_ ~data:_ -> r.return (f ()));
                       assert false)
                   ;;

                   type 'a test_result =
                     ?here:Lexing.position list
                     -> ?message:string
                     -> ?equal:('a -> 'a -> bool)
                     -> expect:'a
                     -> 'a
                     -> unit

                   let mutation_after_callback_does_not_raise
                     (type a)
                     ~make
                     ~caller
                     ~callback
                     ~mutate
                     ~test_result
                     =
                     let (_ : a test_result) = test_result in
                     let t = make () in
                     let (_ : a) = caller t callback in
                     mutate t
                   ;;

                   let mutation_after_callback_inside_iter_does_not_raise
                     (type a)
                     ~make
                     ~caller
                     ~callback
                     ~mutate
                     ~test_result
                     =
                     let (_ : a test_result) = test_result in
                     let t = make () in
                     let (_ : a Or_error.t) =
                       Or_error.try_with (fun () ->
                         inside_iter t (fun () -> caller t callback))
                     in
                     mutate t
                   ;;

                   let mutation_after_iter_inside_callback_does_not_raise
                     (type a)
                     ~make
                     ~caller
                     ~callback
                     ~mutate
                     ~test_result
                     =
                     let (_ : a test_result) = test_result in
                     let t = make () in
                     let (_ : a Or_error.t) =
                       Or_error.try_with (fun () ->
                         caller t (fun x -> inside_iter t (fun () -> callback x)))
                     in
                     mutate t
                   ;;

                   let mutation_after_raising_callback_does_not_raise
                     (type a)
                     ~make
                     ~caller
                     ~callback:_
                     ~mutate
                     ~test_result
                     =
                     let (_ : a test_result) = test_result in
                     let t = make () in
                     let (_ : a Or_error.t) =
                       Or_error.try_with (fun () -> caller t (fun _ -> assert false))
                     in
                     mutate t
                   ;;

                   let mutation_inside_callback_is_consistent
                     ~make
                     ~caller
                     ~callback
                     ~mutate
                     ~test_result
                     =
                     match
                       let t = make () in
                       Or_error.try_with (fun () ->
                         caller t (fun x ->
                           mutate t;
                           callback x))
                     with
                     | Error _ -> ()
                     | Ok actual ->
                       let expect =
                         let t = make () in
                         caller t callback
                       in
                       test_result ?here:None ?message:None ?equal:None ~expect actual
                   ;;

                   let mutation_inside_callback_inside_iter_is_consistent
                     ~make
                     ~caller
                     ~callback
                     ~mutate
                     ~test_result
                     =
                     match
                       let t = make () in
                       Or_error.try_with (fun () ->
                         inside_iter t (fun () ->
                           caller t (fun x ->
                             mutate t;
                             callback x)))
                     with
                     | Error _ -> ()
                     | Ok actual ->
                       let expect =
                         let t = make () in
                         inside_iter t (fun () -> caller t callback)
                       in
                       test_result ?here:None ?message:None ?equal:None ~expect actual
                   ;;

                   let mutation_inside_iter_inside_callback_is_consistent
                     ~make
                     ~caller
                     ~callback
                     ~mutate
                     ~test_result
                     =
                     match
                       let t = make () in
                       Or_error.try_with (fun () ->
                         caller t (fun x ->
                           inside_iter t (fun () ->
                             mutate t;
                             callback x)))
                     with
                     | Error _ -> ()
                     | Ok actual ->
                       let expect =
                         let t = make () in
                         caller t (fun x -> inside_iter t (fun () -> callback x))
                       in
                       test_result ?here:None ?message:None ?equal:None ~expect actual
                   ;;

                   let mutation_inside_iter_after_callback_is_consistent
                     ~make
                     ~caller
                     ~callback
                     ~mutate
                     ~test_result
                     =
                     match
                       let t = make () in
                       Or_error.try_with (fun () ->
                         inside_iter t (fun () ->
                           let x = caller t callback in
                           mutate t;
                           x))
                     with
                     | Error _ -> ()
                     | Ok actual ->
                       let expect =
                         let t = make () in
                         inside_iter t (fun () -> caller t callback)
                       in
                       test_result ?here:None ?message:None ?equal:None ~expect actual
                   ;;

                   let mutation_inside_callback_after_iter_is_consistent
                     ~make
                     ~caller
                     ~callback
                     ~mutate
                     ~test_result
                     =
                     match
                       let t = make () in
                       Or_error.try_with (fun () ->
                         caller t (fun x ->
                           inside_iter t (fun () -> ());
                           mutate t;
                           callback x))
                     with
                     | Error _ -> ()
                     | Ok actual ->
                       let expect =
                         let t = make () in
                         caller t (fun x ->
                           inside_iter t (fun () -> ());
                           callback x)
                       in
                       test_result ?here:None ?message:None ?equal:None ~expect actual
                   ;;

                   let test_cases =
                     [ ( "mutation_inside_callback_is_consistent"
                       , mutation_inside_callback_is_consistent )
                     ; ( "mutation_after_callback_does_not_raise"
                       , mutation_after_callback_does_not_raise )
                     ; ( "mutation_after_raising_callback_does_not_raise"
                       , mutation_after_raising_callback_does_not_raise )
                     ; ( "mutation_inside_callback_inside_iter_is_consistent"
                       , mutation_inside_callback_inside_iter_is_consistent )
                     ; ( "mutation_inside_iter_inside_callback_is_consistent"
                       , mutation_inside_iter_inside_callback_is_consistent )
                     ; ( "mutation_inside_iter_after_callback_is_consistent"
                       , mutation_inside_iter_after_callback_is_consistent )
                     ; ( "mutation_inside_callback_after_iter_is_consistent"
                       , mutation_inside_callback_after_iter_is_consistent )
                     ; ( "mutation_after_callback_inside_iter_does_not_raise"
                       , mutation_after_callback_inside_iter_does_not_raise )
                     ; ( "mutation_after_iter_inside_callback_does_not_raise"
                       , mutation_after_iter_inside_callback_does_not_raise )
                     ]
                   ;;

                   let sexp_of_test_case (name, _) = Sexp.Atom name

                   let test name ~make ~caller ~callback ~mutate ~test_result =
                     for_each "test case" sexp_of_test_case test_cases (fun (_, f) ->
                       note "test kind" name [%sexp_of: string] (fun () ->
                         f ~make ~caller ~callback ~mutate ~test_result))
                   ;;

                   let test_mutate mutate =
                     let caller, callback, test_result = mono_caller () in
                     for_each "table to mutate" sexp_of_maker makers (fun make ->
                       test "mutator" ~make ~caller ~callback ~mutate ~test_result)
                   ;;

                   let test_mutate_multi mutate =
                     let caller, callback, test_result = multi_caller () in
                     for_each
                       "table to mutate"
                       sexp_of_multi_maker
                       multi_makers
                       (fun make ->
                       test "mutator" ~make ~caller ~callback ~mutate ~test_result)
                   ;;

                   let test_caller ~callback ~test_result caller =
                     let mutate = default_mutate in
                     for_each "table to mutate" sexp_of_maker makers (fun make ->
                       test "caller" ~make ~caller ~callback ~mutate ~test_result)
                   ;;

                   let test_mutate_2ts mutate =
                     for_each "table not mutated" sexp_of_maker makers (fun make ->
                       test_mutate (fun t1 ->
                         let t2 = make () in
                         mutate t1 t2);
                       test_mutate (fun t2 ->
                         let t1 = make () in
                         mutate t1 t2))
                   ;;

                   let test_caller_2ts ~callback ~test_result caller =
                     for_each "table not mutated" sexp_of_maker makers (fun make ->
                       test_caller ~callback ~test_result (fun t1 f ->
                         caller t1 (make ()) f);
                       test_caller ~callback ~test_result (fun t2 f ->
                         caller (make ()) t2 f))
                   ;;

                   let test_caller_2fs f1 f2 ~test_result caller =
                     test_caller ~callback:f1 ~test_result (fun t f1 -> caller t f1 f2);
                     test_caller ~callback:f2 ~test_result (fun t f2 -> caller t f1 f2)
                   ;;

                   type ('a, 'b) merge_data =
                     [ `Left of 'a
                     | `Right of 'b
                     | `Both of 'a * 'b
                     ]
                   [@@deriving sexp, compare]
                 end

                 open Test

                 (* functions that both mutate and accept callbacks *)

                 let find_or_add = Hashtbl.find_or_add
                 let findi_or_add = Hashtbl.findi_or_add

                 let%test_unit "find_or_add" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     for_each "data" sexp_of_data sample_data (fun data ->
                       test_mutate (fun t ->
                         Hashtbl.find_or_add t key ~default:(fun () -> data)
                         |> (ignore : int -> unit));
                       let callback () = data in
                       let test_result = [%test_result: int] in
                       test_caller ~callback ~test_result (fun t f ->
                         Hashtbl.find_or_add t key ~default:f)))
                 ;;

                 let change = Hashtbl.change
                 let update = Hashtbl.update
                 let update_and_return = Hashtbl.update_and_return

                 let%test_unit "change" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     for_each
                       "f result"
                       [%sexp_of: data option]
                       (option sample_data)
                       (fun opt ->
                       test_mutate (fun t -> Hashtbl.change t key ~f:(fun _ -> opt));
                       let callback _ = opt in
                       let test_result = [%test_result: int option] in
                       test_caller ~callback ~test_result (fun t f ->
                         Hashtbl.change t key ~f;
                         Hashtbl.find t key)))
                 ;;

                 let%test_unit "update" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     for_each "f result" [%sexp_of: data] sample_data (fun data ->
                       test_mutate (fun t -> Hashtbl.update t key ~f:(fun _ -> data));
                       let callback _ = data in
                       let test_result = [%test_result: int option] in
                       test_caller ~callback ~test_result (fun t f ->
                         Hashtbl.update t key ~f;
                         Hashtbl.find t key)))
                 ;;

                 let%test_unit "update_and_return" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     for_each "f result" [%sexp_of: data] sample_data (fun data ->
                       test_mutate (fun t ->
                         ignore (Hashtbl.update_and_return t key ~f:(fun _ -> data) : _));
                       let callback _ = data in
                       let test_result = [%test_result: int option] in
                       test_caller ~callback ~test_result (fun t f ->
                         Some (Hashtbl.update_and_return t key ~f))))
                 ;;

                 let merge_into = Hashtbl.merge_into

                 let%test_unit "merge_into" =
                   for_each
                     "f result"
                     [%sexp_of: data option]
                     (option sample_data)
                     (fun opt ->
                     let action : _ Hashtbl_intf.Merge_into_action.t =
                       match opt with
                       | None -> Remove
                       | Some x -> Set_to x
                     in
                     test_mutate_2ts (fun dst src ->
                       Hashtbl.merge_into ~dst ~src ~f:(fun ~key:_ _ _ -> action));
                     let callback _ = action in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller_2ts ~callback ~test_result (fun src dst f ->
                       Hashtbl.merge_into ~dst ~src ~f:(fun ~key a b -> f (key, a, b));
                       Hashtbl.to_alist dst))
                 ;;

                 let filter_inplace = Hashtbl.filter_inplace

                 let%test_unit "filter_inplace" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     test_mutate (fun t -> Hashtbl.filter_inplace t ~f:(fun _ -> bool));
                     let callback _ = bool in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.filter_inplace t ~f;
                       Hashtbl.to_alist t))
                 ;;

                 let filteri_inplace = Hashtbl.filteri_inplace

                 let%test_unit "filteri_inplace" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     test_mutate (fun t ->
                       Hashtbl.filteri_inplace t ~f:(fun ~key:_ ~data:_ -> bool));
                     let callback _ = bool in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.filteri_inplace t ~f:(fun ~key ~data -> f (key, data));
                       Hashtbl.to_alist t))
                 ;;

                 let filter_keys_inplace = Hashtbl.filter_keys_inplace

                 let%test_unit "filter_keys_inplace" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     test_mutate (fun t ->
                       Hashtbl.filter_keys_inplace t ~f:(fun _ -> bool));
                     let callback _ = bool in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.filter_keys_inplace t ~f;
                       Hashtbl.to_alist t))
                 ;;

                 let map_inplace = Hashtbl.map_inplace

                 let%test_unit "map_inplace" =
                   for_each "f result" [%sexp_of: int] sample_data (fun data ->
                     test_mutate (fun t -> Hashtbl.map_inplace t ~f:(fun _ -> data));
                     let callback _ = data in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.map_inplace t ~f;
                       Hashtbl.to_alist t))
                 ;;

                 let mapi_inplace = Hashtbl.mapi_inplace

                 let%test_unit "mapi_inplace" =
                   for_each "f result" [%sexp_of: int] sample_data (fun data ->
                     test_mutate (fun t ->
                       Hashtbl.mapi_inplace t ~f:(fun ~key:_ ~data:_ -> data));
                     let callback _ = data in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.mapi_inplace t ~f:(fun ~key ~data -> f (key, data));
                       Hashtbl.to_alist t))
                 ;;

                 let filter_map_inplace = Hashtbl.filter_map_inplace

                 let%test_unit "filter_map_inplace" =
                   for_each
                     "f result"
                     [%sexp_of: int option]
                     (option sample_data)
                     (fun opt ->
                     test_mutate (fun t -> Hashtbl.filter_map_inplace t ~f:(fun _ -> opt));
                     let callback _ = opt in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.filter_map_inplace t ~f;
                       Hashtbl.to_alist t))
                 ;;

                 let filter_mapi_inplace = Hashtbl.filter_mapi_inplace

                 let%test_unit "filter_mapi_inplace" =
                   for_each
                     "f result"
                     [%sexp_of: int option]
                     (option sample_data)
                     (fun opt ->
                     test_mutate (fun t ->
                       Hashtbl.filter_mapi_inplace t ~f:(fun ~key:_ ~data:_ -> opt));
                     let callback _ = opt in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.filter_mapi_inplace t ~f:(fun ~key ~data -> f (key, data));
                       Hashtbl.to_alist t))
                 ;;

                 (* functions that mutate *)

                 let set = Hashtbl.set

                 let%test_unit "set" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     for_each "data" sexp_of_data sample_data (fun data ->
                       test_mutate (fun t -> Hashtbl.set t ~key ~data)))
                 ;;

                 let add = Hashtbl.add
                 let add_exn = Hashtbl.add_exn

                 let%test_unit "add" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     for_each "data" sexp_of_data sample_data (fun data ->
                       test_mutate (fun t ->
                         Hashtbl.add t ~key ~data
                         |> (ignore : [ `Ok | `Duplicate ] -> unit))))
                 ;;

                 let remove = Hashtbl.remove

                 let%test_unit "remove" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     test_mutate (fun t -> Hashtbl.remove t key))
                 ;;

                 let find_and_remove = Hashtbl.find_and_remove

                 let%test_unit "find_and_remove" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     test_mutate (fun t ->
                       Hashtbl.find_and_remove t key |> (ignore : int option -> unit)))
                 ;;

                 let incr = Hashtbl.incr

                 let%test_unit "incr" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     test_mutate (fun t -> Hashtbl.incr t key))
                 ;;

                 let decr = Hashtbl.decr

                 let%test_unit "decr" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     test_mutate (fun t -> Hashtbl.decr t key))
                 ;;

                 let add_multi = Hashtbl.add_multi

                 let%test_unit "add_multi" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     for_each "data" sexp_of_data sample_data (fun data ->
                       test_mutate_multi (fun t -> Hashtbl.add_multi t ~key ~data)))
                 ;;

                 let remove_multi = Hashtbl.remove_multi

                 let%test_unit "remove_multi" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     test_mutate_multi (fun t -> Hashtbl.remove_multi t key))
                 ;;

                 let clear = Hashtbl.clear
                 let%test_unit "clear" = test_mutate (fun t -> Hashtbl.clear t)

                 (* functions that take callbacks *)

                 let find_and_call = Hashtbl.find_and_call
                 let findi_and_call = Hashtbl.findi_and_call
                 let find_and_call1 = Hashtbl.find_and_call1
                 let findi_and_call1 = Hashtbl.findi_and_call1
                 let find_and_call2 = Hashtbl.find_and_call2
                 let findi_and_call2 = Hashtbl.findi_and_call2

                 let%test_unit "find_and_call" =
                   for_each "key" sexp_of_key sample_keys (fun key ->
                     let found x = `Found x in
                     let not_found x = `Not_found x in
                     let test_result =
                       [%test_result: [ `Found of int | `Not_found of int ]]
                     in
                     test_caller_2fs
                       found
                       not_found
                       ~test_result
                       (fun t if_found if_not_found ->
                       Hashtbl.find_and_call t key ~if_found ~if_not_found))
                 ;;

                 let fold = Hashtbl.fold

                 let%test_unit "fold" =
                   let callback (a, b, c) = (a, b) :: c in
                   let test_result = [%test_result: (int * int) list] in
                   test_caller ~callback ~test_result (fun t f ->
                     Hashtbl.fold t ~init:[] ~f:(fun ~key ~data acc -> f (key, data, acc)))
                 ;;

                 let iter = Hashtbl.iter

                 let%test_unit "iter" =
                   let callback a = a in
                   let test_result = [%test_result: int Queue.t] in
                   test_caller ~callback ~test_result (fun t f ->
                     let queue = Queue.create () in
                     Hashtbl.iter t ~f:(fun data -> Queue.enqueue queue (f data));
                     queue)
                 ;;

                 let iter_keys = Hashtbl.iter_keys

                 let%test_unit "iter_keys" =
                   let callback a = a in
                   let test_result = [%test_result: int Queue.t] in
                   test_caller ~callback ~test_result (fun t f ->
                     let queue = Queue.create () in
                     Hashtbl.iter_keys t ~f:(fun key -> Queue.enqueue queue (f key));
                     queue)
                 ;;

                 let iteri = Hashtbl.iteri

                 let%test_unit "iteri" =
                   let callback (a, b) = a, b in
                   let test_result = [%test_result: (int * int) Queue.t] in
                   test_caller ~callback ~test_result (fun t f ->
                     let queue = Queue.create () in
                     Hashtbl.iteri t ~f:(fun ~key ~data ->
                       Queue.enqueue queue (f (key, data)));
                     queue)
                 ;;

                 let map = Hashtbl.map

                 let%test_unit "map" =
                   let callback a = a in
                   let test_result = [%test_result: (int * int) list] in
                   test_caller ~callback ~test_result (fun t f ->
                     Hashtbl.map t ~f |> Hashtbl.to_alist)
                 ;;

                 let mapi = Hashtbl.mapi

                 let%test_unit "mapi" =
                   let callback (a, b) = a, b in
                   let test_result = [%test_result: (int * (int * int)) list] in
                   test_caller ~callback ~test_result (fun t f ->
                     Hashtbl.mapi t ~f:(fun ~key ~data -> f (key, data))
                     |> Hashtbl.to_alist)
                 ;;

                 let filter_keys = Hashtbl.filter_keys

                 let%test_unit "filter_keys" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.filter_keys t ~f |> Hashtbl.to_alist))
                 ;;

                 let filter = Hashtbl.filter

                 let%test_unit "filter" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.filter t ~f |> Hashtbl.to_alist))
                 ;;

                 let filteri = Hashtbl.filteri

                 let%test_unit "filteri" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.filteri t ~f:(fun ~key ~data -> f (key, data))
                       |> Hashtbl.to_alist))
                 ;;

                 let filter_map = Hashtbl.filter_map

                 let%test_unit "filter_map" =
                   for_each
                     "f result"
                     [%sexp_of: data option]
                     (option sample_data)
                     (fun opt ->
                     let callback _ = opt in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.filter_map t ~f |> Hashtbl.to_alist))
                 ;;

                 let filter_mapi = Hashtbl.filter_mapi

                 let%test_unit "filter_mapi" =
                   for_each
                     "f result"
                     [%sexp_of: data option]
                     (option sample_data)
                     (fun opt ->
                     let callback _ = opt in
                     let test_result = [%test_result: (int * int) list] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.filter_mapi t ~f:(fun ~key ~data -> f (key, data))
                       |> Hashtbl.to_alist))
                 ;;

                 let partition_tf = Hashtbl.partition_tf

                 let%test_unit "partition_tf" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result =
                       [%test_result: (int * int) list * (int * int) list]
                     in
                     test_caller ~callback ~test_result (fun t f ->
                       let t1, t2 = Hashtbl.partition_tf t ~f in
                       Hashtbl.to_alist t1, Hashtbl.to_alist t2))
                 ;;

                 let partitioni_tf = Hashtbl.partitioni_tf

                 let%test_unit "partitioni_tf" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result =
                       [%test_result: (int * int) list * (int * int) list]
                     in
                     test_caller ~callback ~test_result (fun t f ->
                       let t1, t2 =
                         Hashtbl.partitioni_tf t ~f:(fun ~key ~data -> f (key, data))
                       in
                       Hashtbl.to_alist t1, Hashtbl.to_alist t2))
                 ;;

                 let partition_map = Hashtbl.partition_map

                 let%test_unit "partition_map" =
                   for_each
                     "f result"
                     [%sexp_of: (data, data) Either.t]
                     (first_or_second sample_data)
                     (fun x ->
                     let callback _ = x in
                     let test_result =
                       [%test_result: (int * int) list * (int * int) list]
                     in
                     test_caller ~callback ~test_result (fun t f ->
                       let t1, t2 = Hashtbl.partition_map t ~f in
                       Hashtbl.to_alist t1, Hashtbl.to_alist t2))
                 ;;

                 let partition_mapi = Hashtbl.partition_mapi

                 let%test_unit "partition_mapi" =
                   for_each
                     "f result"
                     [%sexp_of: (data, data) Either.t]
                     (first_or_second sample_data)
                     (fun x ->
                     let callback _ = x in
                     let test_result =
                       [%test_result: (int * int) list * (int * int) list]
                     in
                     test_caller ~callback ~test_result (fun t f ->
                       let t1, t2 =
                         Hashtbl.partition_mapi t ~f:(fun ~key ~data -> f (key, data))
                       in
                       Hashtbl.to_alist t1, Hashtbl.to_alist t2))
                 ;;

                 let existsi = Hashtbl.existsi

                 let%test_unit "existsi" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result = [%test_result: bool] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.existsi t ~f:(fun ~key ~data -> f (key, data))))
                 ;;

                 let exists = Hashtbl.exists

                 let%test_unit "exists" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result = [%test_result: bool] in
                     test_caller ~callback ~test_result (fun t f -> Hashtbl.exists t ~f))
                 ;;

                 let for_alli = Hashtbl.for_alli

                 let%test_unit "for_alli" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result = [%test_result: bool] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.for_alli t ~f:(fun ~key ~data -> f (key, data))))
                 ;;

                 let for_all = Hashtbl.for_all

                 let%test_unit "for_all" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result = [%test_result: bool] in
                     test_caller ~callback ~test_result (fun t f -> Hashtbl.for_all t ~f))
                 ;;

                 let count = Hashtbl.count

                 let%test_unit "count" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result = [%test_result: int] in
                     test_caller ~callback ~test_result (fun t f -> Hashtbl.count t ~f))
                 ;;

                 let counti = Hashtbl.counti

                 let%test_unit "counti" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result = [%test_result: int] in
                     test_caller ~callback ~test_result (fun t f ->
                       Hashtbl.counti t ~f:(fun ~key ~data -> f (key, data))))
                 ;;

                 let merge = Hashtbl.merge

                 let%test_unit "merge" =
                   let callback (key, data) = Some (key, data) in
                   let test_result =
                     [%test_result: (int * (int * (int, int) merge_data)) list]
                   in
                   test_caller_2ts ~callback ~test_result (fun t1 t2 f ->
                     Hashtbl.merge t1 t2 ~f:(fun ~key data -> f (key, data))
                     |> Hashtbl.to_alist)
                 ;;

                 let equal = Hashtbl.equal
                 let similar = Hashtbl.similar

                 let%test_unit "equal" =
                   for_each "f result" [%sexp_of: bool] bools (fun bool ->
                     let callback _ = bool in
                     let test_result = [%test_result: bool] in
                     test_caller_2ts ~callback ~test_result (fun t1 t2 f ->
                       Hashtbl.equal (fun a b -> f (a, b)) t1 t2))
                 ;;

                 let sexp_of_t = Hashtbl.sexp_of_t

                 let%test_unit "sexp_of_t" =
                   let test_result = [%test_result: Sexp.t] in
                   test_caller_2fs
                     [%sexp_of: int]
                     [%sexp_of: int]
                     ~test_result
                     (fun t sexp_of_key sexp_of_data ->
                     Hashtbl.sexp_of_t sexp_of_key sexp_of_data t)
                 ;;

                 let invariant = Hashtbl.invariant

                 let%test_unit "invariant" =
                   let test_result = [%test_result: int Queue.t * int Queue.t] in
                   test_caller_2fs ignore ignore ~test_result (fun t f_key f_data ->
                     let keys = Queue.create () in
                     let data = Queue.create () in
                     Hashtbl.invariant
                       (fun k ->
                         Queue.enqueue keys k;
                         f_key k)
                       (fun d ->
                         Queue.enqueue data d;
                         f_data d)
                       t;
                     keys, data)
                 ;;

                 (* we do not test [validate], which should never raise externally, but which may go
          from pass to fail if the callback mutates, so it does not fit the normal
          criteria used by tests above *)

                 let validate = Hashtbl.validate

                 (* non-functions, and functions that neither mutate nor have callbacks *)

                 let choose = Hashtbl.choose
                 let choose_exn = Hashtbl.choose_exn
                 let choose_randomly = Hashtbl.choose_randomly
                 let choose_randomly_exn = Hashtbl.choose_randomly_exn
                 let create = Hashtbl.create
                 let create_mapped = Hashtbl.create_mapped
                 let create_with_key = Hashtbl.create_with_key
                 let create_with_key_exn = Hashtbl.create_with_key_exn
                 let create_with_key_or_error = Hashtbl.create_with_key_or_error
                 let group = Hashtbl.group
                 let of_alist = Hashtbl.of_alist
                 let of_alist_exn = Hashtbl.of_alist_exn
                 let of_alist_multi = Hashtbl.of_alist_multi
                 let of_alist_or_error = Hashtbl.of_alist_or_error
                 let of_alist_report_all_dups = Hashtbl.of_alist_report_all_dups
                 let sexp_of_key = Hashtbl.sexp_of_key
                 let copy = Hashtbl.copy
                 let keys = Hashtbl.keys
                 let data = Hashtbl.data
                 let length = Hashtbl.length
                 let capacity = Hashtbl.capacity
                 let is_empty = Hashtbl.is_empty
                 let mem = Hashtbl.mem
                 let find = Hashtbl.find
                 let find_exn = Hashtbl.find_exn
                 let find_multi = Hashtbl.find_multi
                 let to_alist = Hashtbl.to_alist
               end :
                 Hashtbl_for_testing with type ('a, 'b) t := ('a, 'b) Hashtbl.t))
  ;;
end

module Make (Hashtbl : Hashtbl_for_testing) = struct
  include Make_quickcheck_comparison_to_Map (Hashtbl)
  include Make_mutation_in_callbacks (Hashtbl)
end

let%test_module _ = (module Make (Hashtbl))

open! Hashtbl

let%test_unit _ =
  (* [sexp_of_t] output is sorted by key *)
  let module Table =
    Make (struct
      type t = int [@@deriving bin_io, compare, sexp]

      let hash (x : t) = if x >= 0 then x else ~-x
    end)
  in
  let t = Table.create () in
  for key = -10 to 10 do
    Hashtbl.add_exn t ~key ~data:()
  done;
  List.iter
    [ [%sexp_of: unit Table.t]; [%sexp_of: (int, unit) t] ]
    ~f:(fun sexp_of_t ->
      let list = t |> [%sexp_of: t] |> [%of_sexp: (int * unit) list] in
      assert (List.is_sorted list ~compare:(fun (i1, _) (i2, _) -> i1 - i2)))
;;

(* Make sure we follow the conventional type signature for [equal]. *)
let%test_unit _ =
  ignore ([%equal: string Int.Table.t] : string Int.Table.t -> string Int.Table.t -> bool)
;;
