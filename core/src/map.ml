open! Import
open Map_intf
module List = List0

module Symmetric_diff_element = struct
  module Stable = struct
    module V1 = struct
      type ('k, 'v) t = 'k * [ `Left of 'v | `Right of 'v | `Unequal of 'v * 'v ]
      [@@deriving bin_io, compare, sexp, stable_witness]

      let%expect_test _ =
        print_endline [%bin_digest: (int, string) t];
        [%expect {| 00674be9fe8dfe9e9ad476067d7d8101 |}]
      ;;

      let map (k, diff) ~f1 ~f2 =
        let k = f1 k in
        let diff =
          match diff with
          | `Left v -> `Left (f2 v)
          | `Right v -> `Right (f2 v)
          | `Unequal (v1, v2) -> `Unequal (f2 v1, f2 v2)
        in
        k, diff
      ;;

      let map_data t ~f = map t ~f1:Fn.id ~f2:f

      let left (_key, diff) =
        match diff with
        | `Left x | `Unequal (x, _) -> Some x
        | `Right _ -> None
      ;;

      let right (_key, diff) =
        match diff with
        | `Right x | `Unequal (_, x) -> Some x
        | `Left _ -> None
      ;;
    end
  end

  include Stable.V1
end

module Merge_element = Base.Map.Merge_element
module Continue_or_stop = Base.Map.Continue_or_stop
module Finished_or_unfinished = Base.Map.Finished_or_unfinished

let to_comparator = Comparator.of_module
let of_comparator = Comparator.to_module

module For_quickcheck = struct
  let gen_tree ~comparator k_gen v_gen =
    Base_quickcheck.Generator.map_tree_using_comparator ~comparator k_gen v_gen
  ;;

  let quickcheck_generator ~comparator k_gen v_gen =
    Base_quickcheck.Generator.map_t_m (of_comparator comparator) k_gen v_gen
  ;;

  let obs_tree k_obs v_obs = Base_quickcheck.Observer.map_tree k_obs v_obs

  let shr_tree ~comparator k_shr v_shr =
    Base_quickcheck.Shrinker.map_tree_using_comparator ~comparator k_shr v_shr
  ;;
end

let quickcheck_generator = Base_quickcheck.Generator.map_t_m
let quickcheck_observer = Base_quickcheck.Observer.map_t
let quickcheck_shrinker = Base_quickcheck.Shrinker.map_t

module Using_comparator = struct
  include Map.Using_comparator
  include For_quickcheck

  let of_hashtbl_exn ~comparator hashtbl =
    match of_iteri ~comparator ~iteri:(Hashtbl.iteri hashtbl) with
    | `Ok map -> map
    | `Duplicate_key key ->
      Error.failwiths
        ~here:[%here]
        "Map.of_hashtbl_exn: duplicate key"
        key
        comparator.sexp_of_t
  ;;

  let tree_of_hashtbl_exn ~comparator hashtbl =
    to_tree (of_hashtbl_exn ~comparator hashtbl)
  ;;

  let key_set ~comparator t =
    Base.Set.Using_comparator.of_sorted_array_unchecked
      ~comparator
      (List.to_array (keys t))
  ;;

  let key_set_of_tree ~comparator t = key_set ~comparator (of_tree ~comparator t)

  let of_key_set key_set ~f =
    of_sorted_array_unchecked
      ~comparator:(Base.Set.comparator key_set)
      (Array.map (Base.Set.to_array key_set) ~f:(fun key -> key, f key))
  ;;

  let tree_of_key_set key_set ~f = to_tree (of_key_set key_set ~f)
end

module Accessors = struct
  include (
    Map.Using_comparator :
      Map.Accessors_generic
        with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t
        with type ('a, 'b, 'c) t := ('a, 'b, 'c) Map.t
        with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
        with type 'k key := 'k
        with type 'c cmp := 'c)

  let validate ~name f t = Validate.alist ~name f (to_alist t)
  let validatei ~name f t = Validate.list ~name:(Fn.compose name fst) f (to_alist t)
  let quickcheck_observer k v = quickcheck_observer k v
  let quickcheck_shrinker k v = quickcheck_shrinker k v
  let key_set t = Using_comparator.key_set t ~comparator:(Using_comparator.comparator t)
end

let key_set t = Using_comparator.key_set ~comparator:(Using_comparator.comparator t) t
let of_key_set = Using_comparator.of_key_set
let hash_fold_direct = Using_comparator.hash_fold_direct
let comparator = Using_comparator.comparator
let comparator_s = Base.Map.comparator_s

type 'k key = 'k
type 'c cmp = 'c

include (
  struct
    include Map

    let validate ~name f t = Validate.alist ~name f (to_alist t)
    let validatei ~name f t = Validate.list ~name:(Fn.compose name fst) f (to_alist t)
    let of_tree m = Map.Using_comparator.of_tree ~comparator:(to_comparator m)
    let to_tree = Map.Using_comparator.to_tree
  end :
    sig
      type ('a, 'b, 'c) t = ('a, 'b, 'c) Map.t

      include
        Map.Creators_and_accessors_generic
          with type ('a, 'b, 'c) create_options :=
            ('a, 'b, 'c) Map.With_first_class_module.t
          with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Map.Without_comparator.t
          with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
          with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
          with type 'k key := 'k key
          with type 'c cmp := 'c cmp

      val validate
        :  name:('k -> string)
        -> 'v Validate.check
        -> ('k, 'v, _) t Validate.check

      val validatei
        :  name:('k key -> string)
        -> ('k key * 'v) Validate.check
        -> ('k, 'v, _) t Validate.check
    end)

module Empty_without_value_restriction = Using_comparator.Empty_without_value_restriction

let find_or_error t key =
  let comparator = comparator t in
  match find t key with
  | Some data -> Ok data
  | None ->
    let sexp_of_key = comparator.sexp_of_t in
    Or_error.error_s [%message "key not found" ~_:(key : key)]
;;

let merge_skewed = Map.merge_skewed
let of_hashtbl_exn m t = Using_comparator.of_hashtbl_exn ~comparator:(to_comparator m) t

module Creators (Key : Comparator.S1) : sig
  type ('a, 'b, 'c) t_ = ('a Key.t, 'b, Key.comparator_witness) t
  type ('a, 'b, 'c) tree = ('a, 'b, Key.comparator_witness) Tree.t

  val t_of_sexp
    :  (Base.Sexp.t -> 'a Key.t)
    -> (Base.Sexp.t -> 'b)
    -> Base.Sexp.t
    -> ('a, 'b, _) t_

  include
    Creators_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
      with type 'a key := 'a Key.t
      with type 'a cmp := Key.comparator_witness
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Without_comparator.t
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t
end = struct
  let comparator = Key.comparator

  type ('a, 'b, 'c) t_ = ('a Key.t, 'b, Key.comparator_witness) t
  type ('a, 'b, 'c) tree = ('a, 'b, Key.comparator_witness) Tree.t

  module M_empty = Empty_without_value_restriction (Key)

  let empty = M_empty.empty
  let of_tree tree = Using_comparator.of_tree ~comparator tree
  let singleton k v = Using_comparator.singleton ~comparator k v

  let of_sorted_array_unchecked array =
    Using_comparator.of_sorted_array_unchecked ~comparator array
  ;;

  let of_sorted_array array = Using_comparator.of_sorted_array ~comparator array

  let of_increasing_iterator_unchecked ~len ~f =
    Using_comparator.of_increasing_iterator_unchecked ~comparator ~len ~f
  ;;

  let of_increasing_sequence seq = Using_comparator.of_increasing_sequence ~comparator seq
  let of_sequence seq = Using_comparator.of_sequence ~comparator seq
  let of_sequence_or_error seq = Using_comparator.of_sequence_or_error ~comparator seq
  let of_sequence_exn seq = Using_comparator.of_sequence_exn ~comparator seq
  let of_sequence_multi seq = Using_comparator.of_sequence_multi ~comparator seq

  let of_sequence_fold seq ~init ~f =
    Using_comparator.of_sequence_fold ~comparator seq ~init ~f
  ;;

  let of_sequence_reduce seq ~f = Using_comparator.of_sequence_reduce ~comparator seq ~f

  let of_list_with_key list ~get_key =
    Using_comparator.of_list_with_key ~comparator list ~get_key
  ;;

  let of_list_with_key_or_error list ~get_key =
    Using_comparator.of_list_with_key_or_error ~comparator list ~get_key
  ;;

  let of_list_with_key_exn list ~get_key =
    Using_comparator.of_list_with_key_exn ~comparator list ~get_key
  ;;

  let of_list_with_key_multi list ~get_key =
    Using_comparator.of_list_with_key_multi ~comparator list ~get_key
  ;;

  let of_list_with_key_fold list ~get_key ~init ~f =
    Using_comparator.of_list_with_key_fold ~comparator list ~get_key ~init ~f
  ;;

  let of_list_with_key_reduce list ~get_key ~f =
    Using_comparator.of_list_with_key_reduce ~comparator list ~get_key ~f
  ;;

  let of_alist alist = Using_comparator.of_alist ~comparator alist
  let of_alist_or_error alist = Using_comparator.of_alist_or_error ~comparator alist
  let of_alist_exn alist = Using_comparator.of_alist_exn ~comparator alist
  let of_hashtbl_exn hashtbl = Using_comparator.of_hashtbl_exn ~comparator hashtbl
  let of_alist_multi alist = Using_comparator.of_alist_multi ~comparator alist

  let of_alist_fold alist ~init ~f =
    Using_comparator.of_alist_fold ~comparator alist ~init ~f
  ;;

  let of_alist_reduce alist ~f = Using_comparator.of_alist_reduce ~comparator alist ~f
  let of_iteri ~iteri = Using_comparator.of_iteri ~comparator ~iteri
  let of_iteri_exn ~iteri = Using_comparator.of_iteri_exn ~comparator ~iteri

  let t_of_sexp k_of_sexp v_of_sexp sexp =
    Using_comparator.t_of_sexp_direct ~comparator k_of_sexp v_of_sexp sexp
  ;;

  let of_key_set key_set ~f = Using_comparator.of_key_set key_set ~f
  let map_keys t ~f = Using_comparator.map_keys ~comparator t ~f
  let map_keys_exn t ~f = Using_comparator.map_keys_exn ~comparator t ~f
  let transpose_keys t = Using_comparator.transpose_keys ~comparator t

  let quickcheck_generator gen_k gen_v =
    Using_comparator.quickcheck_generator ~comparator gen_k gen_v
  ;;
end

module Make_tree_S1 (Key : Comparator.S1) = struct
  open Tree

  let comparator = Key.comparator
  let sexp_of_t = sexp_of_t
  let t_of_sexp a b c = t_of_sexp_direct a b c ~comparator
  let empty = empty_without_value_restriction
  let of_tree tree = tree
  let singleton a = singleton a ~comparator
  let of_sorted_array_unchecked a = of_sorted_array_unchecked a ~comparator
  let of_sorted_array a = of_sorted_array a ~comparator

  let of_increasing_iterator_unchecked ~len ~f =
    of_increasing_iterator_unchecked ~len ~f ~comparator
  ;;

  let of_increasing_sequence seq = of_increasing_sequence ~comparator seq
  let of_sequence s = of_sequence s ~comparator
  let of_sequence_or_error s = of_sequence_or_error s ~comparator
  let of_sequence_exn s = of_sequence_exn s ~comparator
  let of_sequence_multi s = of_sequence_multi s ~comparator
  let of_sequence_fold s ~init ~f = of_sequence_fold s ~init ~f ~comparator
  let of_sequence_reduce s ~f = of_sequence_reduce s ~f ~comparator
  let of_alist a = of_alist a ~comparator
  let of_alist_or_error a = of_alist_or_error a ~comparator
  let of_alist_exn a = of_alist_exn a ~comparator
  let of_hashtbl_exn a = Using_comparator.tree_of_hashtbl_exn a ~comparator
  let of_alist_multi a = of_alist_multi a ~comparator
  let of_alist_fold a ~init ~f = of_alist_fold a ~init ~f ~comparator
  let of_alist_reduce a ~f = of_alist_reduce a ~f ~comparator
  let of_list_with_key l ~get_key = of_list_with_key l ~get_key ~comparator

  let of_list_with_key_or_error l ~get_key =
    of_list_with_key_or_error l ~get_key ~comparator
  ;;

  let of_list_with_key_exn l ~get_key = of_list_with_key_exn l ~get_key ~comparator
  let of_list_with_key_multi l ~get_key = of_list_with_key_multi l ~get_key ~comparator

  let of_list_with_key_fold l ~get_key ~init ~f =
    of_list_with_key_fold l ~get_key ~init ~f ~comparator
  ;;

  let of_list_with_key_reduce l ~get_key ~f =
    of_list_with_key_reduce l ~get_key ~f ~comparator
  ;;

  let of_iteri ~iteri = of_iteri ~iteri ~comparator
  let of_iteri_exn ~iteri = of_iteri_exn ~iteri ~comparator
  let of_key_set = Using_comparator.tree_of_key_set
  let to_tree t = t
  let invariants a = invariants a ~comparator
  let is_empty a = is_empty a
  let length a = length a
  let set a ~key ~data = set a ~key ~data ~comparator
  let add a ~key ~data = add a ~key ~data ~comparator
  let add_exn a ~key ~data = add_exn a ~key ~data ~comparator
  let add_multi a ~key ~data = add_multi a ~key ~data ~comparator
  let remove_multi a b = remove_multi a b ~comparator
  let find_multi a b = find_multi a b ~comparator
  let change a b ~f = change a b ~f ~comparator
  let update a b ~f = update a b ~f ~comparator
  let find_exn a b = find_exn a b ~comparator
  let find a b = find a b ~comparator
  let remove a b = remove a b ~comparator
  let mem a b = mem a b ~comparator
  let iter_keys = iter_keys
  let iter = iter
  let iteri = iteri
  let iteri_until = iteri_until
  let iter2 a b ~f = iter2 a b ~f ~comparator
  let map = map
  let mapi = mapi
  let fold = fold
  let fold_until = fold_until
  let fold_right = fold_right
  let fold2 a b ~init ~f = fold2 a b ~init ~f ~comparator
  let filter_keys a ~f = filter_keys a ~f
  let filter a ~f = filter a ~f
  let filteri a ~f = filteri a ~f
  let filter_map a ~f = filter_map a ~f
  let filter_mapi a ~f = filter_mapi a ~f
  let partition_mapi t ~f = partition_mapi t ~f
  let partition_map t ~f = partition_map t ~f
  let partitioni_tf t ~f = partitioni_tf t ~f
  let partition_tf t ~f = partition_tf t ~f
  let combine_errors t = combine_errors t ~comparator
  let unzip = unzip
  let compare_direct a b c = compare_direct a b c ~comparator
  let equal a b c = equal a b c ~comparator
  let keys = keys
  let data = data
  let to_alist = to_alist
  let validate ~name f t = Validate.alist ~name f (to_alist t)
  let validatei ~name f t = Validate.list ~name:(Fn.compose name fst) f (to_alist t)
  let symmetric_diff a b ~data_equal = symmetric_diff a b ~data_equal ~comparator

  let fold_symmetric_diff a b ~data_equal ~init ~f =
    fold_symmetric_diff a b ~data_equal ~f ~init ~comparator
  ;;

  let merge a b ~f = merge a b ~f ~comparator
  let merge_disjoint_exn a b = merge_disjoint_exn a b ~comparator
  let merge_skewed a b ~combine = merge_skewed a b ~combine ~comparator
  let min_elt = min_elt
  let min_elt_exn = min_elt_exn
  let max_elt = max_elt
  let max_elt_exn = max_elt_exn
  let for_all = for_all
  let for_alli = for_alli
  let exists = exists
  let existsi = existsi
  let count = count
  let counti = counti
  let sum = sum
  let sumi = sumi
  let split a b = split a b ~comparator
  let split_le_gt a b = split_le_gt a b ~comparator
  let split_lt_ge a b = split_lt_ge a b ~comparator
  let append ~lower_part ~upper_part = append ~lower_part ~upper_part ~comparator

  let subrange t ~lower_bound ~upper_bound =
    subrange t ~lower_bound ~upper_bound ~comparator
  ;;

  let fold_range_inclusive t ~min ~max ~init ~f =
    fold_range_inclusive t ~min ~max ~init ~f ~comparator
  ;;

  let range_to_alist t ~min ~max = range_to_alist t ~min ~max ~comparator
  let closest_key a b c = closest_key a b c ~comparator
  let nth = nth
  let nth_exn = nth_exn
  let rank a b = rank a b ~comparator

  let to_sequence ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t =
    to_sequence ~comparator ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t
  ;;

  let binary_search t ~compare how v = binary_search ~comparator t ~compare how v

  let binary_search_segmented t ~segment_of how =
    binary_search_segmented ~comparator t ~segment_of how
  ;;

  let binary_search_subrange t ~compare ~lower_bound ~upper_bound =
    binary_search_subrange ~comparator t ~compare ~lower_bound ~upper_bound
  ;;

  module Make_applicative_traversals (A : Applicative.Lazy_applicative) = struct
    module Traversals = Make_applicative_traversals (A)

    let mapi = Traversals.mapi
    let filter_mapi = Traversals.filter_mapi
  end

  let key_set t = Using_comparator.key_set_of_tree ~comparator t
  let map_keys t ~f = map_keys t ~f ~comparator
  let map_keys_exn t ~f = map_keys_exn t ~f ~comparator
  let transpose_keys t = transpose_keys ~comparator ~comparator t
  let quickcheck_generator k v = For_quickcheck.gen_tree ~comparator k v
  let quickcheck_observer k v = For_quickcheck.obs_tree k v
  let quickcheck_shrinker k v = For_quickcheck.shr_tree ~comparator k v
end

module Make_tree_plain (Key : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) =
struct
  module Key_S1 = Comparator.S_to_S1 (Key)
  include Make_tree_S1 (Key_S1)

  type +'v t = (Key.t, 'v, Key.comparator_witness) Tree.t

  let sexp_of_t sexp_of_v t = sexp_of_t Key.sexp_of_t sexp_of_v [%sexp_of: _] t

  module Provide_of_sexp
    (X : sig
      type t [@@deriving of_sexp]
    end
    with type t := Key.t) =
  struct
    let t_of_sexp v_of_sexp sexp = t_of_sexp X.t_of_sexp v_of_sexp sexp
  end
end

module Make_tree (Key : sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end) =
struct
  include Make_tree_plain (Key)
  include Provide_of_sexp (Key)
end

(* Don't use [of_sorted_array] to avoid the allocation of an intermediate array *)
let init_for_bin_prot ~len ~f ~comparator =
  let map = Using_comparator.of_increasing_iterator_unchecked ~len ~f ~comparator in
  if invariants map
  then map
  else (
    (* The invariants are broken, but we can still traverse the structure. *)
    match Using_comparator.of_iteri ~iteri:(iteri map) ~comparator with
    | `Ok map -> map
    | `Duplicate_key _key -> failwith "Map.bin_read_t: duplicate element in map")
;;

module Poly = struct
  include Creators (Comparator.Poly)

  type ('a, 'b, 'c) map = ('a, 'b, 'c) t
  type ('k, 'v) t = ('k, 'v, Comparator.Poly.comparator_witness) map
  type comparator_witness = Comparator.Poly.comparator_witness

  include Accessors

  let compare _ cmpv t1 t2 = compare_direct cmpv t1 t2

  let sexp_of_t sexp_of_k sexp_of_v t =
    Using_comparator.sexp_of_t sexp_of_k sexp_of_v [%sexp_of: _] t
  ;;

  let t_sexp_grammar k_grammar v_grammar =
    Sexplib.Sexp_grammar.coerce (List.Assoc.t_sexp_grammar k_grammar v_grammar)
  ;;

  include Bin_prot.Utils.Make_iterable_binable2 (struct
    type nonrec ('a, 'b) t = ('a, 'b) t
    type ('a, 'b) el = 'a * 'b [@@deriving bin_io]

    let _ = bin_el

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "b7d7b1a0-4992-11e6-8a32-bbb221fa025c"
    ;;

    let module_name = Some "Core.Map"
    let length = length
    let iter t ~f = iteri t ~f:(fun ~key ~data -> f (key, data))

    let init ~len ~next =
      init_for_bin_prot ~len ~f:(fun _ -> next ()) ~comparator:Comparator.Poly.comparator
    ;;
  end)

  module Tree = struct
    include Make_tree_S1 (Comparator.Poly)

    type ('k, +'v) t = ('k, 'v, Comparator.Poly.comparator_witness) tree
    type comparator_witness = Comparator.Poly.comparator_witness

    let sexp_of_t sexp_of_k sexp_of_v t = sexp_of_t sexp_of_k sexp_of_v [%sexp_of: _] t

    let t_sexp_grammar k_grammar v_grammar =
      Sexplib.Sexp_grammar.coerce (List.Assoc.t_sexp_grammar k_grammar v_grammar)
    ;;
  end
end

module type Key_plain = Key_plain
module type Key = Key
module type Key_binable = Key_binable
module type Key_hashable = Key_hashable
module type Key_binable_hashable = Key_binable_hashable
module type S_plain = S_plain
module type S = S
module type S_binable = S_binable

module Key_bin_io = Key_bin_io

module Provide_bin_io (Key : Key_bin_io.S) = Bin_prot.Utils.Make_iterable_binable1 (struct
  module Key = Key

  type nonrec 'v t = (Key.t, 'v, Key.comparator_witness) t
  type 'v el = Key.t * 'v [@@deriving bin_io]

  let _ = bin_el

  let caller_identity =
    Bin_prot.Shape.Uuid.of_string "dfb300f8-4992-11e6-9c15-73a2ac6b815c"
  ;;

  let module_name = Some "Core.Map"
  let length = length
  let iter t ~f = iteri t ~f:(fun ~key ~data -> f (key, data))

  let init ~len ~next =
    init_for_bin_prot ~len ~f:(fun _ -> next ()) ~comparator:Key.comparator
  ;;
end)

module Provide_stable_witness (Key : sig
  type t [@@deriving stable_witness]

  include Comparator.S with type t := t
end) =
struct
  (* The binary representation of map is used in the stable modules below, so it's
     assumed to be stable (if the key and data are stable) . *)
  let stable_witness (type data) (_data_stable_witness : data Stable_witness.t)
    : (Key.t, data, Key.comparator_witness) t Stable_witness.t
    =
    let (_ : Key.t Stable_witness.t) = Key.stable_witness in
    Stable_witness.assert_stable
  ;;
end

module Make_plain_using_comparator (Key : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) =
struct
  module Key = Key
  module Key_S1 = Comparator.S_to_S1 (Key)
  include Creators (Key_S1)

  type key = Key.t
  type ('a, 'b, 'c) map = ('a, 'b, 'c) t
  type 'v t = (key, 'v, Key.comparator_witness) map

  include Accessors

  let compare cmpv t1 t2 = compare_direct cmpv t1 t2

  let sexp_of_t sexp_of_v t =
    Using_comparator.sexp_of_t Key.sexp_of_t sexp_of_v [%sexp_of: _] t
  ;;

  module Diff = struct
    type 'a derived_on = 'a t
    type ('a, 'a_diff) t = (Key.t, 'a, 'a_diff) Diffable.Map_diff.t [@@deriving sexp_of]

    let get = Diffable.Map_diff.get
    let apply_exn = Diffable.Map_diff.apply_exn
    let of_list_exn = Diffable.Map_diff.of_list_exn
  end

  module Provide_of_sexp
    (Key : sig
      type t [@@deriving of_sexp]
    end
    with type t := Key.t) =
  struct
    let t_of_sexp v_of_sexp sexp = t_of_sexp Key.t_of_sexp v_of_sexp sexp

    module Diff = struct
      include Diff

      let t_of_sexp v_of_sexp v_diff_of_sexp sexp =
        Diffable.Map_diff.t_of_sexp Key.t_of_sexp v_of_sexp v_diff_of_sexp sexp
      ;;
    end
  end

  module Provide_hash (Key' : Hasher.S with type t := Key.t) = struct
    let hash_fold_t (type a) hash_fold_data state (t : a t) =
      Using_comparator.hash_fold_direct Key'.hash_fold_t hash_fold_data state t
    ;;
  end

  module Provide_bin_io
    (Key' : sig
      type t [@@deriving bin_io]
    end
    with type t := Key.t) =
  Provide_bin_io (struct
    include Key
    include Key'
  end)

  module Provide_stable_witness
    (Key' : sig
      type t [@@deriving stable_witness]
    end
    with type t := Key.t) =
  Provide_stable_witness (struct
    include Key
    include Key'
  end)
end

module Make_plain (Key : Key_plain) = Make_plain_using_comparator (struct
  include Key
  include Comparator.Make (Key)
end)

module Make_using_comparator (Key_sexp : sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end) =
struct
  include Make_plain_using_comparator (Key_sexp)
  module Key = Key_sexp
  include Provide_of_sexp (Key)

  module _ = struct
    include Tree
    include Provide_of_sexp (Key)
  end
end

module Make (Key : Key) = Make_using_comparator (struct
  include Key
  include Comparator.Make (Key)
end)

module Make_binable_using_comparator (Key_bin_sexp : sig
  type t [@@deriving bin_io, sexp]

  include Comparator.S with type t := t
end) =
struct
  include Make_using_comparator (Key_bin_sexp)
  module Key = Key_bin_sexp
  include Provide_bin_io (Key)

  module Diff = struct
    include Diff

    type ('a, 'a_diff) t = (Key.t, 'a, 'a_diff) Diffable.Map_diff.t
    [@@deriving bin_io, sexp]
  end
end

module Make_binable (Key : Key_binable) = Make_binable_using_comparator (struct
  include Key
  include Comparator.Make (Key)
end)

module For_deriving = struct
  module M = Map.M

  let bin_shape_m__t (type t c) (m : (t, c) Key_bin_io.t) =
    let module M = Provide_bin_io ((val m)) in
    M.bin_shape_t
  ;;

  let bin_size_m__t (type t c) (m : (t, c) Key_bin_io.t) =
    let module M = Provide_bin_io ((val m)) in
    M.bin_size_t
  ;;

  let bin_write_m__t (type t c) (m : (t, c) Key_bin_io.t) =
    let module M = Provide_bin_io ((val m)) in
    M.bin_write_t
  ;;

  let bin_read_m__t (type t c) (m : (t, c) Key_bin_io.t) =
    let module M = Provide_bin_io ((val m)) in
    M.bin_read_t
  ;;

  let __bin_read_m__t__ (type t c) (m : (t, c) Key_bin_io.t) =
    let module M = Provide_bin_io ((val m)) in
    M.__bin_read_t__
  ;;

  module type Quickcheck_generator_m = sig
    include Comparator.S

    val quickcheck_generator : t Quickcheck.Generator.t
  end

  module type Quickcheck_observer_m = sig
    include Comparator.S

    val quickcheck_observer : t Quickcheck.Observer.t
  end

  module type Quickcheck_shrinker_m = sig
    include Comparator.S

    val quickcheck_shrinker : t Quickcheck.Shrinker.t
  end

  let quickcheck_generator_m__t
    (type k cmp)
    (module Key : Quickcheck_generator_m with type t = k and type comparator_witness = cmp)
    v_generator
    =
    quickcheck_generator (module Key) Key.quickcheck_generator v_generator
  ;;

  let quickcheck_observer_m__t
    (type k cmp)
    (module Key : Quickcheck_observer_m with type t = k and type comparator_witness = cmp)
    v_observer
    =
    quickcheck_observer Key.quickcheck_observer v_observer
  ;;

  let quickcheck_shrinker_m__t
    (type k cmp)
    (module Key : Quickcheck_shrinker_m with type t = k and type comparator_witness = cmp)
    v_shrinker
    =
    quickcheck_shrinker Key.quickcheck_shrinker v_shrinker
  ;;

  module type For_deriving = Map.For_deriving

  include (Map : For_deriving with type ('a, 'b, 'c) t := ('a, 'b, 'c) t)
end

module For_deriving_stable = struct
  module type Stable_witness_m = sig
    include Comparator.S

    val stable_witness : t Stable_witness.t
  end

  let stable_witness_m__t
    (type k cmp)
    (module Key : Stable_witness_m with type t = k and type comparator_witness = cmp)
    =
    let module M = Provide_stable_witness (Key) in
    M.stable_witness
  ;;
end

include For_deriving

module Tree = struct
  include Tree

  let validate ~name f t = Validate.alist ~name f (to_alist t)
  let validatei ~name f t = Validate.list ~name:(Fn.compose name fst) f (to_alist t)
  let of_hashtbl_exn = Using_comparator.tree_of_hashtbl_exn
  let key_set = Using_comparator.key_set_of_tree
  let of_key_set = Using_comparator.tree_of_key_set
  let quickcheck_generator ~comparator k v = For_quickcheck.gen_tree ~comparator k v
  let quickcheck_observer k v = For_quickcheck.obs_tree k v
  let quickcheck_shrinker ~comparator k v = For_quickcheck.shr_tree ~comparator k v
end

module Stable = struct
  module V1 = struct
    type nonrec ('k, 'v, 'cmp) t = ('k, 'v, 'cmp) t

    module type S = sig
      type key
      type comparator_witness
      type nonrec 'a t = (key, 'a, comparator_witness) t

      include Stable_module_types.S1 with type 'a t := 'a t

      include
        Diffable.S1
          with type 'a t := 'a t
           and type ('a, 'a_diff) Diff.t =
            (key, 'a, 'a_diff) Diffable.Map_diff.Stable.V1.t
    end

    include For_deriving
    include For_deriving_stable
    module Make (Key : Stable_module_types.S0) = Make_binable_using_comparator (Key)

    module With_stable_witness = struct
      module type S = sig
        include S

        val stable_witness : 'a Stable_witness.t -> 'a t Stable_witness.t
      end

      module Make (Key_stable : Stable_module_types.With_stable_witness.S0) = struct
        include Make (Key_stable)
        include Provide_stable_witness (Key_stable)
      end
    end
  end

  module Symmetric_diff_element = Symmetric_diff_element.Stable
end
