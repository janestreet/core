open! Import
module List = List0
open Set_intf
module Merge_to_sequence_element = Merge_to_sequence_element

module type Elt_plain = Elt_plain
module type Elt = Elt
module type Elt_binable = Elt_binable

let to_comparator = Comparator.of_module
let of_comparator = Comparator.to_module

module For_quickcheck = struct
  let quickcheck_generator ~comparator elt_gen =
    Base_quickcheck.Generator.set_t_m (of_comparator comparator) elt_gen
  ;;

  let gen_tree ~comparator elt_gen =
    Base_quickcheck.Generator.set_tree_using_comparator ~comparator elt_gen
  ;;

  let quickcheck_observer elt_obs = Base_quickcheck.Observer.set_t elt_obs
  let obs_tree elt_obs = Base_quickcheck.Observer.set_tree elt_obs
  let quickcheck_shrinker elt_shr = Base_quickcheck.Shrinker.set_t elt_shr

  let shr_tree ~comparator elt_shr =
    Base_quickcheck.Shrinker.set_tree_using_comparator ~comparator elt_shr
  ;;
end

let quickcheck_generator m elt_gen =
  For_quickcheck.quickcheck_generator ~comparator:(to_comparator m) elt_gen
;;

let quickcheck_observer = For_quickcheck.quickcheck_observer
let quickcheck_shrinker = For_quickcheck.quickcheck_shrinker

module Tree = struct
  include Tree

  let to_map ~comparator t = Map.of_key_set (Set.Using_comparator.of_tree t ~comparator)
  let of_map_keys m = Set.Using_comparator.to_tree (Map.key_set m)

  let of_hash_set ~comparator hset =
    Hash_set.fold hset ~init:(empty ~comparator) ~f:(fun t x -> add t x ~comparator)
  ;;

  let of_hashtbl_keys ~comparator hashtbl =
    Hashtbl.fold hashtbl ~init:(empty ~comparator) ~f:(fun ~key:x ~data:_ t ->
      add t x ~comparator)
  ;;

  let quickcheck_generator = For_quickcheck.gen_tree
  let quickcheck_observer = For_quickcheck.obs_tree
  let quickcheck_shrinker = For_quickcheck.shr_tree
end

module Accessors = struct
  include (
    Set.Using_comparator :
      Set.Accessors_generic
        with type ('a, 'b) t := ('a, 'b) Set.t
        with type ('a, 'b) tree := ('a, 'b) Tree.t
        with type 'c cmp := 'c
        with type 'a elt := 'a
        with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t)
end

type 'a cmp = 'a
type 'a elt = 'a

include (
  struct
    include Set

    let of_tree m = Set.Using_comparator.of_tree ~comparator:(to_comparator m)
    let to_tree = Set.Using_comparator.to_tree
    let sexp_of_t = Set.Using_comparator.sexp_of_t
  end :
    sig
      type ('a, 'b) t = ('a, 'b) Set.t [@@deriving sexp_of]

      include
        Set.Creators_and_accessors_generic
          with type ('a, 'b, 'c) create_options :=
            ('a, 'b, 'c) Set.With_first_class_module.t
          with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Set.Without_comparator.t
          with type ('a, 'b) t := ('a, 'b) t
          with type ('a, 'b) set := ('a, 'b) t
          with type ('a, 'b) tree := ('a, 'b) Tree.t
          with type 'a cmp := 'a cmp
          with type 'a elt := 'a elt
          with module Named = Set.Named
    end)

let compare _ _ t1 t2 = compare_direct t1 t2

module Using_comparator = struct
  include (
    Set.Using_comparator :
      module type of struct
        include Set.Using_comparator
      end
      with module Tree := Set.Using_comparator.Tree)

  include For_quickcheck

  let of_map_keys = Map.key_set

  let of_hash_set ~comparator hset =
    of_tree ~comparator (Tree.of_hash_set hset ~comparator)
  ;;

  let of_hashtbl_keys ~comparator hashtbl =
    of_tree ~comparator (Tree.of_hashtbl_keys hashtbl ~comparator)
  ;;
end

let to_map = Map.of_key_set
let of_map_keys = Map.key_set
let hash_fold_direct = Using_comparator.hash_fold_direct
let comparator_s = Using_comparator.comparator_s
let comparator = Using_comparator.comparator
let of_hash_set m hset = Using_comparator.of_hash_set ~comparator:(to_comparator m) hset

let of_hashtbl_keys m hashtbl =
  Using_comparator.of_hashtbl_keys ~comparator:(to_comparator m) hashtbl
;;

module Creators (Elt : Comparator.S1) : sig
  type nonrec ('a, 'comparator) t_ = ('a Elt.t, Elt.comparator_witness) t
  type ('a, 'b) tree = ('a Elt.t, Elt.comparator_witness) Tree.t
  type 'a elt_ = 'a Elt.t
  type 'a cmp_ = Elt.comparator_witness

  val t_of_sexp : (Base.Sexp.t -> 'a Elt.t) -> Base.Sexp.t -> ('a, 'comparator) t_

  include
    Creators_generic
      with type ('a, 'b) t := ('a, 'b) t_
      with type ('a, 'b) set := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) tree
      with type 'a elt := 'a elt_
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Without_comparator.t
      with type 'a cmp := 'a cmp_
end = struct
  open Using_comparator

  type nonrec ('a, 'comparator) t_ = ('a Elt.t, Elt.comparator_witness) t
  type ('a, 'b) tree = ('a Elt.t, Elt.comparator_witness) Tree.t
  type 'a elt_ = 'a Elt.t
  type 'cmp cmp_ = Elt.comparator_witness

  let comparator = Elt.comparator
  let of_tree tree = of_tree ~comparator tree
  let of_sorted_array_unchecked array = of_sorted_array_unchecked ~comparator array

  let of_increasing_iterator_unchecked ~len ~f =
    of_increasing_iterator_unchecked ~comparator ~len ~f
  ;;

  let of_sorted_array array = of_sorted_array ~comparator array

  module M_empty = Empty_without_value_restriction (Elt)

  let empty = M_empty.empty
  let singleton e = singleton ~comparator e
  let union_list l = union_list ~comparator l
  let of_list l = of_list ~comparator l
  let of_sequence s = of_sequence ~comparator s
  let of_hash_set h = of_hash_set ~comparator h
  let of_hashtbl_keys h = of_hashtbl_keys ~comparator h
  let of_array a = of_array ~comparator a
  let stable_dedup_list xs = stable_dedup_list ~comparator xs [@alert "-deprecated"]
  let map t ~f = map ~comparator t ~f
  let filter_map t ~f = filter_map ~comparator t ~f

  let t_of_sexp a_of_sexp sexp =
    of_tree (Tree.t_of_sexp_direct a_of_sexp sexp ~comparator)
  ;;

  let of_map_keys = Map.key_set
  let quickcheck_generator elt = quickcheck_generator ~comparator elt
end

module Make_tree_S1 (Elt : Comparator.S1) = struct
  let comparator = Elt.comparator
  let empty = Tree.empty_without_value_restriction
  let singleton e = Tree.singleton ~comparator e
  let map t ~f = Tree.map t ~f ~comparator
  let filter_map t ~f = Tree.filter_map t ~f ~comparator
  let compare_direct t1 t2 = Tree.compare_direct ~comparator t1 t2
  let equal t1 t2 = Tree.equal t1 t2 ~comparator
  let of_list l = Tree.of_list l ~comparator
  let of_sequence s = Tree.of_sequence s ~comparator
  let of_hash_set h = Tree.of_hash_set h ~comparator
  let of_hashtbl_keys h = Tree.of_hashtbl_keys h ~comparator
  let of_array a = Tree.of_array a ~comparator
  let of_sorted_array_unchecked a = Tree.of_sorted_array_unchecked a ~comparator

  let of_increasing_iterator_unchecked ~len ~f =
    Tree.of_increasing_iterator_unchecked ~len ~f ~comparator
  ;;

  let of_sorted_array a = Tree.of_sorted_array a ~comparator
  let union_list l = Tree.union_list l ~comparator
  let stable_dedup_list xs = Tree.stable_dedup_list xs ~comparator [@alert "-deprecated"]
  let of_tree t = t
  let of_map_keys = Tree.of_map_keys
  let quickcheck_generator elt = For_quickcheck.gen_tree elt ~comparator
end

module Make_tree_plain (Elt : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) =
struct
  module Elt_S1 = Comparator.S_to_S1 (Elt)
  include Make_tree_S1 (Elt_S1)

  type t = (Elt.t, Elt.comparator_witness) Tree.t

  let compare t1 t2 = compare_direct t1 t2
  let sexp_of_t t = Tree.sexp_of_t Elt.sexp_of_t [%sexp_of: _] t

  module Provide_of_sexp
    (X : sig
      type t [@@deriving of_sexp]
    end
    with type t := Elt.t) =
  struct
    let t_of_sexp sexp =
      Tree.t_of_sexp_direct X.t_of_sexp sexp ~comparator:Elt_S1.comparator
    ;;
  end
end

module Make_tree (Elt : sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end) =
struct
  include Make_tree_plain (Elt)
  include Provide_of_sexp (Elt)
end

(* Don't use [of_sorted_array] to avoid the allocation of an intermediate array *)
let init_for_bin_prot ~len ~f ~comparator =
  let set = Using_comparator.of_increasing_iterator_unchecked ~comparator ~len ~f in
  if invariants set
  then set
  else
    Using_comparator.of_tree
      ~comparator
      (fold set ~init:(Tree.empty ~comparator) ~f:(fun acc elt ->
         if Tree.mem acc elt ~comparator
         then failwith "Set.bin_read_t: duplicate element in set"
         else Tree.add acc elt ~comparator))
;;

module Poly = struct
  module Elt = Comparator.Poly
  include Creators (Elt)

  type nonrec 'a t = ('a, Elt.comparator_witness) t

  include Accessors

  let compare _ t1 t2 = compare_direct t1 t2
  let sexp_of_t sexp_of_k t = sexp_of_t sexp_of_k [%sexp_of: _] t

  let t_sexp_grammar elt_grammar =
    Sexplib.Sexp_grammar.coerce (List.t_sexp_grammar elt_grammar)
  ;;

  include Bin_prot.Utils.Make_iterable_binable1 (struct
    type nonrec 'a t = 'a t
    type 'a el = 'a [@@deriving bin_io]

    let _ = bin_el

    let caller_identity =
      Bin_prot.Shape.Uuid.of_string "88bcc478-4992-11e6-a95d-ff4831acf410"
    ;;

    let module_name = Some "Core.Set"
    let length = length
    let iter t ~f = iter ~f:(fun key -> f key) t

    let init ~len ~next =
      init_for_bin_prot ~len ~f:(fun _ -> next ()) ~comparator:Comparator.Poly.comparator
    ;;
  end)

  module Tree = struct
    include Make_tree_S1 (Comparator.Poly)

    type 'elt t = ('elt, Comparator.Poly.comparator_witness) tree

    let sexp_of_t sexp_of_elt t = Tree.sexp_of_t sexp_of_elt [%sexp_of: _] t

    let t_of_sexp elt_of_sexp sexp =
      Tree.t_of_sexp_direct elt_of_sexp sexp ~comparator:Comparator.Poly.comparator
    ;;

    let t_sexp_grammar grammar = Sexplib.Sexp_grammar.coerce (List.t_sexp_grammar grammar)
  end
end

module type S_plain = S_plain
module type S = S
module type S_binable = S_binable

module Elt_bin_io = Elt_bin_io

module Provide_bin_io (Elt : Elt_bin_io.S) = Bin_prot.Utils.Make_iterable_binable (struct
  type nonrec t = (Elt.t, Elt.comparator_witness) t
  type el = Elt.t [@@deriving bin_io]

  let _ = bin_el

  let caller_identity =
    Bin_prot.Shape.Uuid.of_string "8989278e-4992-11e6-8f4a-6b89776b1e53"
  ;;

  let module_name = Some "Core.Set"
  let length = length
  let iter t ~f = iter ~f:(fun key -> f key) t

  let init ~len ~next =
    init_for_bin_prot ~len ~f:(fun _ -> next ()) ~comparator:Elt.comparator
  ;;
end)

module Provide_stable_witness (Elt : sig
  type t [@@deriving stable_witness]

  include Comparator.S with type t := t
end) =
struct
  (* The binary representation of set is used in the stable modules below, so it's
     assumed to be stable (if the elt is stable). *)
  let stable_witness : (Elt.t, Elt.comparator_witness) t Stable_witness.t =
    let (_ : Elt.t Stable_witness.t) = Elt.stable_witness in
    Stable_witness.assert_stable
  ;;
end

module Make_plain_using_comparator (Elt : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) =
struct
  module Elt = Elt
  module Elt_S1 = Comparator.S_to_S1 (Elt)
  include Creators (Elt_S1)

  type ('a, 'b) set = ('a, 'b) t
  type t = (Elt.t, Elt.comparator_witness) set

  include Accessors

  let compare t1 t2 = compare_direct t1 t2
  let sexp_of_t t = sexp_of_t Elt.sexp_of_t [%sexp_of: _] t

  module Diff = struct
    type derived_on = t
    type t = Elt.t Diffable.Set_diff.t [@@deriving sexp_of]

    let get = Diffable.Set_diff.get
    let apply_exn = Diffable.Set_diff.apply_exn
    let of_list_exn = Diffable.Set_diff.of_list_exn
  end

  module Provide_of_sexp
    (Elt : sig
      type t [@@deriving of_sexp]
    end
    with type t := Elt.t) =
  struct
    let t_of_sexp sexp = t_of_sexp Elt.t_of_sexp sexp

    module Diff = struct
      include Diff

      let t_of_sexp sexp = Diffable.Set_diff.t_of_sexp Elt.t_of_sexp sexp
    end
  end

  module Provide_hash (Elt : Hasher.S with type t := Elt.t) = struct
    let hash_fold_t state t = Using_comparator.hash_fold_direct Elt.hash_fold_t state t

    let hash t =
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_t (Ppx_hash_lib.Std.Hash.create ()) t)
    ;;
  end

  module Provide_bin_io
    (Elt' : sig
      type t [@@deriving bin_io]
    end
    with type t := Elt.t) =
  Provide_bin_io (struct
    include Elt
    include Elt'
  end)

  module Provide_stable_witness
    (Elt' : sig
      type t [@@deriving stable_witness]
    end
    with type t := Elt.t) =
  Provide_stable_witness (struct
    include Elt
    include Elt'
  end)

  let quickcheck_observer = quickcheck_observer
  let quickcheck_shrinker = quickcheck_shrinker
end

module Make_plain (Elt : Elt_plain) = Make_plain_using_comparator (struct
  include Elt
  include Comparator.Make (Elt)
end)

module Make_using_comparator (Elt_sexp : sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end) =
struct
  include Make_plain_using_comparator (Elt_sexp)
  module Elt = Elt_sexp
  include Provide_of_sexp (Elt)
end

module Make (Elt : Elt) = Make_using_comparator (struct
  include Elt
  include Comparator.Make (Elt)
end)

module Make_binable_using_comparator (Elt_bin_sexp : sig
  type t [@@deriving bin_io, sexp]

  include Comparator.S with type t := t
end) =
struct
  include Make_using_comparator (Elt_bin_sexp)
  module Elt = Elt_bin_sexp
  include Provide_bin_io (Elt)

  module Diff = struct
    include Diff

    type t = Elt.t Diffable.Set_diff.t [@@deriving bin_io]
  end
end

module Make_binable (Elt : Elt_binable) = Make_binable_using_comparator (struct
  include Elt
  include Comparator.Make (Elt)
end)

module For_deriving = struct
  module M = Set.M

  let bin_shape_m__t (type t c) (m : (t, c) Elt_bin_io.t) =
    let module M = Provide_bin_io ((val m)) in
    M.bin_shape_t
  ;;

  let bin_size_m__t (type t c) (m : (t, c) Elt_bin_io.t) =
    let module M = Provide_bin_io ((val m)) in
    M.bin_size_t
  ;;

  let bin_write_m__t (type t c) (m : (t, c) Elt_bin_io.t) =
    let module M = Provide_bin_io ((val m)) in
    M.bin_write_t
  ;;

  let bin_read_m__t (type t c) (m : (t, c) Elt_bin_io.t) =
    let module M = Provide_bin_io ((val m)) in
    M.bin_read_t
  ;;

  let __bin_read_m__t__ (type t c) (m : (t, c) Elt_bin_io.t) =
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
    (type t cmp)
    (module Elt : Quickcheck_generator_m with type t = t and type comparator_witness = cmp)
    =
    quickcheck_generator (module Elt) Elt.quickcheck_generator
  ;;

  let quickcheck_observer_m__t
    (type t cmp)
    (module Elt : Quickcheck_observer_m with type t = t and type comparator_witness = cmp)
    =
    quickcheck_observer Elt.quickcheck_observer
  ;;

  let quickcheck_shrinker_m__t
    (type t cmp)
    (module Elt : Quickcheck_shrinker_m with type t = t and type comparator_witness = cmp)
    =
    quickcheck_shrinker Elt.quickcheck_shrinker
  ;;

  module type For_deriving = Set.For_deriving

  include (Set : For_deriving with type ('a, 'b) t := ('a, 'b) t)
end

module For_deriving_stable = struct
  module type Stable_witness_m = sig
    include Comparator.S

    val stable_witness : t Stable_witness.t
  end

  let stable_witness_m__t
    (type t cmp)
    (module Elt : Stable_witness_m with type t = t and type comparator_witness = cmp)
    =
    let module M = Provide_stable_witness (Elt) in
    M.stable_witness
  ;;
end

include For_deriving

module Stable = struct
  module V1 = struct
    type nonrec ('a, 'cmp) t = ('a, 'cmp) t

    module type S = sig
      type elt
      type elt_comparator_witness
      type nonrec t = (elt, elt_comparator_witness) t

      include Stable_module_types.S0_without_comparator with type t := t

      include
        Diffable.S with type t := t and type Diff.t = elt Diffable.Set_diff.Stable.V1.t
    end

    include For_deriving
    include For_deriving_stable
    module Make (Elt : Stable_module_types.S0) = Make_binable_using_comparator (Elt)

    module With_stable_witness = struct
      module type S = sig
        include S

        val stable_witness : t Stable_witness.t
      end

      module Make (Elt_stable : Stable_module_types.With_stable_witness.S0) = struct
        include Make_binable_using_comparator (Elt_stable)
        include Provide_stable_witness (Elt_stable)
      end
    end
  end
end
