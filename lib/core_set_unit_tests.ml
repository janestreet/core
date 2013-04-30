module Caml_set = Set
open Std
open Core_set_intf

module Unit_tests
  (Elt : sig
    type 'a t with sexp

    val of_int : int -> int t
    val to_int : int t -> int
  end)
  (Set : sig
    type ('a, 'b) t_
    type ('a, 'b) set
    type ('a, 'b) tree

    type ('a, 'b, 'c) create_options

    include Creators_generic
      with type ('a, 'b) t           := ('a, 'b) t_
      with type ('a, 'b) set         := ('a, 'b) set
      with type ('a, 'b) tree        := ('a, 'b) tree
      with type 'a elt               := 'a Elt.t
      with type ('a, 'b, 'c) options := ('a, 'b, 'c) create_options

    val simplify_creator : (int, Int.comparator, 'c) create_options -> 'c

    type ('a, 'b, 'c) access_options

    include Accessors_generic
      with type ('a, 'b) t           := ('a, 'b) t_
      with type ('a, 'b) tree        := ('a, 'b) tree
      with type 'a elt               := 'a Elt.t
      with type ('a, 'b, 'c) options := ('a, 'b, 'c) access_options

    val simplify_accessor : (int, Int.comparator, 'c) access_options -> 'c

    val kind : [ `Set | `Tree ]
  end)

  : Creators_and_accessors_generic = struct

  module Set = struct
    include Set
    let add            = simplify_accessor add
    (* let remove         = simplify_accessor remove *)
    let mem            = simplify_accessor mem
    (* let filter         = simplify_accessor filter *)
    (* let compare_direct = simplify_accessor compare_direct *)
    let equal          = simplify_accessor equal
    let inter          = simplify_accessor inter
    let subset         = simplify_accessor subset
    let iter2          = simplify_accessor iter2
    let invariants     = simplify_accessor invariants
    let to_list    = to_list
    let to_array   = to_array

    let empty ()       = simplify_creator empty
    let singleton      = simplify_creator singleton
    let of_list        = simplify_creator of_list
    let of_sorted_array = simplify_creator of_sorted_array
    let of_sorted_array_unchecked = simplify_creator of_sorted_array_unchecked
    (* let of_tree        = simplify_creator of_tree *)
  end

  type ('a, 'b) t = Unit_test_follows
  type ('a, 'b) tree = ('a, 'b) t
  type ('a, 'b) set = ('a, 'b) t
  type 'a elt
  type ('a, 'b, 'c) options = ('a, 'b, 'c) Without_comparator.t

  module Elt = struct
    open Elt
    let of_int = of_int
    let to_int = to_int

    module T = struct
      type t = int Elt.t with sexp
      let compare t t' = Pervasives.compare (to_int t) (to_int t')
      let equal t t' = compare t t' = 0
    end
    include T

    let samples = List.dedup ~compare (List.init 10 ~f:(fun i -> of_int (i + 1)))
    let absent = of_int 0
    let present = of_int 1
    let () = assert(List.mem ~equal samples present)
    let () = assert(not (List.mem ~equal samples absent))
  end

  let set_empty = Set.empty ()
  let set_nonempty = Set.of_list Elt.samples

  let add _               = assert false
  let of_list _           = assert false
  let mem _               = assert false

  TEST = List.for_all Elt.samples ~f:(fun e -> Set.mem set_nonempty e)

  let is_empty _          = assert false
  TEST = Set.is_empty set_empty
  TEST = not (Set.is_empty set_nonempty)

  TEST =
    let set' = List.fold Elt.samples ~init:(Set.empty ()) ~f:Set.add in
    Set.equal set_nonempty set'
  ;;

  let inter _             = assert false
  TEST = Set.is_empty (Set.inter set_empty set_nonempty)
  TEST = Set.is_empty (Set.inter set_nonempty set_empty)
  TEST =
    let single = Set.singleton Elt.absent in
    Set.equal single (Set.inter single (Set.add set_nonempty Elt.absent))
  ;;
  TEST = Set.equal set_nonempty (Set.inter set_nonempty set_nonempty)

  let subset _            = assert false
  TEST = Set.subset set_empty set_nonempty
  TEST = not (Set.subset set_nonempty set_empty)
  TEST = Set.subset set_nonempty set_nonempty
  TEST = Set.subset set_empty set_empty
  TEST = not (Set.subset set_nonempty (Set.singleton Elt.present))

  let to_list _           = assert false
  TEST =
    let elts = Set.to_list set_nonempty in
    List.for_all elts ~f:(fun elt ->
      Set.mem set_nonempty elt)
  ;;

  let rec is_list_ordered_ascending xs =
    match xs with
    | [] | [_] -> true
    | a :: b :: xs' ->
      Elt.compare a b < 0 && is_list_ordered_ascending (b :: xs')
  ;;

  TEST =
    is_list_ordered_ascending (Set.to_list set_nonempty)
  ;;

  let to_array _          = assert false
  TEST =
    let a = Set.to_array set_nonempty in
    List.equal (Array.to_list a) (Set.to_list set_nonempty) ~equal:Elt.equal
  ;;

  let of_sorted_array _ = assert false
  let of_sorted_array_unchecked _ = assert false

  TEST = Set.of_sorted_array [||] |! Result.is_ok
  TEST = Set.of_sorted_array [|Elt.of_int 0|] |! Result.is_ok
  TEST = Set.of_sorted_array [|Elt.of_int 0; Elt.of_int 0|] |! Result.is_error
  TEST = Set.of_sorted_array [|Elt.of_int 1
                             ; Elt.of_int 0
                             ; Elt.of_int 1|] |! Result.is_error

  TEST =
    let list = List.init 100 ~f:Elt.of_int in
    let array = Array.of_list list in
    let rev_array = Array.of_list (List.rev list) in
    Set.equal (Set.of_list list) (Set.of_sorted_array_unchecked array)
    && Set.equal (Set.of_list list) (Set.of_sorted_array_unchecked rev_array)
  ;;

  let invariants _        = assert false

  TEST_UNIT =
    for n = 0 to 100 do
      let list = List.init n ~f:Elt.of_int in
      assert (List.permute list |! Set.of_list |! Set.invariants);
      assert (Array.of_list list |! Set.of_sorted_array_unchecked |! Set.invariants);
      assert (List.rev list |! Array.of_list |! Set.of_sorted_array_unchecked |! Set.invariants);
    done
  ;;

  let iter2 _ = assert false

  TEST_UNIT =
    let test l1 l2 expected =
      let result = ref [] in
      let set_of_list l = Set.of_list (List.map l ~f:Elt.of_int) in
      Set.iter2 (set_of_list l1) (set_of_list l2) ~f:(fun a -> result := a :: !result);
      let result =
        List.rev_map !result ~f:(function
        | `Left a -> `Left (Elt.to_int a)
        | `Right a -> `Right (Elt.to_int a)
        | `Both (a, b) -> `Both (Elt.to_int a, Elt.to_int b)
        )
      in
      assert (result = expected)
    in
    test [] [] [];
    test [0] [] [`Left 0];
    test [] [0] [`Right 0];
    test
      [0; 1; 3; 4]
      [3; 4; 5; 6]
      [`Left 0; `Left 1;
       `Both (3, 3); `Both (4, 4);
       `Right 5; `Right 6
      ];
  ;;

  (* Ensure polymorphic equality raises for sets. *)
  TEST_UNIT =
    match Set.kind with
    | `Tree -> ()
    | `Set ->
      let ts = [ Set.empty (); Set.of_list [ Elt.of_int 13 ] ] in
      List.iter ts ~f:(fun t1 ->
        List.iter ts ~f:(fun t2 ->
          assert (Result.is_error (Result.try_with (fun () ->
            Polymorphic_compare.equal t1 t2)))));
  ;;


  let to_tree _           = assert false
  let remove_index _      = assert false
  let find_index _        = assert false
  let find_exn _          = assert false
  let group_by _          = assert false
  let split _             = assert false
  let choose_exn _        = assert false
  let choose _            = assert false
  let max_elt_exn _       = assert false
  let max_elt _           = assert false
  let min_elt_exn _       = assert false
  let min_elt _           = assert false
  let elements _          = assert false
  let partition_tf _      = assert false
  let filter _            = assert false
  let fold_right _        = assert false
  let fold_until _        = assert false
  let equal _             = assert false
  let compare_direct _    = assert false
  let diff _              = assert false
  let union _             = assert false
  let remove _            = assert false
  let find_map _          = assert false
  let find _              = assert false
  let count _             = assert false
  let for_all _           = assert false
  let exists _            = assert false
  let fold _              = assert false
  let iter _              = assert false
  let length _            = assert false
  let of_tree _           = assert false
  let filter_map _        = assert false
  let map _               = assert false
  let stable_dedup_list _ = assert false
  let of_array _          = assert false
  let union_list _        = assert false
  let singleton _         = assert false
  let empty               = Unit_test_follows

end

module Elt_int = struct
  type 'a t = int with sexp
  let of_int = Fn.id
  let to_int = Fn.id
end

module Elt_poly = struct
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

TEST_MODULE "Set" = Unit_tests (Elt_poly) (struct
  include Set
  type ('a, 'b) t_   = ('a, 'b) t
  type ('a, 'b) set  = ('a, 'b) t
  type ('a, 'b) tree = ('a, 'b) Tree.t
  include Create_options_with_comparator
  include Access_options_without_comparator
  let kind = `Set
end)

TEST_MODULE "Set.Poly" = Unit_tests (Elt_poly) (struct
  include Set.Poly
  type ('a, 'b) set  = ('a, 'b) Set.t
  type ('a, 'b) t_   = 'a t
  type ('a, 'b) tree = 'a Tree.t
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Set
end)

TEST_MODULE "Int.Set" = Unit_tests (Elt_int) (struct
  include Int.Set
  type ('a, 'b) set  = ('a, 'b) Set.t
  type ('a, 'b) t_   = t
  type ('a, 'b) tree = Tree.t
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Set
end)

TEST_MODULE "Set.Tree" = Unit_tests (Elt_poly) (struct
  include Set.Tree
  type ('a, 'b) set  = ('a, 'b) Set.Tree.t
  type ('a, 'b) t_   = ('a, 'b) t
  type ('a, 'b) tree = ('a, 'b) t
  include Create_options_with_comparator
  include Access_options_with_comparator
  let kind = `Tree
end)

TEST_MODULE "Set.Poly.Tree" = Unit_tests (Elt_poly) (struct
  include Set.Poly.Tree
  type ('a, 'b) set  = 'a Set.Poly.Tree.t
  type ('a, 'b) t_   = 'a t
  type ('a, 'b) tree = 'a t
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Tree
end)

TEST_MODULE "Int.Set.Tree" = Unit_tests (Elt_int) (struct
  include Int.Set.Tree
  type ('a, 'b) set  = ('a, 'b) Set.Tree.t
  type ('a, 'b) t_   = t
  type ('a, 'b) tree = t
  include Create_options_without_comparator
  include Access_options_without_comparator
  let kind = `Tree
end)
