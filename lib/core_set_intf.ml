open Sexplib

module type Elt = Comparator.Pre
module type Elt_binable = Comparator.Pre_binable

module type Accessors = sig
  include Container.Generic_phantom

  val mem : ('a, _) t -> 'a elt -> bool (* override [Container]'s [mem] *)
  val add    : ('a, 'comparator) t -> 'a elt -> ('a, 'comparator) t
  val remove : ('a, 'comparator) t -> 'a elt -> ('a, 'comparator) t
  val union   : ('a, 'comparator) t -> ('a, 'comparator) t -> ('a, 'comparator) t
  val inter   : ('a, 'comparator) t -> ('a, 'comparator) t -> ('a, 'comparator) t
  val diff    : ('a, 'comparator) t -> ('a, 'comparator) t -> ('a, 'comparator) t
  val compare : ('a, 'comparator) t -> ('a, 'comparator) t -> int
  val equal   : ('a, 'comparator) t -> ('a, 'comparator) t -> bool
  (* [subset x y] returns true iff [x] is a subset of [y]. *)
  val subset : ('a, 'comparator) t -> ('a, 'comparator) t -> bool
  val fold_until
    :  ('a, _) t
    -> init:'b
    -> f:('b -> 'a elt -> [ `Continue of 'b | `Stop of 'b ])
    -> 'b
  val fold_right
    :  ('a, _) t
    -> init:'b
    -> f:('a elt -> 'b -> 'b)
    -> 'b
  val filter : ('a, 'comparator) t -> f:('a elt -> bool) -> ('a, 'comparator) t
  (** if [res = partition_tf set ~f] then [fst res] are the elements on which [f]
      produced [true], and [snd res] are the elements on which [f] produces [false] *)
  val partition_tf
    :  ('a, 'comparator) t
    -> f:('a elt -> bool)
    -> ('a, 'comparator) t * ('a, 'comparator) t

  val elements : ('a, _) t -> 'a elt list

  val min_elt     : ('a, _) t -> 'a elt option
  val min_elt_exn : ('a, _) t -> 'a elt
  val max_elt     : ('a, _) t -> 'a elt option
  val max_elt_exn : ('a, _) t -> 'a elt

  (* returns an arbitrary element, or None if the set is empty *)
  val choose     : ('a, _) t -> 'a elt option
  val choose_exn : ('a, _) t -> 'a elt

  (** [split x set] produces a triple [triple] where [fst3 triple] is the set of elements
      strictly less than [x], [snd3 triple] = [mem set x], and [trd3 triple] is the set of
      elements strictly larger than [x]. *)
  val split
    :  ('a, 'comparator) t
    -> 'a elt
    -> ('a, 'comparator) t * bool * ('a, 'comparator) t

  (** if [equiv] is an equivalence predicate, then [group_by set ~equiv] produces a list
      of equivalence classes (i.e., a set-theoretic quotient).  E.g.,

      [let chars = Set.of_list ['A'; 'a'; 'b'; 'c'] in
      let equiv c c' = Char.equal (Char.uppercase c) (Char.uppercase c') in
      group_by chars ~equiv]

      produces

      [Set.of_list['A';'a']; Set.singleton 'b'; Set.singleton 'c']

      Runs in O(n^2) time. *)
  val group_by
    :  ('a, 'comparator) t
    -> equiv:('a elt -> 'a elt -> bool)
    -> ('a, 'comparator) t list

  val find_exn : ('a, _) t -> f:('a elt -> bool) -> 'a elt
  (* Returns the ith smallest element in the set in O(log n) time.  The smallest element
     is element 0. *)
  val find_index : ('a, _) t -> int -> 'a elt option
  val remove_index : ('a, 'comparator) t -> int -> ('a, 'comparator) t

  (** [tree t] returns the underlying binary tree that represents the set.  This is useful
      if you want to marshal a set between processes.  Since the set contains a closure,
      it cannot be marshalled between processes that use different executables; however,
      the underlying tree can. *)
  type ('a, 'comparator) tree
  val to_tree : ('a, 'comparator) t -> ('a elt, 'comparator) tree

end

type ('key, 'comparator, 'z) create_options_without_comparator =
  ('key, 'comparator, 'z) Core_map_intf.create_options_without_comparator

type ('key, 'comparator, 'z) create_options_with_comparator =
  ('key, 'comparator, 'z) Core_map_intf.create_options_with_comparator

module type Creators = sig
  type ('a, 'comparator) set
  type ('a, 'comparator) t
  type 'a elt
  type ('a, 'comparator, 'z) create_options

  (* [equal x (union_list (List.map ~f:Set.singleton (elements x)))] *)
  val empty : ('a, 'comparator, ('a, 'comparator) t) create_options
  val singleton : ('a, 'comparator, 'a elt -> ('a, 'comparator) t) create_options
  val union_list
    :  ('a, 'comparator,
        ('a, 'comparator) t list -> ('a, 'comparator) t
    ) create_options
  val of_list  : ('a, 'comparator, 'a elt list  -> ('a, 'comparator) t) create_options
  val of_array : ('a, 'comparator, 'a elt array -> ('a, 'comparator) t) create_options
  (* [stable_dedup_list] is here rather than in the List module because the implementation
     relies crucially on sets, and because doing so allows one to avoid uses of
     polymorphic comparison by instantiating the functor at a different implementation of
     Comparator and using the resulting [stable_dedup_list]. *)
  val stable_dedup_list : ('a, _, 'a elt list -> 'a elt list) create_options

  (* The types of [map] and [filter_map] are subtle.  The input set, [('a, _) set],
     reflects the fact that these functions take a set of *any* type, with any comparator,
     while the output set, [('b, 'comparator) t], reflects that the output set has the
     the particular ['comparator] of the creation function.  The comparator can come
     in one of three ways, depending on which set module is used

       [Set.map] -- comparator comes as an argument
       [Set.Poly.map] -- comparator is polymorphic comparison
       [Foo.Set.map] -- comparator is [Foo.comparator] *)
  val map
    : ('b, 'comparator, ('a, _) set -> f:('a -> 'b elt       ) -> ('b, 'comparator) t
    ) create_options
  val filter_map
    : ('b, 'comparator, ('a, _) set -> f:('a -> 'b elt option) -> ('b, 'comparator) t
    ) create_options

  type ('a, 'comparator) tree
  val of_tree
    : ('a, 'comparator,
       ('a elt, 'comparator) tree -> ('a, 'comparator) t
    ) create_options
end

module type S = sig
  module Elt : Comparator.S

  type ('a, 'comparator) set
  type t = (Elt.t, Elt.comparator) set with sexp
  type ('a, 'comparator) t_ = t
  type ('a, 'comparator) tree
  type 'a elt_ = Elt.t

  include Creators
    with type ('a, 'comparator) set := ('a, 'comparator) set
    with type ('a, 'comparator) t := ('a, 'comparator) t_
    with type ('a, 'b) tree := ('a, 'b) tree
    with type 'a elt := 'a elt_
    with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) create_options_without_comparator

  include Accessors
    with type ('a, 'b) t := ('a, 'b) t_
    with type ('a, 'b) tree := ('a, 'b) tree
    with type 'a elt := 'a elt_
end

module type S_binable = sig
  include S
  include Binable.S with type t := t
end
