(** This module defines interfaces used in [Core.Std.Set].  This module uses the same
    organizational approach as [Core_map_intf].  See the documentation in core_map.mli for
    a description of the approach. *)

module Binable = Binable0
module type Elt         = Comparator.Pre
module type Elt_binable = Comparator.Pre_binable

module type Accessors = sig

  (* [to_list] and [to_array] produce sequences sorted in ascending order according
     to [Elt.compare] *)
  include Container.Generic_phantom

  type ('a, 'comparator) tree
  (* The [options] type is used to make [Accessors] flexible as to whether a comparator
     is required to be passed to certain functions.  See core_map.mli for a detailed
     explanation. *)
  type ('a, 'comparator, 'z) options

  (* override [Container]'s [mem] *)
  val mem : ('a, 'comparator, ('a, 'comparator) t -> 'a elt -> bool) options
  val add
    : ('a, 'comparator,
     ('a, 'comparator) t -> 'a elt -> ('a, 'comparator) t
    ) options
  val remove
    : ('a, 'comparator,
       ('a, 'comparator) t -> 'a elt -> ('a, 'comparator) t
    ) options
  val union
    : ('a, 'comparator,
       ('a, 'comparator) t -> ('a, 'comparator) t -> ('a, 'comparator) t
    ) options
  val inter
    : ('a, 'comparator,
       ('a, 'comparator) t -> ('a, 'comparator) t -> ('a, 'comparator) t
    ) options
  val diff
    : ('a, 'comparator,
       ('a, 'comparator) t -> ('a, 'comparator) t -> ('a, 'comparator) t
    ) options
  val compare_direct
    : ('a, 'comparator,
       ('a, 'comparator) t -> ('a, 'comparator) t -> int
    ) options
  val equal
    : ('a, 'comparator,
       ('a, 'comparator) t -> ('a, 'comparator) t -> bool
    ) options
  (** [subset t1 t2] returns true iff [t1] is a subset of [t2]. *)
  val subset
    : ('a, 'comparator,
       ('a, 'comparator) t -> ('a, 'comparator) t -> bool
    ) options
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
  val filter
    :  ('a, 'comparator,
        ('a, 'comparator) t -> f:('a elt -> bool) -> ('a, 'comparator) t
    ) options
  (** if [res = partition_tf set ~f] then [fst res] are the elements on which [f]
      produced [true], and [snd res] are the elements on which [f] produces [false] *)
  val partition_tf
    :  ('a, 'comparator,
        ('a, 'comparator) t
        -> f:('a elt -> bool)
        -> ('a, 'comparator) t * ('a, 'comparator) t
    ) options

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
    : ('a, 'comparator,
       ('a, 'comparator) t
       -> 'a elt
       -> ('a, 'comparator) t * bool * ('a, 'comparator) t
    ) options

  (** if [equiv] is an equivalence predicate, then [group_by set ~equiv] produces a list
      of equivalence classes (i.e., a set-theoretic quotient).  E.g.,

      [let chars = Set.of_list ['A'; 'a'; 'b'; 'c'] in
      let equiv c c' = Char.equal (Char.uppercase c) (Char.uppercase c') in
      group_by chars ~equiv]

      produces

      [Set.of_list['A';'a']; Set.singleton 'b'; Set.singleton 'c']

      Runs in O(n^2) time. *)
  val group_by
    : ('a, 'comparator,
       ('a, 'comparator) t
       -> equiv:('a elt -> 'a elt -> bool)
       -> ('a, 'comparator) t list
    ) options

  val find_exn : ('a, _) t -> f:('a elt -> bool) -> 'a elt
  (** [find_index t i] returns the [i]th smallest element of [t] in O(log n) time.  The
      smallest element has [i = 0]. *)
  val find_index : ('a, _) t -> int -> 'a elt option
  val remove_index
    : ('a, 'comparator,
       ('a, 'comparator) t -> int -> ('a, 'comparator) t
    ) options

  val to_tree : ('a, 'comparator) t -> ('a elt, 'comparator) tree
end

type ('key, 'comparator, 'z) without_comparator =
  ('key, 'comparator, 'z) Core_map_intf.without_comparator

type ('key, 'comparator, 'z) with_comparator =
  ('key, 'comparator, 'z) Core_map_intf.with_comparator

module type Creators = sig
  type ('a, 'comparator) t
  type ('a, 'comparator) set
  type ('a, 'comparator) tree
  type 'a elt
  type ('a, 'comparator, 'z) options

  (* [equal x (union_list (List.map ~f:Set.singleton (elements x)))] *)
  val empty : ('a, 'comparator, ('a, 'comparator) t) options
  val singleton : ('a, 'comparator, 'a elt -> ('a, 'comparator) t) options
  val union_list
    :  ('a, 'comparator,
        ('a, 'comparator) t list -> ('a, 'comparator) t
    ) options
  (* The list or array need not be sorted *)
  val of_list  : ('a, 'comparator, 'a elt list  -> ('a, 'comparator) t) options
  val of_array : ('a, 'comparator, 'a elt array -> ('a, 'comparator) t) options
  (* [stable_dedup_list] is here rather than in the List module because the implementation
     relies crucially on sets, and because doing so allows one to avoid uses of
     polymorphic comparison by instantiating the functor at a different implementation of
     Comparator and using the resulting [stable_dedup_list]. *)
  val stable_dedup_list : ('a, _, 'a elt list -> 'a elt list) options

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
    ) options
  val filter_map
    : ('b, 'comparator, ('a, _) set -> f:('a -> 'b elt option) -> ('b, 'comparator) t
    ) options

  val of_tree
    : ('a, 'comparator,
       ('a elt, 'comparator) tree -> ('a, 'comparator) t
    ) options
end

module type Creators_and_accessors = sig
  include Creators
  include Accessors
    with type ('a, 'b) t    := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) tree
    with type 'a elt := 'a elt
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) options
end

module type S = sig
  module Elt : Comparator.S

  type ('a, 'comparator) set
  type ('a, 'comparator) tree
  type t = (Elt.t, Elt.comparator) set with compare, sexp
  type ('a, 'comparator) t_ = t
  type 'a elt_ = Elt.t

  include Creators_and_accessors
    with type ('a, 'b) t    := ('a, 'b) t_
    with type ('a, 'b) set  := ('a, 'b) set
    with type ('a, 'b) tree := ('a, 'b) tree
    with type 'a elt := 'a elt_
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) without_comparator

  (* [empty] has the same spec in [Creators_and_accessors], but adding it here prevents a
     type-checker issue with nongeneralizable type variables. *)
  val empty : t

  module Tree : sig
    type t = (Elt.t, Elt.comparator) tree with compare, sexp
    type ('a, 'b) t_ = t

    include Creators_and_accessors
    with type ('a, 'b) t    := ('a, 'b) t_
      with type ('a, 'b) set  := ('a, 'b) tree
      with type ('a, 'b) tree := ('a, 'b) tree
      with type 'a elt := 'a elt_
      with type ('a, 'b, 'c) options := ('a, 'b, 'c) without_comparator
  end
end

module type S_binable = sig
  include S
  include Binable.S with type t := t
end
