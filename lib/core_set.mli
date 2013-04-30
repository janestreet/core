(** This module defines the [Set] module for [Core.Std].  We use "core_set" as the file
    name rather than "set" to avoid conflicts with OCaml's standard set module.

    This module uses the same organizational approach as [Core_map].  See the
    documentation in core_map.mli for a description of the approach. *)

open Core_set_intf

type ('elt, 'cmp) t with compare

module Tree : sig
  type ('a, 'cmp) t with sexp_of

  include Creators_and_accessors2_with_comparator
    with type ('a, 'b) set  := ('a, 'b) t
    with type ('a, 'b) t    := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) t
end

(** Test if invariants of internal AVL search tree hold. *)
val invariants : (_, _) t -> bool

val comparator : ('a, 'cmp) t -> ('a, 'cmp) Comparator.t

val empty          : comparator:('a, 'cmp) Comparator.t -> ('a, 'cmp) t
val singleton      : comparator:('a, 'cmp) Comparator.t -> 'a -> ('a, 'cmp) t
val length         : (_, _) t -> int
val is_empty       : (_, _) t -> bool
val mem            : ('a, _) t -> 'a -> bool
val add            : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t
val remove         : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t
val union          : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t
val union_list     : comparator:('a, 'cmp) Comparator.t -> ('a, 'cmp) t list -> ('a, 'cmp) t
val inter          : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t
val diff           : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t
val compare_direct : ('a, 'cmp) t -> ('a, 'cmp) t -> int
val equal          : ('a, 'cmp) t -> ('a, 'cmp) t -> bool
val exists         : ('a, _) t -> f:('a -> bool) -> bool
val for_all        : ('a, _) t -> f:('a -> bool) -> bool
val count          : ('a, _) t -> f:('a -> bool) -> int
val find           : ('a, _) t -> f:('a -> bool) -> 'a option
val find_map       : ('a, _) t -> f:('a -> 'b option) -> 'b option
val find_exn       : ('a, _) t -> f:('a -> bool) -> 'a

(** [find_index t i] returns the [i]th smallest element of [t] in O(log n) time.  The
    smallest element has [i = 0]. *)
val find_index : ('a, _) t -> int -> 'a option
val remove_index : ('a, 'cmp) t -> int -> ('a, 'cmp) t

(** [subset t1 t2] returns true iff [t1] is a subset of [t2]. *)
val subset : ('a, 'cmp) t -> ('a, 'cmp) t -> bool

(** The list or array given to [of_list] and [of_array] need not be sorted. *)
val of_list  : comparator:('a, 'cmp) Comparator.t -> 'a list  -> ('a, 'cmp) t
val of_array : comparator:('a, 'cmp) Comparator.t -> 'a array -> ('a, 'cmp) t

(** [to_list] and [to_array] produce sequences sorted in ascending order according to the
    comparator. *)
val to_list  : ('a, _) t -> 'a list
val to_array : ('a, _) t -> 'a array

val to_tree : ('a, 'cmp) t -> ('a, 'cmp) Tree.t
val of_tree : comparator:('a, 'cmp) Comparator.t -> ('a, 'cmp) Tree.t -> ('a, 'cmp) t

(** Create set from sorted array.  The input must be sorted (either in ascending or
    descending order as given by the comparator) and contain no duplicates, otherwise the
    result is an error.  The complexity of this function is O(N). *)
val of_sorted_array
  :  comparator:('a, 'cmp) Comparator.t
  -> 'a array
  -> ('a, 'cmp) t Or_error.t

(** Similar to [of_sorted_array], but without checking the input array. *)
val of_sorted_array_unchecked
  :  comparator:('a, 'cmp) Comparator.t
  -> 'a array
  -> ('a, 'cmp) t

(** [stable_dedup_list] is here rather than in the [List] module because the
    implementation relies crucially on sets, and because doing so allows one to avoid uses
    of polymorphic comparison by instantiating the functor at a different implementation
    of [Comparator] and using the resulting [stable_dedup_list]. *)
val stable_dedup_list : comparator:('a, _) Comparator.t -> 'a list -> 'a list

val map
  :  comparator:('b, 'cmp) Comparator.t
  -> ('a, _) t
  -> f:('a -> 'b)
  -> ('b, 'cmp) t
val filter_map
  :  comparator:('b, 'cmp) Comparator.t
  -> ('a, _) t
  -> f:('a -> 'b option)
  -> ('b, 'cmp) t

val filter :  ('a, 'cmp) t -> f:('a -> bool) -> ('a, 'cmp) t

val fold
  :  ('a, _) t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum)
  -> 'accum
val fold_until
  :  ('a, _) t
  -> init:'accum
  -> f:('accum -> 'a -> [ `Continue of 'accum | `Stop of 'accum ])
  -> 'accum
val fold_right
  :  ('a, _) t
  -> init:'accum
  -> f:('a -> 'accum -> 'accum)
  -> 'accum

val iter : ('a, _) t -> f:('a -> unit) -> unit

(** Iterate two sets side by side.  Complexity is O(M+N) where M and N are the sizes of
    the two input sets. As an example, with the inputs [0; 1] and [1; 2], [f] will be
    called with [`Left 0]; [`Both (1, 1)]; and [`Right 2]. *)
val iter2
  :  ('a, 'cmp) t
  -> ('a, 'cmp) t
  -> f:([`Left of 'a | `Right of 'a | `Both of 'a * 'a] -> unit)
  -> unit

(** if [a, b = partition_tf set ~f] then [a] is the elements on which [f] produced [true],
    and [b] is the elements on which [f] produces [false]. *)
val partition_tf
  :  ('a, 'cmp) t
  -> f:('a -> bool)
  -> ('a, 'cmp) t * ('a, 'cmp) t

(** Same as {!to_list}. *)
val elements : ('a, _) t -> 'a list

val min_elt     : ('a, _) t -> 'a option
val min_elt_exn : ('a, _) t -> 'a
val max_elt     : ('a, _) t -> 'a option
val max_elt_exn : ('a, _) t -> 'a

(** returns an arbitrary element, or [None] if the set is empty. *)
val choose     : ('a, _) t -> 'a option
val choose_exn : ('a, _) t -> 'a

(** [split t x] produces a triple [(t1, b, t2)] where [t1] is the set of elements strictly
    less than [x], [b = mem set x], and [t2] is the set of elements strictly larger than
    [x]. *)
val split : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t * bool * ('a, 'cmp) t

(** if [equiv] is an equivalence predicate, then [group_by set ~equiv] produces a list
    of equivalence classes (i.e., a set-theoretic quotient).  E.g.,

    {[
      let chars = Set.of_list ['A'; 'a'; 'b'; 'c'] in
      let equiv c c' = Char.equal (Char.uppercase c) (Char.uppercase c') in
      group_by chars ~equiv
    ]}

    produces:

    {[
      Set.of_list['A';'a']; Set.singleton 'b'; Set.singleton 'c']
    ]}

    [group_by] runs in O(n^2) time. *)
val group_by :  ('a, 'cmp) t -> equiv:('a -> 'a -> bool) -> ('a, 'cmp) t list

module Poly : sig
  type ('a, 'b) set

  module Tree : sig
    type 'elt t = ('elt, Comparator.Poly.comparator) Tree.t with sexp

    include Creators_and_accessors1
      with type ('a, 'b) set := ('a, 'b) Tree.t
      with type 'elt t       := 'elt t
      with type 'elt tree    := 'elt t
  end

  type 'elt t = ('elt, Comparator.Poly.comparator) set with bin_io, compare, sexp

  include Creators_and_accessors1
    with type ('a, 'b) set := ('a, 'b) set
    with type 'elt t       := 'elt t
    with type 'elt tree    := 'elt Tree.t
end
  with type ('a, 'b) set := ('a, 'b) t

module type Elt = Elt

module type Elt_binable = Elt_binable

module type S = S0
  with type ('a, 'b) set  := ('a, 'b) t
  with type ('a, 'b) tree := ('a, 'b) Tree.t

module type S_binable = S0_binable
  with type ('a, 'b) set  := ('a, 'b) t
  with type ('a, 'b) tree := ('a, 'b) Tree.t

module Make (Elt : Elt) : S with type Elt.t = Elt.t

module Make_using_comparator (Elt : Comparator.S)
  : S
    with type Elt.t = Elt.t
    with type Elt.comparator = Elt.comparator

module Make_binable (Elt : Elt_binable) : S_binable with type Elt.t = Elt.t

module Make_binable_using_comparator (Elt : Comparator.S_binable)
  : S_binable
    with type Elt.t = Elt.t
    with type Elt.comparator = Elt.comparator
