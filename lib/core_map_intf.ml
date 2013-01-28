(** This module defines interfaces used in [Core.Std.Map].  See the documentation in
    core_map.mli for a description of the approach. *)

module Binable = Binable0
module List = Core_list
module type Key = Comparator.Pre
module type Key_binable = Comparator.Pre_binable

type ('key, 'comparator, 'z) without_comparator = 'z

type ('key, 'comparator, 'z) with_comparator =
  comparator:('key, 'comparator) Comparator.t -> 'z

module type Accessors = sig
  type ('a, 'b, 'comparator) t
  type ('a, 'b, 'comparator) tree
  type 'a key
  type ('a, 'comparator, 'z) options

  (** Test if invariants of internal AVL search tree hold. *)
  val invariants :
    ('k, 'comparator,
     ('k, 'v, 'comparator) t -> bool
    ) options

  (** Test whether a map is empty or not. *)
  val is_empty : (_, _, _) t -> bool

  (** [length map] @return number of elements in [map]. *)
  val length : (_, _, _) t -> int

  (** returns a new map with the specified new binding;
      if the key was already bound, its previous binding disappears. *)
  val add :
    ('k, 'comparator,
     ('k, 'v, 'comparator) t -> key:'k key -> data:'v -> ('k, 'v, 'comparator) t
    ) options

  (** if key is not present then add a singleton list, otherwise, cons data
      on the head of the existing list. *)
  val add_multi
    : ('k, 'comparator,
       ('k, 'v list, 'comparator) t
       -> key:'k key
       -> data:'v
       -> ('k, 'v list, 'comparator) t
    ) options

  (** [change map key f] updates the given map by changing the value stored
      under [key] according to [f].  Thus, for example, one might write:

      {[change m k (function None -> Some 0 | Some x -> Some (x + 1))]}

      to produce a new map where the integer stored under key [k] is
      incremented by one (treating an unknown key as zero) *)
  val change
    : ('k, 'comparator,
       ('k, 'v, 'comparator) t
       -> 'k key
       -> ('v option -> 'v option)
       -> ('k, 'v, 'comparator) t
    ) options

  (** returns the value bound to the given key, raising [Not_found] if none
      such exists *)
  val find     : ('k, 'comparator, ('k, 'v, 'comparator) t -> 'k key -> 'v option) options
  val find_exn : ('k, 'comparator, ('k, 'v, 'comparator) t -> 'k key -> 'v       ) options

  (** returns a new map with any binding for the key in question removed *)
  val remove :
    ('k, 'comparator, ('k, 'v, 'comparator) t -> 'k key -> ('k, 'v, 'comparator) t
    ) options

  (** [mem map key] tests whether [map] contains a binding for [key] *)
  val mem : ('k, 'comparator, ('k, _, 'comparator) t -> 'k key -> bool) options

  (** iterator for map *)
  val iter : ('k, 'v, _) t -> f:(key:'k key -> data:'v -> unit) -> unit

  (** Iterate two maps side by side. Complexity of this function is O(M+N). If two inputs
      are [(0, a); (1, a)] and [(1, b); (2, b)], [f] will be called with
      [(0, `Left a); (1, `Both (a, b)); (2, `Right b)] *)
  val iter2
    : ('k, 'comparator,
       ('k, 'v1, 'comparator) t
       -> ('k, 'v2, 'comparator) t
       -> f:(key:'k key
             -> data:[ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ]
             -> unit)
       -> unit
    ) options

  (** returns new map with bound values replaced by f applied to the bound values *)
  val map : ('k, 'v1, 'comparator) t -> f:('v1 -> 'v2) -> ('k, 'v2, 'comparator) t

  (** like [map], but function takes both key and data as arguments *)
  val mapi
    :  ('k, 'v1, 'comparator) t
    -> f:(key:'k key -> data:'v1 -> 'v2)
    -> ('k, 'v2, 'comparator) t

  (** folds over keys and data in map in increasing order of key. *)
  val fold : ('k, 'v, _) t -> init:'a -> f:(key:'k key -> data:'v -> 'a -> 'a) -> 'a

  (** folds over keys and data in map in decreasing order of key. *)
  val fold_right : ('k, 'v, _) t -> init:'a -> f:(key:'k key -> data:'v -> 'a -> 'a) -> 'a

  val filter
    : ('k, 'comparator,
       ('k, 'v, 'comparator) t
       -> f:(key:'k key -> data:'v -> bool)
       -> ('k, 'v, 'comparator) t
    ) options

  (** returns new map with bound values filtered by f applied to the bound values *)
  val filter_map
    : ('k, 'comparator,
       ('k, 'v1, 'comparator) t
       -> f:('v1 -> 'v2 option)
       -> ('k, 'v2, 'comparator) t
    ) options

  (** like [filter_map], but function takes both key and data as arguments*)
  val filter_mapi
    : ('k, 'comparator,
       ('k, 'v1, 'comparator) t
       -> f:(key:'k key -> data:'v1 -> 'v2 option)
       -> ('k, 'v2, 'comparator) t
    ) options

  (** Total ordering between maps.  The first argument is a total ordering used to compare
      data associated with equal keys in the two maps. *)
  val compare_direct
    : ('k, 'comparator,
       ('v -> 'v -> int)
       -> ('k, 'v, 'comparator) t
       -> ('k, 'v, 'comparator) t
       -> int
    ) options

  (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are equal, that is, contain
      equal keys and associate them with equal data.  [cmp] is the equality predicate used
      to compare the data associated with the keys. *)
  val equal
    : ('k, 'comparator,
       ('v -> 'v -> bool)
       -> ('k, 'v, 'comparator) t
       -> ('k, 'v, 'comparator) t
       -> bool
    ) options

  (** returns list of keys in map *)
  val keys : ('k, _, _) t -> 'k key list

  (** returns list of data in map *)
  val data : (_, 'v, _) t -> 'v list

  (** creates association list from map.  No guarantee about order. *)
  val to_alist : ('k, 'v, _) t -> ('k key * 'v) list

  (** {6 Additional operations on maps} *)

  (** merges two maps *)
  val merge
    : ('k, 'comparator,
       ('k, 'v1, 'comparator) t
       -> ('k, 'v2, 'comparator) t
       -> f:(key:'k key
             -> [ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ]
             -> 'v3 option)
       -> ('k, 'v3, 'comparator) t
    ) options

  (** [min_elt map] @return Some [(key, data)] pair corresponding to the minimum key in
      [map], None if empty. *)
  val min_elt     : ('k, 'v, _) t -> ('k key * 'v) option
  val min_elt_exn : ('k, 'v, _) t ->  'k key * 'v

  (** [max_elt map] @return Some [(key, data)] pair corresponding to the maximum key in
      [map], and None if [map] is empty. *)
  val max_elt     : ('k, 'v, _) t -> ('k key * 'v) option
  val max_elt_exn : ('k, 'v, _) t ->  'k key * 'v

  (** same semantics as similar functions in List *)
  val for_all : ('k, 'v, _) t -> f:('v -> bool) -> bool
  val exists  : ('k, 'v, _) t -> f:('v -> bool) -> bool

  (** [fold_range_inclusive t ~min ~max ~init ~f]
      folds f (with initial value ~init) over all keys (and their associated values)
      that are in the range [min, max] (inclusive).  *)
  val fold_range_inclusive
    : ('k, 'comparator,
       ('k, 'v, 'comparator) t
       -> min:'k key
       -> max:'k key
       -> init:'a
       -> f:(key:'k key -> data:'v -> 'a -> 'a)
       -> 'a
    ) options

  (** [range_to_alist t ~min ~max] returns an associative list of the elements whose
      keys lie in [min, max] (inclusive), with the smallest key being at the head of the
      list. *)
  val range_to_alist
    : ('k, 'comparator,
       ('k, 'v, 'comparator) t -> min:'k key -> max:'k key -> ('k key * 'v) list
    ) options

  (** [prev_key t k] returns the largest (key, value) pair in t with key less than k *)
  val prev_key
    : ('k, 'comparator,
       ('k, 'v, 'comparator) t -> 'k key -> ('k key * 'v) option
    ) options
  (** [next_key t k] returns the smallest (key, value) pair in t with key greater than k *)
  val next_key
    : ('k, 'comparator,
       ('k, 'v, 'comparator) t -> 'k key -> ('k key * 'v) option
    ) options
  (** [rank t k] if k is in t, returns the number of keys strictly less than k in t,
      otherwise None *)
  val rank
    : ('k, 'comparator,
       ('k, 'v, 'comparator) t -> 'k key -> int option
    ) options

  val to_tree : ('k, 'v, 'comparator) t -> ('k key, 'v, 'comparator) tree
end

module type Creators = sig
  type ('k, 'v, 'comparator) t
  type ('k, 'v, 'comparator) tree
  type 'k key
  type ('a, 'comparator, 'z) options

  (** the empty map *)
  val empty: ('k, 'comparator, ('k, _, 'comparator) t) options

  (** map with one key, data pair *)
  val singleton: ('k, 'comparator, 'k key -> 'v -> ('k, 'v, 'comparator) t) options

  (** creates map from sorted array of key-data pairs. The input array must be sorted,
      as given by the relevant comparator (either in ascending or descending order),
      and must not contain any duplicate keys.
      If either of these conditions do not hold, an error is returned.
  *)
  val of_sorted_array:
    ('k, 'comparator, ('k key * 'v) array -> ('k, 'v, 'comparator) t Or_error.t) options

  (** Like [of_sorted_array] except behavior is undefined when an [Error] would have been
      returned. *)
  val of_sorted_array_unchecked:
    ('k, 'comparator, ('k key * 'v) array -> ('k, 'v, 'comparator) t) options

  (** creates map from association list with unique keys *)
  val of_alist:
    ('k,
     'comparator,
     ('k key * 'v) list -> [ `Ok of ('k, 'v, 'comparator) t | `Duplicate_key of 'k key ]
    ) options

  (** creates map from association list with unique keys.  Raises an exception if
      duplicate 'a keys are found. *)
  val of_alist_exn
    : ('k, 'comparator, ('k key * 'v) list -> ('k, 'v, 'comparator) t) options

  (** creates map from association list with possibly repeated keys. *)
  val of_alist_multi
    : ('k, 'comparator, ('k key * 'v) list -> ('k, 'v list, 'comparator) t) options

  (** combines an association list into a map, folding together bound values with common
      keys *)
  val of_alist_fold
    : ('k, 'comparator,
       ('k key * 'v1) list
       -> init:'v2
       -> f:('v2 -> 'v1 -> 'v2)
       -> ('k, 'v2, 'comparator) t
    ) options

  val of_tree
    : ('k, 'comparator,
       ('k key, 'v, 'comparator) tree -> ('k, 'v, 'comparator) t
    ) options
end

module type Creators_and_accessors = sig
  include Creators
  include Accessors
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
    with type 'a key := 'a key
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) options
end

module type S = sig
  module Key : Comparator.S

  type ('k, +'v, 'comparator) map
  type ('k, +'v, 'comparator) tree
  type +'v t = (Key.t, 'v, Key.comparator) map with sexp, compare
  type ('k, 'v, 'comparator) t_ = 'v t
  type 'a key_ = Key.t
  type ('a, 'b, 'c) options = ('a, 'b, 'c) without_comparator

  include Creators_and_accessors
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
    with type 'a key := 'a key_
    with type ('a, 'b, 'c) options := ('a, 'b, 'c) without_comparator

  module Tree : sig
    type +'v t = (Key.t, 'v, Key.comparator) tree with sexp
    type ('k, +'v, 'c) t_ = 'v t

    include Creators_and_accessors
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
      with type 'a key := 'a key_
      with type ('a, 'b, 'c) options := ('a, 'b, 'c) without_comparator
  end
end

module type S_binable = sig
  include S
  include Binable.S1 with type 'a t := 'a t
end
