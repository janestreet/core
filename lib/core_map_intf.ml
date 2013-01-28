open Sexplib

module List = Core_list
module type Key = Comparator.Pre
module type Key_binable = Comparator.Pre_binable

module type Accessors = sig
  type ('a, 'b, 'comparator) t
  type 'a key

  (** Test whether a map is empty or not. *)
  val is_empty : (_, _, _) t -> bool

  (** [length map] @return number of elements in [map]. *)
  val length : (_, _, _) t -> int

  (** returns a new map with the specified new binding;
      if the key was already bound, its previous binding disappears. *)
  val add : ('k, 'v, 'comparator) t -> key:'k key -> data:'v -> ('k, 'v, 'comparator) t

  (** if key is not present then add a singleton list, otherwise, cons data
      on the head of the existing list. *)
  val add_multi
    :  ('k, 'v list, 'comparator) t
    -> key:'k key
    -> data:'v
    -> ('k, 'v list, 'comparator) t

  (** [change map key f] updates the given map by changing the value stored
      under [key] according to [f].  Thus, for example, one might write:

      {[change m k (function None -> Some 0 | Some x -> Some (x + 1))]}

      to produce a new map where the integer stored under key [k] is
      incremented by one (treating an unknown key as zero) *)
  val change
    : ('k, 'v, 'comparator) t
    -> 'k key
    -> ('v option -> 'v option)
    -> ('k, 'v, 'comparator) t


  (** returns the value bound to the given key, raising [Not_found] if none
      such exists *)
  val find     : ('k, 'v, _) t -> 'k key -> 'v option
  val find_exn : ('k, 'v, _) t -> 'k key -> 'v

  (** returns a new map with any binding for the key in question removed *)
  val remove : ('k, 'v, 'comparator) t -> 'k key -> ('k, 'v, 'comparator) t

  (** [mem key map] tests whether [map] contains a binding for [key] *)
  val mem : ('k, _, _) t -> 'k key -> bool

  (** iterator for map *)
  val iter : ('k, 'v, _) t -> f:(key:'k key -> data:'v -> unit) -> unit

  (** returns new map with bound values replaced by f applied to the bound values *)
  val map : ('k, 'v1, 'comparator) t -> f:('v1 -> 'v2) -> ('k, 'v2, 'comparator) t

  (** like [map], but function takes both key and data as arguments *)
  val mapi
    :  ('k, 'v1, 'comparator) t
    -> f:(key:'k key -> data:'v1 -> 'v2)
    -> ('k, 'v2, 'comparator) t

  (** folds over keys and data in map *)
  val fold : ('k, 'v, _) t -> init:'a -> f:(key:'k key -> data:'v -> 'a -> 'a) -> 'a

  (** folds over keys and data in map in reverse order *)
  val fold_right : ('k, 'v, _) t -> init:'a -> f:(key:'k key -> data:'v -> 'a -> 'a) -> 'a

  val filter
    :  ('k, 'v, 'comparator) t
    -> f:(key:'k key -> data:'v -> bool)
    -> ('k, 'v, 'comparator) t

  (** returns new map with bound values filtered by f applied to the bound values *)
  val filter_map
    :  ('k, 'v1, 'comparator) t
    -> f:('v1 -> 'v2 option)
    -> ('k, 'v2, 'comparator) t

  (** like [filter_map], but function takes both key and data as arguments*)
  val filter_mapi
    :  ('k, 'v1, 'comparator) t
    -> f:(key:'k key -> data:'v1 -> 'v2 option)
    -> ('k, 'v2, 'comparator) t

  (** Total ordering between maps.  The first argument is a total ordering used to compare
      data associated with equal keys in the two maps. *)
  val compare
    :  ('v -> 'v -> int)
    -> ('k, 'v, 'comparator) t
    -> ('k, 'v, 'comparator) t
    -> int

  (** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are equal, that is, contain
      equal keys and associate them with equal data.  [cmp] is the equality predicate used
      to compare the data associated with the keys. *)
  val equal
    :  ('v -> 'v -> bool)
    -> ('k, 'v, 'comparator) t
    -> ('k, 'v, 'comparator) t
    -> bool

  (** returns list of keys in map *)
  val keys : ('k, _, _) t -> 'k key list

  (** returns list of data in map *)
  val data : (_, 'v, _) t -> 'v list

  (** creates association list from map.  No guarantee about order. *)
  val to_alist : ('k, 'v, _) t -> ('k key * 'v) list

  (** {6 Additional operations on maps} *)

  (** merges two maps *)
  val merge
    :  ('k, 'v1, 'comparator) t
    -> ('k, 'v2, 'comparator) t
    -> f:(key:'k key
          -> [ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ]
          -> 'v3 option)
    -> ('k, 'v3, 'comparator) t

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
    :  ('k, 'v, _) t
    -> min:'k key
    -> max:'k key
    -> init:'a
    -> f:(key:'k key -> data:'v -> 'a -> 'a)
    -> 'a

  (** [range_to_alist t ~min ~max] returns an associative list of the elements whose
      keys lie in [min, max] (inclusive), with the smallest key being at the head of the
      list. *)
  val range_to_alist : ('k, 'v, _) t -> min:'k key -> max:'k key -> ('k key * 'v) list

  (** [prev_key t k] returns the largest (key, value) pair in t with key less than k *)
  val prev_key : ('k, 'v, _) t -> 'k key -> ('k key * 'v) option
  (** [next_key t k] returns the smallest (key, value) pair in t with key greater than k *)
  val next_key : ('k, 'v, _) t -> 'k key -> ('k key * 'v) option
  (** [rank t k] if k is in t, returns the number of keys strictly less than k in t,
      otherwise None *)
  val rank : ('k, _, _) t -> 'k key -> int option

  (** [tree t] returns the underlying binary tree that represents the map.  This is useful
      if you want to marshal a map between processes.  Since the set contains a closure,
      it cannot be marshalled between processes that use different executables; however,
      the underlying tree can, so long as the tree type hasn't changed. *)
  type ('a, 'b, 'comparator) tree
  val to_tree : ('k, 'v, 'comparator) t -> ('k key, 'v, 'comparator) tree
end

type ('a, 'comparator, 'z) create_options_without_comparator = 'z

type ('a, 'comparator, 'z) create_options_with_comparator =
  comparator:('a, 'comparator) Comparator.t
  -> 'z

module type Creators = sig
  type ('k, 'v, 'comparator) t
  type ('k, 'v, 'comparator) tree
  type 'k key
  type ('a, 'comparator, 'z) create_options

  (** the empty map *)
  val empty: ('k, 'comparator, ('k, _, 'comparator) t) create_options

  (** map with one key, data pair *)
  val singleton: ('k, 'comparator, 'k key -> 'v -> ('k, 'v, 'comparator) t) create_options

  (** creates map from association list with unique keys *)
  val of_alist:
    ('k,
     'comparator,
     ('k key * 'v) list -> [ `Ok of ('k, 'v, 'comparator) t | `Duplicate_key of 'k key ]
    ) create_options

  (** creates map from association list with unique keys.  Raises an exception if
      duplicate 'a keys are found. *)
  val of_alist_exn
    : ('k, 'comparator, ('k key * 'v) list -> ('k, 'v, 'comparator) t) create_options

  (** creates map from association list with possibly repeated keys. *)
  val of_alist_multi
    : ('k, 'comparator, ('k key * 'v) list -> ('k, 'v list, 'comparator) t) create_options

  (** combines an association list into a map, folding together bound values with common
      keys *)
  val of_alist_fold
    : ('k, 'comparator,
       ('k key * 'v1) list
       -> init:'v2
       -> f:('v2 -> 'v1 -> 'v2)
       -> ('k, 'v2, 'comparator) t
    ) create_options

  val of_tree
    : ('k, 'comparator,
       ('k key, 'v, 'comparator) tree -> ('k, 'v, 'comparator) t
    ) create_options
end

module type S = sig
  module Key : Comparator.S

  type ('k, +'v, 'comparator) map
  type +'v t = (Key.t, 'v, Key.comparator) map with sexp
  type ('k, 'v, 'comparator) t_ = 'v t
  type ('k, 'v, 'comparator) tree
  type 'a key_ = Key.t
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) create_options_without_comparator

  include Creators
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
    with type 'a key := 'a key_
    with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) create_options

  include Accessors
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
    with type 'a key := 'a key_
end

module type S_binable = sig
  include S
  include Binable.S1 with type 'a t := 'a t
end
