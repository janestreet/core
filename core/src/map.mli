(** [Map] is a functional data structure (balanced binary tree) implementing finite maps
    over a totally-ordered domain, called a "key".

    For example:

    {[
      let empty = Map.empty (module String)
      let numbers =
        Map.of_alist_exn (module String)
          ["three", Substr "three"; "four", Substr "four"]
    ]}

    Note that the functions in Map are polymorphic over the type of the key and of the data; you
    just need to pass in the first-class module for the key type (here, [String]).

    Suppose you wanted to define a new module [Foo] to use in a map. You would write:

    {[
      module Foo = struct
        module T = struct
          type t = int * int [@@deriving compare, sexp_of]
        end
        include T
        include Comparable.Make_plain(T)
      end
    ]}

    This gives you a module [Foo] with the appropriate comparator in it, and then this:

    {[
      let m = Map.empty (module Foo)
    ]}

    lets you create a map keyed by [Foo]. The reason you need to write a sexp-converter
    and a comparison function for this to work is that maps both need comparison and the
    ability to serialize the key for generating useful errors.

    {2 The interface}
*)

open! Import
open Map_intf

type (!'key, +!'value, !'cmp) t = ('key, 'value, 'cmp) Base.Map.t

(** Test if invariants of internal AVL search tree hold. *)
val invariants : (_, _, _) t -> bool

val comparator : ('a, _, 'cmp) t -> ('a, 'cmp) Comparator.t
val comparator_s : ('a, _, 'cmp) t -> ('a, 'cmp) Comparator.Module.t

(** The empty map. *)
val empty : ('a, 'cmp) Comparator.Module.t -> ('a, 'b, 'cmp) t

(** Map with one (key, data) pair. *)
val singleton : ('a, 'cmp) Comparator.Module.t -> 'a -> 'b -> ('a, 'b, 'cmp) t

(** Creates map from an association list with unique keys. *)
val of_alist
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) list
  -> [ `Ok of ('a, 'b, 'cmp) t | `Duplicate_key of 'a ]

(** Creates map from an association list with unique keys. Returns an error if duplicate
    ['a] keys are found. *)
val of_alist_or_error
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) list
  -> ('a, 'b, 'cmp) t Or_error.t

(** Creates map from an association list with unique keys. Raises an exception if
    duplicate ['a] keys are found. *)
val of_alist_exn : ('a, 'cmp) Comparator.Module.t -> ('a * 'b) list -> ('a, 'b, 'cmp) t

(** [of_hashtbl_exn] creates a map from bindings present in a hash table.
    [of_hashtbl_exn] raises if there are distinct keys [a1] and [a2] in the table with
    [comparator.compare a1 a2 = 0], which is only possible if the hash-table comparison
    function is different than [comparator.compare]. In the common case, the comparison
    is the same, in which case [of_hashtbl_exn] does not raise, regardless of the keys
    present in the table. *)
val of_hashtbl_exn
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a, 'b) Hashtbl.t
  -> ('a, 'b, 'cmp) t

(** Creates map from an association list with possibly repeated keys. *)
val of_alist_multi
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) list
  -> ('a, 'b list, 'cmp) t

(** Combines an association list into a map, folding together bound values with common
    keys. *)
val of_alist_fold
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) list
  -> init:'c
  -> f:('c -> 'b -> 'c)
  -> ('a, 'c, 'cmp) t

(** Combines an association list into a map, reducing together bound values with common
    keys. *)
val of_alist_reduce
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) list
  -> f:('b -> 'b -> 'b)
  -> ('a, 'b, 'cmp) t

(** [of_iteri ~iteri] behaves like [of_alist], except that instead of taking a concrete
    datastructure, it takes an iteration function. For instance, to convert a string table
    into a map: [of_iteri (module String) ~iteri:(Hashtbl.iteri table)]. It is faster than
    adding the elements one by one. *)
val of_iteri
  :  ('a, 'cmp) Comparator.Module.t
  -> iteri:(f:(key:'a -> data:'b -> unit) -> unit)
  -> [ `Ok of ('a, 'b, 'cmp) t | `Duplicate_key of 'a ]

(** Like [of_iteri] except that it raises an exception if duplicate ['a] keys are found. *)
val of_iteri_exn
  :  ('a, 'cmp) Comparator.Module.t
  -> iteri:(f:(key:'a -> data:'b -> unit) -> unit)
  -> ('a, 'b, 'cmp) t

(**
   {2 Trees}

   Parallel to the map modules [Map] and [Map.Poly], there are also tree modules
   [Map.Tree] and [Map.Poly.Tree]. A tree is a bare representation of a map, without the
   comparator. Thus tree operations need to obtain the comparator from somewhere. For
   [Map.Poly.Tree], the comparator is implicit in the module name. For [Map.Tree], the
   comparator must be passed to each operation.

   The main advantages of trees over maps are slightly improved space usage
   (there is no outer container holding the comparator) and the ability to marshal trees,
   because a tree doesn't contain a closure, the way a map does.

   The main disadvantages of using trees are needing to be more explicit about the
   comparator, and the possibility of accidentally using polymorphic equality on a tree
   (for which maps dynamically detect failure due to the presence of a closure in the data
   structure).
*)

module Tree : sig
  type ('k, +'v, 'cmp) t = ('k, 'v, 'cmp) Tree.t [@@deriving sexp_of]

  include
    Creators_and_accessors_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) t
      with type 'cmp cmp := 'cmp
      with type 'key key := 'key
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_comparator.t
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) With_comparator.t
end

val to_tree : ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) Tree.t

(** Creates a [t] from a [Tree.t] and a [Comparator.t].  This is an O(n) operation as it
    must discover the length of the [Tree.t]. *)
val of_tree : ('k, 'cmp) Comparator.Module.t -> ('k, 'v, 'cmp) Tree.t -> ('k, 'v, 'cmp) t

(** {2 More interface} *)

(** Creates map from a sorted array of key-data pairs. The input array must be sorted, as
    given by the relevant comparator (either in ascending or descending order), and must
    not contain any duplicate keys.  If either of these conditions does not hold, an error
    is returned.  *)
val of_sorted_array
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) array
  -> ('a, 'b, 'cmp) t Or_error.t

(** Like [of_sorted_array] except it returns a map with broken invariants when an [Error]
    would have been returned. *)
val of_sorted_array_unchecked
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) array
  -> ('a, 'b, 'cmp) t

(** [of_increasing_iterator_unchecked c ~len ~f] behaves like
    [of_sorted_array_unchecked c (Array.init len ~f)], with the additional
    restriction that a decreasing order is not supported.  The advantage is not requiring
    you to allocate an intermediate array. [f] will be called with 0, 1, ... [len - 1],
    in order. *)
val of_increasing_iterator_unchecked
  :  ('a, 'cmp) Comparator.Module.t
  -> len:int
  -> f:(int -> 'a * 'b)
  -> ('a, 'b, 'cmp) t

(** [of_increasing_sequence c seq] behaves like [of_sorted_array c
    (Sequence.to_array seq)], but does not allocate the intermediate array.

    The sequence will be folded over once, and the additional time complexity is O(n).
*)
val of_increasing_sequence
  :  ('k, 'cmp) Comparator.Module.t
  -> ('k * 'v) Sequence.t
  -> ('k, 'v, 'cmp) t Or_error.t

(** Creates a map from an association sequence with unique keys.

    [of_sequence c seq] behaves like [of_alist c (Sequence.to_list seq)] but
    does not allocate the intermediate list.

    If your sequence is increasing, use {!of_increasing_sequence} for better performance.
*)
val of_sequence
  :  ('k, 'cmp) Comparator.Module.t
  -> ('k * 'v) Sequence.t
  -> [ `Ok of ('k, 'v, 'cmp) t | `Duplicate_key of 'k ]

(** Creates a map from an association sequence with unique keys, returning an error if
    duplicate ['a] keys are found.

    [of_sequence_or_error c seq] behaves like [of_alist_or_error c (Sequence.to_list seq)]
    but does not allocate the intermediate list.
*)
val of_sequence_or_error
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) Sequence.t
  -> ('a, 'b, 'cmp) t Or_error.t

(** Creates a map from an association sequence with unique keys, raising an exception if
    duplicate ['a] keys are found.

    [of_sequence_exn c seq] behaves like [of_alist_exn c (Sequence.to_list seq)] but
    does not allocate the intermediate list.
*)
val of_sequence_exn
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) Sequence.t
  -> ('a, 'b, 'cmp) t

(** Creates a map from an association sequence with possibly repeated keys. The values in
    the map for a given key appear in the same order as they did in the association
    list.

    [of_sequence_multi c seq] behaves like [of_alist_multi c (Sequence.to_list seq)] but
    does not allocate the intermediate list.
*)
val of_sequence_multi
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) Sequence.t
  -> ('a, 'b list, 'cmp) t

(** Combines an association sequence into a map, folding together bound values with common
    keys.

    [of_sequence_fold c seq ~init ~f] behaves like [of_alist_fold c (Sequence.to_list seq) ~init ~f]
    but does not allocate the intermediate list.
*)
val of_sequence_fold
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) Sequence.t
  -> init:'c
  -> f:('c -> 'b -> 'c)
  -> ('a, 'c, 'cmp) t

(** Combines an association sequence into a map, reducing together bound values with
    common keys.

    [of_sequence_reduce c seq ~f] behaves like [of_alist_reduce c (Sequence.to_list seq) ~f]
    but does not allocate the intermediate list.
*)
val of_sequence_reduce
  :  ('a, 'cmp) Comparator.Module.t
  -> ('a * 'b) Sequence.t
  -> f:('b -> 'b -> 'b)
  -> ('a, 'b, 'cmp) t

(** Constructs a map from a list of values, where [get_key] extracts a key from a value.
*)
val of_list_with_key
  :  ('k, 'cmp) Comparator.Module.t
  -> 'v list
  -> get_key:('v -> 'k)
  -> [ `Ok of ('k, 'v, 'cmp) t | `Duplicate_key of 'k ]

(** Like [of_list_with_key]; returns [Error] on duplicate key. *)
val of_list_with_key_or_error
  :  ('k, 'cmp) Comparator.Module.t
  -> 'v list
  -> get_key:('v -> 'k)
  -> ('k, 'v, 'cmp) t Or_error.t

(** Like [of_list_with_key]; raises on duplicate key. *)
val of_list_with_key_exn
  :  ('k, 'cmp) Comparator.Module.t
  -> 'v list
  -> get_key:('v -> 'k)
  -> ('k, 'v, 'cmp) t

(** Like [of_list_with_key]; produces lists of all values associated with each key. *)
val of_list_with_key_multi
  :  ('k, 'cmp) Comparator.Module.t
  -> 'v list
  -> get_key:('v -> 'k)
  -> ('k, 'v list, 'cmp) t

(** Like [of_list_with_key]; resolves duplicate keys the same way [of_alist_fold] does. *)
val of_list_with_key_fold
  :  ('k, 'cmp) Comparator.Module.t
  -> 'v list
  -> get_key:('v -> 'k)
  -> init:'acc
  -> f:('acc -> 'v -> 'acc)
  -> ('k, 'acc, 'cmp) t

(** Like [of_list_with_key]; resolves duplicate keys the same way [of_alist_fold] does. *)
val of_list_with_key_reduce
  :  ('k, 'cmp) Comparator.Module.t
  -> 'v list
  -> get_key:('v -> 'k)
  -> f:('v -> 'v -> 'v)
  -> ('k, 'v, 'cmp) t

(** Tests whether a map is empty or not. *)
val is_empty : (_, _, _) t -> bool

(** [length map] returns number of elements in [map]. O(1), but [Tree.length] is O(n). *)
val length : (_, _, _) t -> int

(** [add t ~key ~data] adds a new entry to [t] mapping [key] to [data] and returns [`Ok]
    with the new map, or if [key] is already present in [t], returns [`Duplicate]. *)
val add : ('k, 'v, 'cmp) t -> key:'k -> data:'v -> ('k, 'v, 'cmp) t Or_duplicate.t

(** [add_exn t ~key ~data] adds a new entry to [t] mapping [key] to [data] and returns the
    new map, or if [key] is already present in [t], raises. *)
val add_exn : ('k, 'v, 'cmp) t -> key:'k -> data:'v -> ('k, 'v, 'cmp) t

(** Returns a new map with the specified new binding;
    if the key was already bound, its previous binding disappears. *)
val set : ('k, 'v, 'cmp) t -> key:'k -> data:'v -> ('k, 'v, 'cmp) t

(** If [key] is not present then add a singleton list, otherwise, cons data onto the head
    of the existing list. *)
val add_multi : ('k, 'v list, 'cmp) t -> key:'k -> data:'v -> ('k, 'v list, 'cmp) t

(** If [k] is present then remove its head element; if result is empty, remove the key. *)
val remove_multi : ('k, 'v list, 'cmp) t -> 'k -> ('k, 'v list, 'cmp) t

(** [find_multi t key] returns [t]'s values for [key] if [key] is present in the table,
    and returns the empty list otherwise. *)
val find_multi : ('k, 'v list, 'cmp) t -> 'k -> 'v list

(** [change t key ~f] returns a new map [m] that is the same as [t] on all keys except for
    [key], and whose value for [key] is defined by [f], i.e., [find m key = f (find t
    key)]. *)
val change : ('k, 'v, 'cmp) t -> 'k -> f:('v option -> 'v option) -> ('k, 'v, 'cmp) t

(** [update t key ~f] is [change t key ~f:(fun o -> Some (f o))]. *)
val update : ('k, 'v, 'cmp) t -> 'k -> f:('v option -> 'v) -> ('k, 'v, 'cmp) t

(** Returns the value bound to the given key if it exists, and [None] otherwise. *)
val find : ('k, 'v, 'cmp) t -> 'k -> 'v option

(** Returns the value bound to the given key, raising [Caml.Not_found] or [Not_found_s] if
    none exists. *)
val find_exn : ('k, 'v, 'cmp) t -> 'k -> 'v

val find_or_error : ('k, 'v, 'cmp) t -> 'k -> 'v Or_error.t

(** Returns a new map with any binding for the key in question removed. *)
val remove : ('k, 'v, 'cmp) t -> 'k -> ('k, 'v, 'cmp) t

(** [mem map key] tests whether [map] contains a binding for [key]. *)
val mem : ('k, _, 'cmp) t -> 'k -> bool

(** [iter_keys t ~f] calls [f] on every key in the map, going in order from the smallest
    to the largest keys.  *)
val iter_keys : ('k, _, _) t -> f:('k -> unit) -> unit

(** [iter t ~f] calls [f] on every element in the map, going in order from the smallest
    to the largest keys.  *)
val iter : (_, 'v, _) t -> f:('v -> unit) -> unit

(** [iteri t ~f] calls [f] on every key and element in the map, going in order from the
    smallest to the largest keys.  *)
val iteri : ('k, 'v, _) t -> f:(key:'k -> data:'v -> unit) -> unit

module Continue_or_stop : sig
  type t = Base.Map.Continue_or_stop.t =
    | Continue
    | Stop
  [@@deriving compare, enumerate, equal, sexp_of]
end

module Finished_or_unfinished : sig
  type t = Base.Map.Finished_or_unfinished.t =
    | Finished
    | Unfinished
  [@@deriving compare, enumerate, equal, sexp_of]

  (** Maps [Continue] to [Finished] and [Stop] to [Unfinished]. *)
  val of_continue_or_stop : Continue_or_stop.t -> t

  (** Maps [Finished] to [Continue] and [Unfinished] to [Stop]. *)
  val to_continue_or_stop : t -> Continue_or_stop.t
end

(** Iterates until [f] returns [Stop]. If [f] returns [Stop], the final result is
    [Unfinished]. Otherwise, the final result is [Finished]. *)
val iteri_until
  :  ('k, 'v, _) t
  -> f:(key:'k -> data:'v -> Continue_or_stop.t)
  -> Finished_or_unfinished.t

module Merge_element : sig
  type ('left, 'right) t =
    [ `Both of 'left * 'right
    | `Left of 'left
    | `Right of 'right
    ]

  val left : ('left, 'a) t -> 'left option
  val right : ('a, 'right) t -> 'right option
  val left_value : ('left, 'a) t -> default:'left -> 'left
  val right_value : ('a, 'right) t -> default:'right -> 'right

  val values
    :  ('left, 'right) t
    -> left_default:'left
    -> right_default:'right
    -> 'left * 'right
end

(** Iterates two maps side by side. The complexity of this function is O(M+N). If two
    inputs are [[(0, a); (1, a)]] and [[(1, b); (2, b)]], [f] will be called with
    [[(0, `Left a); (1, `Both (a, b)); (2, `Right b)]] *)
val iter2
  :  ('k, 'v1, 'cmp) t
  -> ('k, 'v2, 'cmp) t
  -> f:(key:'k -> data:('v1, 'v2) Merge_element.t -> unit)
  -> unit

(** Returns new map with bound values replaced by the result of [f] applied to them. *)
val map : ('k, 'v1, 'cmp) t -> f:('v1 -> 'v2) -> ('k, 'v2, 'cmp) t

(** Like [map], but [f] takes both key and data as arguments. *)
val mapi : ('k, 'v1, 'cmp) t -> f:(key:'k -> data:'v1 -> 'v2) -> ('k, 'v2, 'cmp) t

(** Convert map with keys of type ['k1] to a map with keys of type ['k2] using [f]. *)
val map_keys
  :  ('k2, 'cmp2) Comparator.Module.t
  -> ('k1, 'v, 'cmp1) t
  -> f:('k1 -> 'k2)
  -> [ `Ok of ('k2, 'v, 'cmp2) t | `Duplicate_key of 'k2 ]

(** Like [map_keys], but raises on duplicate key. *)
val map_keys_exn
  :  ('k2, 'cmp2) Comparator.Module.t
  -> ('k1, 'v, 'cmp1) t
  -> f:('k1 -> 'k2)
  -> ('k2, 'v, 'cmp2) t

(** Folds over keys and data in map in increasing order of key. *)
val fold : ('k, 'v, _) t -> init:'a -> f:(key:'k -> data:'v -> 'a -> 'a) -> 'a

(** Folds over keys and data in the map in increasing order of [key], until the first
    time that [f] returns [Stop _]. If [f] returns [Stop final], this function returns
    immediately with the value [final]. If [f] never returns [Stop _], and the final
    call to [f] returns [Continue last], this function returns [finish last]. *)
val fold_until
  :  ('k, 'v, _) t
  -> init:'acc
  -> f:(key:'k -> data:'v -> 'acc -> ('acc, 'final) Container.Continue_or_stop.t)
  -> finish:('acc -> 'final)
  -> 'final

(** Folds over keys and data in map in decreasing order of key. *)
val fold_right : ('k, 'v, _) t -> init:'a -> f:(key:'k -> data:'v -> 'a -> 'a) -> 'a

(** Folds over two maps side by side, like [iter2]. *)
val fold2
  :  ('k, 'v1, 'cmp) t
  -> ('k, 'v2, 'cmp) t
  -> init:'a
  -> f:(key:'k -> data:('v1, 'v2) Merge_element.t -> 'a -> 'a)
  -> 'a

(** [filter], [filteri], [filter_keys], [filter_map], and [filter_mapi] run in O(n * lg n)
    time; they simply accumulate each key & data retained by [f] into a new map using
    [add]. *)

val filter_keys : ('k, 'v, 'cmp) t -> f:('k -> bool) -> ('k, 'v, 'cmp) t
val filter : ('k, 'v, 'cmp) t -> f:('v -> bool) -> ('k, 'v, 'cmp) t
val filteri : ('k, 'v, 'cmp) t -> f:(key:'k -> data:'v -> bool) -> ('k, 'v, 'cmp) t

(** Returns new map with bound values filtered by the result of [f] applied to them. *)
val filter_map : ('k, 'v1, 'cmp) t -> f:('v1 -> 'v2 option) -> ('k, 'v2, 'cmp) t

(** Like [filter_map], but function takes both key and data as arguments. *)
val filter_mapi
  :  ('k, 'v1, 'cmp) t
  -> f:(key:'k -> data:'v1 -> 'v2 option)
  -> ('k, 'v2, 'cmp) t

(** [partition_mapi t ~f] returns two new [t]s, with each key in [t] appearing in exactly
    one of the result maps depending on its mapping in [f]. *)
val partition_mapi
  :  ('k, 'v1, 'cmp) t
  -> f:(key:'k -> data:'v1 -> ('v2, 'v3) Either.t)
  -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t

(** [partition_map t ~f = partition_mapi t ~f:(fun ~key:_ ~data -> f data)] *)
val partition_map
  :  ('k, 'v1, 'cmp) t
  -> f:('v1 -> ('v2, 'v3) Either.t)
  -> ('k, 'v2, 'cmp) t * ('k, 'v3, 'cmp) t

(**
   {[
     partitioni_tf t ~f
     =
     partition_mapi t ~f:(fun ~key ~data ->
       if f ~key ~data
       then First data
       else Second data)
   ]}
*)
val partitioni_tf
  :  ('k, 'v, 'cmp) t
  -> f:(key:'k -> data:'v -> bool)
  -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

(** [partition_tf t ~f = partitioni_tf t ~f:(fun ~key:_ ~data -> f data)] *)
val partition_tf
  :  ('k, 'v, 'cmp) t
  -> f:('v -> bool)
  -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

(** Produces [Ok] of a map including all keys if all data is [Ok], or an [Error]
    including all errors otherwise. *)
val combine_errors : ('k, 'v Or_error.t, 'cmp) t -> ('k, 'v, 'cmp) t Or_error.t

(** Given a map of tuples, produces a tuple of maps. Equivalent to:
    [map t ~f:fst, map t ~f:snd] *)
val unzip : ('k, 'v1 * 'v2, 'cmp) t -> ('k, 'v1, 'cmp) t * ('k, 'v2, 'cmp) t

(** Total ordering between maps.  The first argument is a total ordering used to compare
    data associated with equal keys in the two maps. *)
val compare_direct : ('v -> 'v -> int) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> int

(** Hash function: a building block to use when hashing data structures containing
    maps in them. [hash_fold_direct hash_fold_key] is compatible with
    [compare_direct] iff [hash_fold_key] is compatible with [(comparator m).compare]
    of the map [m] being hashed. *)
val hash_fold_direct : 'k Hash.folder -> 'v Hash.folder -> ('k, 'v, 'cmp) t Hash.folder

(** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are equal, that is, contain
    equal keys and associate them with equal data.  [cmp] is the equality predicate used
    to compare the data associated with the keys. *)
val equal : ('v -> 'v -> bool) -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> bool

(** Returns list of keys in map in increasing order. *)
val keys : ('k, _, _) t -> 'k list

(** Returns list of data in map in increasing order of key. *)
val data : (_, 'v, _) t -> 'v list

(** Creates association list from map.

    @param key_order default is [`Increasing]
*)
val to_alist : ?key_order:[ `Increasing | `Decreasing ] -> ('k, 'v, _) t -> ('k * 'v) list

val validate : name:('k -> string) -> 'v Validate.check -> ('k, 'v, _) t Validate.check

val validatei
  :  name:('k -> string)
  -> ('k * 'v) Validate.check
  -> ('k, 'v, _) t Validate.check

(** {2 Additional operations on maps} *)

(** Merges two maps. The runtime is O(length(t1) + length(t2)). In particular,
    you shouldn't use this function to merge a list of maps. Consider using
    [merge_disjoint_exn] or [merge_skewed] instead. *)
val merge
  :  ('k, 'v1, 'cmp) t
  -> ('k, 'v2, 'cmp) t
  -> f:(key:'k -> ('v1, 'v2) Merge_element.t -> 'v3 option)
  -> ('k, 'v3, 'cmp) t

(** Merges two dictionaries with the same type of data and disjoint sets of keys.
    Raises if any keys overlap. *)
val merge_disjoint_exn : ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t -> ('k, 'v, 'cmp) t

(** A special case of [merge], [merge_skewed t1 t2] is a map containing all the
    bindings of [t1] and [t2]. Bindings that appear in both [t1] and [t2] are
    merged using the [combine] function. In a call [combine ~key v1 v2] the
    value [v1] comes from [t1] and [v2] from [t2].

    The runtime of [merge_skewed] is [O(l1 * log(l2))], where [l1] is the length
    of the smaller map and [l2] the length of the larger map. This is likely to
    be faster than [merge] when one of the maps is a lot smaller, or when you
    merge a list of maps. *)
val merge_skewed
  :  ('k, 'v, 'cmp) t
  -> ('k, 'v, 'cmp) t
  -> combine:(key:'k -> 'v -> 'v -> 'v)
  -> ('k, 'v, 'cmp) t

module Symmetric_diff_element : sig
  type ('k, 'v) t = 'k * [ `Left of 'v | `Right of 'v | `Unequal of 'v * 'v ]
  [@@deriving bin_io, compare, sexp]

  val map_data : ('k, 'v1) t -> f:('v1 -> 'v2) -> ('k, 'v2) t

  (** [left] is defined as:
      {[ function
        | (`Left x | `Unequal (x, _)) -> Some x
        | `Right _ -> None
      ]}
      and [right] is similar.
  *)

  val left : (_, 'v) t -> 'v option
  val right : (_, 'v) t -> 'v option
end

(** [symmetric_diff t1 t2 ~data_equal] returns a list of changes between [t1] and [t2].
    It is intended to be efficient in the case where [t1] and [t2] share a large amount of
    structure. In the case where [t2] (resp. [t1]) is obtained by applying k additions
    and/or removals to [t1] (resp. [t2]), this runs in [min(O(k log n), O(n))], where [n]
    is [length t1 + length t2]. The keys in the output sequence will be in sorted order. *)
val symmetric_diff
  :  ('k, 'v, 'cmp) t
  -> ('k, 'v, 'cmp) t
  -> data_equal:('v -> 'v -> bool)
  -> ('k, 'v) Symmetric_diff_element.t Sequence.t

(** [fold_symmetric_diff t1 t2 ~data_equal] folds across an implicit sequence of changes
    between [t1] and [t2], in sorted order by keys. Equivalent to
    [Sequence.fold (symmetric_diff t1 t2 ~data_equal)], and more efficient. *)
val fold_symmetric_diff
  :  ('k, 'v, 'cmp) t
  -> ('k, 'v, 'cmp) t
  -> data_equal:('v -> 'v -> bool)
  -> init:'a
  -> f:('a -> ('k, 'v) Symmetric_diff_element.t -> 'a)
  -> 'a

(** [min_elt map] returns [Some (key, data)] pair corresponding to the minimum key in
    [map], [None] if [map] is empty. *)
val min_elt : ('k, 'v, _) t -> ('k * 'v) option

val min_elt_exn : ('k, 'v, _) t -> 'k * 'v

(** [max_elt map] returns [Some (key, data)] pair corresponding to the maximum key in
    [map], and [None] if [map] is empty. *)
val max_elt : ('k, 'v, _) t -> ('k * 'v) option

val max_elt_exn : ('k, 'v, _) t -> 'k * 'v

(** Swap the inner and outer keys of nested maps. If [transpose_keys m a = b], then
    [find_exn (find_exn a i) j = find_exn (find_exn b j) i]. *)
val transpose_keys
  :  ('k2, 'cmp2) Comparator.Module.t
  -> ('k1, ('k2, 'v, 'cmp2) t, 'cmp1) t
  -> ('k2, ('k1, 'v, 'cmp1) t, 'cmp2) t

(** The following functions have the same semantics as similar functions in
    {!Core.List}. *)

val for_all : (_, 'v, _) t -> f:('v -> bool) -> bool
val for_alli : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) -> bool
val exists : (_, 'v, _) t -> f:('v -> bool) -> bool
val existsi : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) -> bool
val count : (_, 'v, _) t -> f:('v -> bool) -> int
val counti : ('k, 'v, _) t -> f:(key:'k -> data:'v -> bool) -> int

val sum
  :  (module Container.Summable with type t = 'a)
  -> (_, 'v, _) t
  -> f:('v -> 'a)
  -> 'a

val sumi
  :  (module Container.Summable with type t = 'a)
  -> ('k, 'v, _) t
  -> f:(key:'k -> data:'v -> 'a)
  -> 'a

(** [split t key] returns a map of keys strictly less than [key], the mapping of [key] if
    any, and a map of keys strictly greater than [key].

    Runtime is O(m + log n) where n is the size of the input map, and m is the size of the
    smaller of the two output maps.  The O(m) term is due to the need to calculate the
    length of the output maps. **)
val split
  :  ('k, 'v, 'cmp) t
  -> 'k
  -> ('k, 'v, 'cmp) t * ('k * 'v) option * ('k, 'v, 'cmp) t

(** [split_le_gt t key] returns a map of keys that are less or equal to [key] and a
    map of keys strictly greater than [key].

    Runtime is O(m + log n), where n is the size of the input map and m is the size of
    the smaller of the two output maps.  The O(m) term is due to the need to calculate
    the length of the output maps. *)
val split_le_gt : ('k, 'v, 'cmp) t -> 'k -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

(** [split_lt_ge t key] returns a map of keys strictly less than [key] and a map of
    keys that are greater or equal to [key].

    Runtime is O(m + log n), where n is the size of the input map and m is the size of
    the smaller of the two output maps.  The O(m) term is due to the need to calculate
    the length of the output maps. *)
val split_lt_ge : ('k, 'v, 'cmp) t -> 'k -> ('k, 'v, 'cmp) t * ('k, 'v, 'cmp) t

(** [append ~lower_part ~upper_part] returns [`Ok map] where [map] contains all the [(key,
    value)] pairs from the two input maps if all the keys from [lower_part] are less than
    all the keys from [upper_part]. Otherwise it returns [`Overlapping_key_ranges].

    Runtime is O(log n) where n is the size of the larger input map. This can be
    significantly faster than [Map.merge] or repeated [Map.add].

    {[
      assert (match Map.append ~lower_part ~upper_part with
        | `Ok whole_map ->
          whole_map
          = Map.(of_alist_exn (List.append (to_alist lower_part) (to_alist upper_part)))
        | `Overlapping_key_ranges -> true);
    ]}
*)
val append
  :  lower_part:('k, 'v, 'cmp) t
  -> upper_part:('k, 'v, 'cmp) t
  -> [ `Ok of ('k, 'v, 'cmp) t | `Overlapping_key_ranges ]

(** [subrange t ~lower_bound ~upper_bound] returns a map containing all the entries from
    [t] whose keys lie inside the interval indicated by [~lower_bound] and [~upper_bound].
    If this interval is empty, an empty map is returned.

    Runtime is O(m + log n) where n is the size of the input map, and m is the size of the
    output map.  The O(m) term is due to the need to calculate the length of the output
    map. *)
val subrange
  :  ('k, 'v, 'cmp) t
  -> lower_bound:'k Maybe_bound.t
  -> upper_bound:'k Maybe_bound.t
  -> ('k, 'v, 'cmp) t

(** [fold_range_inclusive t ~min ~max ~init ~f] folds [f] (with initial value [~init])
    over all keys (and their associated values) that are in the range [[min, max]]
    (inclusive).
*)
val fold_range_inclusive
  :  ('k, 'v, 'cmp) t
  -> min:'k
  -> max:'k
  -> init:'a
  -> f:(key:'k -> data:'v -> 'a -> 'a)
  -> 'a

(** [range_to_alist t ~min ~max] returns an associative list of the elements whose
    keys lie in [[min, max]] (inclusive), with the smallest key being at the head of the
    list. *)
val range_to_alist : ('k, 'v, 'cmp) t -> min:'k -> max:'k -> ('k * 'v) list

(** [closest_key t dir k] returns the [(key, value)] pair in [t] with [key] closest to
    [k], which satisfies the given inequality bound.

    For example, [closest_key t `Less_than k] would be the pair with the closest key to
    [k] where [key < k].

    [to_sequence] can be used to get the same results as [closest_key].  It is less
    efficient for individual lookups but more efficient for finding many elements starting
    at some value. *)
val closest_key
  :  ('k, 'v, 'cmp) t
  -> [ `Greater_or_equal_to | `Greater_than | `Less_or_equal_to | `Less_than ]
  -> 'k
  -> ('k * 'v) option

(** [nth t n] finds the (key, value) pair of rank n (i.e., such that there are exactly n
    keys strictly less than the found key), if one exists.  O(log(length t) + n) time. *)
val nth : ('k, 'v, _) t -> int -> ('k * 'v) option

val nth_exn : ('k, 'v, _) t -> int -> 'k * 'v

(** [rank t k] if [k] is in [t], returns the number of keys strictly less than [k] in [t],
    otherwise [None]. *)
val rank : ('k, 'v, 'cmp) t -> 'k -> int option

(** [to_sequence ?order ?keys_greater_or_equal_to ?keys_less_or_equal_to t] gives a
    sequence of key-value pairs between [keys_less_or_equal_to] and
    [keys_greater_or_equal_to] inclusive, presented in [order]. If
    [keys_greater_or_equal_to > keys_less_or_equal_to], the sequence is empty. Cost is
    O(log n) up front and amortized O(1) to produce each element.

    @param order [`Increasing_key] is the default
*)
val to_sequence
  :  ?order:[ `Increasing_key | `Decreasing_key ]
  -> ?keys_greater_or_equal_to:'k
  -> ?keys_less_or_equal_to:'k
  -> ('k, 'v, 'cmp) t
  -> ('k * 'v) Sequence.t

(** [binary_search t ~compare which elt] returns the [(key, value)] pair in [t]
    specified by [compare] and [which], if one exists.

    [t] must be sorted in increasing order according to [compare], where [compare] and
    [elt] divide [t] into three (possibly empty) segments:

    {v
      |  < elt  |  = elt  |  > elt  |
    v}

    [binary_search] returns an element on the boundary of segments as specified by
    [which].  See the diagram below next to the [which] variants.

    [binary_search] does not check that [compare] orders [t], and behavior is
    unspecified if [compare] doesn't order [t].  Behavior is also unspecified if
    [compare] mutates [t]. *)
val binary_search
  :  ('k, 'v, 'cmp) t
  -> compare:(key:'k -> data:'v -> 'key -> int)
  -> [ `Last_strictly_less_than (**        {v | < elt X |                       v} *)
     | `Last_less_than_or_equal_to (**     {v |      <= elt       X |           v} *)
     | `Last_equal_to (**                  {v           |   = elt X |           v} *)
     | `First_equal_to (**                 {v           | X = elt   |           v} *)
     | `First_greater_than_or_equal_to (** {v           | X       >= elt      | v} *)
     | `First_strictly_greater_than (**    {v                       | X > elt | v} *)
     ]
  -> 'key
  -> ('k * 'v) option

(** [binary_search_segmented t ~segment_of which] takes a [segment_of] function that
    divides [t] into two (possibly empty) segments:

    {v
      | segment_of elt = `Left | segment_of elt = `Right |
    v}

    [binary_search_segmented] returns the [(key, value)] pair on the boundary of the
    segments as specified by [which]: [`Last_on_left] yields the last element of the
    left segment, while [`First_on_right] yields the first element of the right segment.
    It returns [None] if the segment is empty.

    [binary_search_segmented] does not check that [segment_of] segments [t] as in the
    diagram, and behavior is unspecified if [segment_of] doesn't segment [t].  Behavior
    is also unspecified if [segment_of] mutates [t]. *)
val binary_search_segmented
  :  ('k, 'v, 'cmp) t
  -> segment_of:(key:'k -> data:'v -> [ `Left | `Right ])
  -> [ `Last_on_left | `First_on_right ]
  -> ('k * 'v) option

(** [binary_search_subrange] takes a [compare] function that divides [t] into three
    (possibly empty) segments with respect to [lower_bound] and [upper_bound]:

    {v
      | Below_lower_bound | In_range | Above_upper_bound |
    v}

    and returns a map of the key-value pairs in [range].

    Runtime is O(log n + m) where [n] is the length of the input map and [m] is the length
    of the output. The linear term is to compute the length of the output.

    Behavior is undefined if [compare] does not segment [t] as shown above, or is
    [compare] mutates its inputs. *)
val binary_search_subrange
  :  ('k, 'v, 'cmp) t
  -> compare:(key:'k -> data:'v -> 'bound -> int)
  -> lower_bound:'bound Maybe_bound.t
  -> upper_bound:'bound Maybe_bound.t
  -> ('k, 'v, 'cmp) t

(** Creates traversals to reconstruct a map within an applicative. Uses
    [Lazy_applicative] so that the map can be traversed within the applicative, rather
    than needing to be traversed all at once, outside the applicative. *)
module Make_applicative_traversals (A : Applicative.Lazy_applicative) : sig
  val mapi
    :  ('k, 'v1, 'cmp) t
    -> f:(key:'k -> data:'v1 -> 'v2 A.t)
    -> ('k, 'v2, 'cmp) t A.t

  val filter_mapi
    :  ('k, 'v1, 'cmp) t
    -> f:(key:'k -> data:'v1 -> 'v2 option A.t)
    -> ('k, 'v2, 'cmp) t A.t
end

(** Convert a set to a map. Runs in [O(length t)] time plus a call to [f] for each key to
    compute the associated data. *)
val of_key_set : ('key, 'cmp) Base.Set.t -> f:('key -> 'data) -> ('key, 'data, 'cmp) t

(** Converts a map to a set of its keys. Runs in [O(length t)] time. *)
val key_set : ('key, _, 'cmp) t -> ('key, 'cmp) Base.Set.t

val quickcheck_generator
  :  ('k, 'cmp) Comparator.Module.t
  -> 'k Quickcheck.Generator.t
  -> 'v Quickcheck.Generator.t
  -> ('k, 'v, 'cmp) t Quickcheck.Generator.t

val quickcheck_observer
  :  'k Quickcheck.Observer.t
  -> 'v Quickcheck.Observer.t
  -> ('k, 'v, 'cmp) t Quickcheck.Observer.t

(** This shrinker and the other shrinkers for maps and trees produce a shrunk
    value by dropping a key-value pair, shrinking a key or shrinking a value.
    A shrunk key will override an existing key's value. *)
val quickcheck_shrinker
  :  'k Quickcheck.Shrinker.t
  -> 'v Quickcheck.Shrinker.t
  -> ('k, 'v, 'cmp) t Quickcheck.Shrinker.t

(**
   {2 Which Map module should you use?}

   The map types and operations appear in three places:

   - Map: polymorphic map operations
   - Map.Poly: maps that use polymorphic comparison to order keys
   - Key.Map: maps with a fixed key type that use [Key.compare] to order keys

   where [Key] is any module defining values that can be used as keys of a map, like
   [Int], [String], etc. To add this functionality to an arbitrary module, use the
   [Comparable.Make] functor.

   You should use [Map] for functions that access existing maps, like [find], [mem],
   [add], [fold], [iter], and [to_alist]. For functions that create maps, like [empty],
   [singleton], and [of_alist], strive to use the corresponding [Key.Map] function, which
   will use the comparison function specifically for [Key]. As a last resort, if you
   don't have easy access to a comparison function for the keys in your map, use
   [Map.Poly] to create the map. This will use OCaml's built-in polymorphic comparison to
   compare keys, with all the usual performance and robustness problems that entails.


   {2 Interface design details}

   An instance of the map type is determined by the types of the map's keys and values,
   and the comparison function used to order the keys:

   {[ type ('key, 'value, 'cmp) Map.t ]}

   ['cmp] is a phantom type uniquely identifying the comparison function, as generated by
   [Comparator.Make].

   [Map.Poly] supports arbitrary key and value types, but enforces that the comparison
   function used to order the keys is polymorphic comparison. [Key.Map] has a fixed key
   type and comparison function, and supports arbitrary values.

   {[
     type ('key, 'value) Map.Poly.t = ('key , 'value, Comparator.Poly.t     ) Map.t
     type 'value Key.Map.t          = (Key.t, 'value, Key.comparator_witness) Map.t
   ]}

   The same map operations exist in [Map], [Map.Poly], and [Key.Map], albeit with
   different types. For example:

   {[
     val Map.length      : (_, _, _) Map.t   -> int
     val Map.Poly.length : (_, _) Map.Poly.t -> int
     val Key.Map.length  : _ Key.Map.t       -> int
   ]}

   Because [Map.Poly.t] and [Key.Map.t] are exposed as instances of the more general
   [Map.t] type, one can use [Map.length] on any map. The same is true for all of the
   functions that access an existing map, such as [add], [change], [find], [fold],
   [iter], [map], [to_alist], etc.

   Depending on the number of type variables [N], the type of accessor (resp. creator)
   functions is defined in the module type [AccessorsN] ([CreatorsN]) in {!Map_intf}.
   Also for creators, when the comparison function is not fixed, i.e., the ['cmp]
   variable of [Map.t] is free, we need to pass a comparator to the function creating the
   map. The module type is called [Creators3_with_comparator]. There is also a module
   type [Accessors3_with_comparator] in addition to [Accessors3] which used for trees
   since the comparator is not known.
*)

module Using_comparator : sig
  include
    Creators_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
      with type 'k key := 'k
      with type 'c cmp := 'c
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_comparator.t
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t
end

module Poly : sig
  type ('a, +'b, 'c) map

  module Tree : sig
    type comparator_witness = Comparator.Poly.comparator_witness
    type ('k, +'v) t = ('k, 'v, comparator_witness) Tree.t [@@deriving sexp, sexp_grammar]

    include
      Creators_and_accessors_generic
        with type ('a, 'b, 'c) t := ('a, 'b) t
        with type ('a, 'b, 'c) tree := ('a, 'b) t
        with type 'k key := 'k
        with type 'c cmp := comparator_witness
        with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Without_comparator.t
        with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t
  end

  type comparator_witness = Comparator.Poly.comparator_witness

  type ('a, +'b) t = ('a, 'b, comparator_witness) map
  [@@deriving bin_io, sexp, sexp_grammar, compare]

  include
    Creators_and_accessors_generic
      with type ('a, 'b, 'c) t := ('a, 'b) t
      with type ('a, 'b, 'c) tree := ('a, 'b) Tree.t
      with type 'k key := 'k
      with type 'c cmp := comparator_witness
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Without_comparator.t
      with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) Without_comparator.t
end
with type ('a, 'b, 'c) map = ('a, 'b, 'c) t

module type Key_plain = Key_plain
module type Key = Key
module type Key_binable = Key_binable
module type S_plain = S_plain
module type S = S
module type S_binable = S_binable

module Make_plain (Key : Key_plain) : S_plain with type Key.t = Key.t

module Make_plain_using_comparator (Key : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) :
  S_plain
    with type Key.t = Key.t
    with type Key.comparator_witness = Key.comparator_witness

module Make (Key : Key) : S with type Key.t = Key.t

module Make_using_comparator (Key : sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end) : S with type Key.t = Key.t with type Key.comparator_witness = Key.comparator_witness

module Make_binable (Key : Key_binable) : S_binable with type Key.t = Key.t

module Make_binable_using_comparator (Key : sig
  type t [@@deriving bin_io, sexp]

  include Comparator.S with type t := t
end) :
  S_binable
    with type Key.t = Key.t
    with type Key.comparator_witness = Key.comparator_witness

module Key_bin_io = Key_bin_io
include For_deriving with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

module Make_tree_plain (Key : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) : Make_S_plain_tree(Key).S

module Make_tree (Key : sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end) : sig
  include Make_S_plain_tree(Key).S
  include Sexpable.S1 with type 'a t := 'a t
end

(** The following functors may be used to define stable modules *)
module Stable : sig
  module V1 : sig
    type nonrec ('a, 'b, 'c) t = ('a, 'b, 'c) t

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

    include For_deriving with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
    include For_deriving_stable with type ('a, 'b, 'c) t := ('a, 'b, 'c) t

    module Make (Key : Stable_module_types.S0) :
      S with type key := Key.t with type comparator_witness := Key.comparator_witness

    module With_stable_witness : sig
      module type S = sig
        include S

        val stable_witness : 'a Stable_witness.t -> 'a t Stable_witness.t
      end

      module Make (Key : Stable_module_types.With_stable_witness.S0) :
        S with type key := Key.t with type comparator_witness := Key.comparator_witness
    end
  end

  module Symmetric_diff_element : sig
    module V1 :
      Stable_module_types.With_stable_witness.S2
        with type ('a, 'b) t = ('a, 'b) Symmetric_diff_element.t
  end
end
