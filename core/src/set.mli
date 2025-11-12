@@ portable

(** This module defines the [Set] module for [Core]. Functions that construct a set take
    as an argument the comparator for the element type.

    This module uses the same organizational approach as {{!Map} [Map]}. *)

open! Import
open Set_intf

(** The type of a set. The first type parameter identifies the type of the element, and
    the second identifies the comparator, which determines the comparison function that is
    used for ordering elements in this set. Many operations (e.g., {!union}), require that
    they be passed sets with the same element type and the same comparator type. *)
type ('elt, 'cmp) t = ('elt, 'cmp) Base.Set.t [@@deriving compare ~localize]

module Tree : sig
  type weight = Tree.weight

  (** A [Tree.t] contains just the tree data structure that a set is based on, without
      including the comparator. Accordingly, any operation on a [Tree.t] must also take as
      an argument the corresponding comparator. *)
  type ('elt, 'cmp) t = ('elt, 'cmp) Tree.t = private
    | Empty
    | Leaf of { global_ elt : 'elt }
    | Node of
        { global_ left : ('elt, 'cmp) t
        ; global_ elt : 'elt
        ; global_ right : ('elt, 'cmp) t
        ; weight : weight
        }
  [@@deriving sexp_of]

  module Named = Tree.Named

  include
    Creators_and_accessors_generic
    with type ('a, 'b) set := ('a, 'b) t
    with type ('a, 'b) t := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) t
    with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_comparator.t
    with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) With_comparator.t
    with type 'a elt := 'a
    with type 'c cmp := 'c
    with module Named := Named

  module Expert : sig
    (** Sexp prints the internal node structure. *)
    type nonrec ('a, 'cmp) t = ('a, 'cmp) t [@@deriving sexp_of]

    (** Just the tree balance checks from [invariants]. Excludes the checks in
        [order_invariants]. *)
    val balance_invariants : (_, _) t -> bool

    (** Just the key ordering checks from [invariants]. Excludes the checks in
        [balance_invariants]. *)
    val order_invariants : comparator:('a, 'cmp) Comparator.t -> ('a, 'cmp) t -> bool

    (** Reports whether two trees are sufficiently balanced for
        [create_assuming_balanced_unchecked]. Two trees with the same or mirrored shape
        are guaranteed to be balanced. The left and right subtrees of a [Node] constructor
        are also guaranteed to be balanced.

        We do not describe our balance invariants in detail in this interface, as they
        have changed in the past and may change again in the future. *)
    val are_balanced : ('a, 'cmp) t -> ('a, 'cmp) t -> bool

    (** Reports whether two trees are sufficiently balanced for
        [create_and_rebalance_at_most_once_unchecked].

        If two trees satisfy [are_balanced], at most a single key is added or removed from
        one of them, and the tree is rebuilt via
        [create_and_rebalance_at_most_once_unchecked], then the result should satisfy
        [need_rebalance_at_most_once].

        The preceding operations are equivalent to a single call to most single-key update
        functions, e.g. [add], [remove], [change], etc. *)
    val need_rebalance_at_most_once : ('a, 'cmp) t -> ('a, 'cmp) t -> bool

    (** [create_assuming_balanced_unchecked left key data right] constructs a single
        [Node]. Given keys must be unique and strictly sorted, and
        [are_balanced left right] must be true. Otherwise set/tree behavior will be
        unspecified. *)
    val create_assuming_balanced_unchecked
      :  ('a, 'cmp) t
      -> 'a
      -> ('a, 'cmp) t
      -> ('a, 'cmp) t

    (** [create_and_rebalance_at_most_once_unchecked left key data right] constructs a
        [Node], possibly rebalancing [left] and [right] once. Given keys must be unique
        and strictly sorted, and [need_rebalance_at_most_once left right] must be true.
        Otherwise set/tree behavior will be unspecified. *)
    val create_and_rebalance_at_most_once_unchecked
      :  ('a, 'cmp) t
      -> 'a
      -> ('a, 'cmp) t
      -> ('a, 'cmp) t

    (** [create_and_rebalance_unchecked left key data right] constructs a [Node], possibly
        rebalancing [left] and [right] recursively. Given keys must be unique and strictly
        sorted. Otherwise set/tree behavior will be unspecified. The subtrees may be
        arbitrarily imbalanced with respect to each other. *)
    val create_and_rebalance_unchecked
      :  ('a, 'cmp) t
      -> 'a
      -> ('a, 'cmp) t
      -> ('a, 'cmp) t

    (** [concat_and_rebalance_at_most_once_unchecked left right] appends [left] and
        [right] in that order, possibly rebalancing the result once. Given keys must be
        unique and strictly sorted, and [are_balanced left right] must be true. Otherwise
        set/tree behavior will be unspecified. *)
    val concat_and_rebalance_at_most_once_unchecked
      :  ('a, 'cmp) t
      -> ('a, 'cmp) t
      -> ('a, 'cmp) t

    (** [concat_and_rebalance_unchecked left right] appends [left] and [right] in that
        order, rebalancing the result recursively. Given keys must be unique and strictly
        sorted. Otherwise set/tree behavior will be unspecified. The subtrees may be
        arbitrarily imbalanced with respect to each other. *)
    val concat_and_rebalance_unchecked : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

    (** Like [Tree.singleton], but does not require a comparator. *)
    val singleton : 'a -> ('a, 'cmp) t

    (** Equivalent to [empty_without_value_restriction]. *)
    val empty : ('a, 'cmp) t

    (** Compute a tree's length from its weight field. *)
    val length_of_weight : weight -> int
  end
end

module Using_comparator : sig
  include
    Creators_generic
    with type ('a, 'b) set := ('a, 'b) t
    with type ('a, 'b) t := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) Tree.t
    with type 'a elt := 'a
    with type 'c cmp := 'c
    with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_comparator.t
end

(** Tests internal invariants of the set data structure. Returns true on success. *)
val invariants : (_, _) t -> bool

(** Returns a first-class module that can be used to build other map/set/etc with the same
    notion of comparison. *)
val comparator_s : ('a, 'cmp) t -> ('a, 'cmp) Comparator.Module.t

val comparator : ('a, 'cmp) t -> ('a, 'cmp) Comparator.t

(** Creates an empty set based on the provided comparator. *)
val empty : ('a, 'cmp) Comparator.Module.t -> ('a, 'cmp) t

(** Creates a set based on the provided comparator that contains only the provided
    element. *)
val singleton : ('a, 'cmp) Comparator.Module.t -> 'a -> ('a, 'cmp) t

(** Returns the cardinality of the set. [O(1)]. *)
val length : (_, _) t -> int

(** [is_empty t] is [true] iff [t] is empty. [O(1)]. *)
val is_empty : (_, _) t -> bool

(** [mem t a] returns [true] iff [a] is in [t]. [O(log n)]. *)
val mem : ('a, _) t -> 'a -> bool

(** [add t a] returns a new set with [a] added to [t], or returns [t] if [mem t a].
    [O(log n)]. *)
val add : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t

(** [remove t a] returns a new set with [a] removed from [t] if [mem t a], or returns [t]
    otherwise. [O(log n)]. *)
val remove : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t

(** [union t1 t2] returns the union of the two sets. [O(length t1 + length t2)]. *)
val union : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

(** [union c list] returns the union of all the sets in [list]. The [c] argument is
    required for the case where [list] is empty. [O(max(List.length list, n log n))],
    where [n] is the sum of sizes of the input sets. *)
val union_list : ('a, 'cmp) Comparator.Module.t -> ('a, 'cmp) t list -> ('a, 'cmp) t

(** [inter t1 t2] computes the intersection of sets [t1] and [t2].
    [O(length t1 + length t2)]. *)
val inter : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

(** [diff t1 t2] computes the set difference [t1 - t2], i.e., the set containing all
    elements in [t1] that are not in [t2]. [O(length t1 + length t2)]. *)
val diff : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'cmp) t

(** [symmetric_diff t1 t2] returns a sequence of changes between [t1] and [t2]. It is
    intended to be efficient in the case where [t1] and [t2] share a large amount of
    structure. In the case where [t2] (resp. [t1]) is obtained by applying k additions
    and/or removals to [t1] (resp. [t2]), this runs in [min(O(k log n), O(n))], where [n]
    is [length t1 + length t2]. *)
val symmetric_diff : ('a, 'cmp) t -> ('a, 'cmp) t -> ('a, 'a) Either.t Sequence.t

(** [compare_direct t1 t2] compares the sets [t1] and [t2]. It returns the same result as
    [compare], but unlike compare, doesn't require arguments to be passed in for the type
    parameters of the set. [O(length t1 + length t2)]. *)
val%template compare_direct : ('a, 'cmp) t @ m -> ('a, 'cmp) t @ m -> int
[@@mode m = (local, global)]

(** Hash function: a building block to use when hashing data structures containing sets in
    them. [hash_fold_direct hash_fold_key] is compatible with [compare_direct] iff
    [hash_fold_key] is compatible with [(comparator s).compare] of the set [s] being
    hashed. *)
val hash_fold_direct : 'a Hash.folder -> ('a, 'cmp) t Hash.folder

(** [equal t1 t2] returns [true] iff the two sets have the same elements.
    [O(length t1 + length t2)] *)
val%template equal : ('a, 'cmp) t @ m -> ('a, 'cmp) t @ m -> bool
[@@mode m = (local, global)]

(** [exists t ~f] returns [true] iff there exists an [a] in [t] for which [f a]. [O(n)],
    but returns as soon as it finds an [a] for which [f a]. *)
val exists : ('a, _) t -> f:local_ ('a -> bool) -> bool

(** [for_all t ~f] returns [true] iff for all [a] in [t], [f a]. [O(n)], but returns as
    soon as it finds an [a] for which [not (f a)]. *)
val for_all : ('a, _) t -> f:local_ ('a -> bool) -> bool

(** [count t] returns the number of elements of [t] for which [f] returns [true]. [O(n)]. *)
val count : ('a, _) t -> f:local_ ('a -> bool) -> int

(** [sum t] returns the sum of [f t] for each [t] in the set. [O(n)]. *)
val sum
  :  (module Container.Summable with type t = 'sum)
  -> ('a, _) t
  -> f:local_ ('a -> 'sum)
  -> 'sum

(** [find t f] returns an element of [t] for which [f] returns true, with no guarantee as
    to which element is returned. [O(n)], but returns as soon as a suitable element is
    found. *)
val find : ('a, _) t -> f:local_ ('a -> bool) -> 'a option

(** [find_map t f] returns [b] for some [a] in [t] for which [f a = Some b]. If no such
    [a] exists, then [find] returns [None]. [O(n)], but returns as soon as a suitable
    element is found. *)
val find_map : ('a, _) t -> f:local_ ('a -> 'b option) -> 'b option

(** Like [find], but throws an exception on failure. *)
val find_exn : ('a, _) t -> f:local_ ('a -> bool) -> 'a

(** [nth t i] returns the [i]th smallest element of [t], in [O(log n)] time. The smallest
    element has [i = 0]. Returns [None] if [i < 0] or [i >= length t]. *)
val nth : ('a, _) t -> int -> 'a option

(** [rank t elt] if [elt] is in [t], returns the number of elements strictly less than
    [elt] in [t], otherwise [None]. *)
val rank : ('a, _) t -> 'a -> int option

(** [remove_index t i] returns a version of [t] with the [i]th smallest element removed,
    in [O(log n)] time. The smallest element has [i = 0]. Returns [t] if [i < 0] or
    [i >= length t]. *)
val remove_index : ('a, 'cmp) t -> int -> ('a, 'cmp) t

(** [is_subset t1 ~of_:t2] returns true iff [t1] is a subset of [t2]. *)
val is_subset : ('a, 'cmp) t -> of_:('a, 'cmp) t -> bool

(** [are_disjoint t1 t2] returns [true] iff [is_empty (inter t1 t2)], but is more
    efficient. *)
val are_disjoint : ('a, 'cmp) t -> ('a, 'cmp) t -> bool

(** [Named] allows the validation of subset and equality relationships between sets. A
    [Named.t] is a record of a set and a name, where the name is used in error messages,
    and [Named.is_subset] and [Named.equal] validate subset and equality relationships
    respectively.

    The error message for, e.g.,
    {[
      Named.is_subset { set = set1; name = "set1" } ~of_:{ set = set2; name = "set2" }
    ]}

    looks like
    {v
       ("set1 is not a subset of set2" (invalid_elements (...elements of set1 - set2...)))
    v}

    so [name] should be a noun phrase that doesn't sound awkward in the above error
    message. Even though it adds verbosity, choosing [name]s that start with the phrase
    "the set of" often makes the error message sound more natural. *)
module Named : sig
  type ('a, 'cmp) set := ('a, 'cmp) t

  type 'a t = 'a Set.Named.t =
    { set : 'a
    ; name : string
    }

  (** [is_subset t1 ~of_:t2] returns [Ok ()] if [t1] is a subset of [t2] and a
      human-readable error otherwise. *)
  val is_subset : ('a, 'cmp) set t -> of_:('a, 'cmp) set t -> unit Or_error.t

  (** [equal t1 t2] returns [Ok ()] if [t1] is equal to [t2] and a human-readable error
      otherwise. *)
  val equal : ('a, 'cmp) set t -> ('a, 'cmp) set t -> unit Or_error.t
end

(** The list or array given to [of_list] and [of_array] need not be sorted. *)
val of_list : ('a, 'cmp) Comparator.Module.t -> 'a list -> ('a, 'cmp) t

val of_sequence : ('a, 'cmp) Comparator.Module.t -> 'a Sequence.t -> ('a, 'cmp) t
val of_array : ('a, 'cmp) Comparator.Module.t -> 'a array -> ('a, 'cmp) t
val of_hash_set : ('a, 'cmp) Comparator.Module.t -> 'a Hash_set.t -> ('a, 'cmp) t
val of_hashtbl_keys : ('a, 'cmp) Comparator.Module.t -> ('a, _) Hashtbl.t -> ('a, 'cmp) t

(** [to_list] and [to_array] produce sequences sorted in ascending order according to the
    comparator. *)
val to_list : ('a, _) t -> 'a list

val to_array : ('a, _) t -> 'a array
val to_tree : ('a, 'cmp) t -> ('a, 'cmp) Tree.t
val of_tree : ('a, 'cmp) Comparator.Module.t -> ('a, 'cmp) Tree.t -> ('a, 'cmp) t

(** Create set from sorted array. The input must be sorted (either in ascending or
    descending order as given by the comparator) and contain no duplicates, otherwise the
    result is an error. The complexity of this function is [O(n)]. *)
val of_sorted_array
  :  ('a, 'cmp) Comparator.Module.t
  -> 'a array
  -> ('a, 'cmp) t Or_error.t

(** Similar to [of_sorted_array], but without checking the input array. *)
val of_sorted_array_unchecked : ('a, 'cmp) Comparator.Module.t -> 'a array -> ('a, 'cmp) t

(** [of_increasing_iterator_unchecked c ~len ~f] behaves like
    [of_sorted_array_unchecked c (Array.init len ~f)], with the additional restriction
    that a decreasing order is not supported. The advantage is not requiring you to
    allocate an intermediate array. [f] will be called with 0, 1, ... [len - 1], in order. *)
val of_increasing_iterator_unchecked
  :  ('a, 'cmp) Comparator.Module.t
  -> len:int
  -> f:local_ (int -> 'a)
  -> ('a, 'cmp) t

(** [map c t ~f] returns a new set created by applying [f] to every element in [t]. The
    returned set is based on the provided [c]. [O(n log n)]. *)
val map
  :  ('b, 'cmp) Comparator.Module.t
  -> ('a, _) t
  -> f:local_ ('a -> 'b)
  -> ('b, 'cmp) t

(** Like {!map}, except elements for which [f] returns [None] will be dropped. *)
val filter_map
  :  ('b, 'cmp) Comparator.Module.t
  -> ('a, _) t
  -> f:local_ ('a -> 'b option)
  -> ('b, 'cmp) t

(** [filter t ~f] returns the subset of [t] for which [f] evaluates to true. [O(n log n)]. *)
val filter : ('a, 'cmp) t -> f:local_ ('a -> bool) -> ('a, 'cmp) t

(** [fold t ~init ~f] folds over the elements of the set from smallest to largest. *)
val fold : ('a, _) t -> init:'accum -> f:local_ ('accum -> 'a -> 'accum) -> 'accum

(** [fold_result ~init ~f] folds over the elements of the set from smallest to largest,
    short circuiting the fold if [f accum x] is an [Error _] *)
val fold_result
  :  ('a, _) t
  -> init:'accum
  -> f:local_ ('accum -> 'a -> ('accum, 'e) Result.t)
  -> ('accum, 'e) Result.t

(** [fold_until t ~init ~f] is a short-circuiting version of [fold]. If [f] returns
    [Stop _] the computation ceases and results in that value. If [f] returns
    [Continue _], the fold will proceed. *)
val fold_until
  :  ('a, _) t
  -> init:'accum
  -> f:local_ ('accum -> 'a -> ('accum, 'final) Continue_or_stop.t)
  -> finish:local_ ('accum -> 'final)
  -> 'final

(** Like {!fold}, except that it goes from the largest to the smallest element. *)
val fold_right : ('a, _) t -> init:'accum -> f:local_ ('a -> 'accum -> 'accum) -> 'accum

(** [iter t ~f] calls [f] on every element of [t], going in order from the smallest to
    largest. *)
val iter : ('a, _) t -> f:local_ ('a -> unit) -> unit

(** Iterate two sets side by side. Complexity is [O(m+n)] where [m] and [n] are the sizes
    of the two input sets. As an example, with the inputs [0; 1] and [1; 2], [f] will be
    called with [`Left 0]; [`Both (1, 1)]; and [`Right 2]. *)
val iter2
  :  ('a, 'cmp) t
  -> ('a, 'cmp) t
  -> f:local_ ([ `Left of 'a | `Right of 'a | `Both of 'a * 'a ] -> unit)
  -> unit

(** [iter_until t ~f ~finish] is a short-circuiting version of [iter]. If [f] returns
    [Stop x] the computation ceases and returns [x]. If [f] always returns [Continue ()]
    the final result is computed by [finish]. *)
val iter_until
  :  ('a, 'cmp') t
  -> f:('a -> (unit, 'final) Continue_or_stop.t) @ local
  -> finish:(unit -> 'final) @ local
  -> 'final

(** If [a, b = partition_tf set ~f] then [a] is the elements on which [f] produced [true],
    and [b] is the elements on which [f] produces [false]. *)
val partition_tf : ('a, 'cmp) t -> f:local_ ('a -> bool) -> ('a, 'cmp) t * ('a, 'cmp) t

(** Same as {!to_list}. *)
val elements : ('a, _) t -> 'a list

(** Returns the smallest element of the set. [O(log n)]. *)
val min_elt : ('a, _) t -> 'a option

(** Like {!min_elt}, but throws an exception when given an empty set. *)
val min_elt_exn : ('a, _) t -> 'a

(** Returns the largest element of the set. [O(log n)]. *)
val max_elt : ('a, _) t -> 'a option

(** Like {!max_elt}, but throws an exception when given an empty set. *)
val max_elt_exn : ('a, _) t -> 'a

(** returns an arbitrary element, or [None] if the set is empty. *)
val choose : ('a, _) t -> 'a option

(** Like {!choose}, but throws an exception on an empty set. *)
val choose_exn : ('a, _) t -> 'a

(** [split t x] produces a triple [(t1, maybe_x, t2)].

    [t1] is the set of elements strictly less than [x], [maybe_x] is the member (if any)
    of [t] which compares equal to [x], [t2] is the set of elements strictly larger than
    [x]. *)
val split : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t * 'a option * ('a, 'cmp) t

(** [split_le_gt t x] produces a pair [(t1, t2)].

    [t1] is the set of elements less than or equal to [x], [t2] is the set of elements
    strictly larger than [x]. *)
val split_le_gt : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t * ('a, 'cmp) t

(** [split_lt_ge t x] produces a pair [(t1, t2)].

    [t1] is the set of elements strictly less than [x], [t2] is the set of elements larger
    or equal to [x]. *)
val split_lt_ge : ('a, 'cmp) t -> 'a -> ('a, 'cmp) t * ('a, 'cmp) t

(** If [equiv] is an equivalence predicate, then [group_by set ~equiv] produces a list of
    equivalence classes (i.e., a set-theoretic quotient). E.g.,

    {[
      let chars = Set.of_list [ 'A'; 'a'; 'b'; 'c' ] in
      let equiv c c' = Char.equal (Char.uppercase c) (Char.uppercase c') in
      group_by chars ~equiv
    ]}

    produces:

    {[
      [ Set.of_list [ 'A'; 'a' ]; Set.singleton 'b'; Set.singleton 'c' ]
    ]}

    [group_by] runs in O(n^2) time, so if you have a comparison function, it's usually
    much faster to use [Set.of_list]. *)
val group_by : ('a, 'cmp) t -> equiv:local_ ('a -> 'a -> bool) -> ('a, 'cmp) t list
[@@deprecated
  "[since 2024-08] This function is slow (O(n^2)) and pretty much never the right thing \
   to use. Consider using [to_list] along with [List.sort_and_group] or [List.group]."]

(** [to_sequence t] converts the set [t] to a sequence of the elements between
    [greater_or_equal_to] and [less_or_equal_to] inclusive in the order indicated by
    [order]. If [greater_or_equal_to > less_or_equal_to] the sequence is empty. Cost is
    O(log n) up front and amortized O(1) for each element produced. *)
val to_sequence
  :  ?order:[ `Increasing (** default *) | `Decreasing ]
  -> ?greater_or_equal_to:'a
  -> ?less_or_equal_to:'a
  -> ('a, 'cmp) t
  -> 'a Sequence.t

(** [binary_search t ~compare which elt] returns the element in [t] specified by [compare]
    and [which], if one exists.

    [t] must be sorted in increasing order according to [compare], where [compare] and
    [elt] divide [t] into three (possibly empty) segments:

    {v
      |  < elt  |  = elt  |  > elt  |
    v}

    [binary_search] returns an element on the boundary of segments as specified by
    [which]. See the diagram below next to the [which] variants.

    [binary_search] does not check that [compare] orders [t], and behavior is unspecified
    if [compare] doesn't order [t]. Behavior is also unspecified if [compare] mutates [t]. *)
val binary_search
  :  ('a, 'cmp) t
  -> compare:local_ ('a -> 'key -> int)
  -> [ `Last_strictly_less_than (** [         | < elt X |                       ] *)
     | `Last_less_than_or_equal_to (** [      |      <= elt       X |           ] *)
     | `Last_equal_to (** [                             |   = elt X |           ] *)
     | `First_equal_to (** [                            | X = elt   |           ] *)
     | `First_greater_than_or_equal_to (** [            | X       >= elt      | ] *)
     | `First_strictly_greater_than (** [                           | X > elt | ] *)
     ]
  -> 'key
  -> 'a option

(** [binary_search_segmented t ~segment_of which] takes a [segment_of] function that
    divides [t] into two (possibly empty) segments:

    {v
      | segment_of elt = `Left | segment_of elt = `Right |
    v}

    [binary_search_segmented] returns the element on the boundary of the segments as
    specified by [which]: [`Last_on_left] yields the last element of the left segment,
    while [`First_on_right] yields the first element of the right segment. It returns
    [None] if the segment is empty.

    [binary_search_segmented] does not check that [segment_of] segments [t] as in the
    diagram, and behavior is unspecified if [segment_of] doesn't segment [t]. Behavior is
    also unspecified if [segment_of] mutates [t]. *)
val binary_search_segmented
  :  ('a, 'cmp) t
  -> segment_of:local_ ('a -> [ `Left | `Right ])
  -> [ `Last_on_left | `First_on_right ]
  -> 'a option

(** Produces the elements of the two sets between [greater_or_equal_to] and
    [less_or_equal_to] in [order], noting whether each element appears in the left set,
    the right set, or both. In the both case, both elements are returned, in case the
    caller can distinguish between elements that are equal to the sets' comparator. Runs
    in O(length t + length t'). *)
module Merge_to_sequence_element : sig
  type ('a, 'b) t = ('a, 'b) Sequence.Merge_with_duplicates_element.t =
    | Left of 'a
    | Right of 'b
    | Both of 'a * 'b
  [@@deriving bin_io, compare ~localize, sexp, sexp_grammar]
end

val merge_to_sequence
  :  ?order:[ `Increasing (** default *) | `Decreasing ]
  -> ?greater_or_equal_to:'a
  -> ?less_or_equal_to:'a
  -> ('a, 'cmp) t
  -> ('a, 'cmp) t
  -> ('a, 'a) Merge_to_sequence_element.t Sequence.t

(** Convert a set to or from a map. [to_map] takes a function to produce data for each
    key. Both functions run in O(n) time (assuming the function passed to [to_map] runs in
    constant time). *)
val to_map : ('key, 'cmp) t -> f:local_ ('key -> 'data) -> ('key, 'data, 'cmp) Base.Map.t

val of_map_keys : ('key, _, 'cmp) Base.Map.t -> ('key, 'cmp) t

val quickcheck_generator
  :  ('key, 'cmp) Comparator.Module.t
  -> 'key Quickcheck.Generator.t
  -> ('key, 'cmp) t Quickcheck.Generator.t

val quickcheck_observer
  :  'key Quickcheck.Observer.t
  -> ('key, 'cmp) t Quickcheck.Observer.t

val quickcheck_shrinker
  :  'key Quickcheck.Shrinker.t
  -> ('key, 'cmp) t Quickcheck.Shrinker.t

(** {2 Polymorphic sets}

    Module {!Poly} deals with sets that use OCaml's polymorphic comparison to compare
    elements. *)

module Poly : sig
    type ('a, 'b) set

    module Tree : sig
      type 'elt t = ('elt, Comparator.Poly.comparator_witness) Tree.t
      [@@deriving sexp, sexp_grammar]

      include
        Creators_generic
        with type ('a, 'b) set := ('a, 'b) Tree.t
        with type ('elt, 'cmp) t := 'elt t
        with type ('elt, 'cmp) tree := 'elt t
        with type 'c cmp := Comparator.Poly.comparator_witness
        with type 'a elt := 'a
        with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Without_comparator.t
    end

    type 'elt t = ('elt, Comparator.Poly.comparator_witness) set
    [@@deriving bin_io, compare ~localize, sexp, sexp_grammar]

    include
      Creators_generic
      with type ('a, 'b) set := ('a, 'b) set
      with type ('elt, 'cmp) t := 'elt t
      with type ('elt, 'cmp) tree := 'elt Tree.t
      with type 'c cmp := Comparator.Poly.comparator_witness
      with type 'a elt := 'a
      with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) Without_comparator.t
  end
  with type ('a, 'b) set := ('a, 'b) t

(** {2 Signatures and functors for building [Set] modules} *)

[%%template:
[@@@mode.default m = (local, global)]

module type Elt_plain = Elt_plain [@mode m]

(** The signature that something needs to match in order to be used as a set element. *)
module type Elt = Elt [@mode m]

(** The signature that something needs to match in order to be used as a set element if
    the resulting set is going to support [bin_io]. *)
module type Elt_binable = Elt_binable [@mode m]]

[%%template:
[@@@modality.default p = (portable, nonportable)]

(** Module signature for a Set that doesn't support [of_sexp]. *)
module type S_plain = S_plain [@modality p]

(** Module signature for a Set. *)
module type S = S [@modality p]

(** Module signature for a Set that supports [bin_io]. *)
module type S_binable = S_binable [@modality p]]

(** [Make] builds a set from an element type that has a [compare] function but doesn't
    have a comparator. This generates a new comparator.

    [Make_binable] is similar, except the element and set types support [bin_io]. *)
module%template.portable Make_plain (Elt : Elt_plain) : S_plain with type Elt.t = Elt.t

module%template.portable Provide_of_sexp (Elt : sig
    type t [@@deriving of_sexp]

    include Comparator.S with type t := t
  end) : sig
    type t [@@deriving of_sexp]
  end
  with type t := (Elt.t, Elt.comparator_witness) t

module%template.portable Make (Elt : Elt) : S with type Elt.t = Elt.t

module%template.portable Make_binable (Elt : Elt_binable) :
  S_binable with type Elt.t = Elt.t

module%template.portable
  [@modality p] Make_plain_using_comparator (Elt : sig
    type t [@@deriving sexp_of]

    include Comparator.S [@modality p] with type t := t
  end) :
  S_plain
  with type Elt.t = Elt.t
  with type Elt.comparator_witness = Elt.comparator_witness

(** [Make_using_comparator] builds a set from an element type that has a comparator.

    [Make_binable_using_comparator] is similar, except the element and set types support
    [bin_io]. *)
module%template.portable
  [@modality p] Make_using_comparator (Elt : sig
    type t [@@deriving sexp]

    include Comparator.S [@modality p] with type t := t
  end) :
  S with type Elt.t = Elt.t with type Elt.comparator_witness = Elt.comparator_witness

module Elt_bin_io = Elt_bin_io

module%template.portable
  [@modality p] Provide_bin_io
    (Elt : Elt_bin_io.S) : sig
    type t [@@deriving bin_io]
  end
  with type t := (Elt.t, Elt.comparator_witness) t

module%template.portable
  [@modality p] Make_binable_using_comparator (Elt : sig
    type t [@@deriving bin_io, sexp]

    include Comparator.S [@modality p] with type t := t
  end) :
  S_binable
  with type Elt.t = Elt.t
  with type Elt.comparator_witness = Elt.comparator_witness

module%template Provide_stable_witness (Elt : sig
    type t [@@deriving stable_witness]
    type comparator_witness
  end) : sig
    type t [@@deriving stable_witness]
  end
  with type t := (Elt.t, Elt.comparator_witness) t

module%template.portable Provide_hash (Elt : sig
    type t
    type comparator_witness

    include Hasher.S with type t := t
  end) : sig
    type t [@@deriving hash]
  end
  with type t := (Elt.t, Elt.comparator_witness) t

include For_deriving with type ('a, 'b) t := ('a, 'b) t

module%template.portable
  [@modality p] Make_tree_plain (Elt : sig
    type t [@@deriving sexp_of]

    include Comparator.S [@modality p] with type t := t
  end) : S_plain_tree with module Elt := Elt

module%template.portable
  [@modality p] Make_tree (Elt : sig
    type t [@@deriving sexp]

    include Comparator.S [@modality p] with type t := t
  end) : sig
  include S_plain_tree with module Elt := Elt
  include Sexpable.S with type t := t
end

(** The following types and functors may be used to define stable modules. *)
module Stable : sig
  module V1 : sig
    type nonrec ('a, 'b) t = ('a, 'b) t

    module type S = sig
      type elt
      type elt_comparator_witness
      type nonrec t = (elt, elt_comparator_witness) t

      include%template
        Stable_module_types.S0_without_comparator [@mode local] with type t := t

      include
        Diffable.S with type t := t and type Diff.t = elt Diffable.Set_diff.Stable.V1.t
    end

    include For_deriving with type ('a, 'b) t := ('a, 'b) t
    include For_deriving_stable with type ('a, 'b) t := ('a, 'b) t

    module%template.portable
      [@modality p] Make
        (Elt : Stable_module_types.S0
      [@modality p]) :
      S with type elt := Elt.t with type elt_comparator_witness := Elt.comparator_witness

    module With_stable_witness : sig
      module type S = sig
        include S

        val stable_witness : t Stable_witness.t
      end

      module%template.portable
        [@modality p] Make
          (Elt : Stable_module_types.With_stable_witness.S0
        [@modality p]) :
        S
        with type elt := Elt.t
        with type elt_comparator_witness := Elt.comparator_witness
    end
  end
end
