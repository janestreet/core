(** An implementation of compressed integer sets using lists of integer ranges. Operations
    such as adding and membership are O(n) where n is the number of contigous ranges in
    the set. For data that is mostly serial, n should remain very small.

    Note that when n gets very large, in addition to poor performance, this behavior may
    throw exceptions since some of the code is not tail-recursive.
*)

type t

val empty : t

val to_string : t -> string

(** [add_range t i j] add all the numbers between [i] and [j] (inclusive) to the set. Note
    that it doesn't matter which order [i] and [j] are specified in, the effect is the
    same. *)
val add_range : t -> int -> int -> t

(** [add t i] add [i] to the set *)
val add : t -> int -> t

(** [mem t i] test whether [i] is a member of the set *)
val mem : t -> int -> bool

(** [ranges t] return a list of all ranges that make up the set *)
val ranges : t -> (int * int) list

(** [max t] the biggest number in the set (if it exists) *)
val max : t -> int option

(** [min t] the smallest number in the set (if it exists) *)
val min : t -> int option

