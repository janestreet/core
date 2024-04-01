(** This module extends {{!Base.List}[Base.List]} with bin_io and quickcheck. *)

open! Import

(** {2 The interface from Base} *)

(** @inline *)
include module type of struct
  include Base.List
end

type 'a t = 'a list [@@deriving bin_io ~localize, typerep]

module Assoc : sig
  type ('a, 'b) t = ('a, 'b) Base.List.Assoc.t [@@deriving bin_io ~localize]

  val compare : [%compare: 'a] -> [%compare: 'b] -> [%compare: ('a, 'b) t]
    [@@deprecated
      "[since 2016-06] This does not respect the equivalence class promised by List.Assoc.\n\
       Use List.compare directly if that's what you want."]

  include module type of struct
      include Base.List.Assoc
    end
    with type ('a, 'b) t := ('a, 'b) t
end

(** {2 Extensions} *)

(** [stable_dedup_staged] is the same as [dedup_and_sort] but maintains the order of the
    list.  This function is staged because it instantiates a functor when [compare] is
    passed.

    See also [Set.stable_dedup_list], which is the underlying implementation of this
    function and lets you avoid the functor instantiation when you already have such a
    module on hand. *)
val stable_dedup_staged : compare:('a -> 'a -> int) -> ('a list -> 'a list) Staged.t
  [@@deprecated "[since 2023-04] Use [List.stable_dedup] instead."]

(** Only raised in [exn_if_dup] below. *)
exception
  Duplicate_found of (unit -> Base.Sexp.t) * string
    [@deprecated
      "[since 2018-03] stop matching on Duplicate_found. [exn_if_dup] will eventually \
       raise a different and unspecified exception"]

(** [exn_if_dup ~compare ?context t ~to_sexp] raises if [t] contains a duplicate. It will
    specifically raise a [Duplicate_found] exception and use [context] as its second
    argument. O(n log n) time complexity. *)
val exn_if_dup
  :  compare:('a -> 'a -> int)
  -> ?context:string
  -> 'a t
  -> to_sexp:('a -> Base.Sexp.t)
  -> unit

(** [slice t start stop] returns a new list including elements [t.(start)] through
    [t.(stop-1)], normalized Python-style with the exception that [stop = 0] is treated as
    [stop = length t]. *)
val slice : 'a t -> int -> int -> 'a t

include Comparator.Derived with type 'a t := 'a t
include Quickcheckable.S1 with type 'a t := 'a t

val to_string : f:('a -> string) -> 'a t -> string

(** Like [gen], but never generates the empty list. *)
val gen_non_empty : 'a Quickcheck.Generator.t -> 'a t Quickcheck.Generator.t

(** Like [gen], but generates lists with the given length. *)
val gen_with_length : int -> 'a Quickcheck.Generator.t -> 'a t Quickcheck.Generator.t

(** Randomly drops elements from the input list. Length is chosen uniformly between 0 and
    the length of the input, inclusive. *)
val gen_filtered : 'a t -> 'a t Quickcheck.Generator.t

(** [gen_permutations t] generates all permutations of [list].  If [t] contains duplicate
    values, then [gen_permutations t] will produce duplicate lists. *)
val gen_permutations : 'a t -> 'a t Quickcheck.Generator.t

(** [zip_with_remainder xs ys] zips as many elements as possible of [xs] and [ys] together
    and also returns the un-zipped remainder of the longer input, if the inputs have
    different lengths.

    If [xs] and [ys] have the same length, [zip_with_remainder xs ys] returns the same
    thing as [(zip_exn xs ys, None)] *)
val zip_with_remainder
  :  'a list
  -> 'b list
  -> ('a * 'b) list * ('a list, 'b list) Either.t option

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t
    [@@deriving
      sexp, sexp_grammar, bin_io ~localize, compare, equal, hash, stable_witness]
  end
end
