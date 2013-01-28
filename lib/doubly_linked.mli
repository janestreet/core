(** doubly-linked lists *)

(** There is a fundamental problem with a data structure (like doubly-linked lists) that
    is both mutable and provides iteration function that call back to user-supplied
    functions.  If those user-supplied functions modify the data structure, what is the
    semantics of the remainder of the iteration?  This module sidesteps this issue by
    detecting attempts by user-supplied functions to modify a doubly-linked list while in
    the middle of iterating over it.

    Modification functions include: insert_*, remove*, transfer
    Iteration functions include: exists, fold*, for_all, find

    Calls to modification functions detect if the list is being iterated over, and if so
    raise an exception rather than modify the list.  For example, a use like the following
    would raise.

    iter t ~f:(fun _ -> ... remove t e ...) *)

open Sexplib

module Elt : sig
  type 'a t

  val value : 'a t -> 'a
  val equal : 'a t -> 'a t -> bool (* pointer equality *)
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
end

type 'a t with sexp

include Container.S1 with type 'a t := 'a t

val invariant : 'a t -> unit

(** creating doubly-linked lists *)
val create : unit -> 'a t

(** [of_list l] returns a doubly-linked list [t] with the same elements as [l] and in the
    same order (i.e. the first element of [l] is the first element of [t]).  It is always
    the case that [l = to_list (of_list l)]. *)
val of_list : 'a list -> 'a t

(** predicates *)
val equal : 'a t -> 'a t -> bool (* pointer equality *)
val is_first : 'a t -> 'a Elt.t -> bool
val is_last : 'a t -> 'a Elt.t -> bool

(** constant-time extraction of first and last elements. *)
val first_elt : 'a t -> 'a Elt.t option
val last_elt : 'a t -> 'a Elt.t option
val first : 'a t -> 'a option
val last : 'a t -> 'a option

(** constant-time move to next or previous element. *)
val next : 'a t -> 'a Elt.t -> 'a Elt.t option
val prev : 'a t -> 'a Elt.t -> 'a Elt.t option

(** constant-time insertion of a new element.  It is an error to call [insert_before t e
    a] or [insert_after t e a] if [e] is not an element in [t], and will break
    invariants. *)
val insert_before : 'a t -> 'a Elt.t -> 'a -> 'a Elt.t
val insert_after : 'a t -> 'a Elt.t -> 'a -> 'a Elt.t
val insert_first : 'a t -> 'a -> 'a Elt.t
val insert_last : 'a t -> 'a -> 'a Elt.t

(** constant-time removal of an element.  It is an error to call [remove t e] when [e] is
    not in [t], and will break [invariant]. *)
val remove : 'a t -> 'a Elt.t -> unit
val remove_first : 'a t -> 'a option
val remove_last : 'a t -> 'a option


(** [fold_elt t ~init ~f] is the same as fold, except [f] is called with the ['a Elt.t]'s
    from the list instead of the contained ['a] values.

    Note that like other iteration functions, it is an error to mutate [t] inside the
    fold. If you'd like to call [remove] on any of the ['a Elt.t]'s, accumulate them here
    and do so after [fold_elt] returns. *)
val fold_elt : 'a t -> init:'b -> f:('b -> 'a Elt.t -> 'b) -> 'b

val fold_right : 'a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b

(** [find_elt t ~f] finds the first element in [t] that satisfies [f], by testing each of
    element of [t] in turn until [f] succeeds. *)
val find_elt : 'a t -> f:('a -> bool) -> 'a Elt.t option

(** [clear t] removes all elements from the list in constant time. *)
val clear : 'a t -> unit

(** [copy t] returns a copy of [t]. *)
val copy : 'a t -> 'a t

(** [transfer ~src ~dst] has the same behavior as
    [iter src ~f:(insert_last dst); clear src]
    except that it runs in constant time.

    If [s = to_list src] and [d = to_list dst], then after [transfer ~src ~dst]:
      [to_list src = []]
      [to_list dst = d @ s] *)
val transfer : src:'a t -> dst:'a t -> unit

(** [filter_inplace t ~f] removes all elements of [t] that don't satisfy [f]. *)
val filter_inplace : 'a t -> f:('a -> bool) -> unit
