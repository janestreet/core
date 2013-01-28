(** Core_stack is a replacement for OCaml's standard Stack module that follows
    Core idioms and adds some functions.

    Differences from the standard module:
      [pop] and [top] return an [option] rather than raise [Empty].
      [iter] takes a labeled argument.
      [push] takes the stack argument first.
      [length] is O(1)
*)
open Sexplib

exception Empty

type 'a t

include Binable.S1 with type 'a t := 'a t
include Sexpable.S1 with type 'a t := 'a t
include Container.S1 with type 'a t := 'a t
  (** [to_list] and [to_array] returns the elements in order from the top of
      the stack to the bottom. *)

(** [of_list l] returns a stack whose top is the first element of [l] and
 * bottom is the last element of [l].
 *)
val of_list : 'a list -> 'a t

val invariant : 'a t -> unit

(** [create ()] returns an empty stack. *)
val create : unit -> 'a t

(** [push t x] adds [x] to the top of stack [t]. *)
val push : 'a t -> 'a -> unit

(** [pop t] returns [None] if [t] is empty, otherwise it returns [Some x] where
    [x] is the top of [t] and removes [x] from the top of [t]. *)
val pop : 'a t -> 'a option

(** [pop_exn t] removes and returns the top element of [t], raising [Empty] if
    [t] is empty. *)
val pop_exn : 'a t -> 'a

(** [top t] returns [None] if [t] is empty, otherwise it returns [Some x] where
    [x] is the top of [t]. *)
val top : 'a t -> 'a option

(** [top_exn t] returns the top element of [t], raising [Empty] if [t] is empty.
*)
val top_exn : 'a t -> 'a

(** [clear t] discards all elements from [t]. *)
val clear : 'a t -> unit

(** [copy t] returns a copy of [t]. *)
val copy : 'a t -> 'a t

(** [until_empty t f] repeatedly pops an element [v] off of [t] and runs [f v]
    until [t] becomes empty.  It is fine if [f] adds more elements to [t], in
    which case the most-recently-added element will be processed first.
*)
val until_empty : 'a t -> ('a -> unit) -> unit
