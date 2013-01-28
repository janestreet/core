(** [Weak] is like the OCaml standard library module of the same name, except that it
    requires that the values in the weak set are heap blocks. *)

type 'a t with sexp_of

val create : len:int -> _ t

val length : _ t -> int

val set : 'a t -> int -> 'a Heap_block.t option -> unit

val get : 'a t -> int -> 'a Heap_block.t option
