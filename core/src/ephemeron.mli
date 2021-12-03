(** An ephemeron is a pair of pointers, one to a "key" and one to "data".

    The key pointer is a weak pointer: the garbage collector doesn't follow it when
    determining liveness. The garbage collector follows an ephemeron's data pointer iff
    the key is alive. If the garbage collector nulls an ephemeron's weak pointer then it
    also nulls the data pointer. Ephemerons are more powerful than weak pointers because
    they express conjunction of liveness -- the data in an ephemeron is live iff both the
    key {e and} the ephemeron are live. See "Ephemerons: A New Finalization Mechanism",
    Barry Hayes 1997.

    This module is like the OCaml standard library module [Ephemerons.K1], except that it
    requires that the keys and data are heap blocks. *)

open! Import

type ('a, 'b) t [@@deriving sexp_of]

val create : unit -> _ t
val set_key : ('a, _) t -> 'a Heap_block.t option -> unit
val get_key : ('a, _) t -> 'a Heap_block.t option
val set_data : (_, 'b) t -> 'b Heap_block.t option -> unit
val get_data : (_, 'b) t -> 'b Heap_block.t option
val is_key_some : _ t -> bool
val is_key_none : _ t -> bool
val is_data_some : _ t -> bool
val is_data_none : _ t -> bool
