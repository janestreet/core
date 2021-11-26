(** A heap block is a value that is guaranteed to live on the OCaml heap, and is hence
    guaranteed to be usable with finalization or in a weak pointer.

    It is an abstract type so we can use the type system to guarantee that the values we
    put in weak pointers and use with finalizers are heap blocks.

    Some examples of values that are not heap-allocated are integers, constant
    constructors, booleans, the empty array, the empty list, the unit value.  The exact
    list of what is heap-allocated or not is implementation-dependent.  Some constant
    values can be heap-allocated but never deallocated during the lifetime of the program,
    for example a list of integer constants; this is also implementation-dependent.  You
    should also be aware that compiler optimizations may duplicate some immutable values,
    for example floating-point numbers when stored into arrays; thus they can be finalized
    and collected while another copy is still in use by the program.

    The results of calling {!String.make}, {!Bytes.create}, {!Bytes.make}, {!Array.make},
    and {!Pervasives.ref} are guaranteed to be heap-allocated and non-constant except when
    the length argument is [0]. *)

open! Import

type +'a t = private 'a [@@deriving sexp_of]

(** [create v] returns [Some t] if [v] is a heap block, where [t] is physically equal
    to [v]. *)
val create : 'a -> 'a t option

val create_exn : 'a -> 'a t

(** [value t] returns the value that is physically equal to [t]. *)
val value : 'a t -> 'a

(** [bytes t] returns the number of bytes on the heap taken by heap block [t], including
    the header.  This is just the space for the single block, not anything it points
    to. *)
val bytes : _ t -> int

