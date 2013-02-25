(** Provides a functor for creating an interned string type.

    An interned string is internally an int which is an index into a lookup table, which
    makes hashing and (non-lexicographic) comparison faster than for strings.  The
    tradeoff is that converting an interned string to and from a string is slower, since
    they require table lookups instead of being no-ops.

    Some notes on the implementation:

    - It is not thread-safe (different applications of the functor can be used
      safely from different threads, but an individual interned string module
      is not safe to use concurrently from different threads.)
    - It is leaky, meaning that an interned string, once allocated, will never be
      deallocated.
    - Interned strings are compared based on the integer they are assigned on creation,
      which is not a lexicographic order, and is not necessarily stable between runs of a
      program.

    The semantics of the other operations should be the same as for [String].

    We don't fix the memory leak with weak pointers for performance reasons.  See
    [jane]/interns/ahuq/strint/README at revision b9a2b9dbf290.
*)

open Std_internal

module Make (T : sig val initial_table_size : int end) : sig
  include Identifiable
  val of_bigstring : ?pos:int -> ?len:int -> Bigstring.t -> t
end
