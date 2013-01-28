(** A [Bounded_int_table] is a table whose keys can be mapped to integers in a fixed
    range, 0 ... num_keys-1, where [num_keys] is specified at table-creation time.  The
    purpose of [Bounded_int_table] is to be faster than [Hashtbl] in situations where one
    is willing to pay a space cost for the speed.

    [Bounded_int_table] presents a subset of the [Hashtbl] interface.  The key type can be
    any type, but table creation requires a [key_to_int] function, which will be used
    to extract the integer of all keys.  If multiple keys map to the same integer, then
    only one of them can be in the table at a time.  Any operation that supplies a key
    whose corresponding integer is outside the allowed range for the table will cause an
    exception.

    A [Bounded_int_table] is implemented using two fixed size arrays of size [num_keys],
    which is supplied at table-creation time.  The space used does not depend on the
    [length] of the table but rather only on [num_keys].  Operations that deal with a
    single element (find, mem, add, remove, set) take constant time, and perform one or
    two array operations.  Operations that deal with all of the keys defined in the table
    (data, fold, iter, iter_vals, keys, to_alist) take time proportional to the [length]
    of the table, not [num_keys]. *)
open Std_internal

type ('key, 'data) t with sexp_of

val invariant : (_, _) t -> unit

(** [create ~num_keys ~key_to_int] returns a table where the keys can map to 0
    .. num_keys-1, according to [key_to_int].  It is an error if [num_keys < 0].

    [sexp_of_key], if supplied, will be used to display keys in error messages. *)
val create
  :  ?sexp_of_key:('key -> Sexp.t)
  -> num_keys:int
  -> key_to_int:('key -> int)
  -> unit
  -> ('key, 'data) t

(** Standard hashtbl functions. *)
val data : (_, 'data) t -> 'data list
val find : ('key, 'data) t -> 'key -> 'data option
val fold
  :  ('key, 'data) t
  -> init:'accum
  -> f:(key:'key -> data:'data -> 'accum -> 'accum)
  -> 'accum
val iter      : ('key, 'data) t -> f:(key:'key -> data:'data -> unit) -> unit
val iter_vals : (_   , 'data) t -> f:(                 'data -> unit) -> unit
val keys : ('key, _) t -> 'key list
val length : (_, _) t -> int
val mem : ('key, _) t -> 'key -> bool
val remove : ('key, _) t -> 'key -> unit
val set     : ('a, 'b) t -> key:'a -> data:'b -> unit
val add     : ('a, 'b) t -> key:'a -> data:'b -> [ `Ok | `Duplicate ]
val add_exn : ('a, 'b) t -> key:'a -> data:'b -> unit
val to_alist : ('key, 'data) t -> ('key * 'data) list

(** set [debug := true] to turn on debugging, including potentially slow invariant
    checking. *)
val debug : bool ref
