(** A hashtable that keeps a weak pointer to each key's data and uses a finalizer to
    detect when the data is no longer referenced (by any non-weak pointers).

    Once a key's data is finalized, the table will effectively behave as if the key is not
    in the table, e.g., [find] will return [None]. However, one must call
    [reclaim_space_for_keys_with_unused_data] to actually reclaim the space used by the
    table for such keys.

    Unlike (OCaml's) [Weak.Make], which also describes itself as a "weak hashtable,"
    [Weak_hashtbl] gives a dictionary-style structure.  In fact, OCaml's [Weak.Make] may
    better be described as a weak set.

    There's a tricky type of bug one can write with this module, e.g.:

    {[
      type t =
        { foo : string
        ; bar : float Incr.t
        }

      let tbl = Weak_hashtbl.create ()
      let x1 =
        let t = Weak_hashtbl.find_or_add tbl key ~default:(fun () ->
          (... some function that computes a t...))
        in
        t.bar
    ]}

    At this point, the data associated with [key] is unreachable (since all we did with it
    was project out field [bar]), so it may disappear from the table at any time. *)

open! Import

type ('a, 'b) t [@@deriving sexp_of]

(** [growth_allowed] and [size] are both optionally passed on to the underlying call to
    [Hashtbl.create]. *)
val create
  :  ?growth_allowed : bool  (** default is [true] *)
  -> ?size           : int   (** default is [128] *)
  -> (module Hashtbl.Key_plain with type t = 'a)
  -> ('a, 'b) t

module Using_hashable : sig
  val create
    :  ?growth_allowed : bool  (** default is [true] *)
    -> ?size           : int   (** default is [128] *)
    -> 'a Hashtbl.Hashable.t
    -> ('a, 'b) t
end

val mem : ('a, _) t -> 'a -> bool

val find        : ('a, 'b) t -> 'a -> 'b Heap_block.t option
val find_or_add : ('a, 'b) t -> 'a -> default:(unit -> 'b Heap_block.t) -> 'b Heap_block.t
val remove      : ('a, 'b) t -> 'a -> unit
val add_exn     : ('a, 'b) t -> key:'a -> data:'b Heap_block.t -> unit
val replace     : ('a, 'b) t -> key:'a -> data:'b Heap_block.t -> unit

(** [key_is_using_space t key] returns [true] if [key] is using some space in [t].  [mem t
    key] implies [key_is_using_space t key], but it is also possible that that
    [key_is_using_space t key && not (mem t key)]. *)
val key_is_using_space : ('a, _) t -> 'a -> bool

(** [reclaim_space_for_keys_with_unused_data t] reclaims space for all keys in [t] whose
    data has been detected (by a finalizer) to be unused.  Only [key]s such that
    [key_is_using_space t key && not (mem t key)] will be reclaimed. *)
val reclaim_space_for_keys_with_unused_data : (_, _) t -> unit

(** [set_run_when_unused_data t ~thread_safe_f] calls [thread_safe_f] in the finalizer
    attached to each [data] in [t], after ensuring the entry being finalized will be
    handled in the next call to [reclaim_space_for_keys_with_unused_data].  This can be
    used to arrange to call [reclaim_space_for_keys_with_unused_data] at a convenient time
    in the future.  [thread_safe_f] must be thread safe -- it is {e not} safe for it to
    call any [Weak_hashtbl] functions. *)
val set_run_when_unused_data : (_, _) t -> thread_safe_f:(unit -> unit) -> unit
