(* A thread-safe non-blocking queue of unbounded size.

   The implementation does not use mutexes, and so is safe to use in situations when
   one doesn't want to block, e.g. a finalizer or an async job.
*)

type 'a t with sexp_of

include Invariant.S1 with type 'a t := 'a t

(* [create ()] returns an empty queue. *)
val create : unit -> 'a t

val length : _ t -> int

val enqueue : 'a t -> 'a -> unit

(** [dequeue_exn t] raises if [length t = 0].  The idiom for dequeueing a single element
    is:

    {[
      if length t > 0 then dequeue_exn t else ...
    ]}

    The idiom for dequeueing until empty is:

    {[
      while length t > 0 do
        let a = dequeue_exn t in
        ...
      done
    ]}

    These idioms work in the presence of threads because OCaml will not context switch
    between the [length t > 0] test and the call to [dequeue_exn].  Also, if one has only
    a single thread calling [dequeue_exn], then the idiom is obviously OK even in the
    presence of a context switch. *)
val dequeue_exn : 'a t -> 'a

(** The queue maintains an internal pool of unused elements, which are used by [enqueue]
    and returned to the pool by [dequeue_exn].  [enqueue] creates a new element if the
    pool is empty.  Nothing shrinks the pool automatically.  One can call
    [clear_internal_pool] to clear the pool, so that all unused elements will be reclaimed
    by the garbage collector. *)
val clear_internal_pool : _ t -> unit
