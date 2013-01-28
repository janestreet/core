(* A thread-safe non-blocking queue of unbounded size.

   The implementation does not use mutexes, so operations are quite fast, just
   a handful of instructions. *)

type 'a t with sexp_of

(* [create ()] returns an empty queue. *)
val create : unit -> 'a t

(* [create' ()] is a variant of create that returns a pair of functions [(dequeue,
   enqueue)] for operating on the queue. *)
val create' : unit -> (unit -> 'a option) * ('a -> unit)

val dequeue : 'a t -> 'a option
val dequeue_until_empty : 'a t -> ('a -> unit) -> unit

val enqueue : 'a t -> 'a -> unit

val length : 'a t -> int
