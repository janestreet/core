(** A simple polymorphic functional queue.

    Amortized running times assumes that enqueue/dequeue are used sequentially, threading
    the changing Fqueue through the calls. *)

exception Empty

type 'a t with bin_io, sexp

(** test via asserts whether invariants hold *)
val test_invariants : 'a t -> unit

(** The empty queue *)
val empty : 'a t

(** [enqueue t x] returns a queue with adds [x] to the end of [t]. Complexity: O(1) *)
val enqueue : 'a t -> 'a -> 'a t

(** enqueue a single element on the *top* of the queue.  Complexity: amortized O(1) *)
val enqueue_top : 'a t -> 'a -> 'a t

(** returns the bottom (most-recently enqueued element).  Raises [Empty] if no element is
    found.  Complexity: O(1) *)
val bot_exn : 'a t -> 'a

(** like [bot_exn], but returns result optionally, without exception.  Complexity: O(1) *)
val bot : 'a t -> 'a option

(** Like [bot_exn], except returns top (least-recently enqueued element.  Complexity:
    O(1) *)
val top_exn : 'a t -> 'a

(** like [top_exn], but returns result optionally, without exception, Complexity: O(1) *)
val top : 'a t -> 'a option

(** [dequeue_exn t] removes and returns the front of [t], raising [Empty] if [t]
    is empty.  Complexity: amortized O(1)*)
val dequeue_exn : 'a t -> 'a * 'a t

(** Like [dequeue_exn], but returns result optionally, without exception.  Complexity:
    amortized O(1) *)
val dequeue : 'a t -> ('a * 'a t) option

(** Returns version of queue with top element removed.  Complexity: amortized O(1) *)
val discard_exn : 'a t -> 'a t

(** [to_list t] returns a list of the elements in [t] in order from least-recently-added
    (at the head) to most-recently added (at the tail). Complexity: O(n) *)
val to_list : 'a t -> 'a list

(** complexity: O(1) *)
val length : 'a t -> int
(** complexity: O(1) *)
val is_empty : 'a t -> bool

