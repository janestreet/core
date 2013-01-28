(** Thread-safe queue module, using locks. *)

open Std_internal

type 'a t with sexp_of

(** [create maxsize] returns a synchronized queue bounded to have no more than
    [maxsize] elements. *)
val create : int -> 'a t

(** Blocks until there's room on the queue, then pushes. *)
val push : 'a t -> 'a -> unit

(** Does not block, may grow the queue past maxsize *)
val push_uncond : 'a t -> 'a -> unit

(** Pushes an event on the queue if the queue is less than maxsize, otherwise drops it.
  Returns true if the push was successful *)
val push_or_drop : 'a t -> 'a -> bool

(** returns the number of elements in the queue. *)
val length : 'a t -> int

(** pops an element off the queue, blocking until something is
 * available *)
val pop : 'a t -> 'a

(** returns the element popped and the length of the queue after
    * this element was popped. *)
val lpop : 'a t -> 'a * int

(** Transfers all the elements from an ordinary queue into the
    squeue. Blocks until there's room on the queue, then pushes. may
    grow queue past maxsize. *)
val transfer_queue_in : 'a t -> 'a Queue.t -> unit

val transfer_queue_in_uncond : 'a t -> 'a Queue.t -> unit

(** Transfers all elements from the squeue to an ordinary queue.
    The elements remain in order.
    Waits until at least one element can be transfered. *)
val transfer_queue : 'a t -> 'a Queue.t -> unit

(** Transfers all elements from the squeue to an ordinary queue.
    The elements remain in order.
    Does not wait for elements to arrive. *)
val transfer_queue_nowait : 'a t -> 'a Queue.t -> unit

(** clears the queue *)
val clear : 'a t -> unit

(** [wait_not_empty sq] Waits for something to be available. This is
    useful if you want to wait, but not take something out. This
    function is not useful in most cases, but in some complex cases it
    is essential. For example you might need to take another lock
    before you remove something from the queue for processing, you
    might want to try to take that other lock, and if it fails do
    something else.

    This function is not dangerous, there is just ONE thing you HAVE
    to remember if you use it. Just because this function returns
    doesn't mean that pop will succeed, someone might have gotten
    there first, so you have to use transfer_queue_nowait if you don't
    want to block. *)
val wait_not_empty : 'a t -> unit
