(** A double ended queue that can shrink and expand on both ends.

    An index is assigned to an element when it enters the queue, and the index of an
    element is static (i.e. an index refers to a distinct element until that element is
    removed from the queue, no matter how many intervening push/pop operations occur).

    One consequence of this is that the minimum index may be < 0.

    The "front" is the smallest valid index, while the "back" is the largest.

    All operations are amortized O(1) with a small constant. *)

open Std_internal

type 'a t

include Sexpable.S1 with type 'a t := 'a t
include Binable.S1 with type 'a t := 'a t

(* if never_shrink is true, the physical array will never shrink; only expand *)
(* a dummy element is required to satisfy the type-checker and will never be returned *)
val create : ?never_shrink:bool -> ?initial_index:int -> dummy:'a -> unit -> 'a t

(* number of elements in the dequeue, i.e. back_index - front_index + 1 *)
val length : 'a t -> int
(* same as Dequeue.length = 0 *)
val is_empty : 'a t -> bool

(* minimum and maximum valid indices (inclusive) *)
val front_index : 'a t -> int
val back_index : 'a t -> int

(* returns an element, and leaves it in the dequeue *)
(* [get q i] raises Invalid_argument unless front_index <= i <= back_index *)
val get_exn : 'a t -> int -> 'a

(* The _exn versions raise Invalid_argument iff dequeue is empty *)
val get_front     : 'a t -> 'a option
val get_front_exn : 'a t -> 'a
val get_back      : 'a t -> 'a option
val get_back_exn  : 'a t -> 'a

(* mutates the indexed element *)
val set_exn : 'a t -> int -> 'a -> unit

(* same as Array.iteri (iterates passing the index) *)
val iteri : f:(int -> 'a -> unit) -> 'a t -> unit

(* same as iteri but don't pass the index *)
val iter : f:('a -> unit) -> 'a t -> unit

(* fold across the index element pairs of the dequeue *)
val foldi : f:('a -> int -> 'b -> 'a) -> init:'a -> 'b t -> 'a

(* fold across just the elements of the dequeue *)
val fold : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a

(* decreases front_index by one, and places the new element at the new front_index *)
val push_front : 'a t -> 'a -> unit

(* increases back_index by one, and places the new element at the new back_index *)
val push_back : 'a t -> 'a -> unit

(* drop functions raise Invalid_argument if asked to drop more than Dequeue.length
   elements *)

(* drops n elements (default 1) at front *)
val drop_front_exn : ?n:int -> 'a t -> unit
(* drops n elements (default 1) at back *)
val drop_back_exn : ?n:int -> 'a t -> unit

(* drop the front and return it *)
val take_front_exn : 'a t -> 'a

(* drop the back and return it *)
val take_back_exn : 'a t -> 'a

(* drops index j iff j < i *)
val drop_indices_less_than_exn : 'a t -> int -> unit
(* drops index j iff j > i *)
val drop_indices_greater_than_exn : 'a t -> int -> unit

val invariant : 'a t -> unit
