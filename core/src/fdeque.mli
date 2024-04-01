(** A simple polymorphic functional double-ended queue. Use this if you need a queue-like
    data structure that provides enqueue and dequeue accessors on both ends. For
    strictly first-in, first-out access, see [Fqueue].

    Amortized running times assume that [enqueue]/[dequeue] are used sequentially,
    threading the changing deque through the calls. *)

open! Import

type 'a t [@@deriving bin_io, compare, equal, hash, sexp]

(** [Container] operations traverse deque elements front-to-back, like [Front_to_back]
    below. If you need faster traversal and don't care about the order, use
    [Arbitrary_order] below.

    [is_empty] and [length] have worst-case complexity O(1). *)
include Container.S1 with type 'a t := 'a t

include Invariant.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

(** Traverse deque elements in arbitrary order. *)
module Arbitrary_order : sig
  include Container.S1 with type 'a t := 'a t

  (** This does not match the ordering of [to_list] *)
  val to_sequence : 'a t -> 'a Sequence.t
end

(** Traverse deque elements front-to-back. Incurs up to O(n) additional time and space
    cost over [Arbitrary_order]. *)
module Front_to_back : sig
  val of_list : 'a list -> 'a t

  include Container.S1 with type 'a t := 'a t

  val to_sequence : 'a t -> 'a Sequence.t
  val of_sequence : 'a Sequence.t -> 'a t
end

(** Traverse deque elements back-to-front. Incurs up to O(n) additional time and space
    cost over [Arbitrary_order]. *)
module Back_to_front : sig
  val of_list : 'a list -> 'a t

  include Container.S1 with type 'a t := 'a t

  val to_sequence : 'a t -> 'a Sequence.t
  val of_sequence : 'a Sequence.t -> 'a t
end

(** The empty deque. *)
val empty : _ t

(** A one-element deque. *)
val singleton : 'a -> 'a t

(** [of_list] returns a deque with elements in the same front-to-back order as the
    list. *)
val of_list : 'a list -> 'a t

(** [rev t] returns [t], reversed.

    Complexity: worst-case O(1). *)
val rev : 'a t -> 'a t

(** [enqueue t side x] produces [t] updated with [x] added to its [side].

    Complexity: worst-case O(1). *)
val enqueue : 'a t -> [ `back | `front ] -> 'a -> 'a t

val enqueue_front : 'a t -> 'a -> 'a t
val enqueue_back : 'a t -> 'a -> 'a t

(** [peek t side] produces [Some] of the element at the [side] of [t], or [None] if [t] is
    empty.

    Complexity: worst-case O(1). *)
val peek : 'a t -> [ `back | `front ] -> 'a option

val peek_exn : 'a t -> [ `back | `front ] -> 'a
val peek_front : 'a t -> 'a option
val peek_front_exn : 'a t -> 'a
val peek_back : 'a t -> 'a option
val peek_back_exn : 'a t -> 'a

(** [drop t side] produces [Some] of [t] with the element at its [side] removed, or
    [None] if [t] is empty.

    Complexity: amortized O(1), worst-case O(length t). *)
val drop : 'a t -> [ `back | `front ] -> 'a t option

val drop_exn : 'a t -> [ `back | `front ] -> 'a t
val drop_front : 'a t -> 'a t option
val drop_front_exn : 'a t -> 'a t
val drop_back : 'a t -> 'a t option
val drop_back_exn : 'a t -> 'a t

(** [dequeue t side] produces [Option.both (peek t side) (drop t side)].

    Complexity: amortized O(1), worst-case O(length t). *)
val dequeue : 'a t -> [ `back | `front ] -> ('a * 'a t) option

val dequeue_exn : 'a t -> [ `back | `front ] -> 'a * 'a t
val dequeue_front : 'a t -> ('a * 'a t) option
val dequeue_front_exn : 'a t -> 'a * 'a t
val dequeue_back : 'a t -> ('a * 'a t) option
val dequeue_back_exn : 'a t -> 'a * 'a t

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving equal]

    include Stable_module_types.With_stable_witness.S1 with type 'a t := 'a t
  end
end

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val build : front:'a list -> back:'a list -> 'a t
end
