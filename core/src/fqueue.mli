(** A simple polymorphic functional queue.  Use this data structure for strictly first-in,
    first-out access to a sequence of values.  For a similar data structure with enqueue
    and dequeue accessors on both ends of a sequence, see
    {{!Core.Fdeque}[Core.Fdeque]}.

    Amortized running times assume that [enqueue]/[dequeue] are used sequentially,
    threading the changing Fqueue through the calls. *)

open! Import

type 'a t [@@deriving bin_io, compare, equal, hash, sexp]

include Container.S1 with type 'a t := 'a t
include Invariant.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t

(** The empty queue. *)
val empty : 'a t

(** [enqueue t x] returns a queue with adds [x] to the end of [t].  Complexity: O(1). *)
val enqueue : 'a t -> 'a -> 'a t

(** Returns the front (least recently enqueued) element.  Raises [Empty] if no element is
    found.  Complexity: O(1). *)
val peek_exn : 'a t -> 'a

val top_exn : 'a t -> 'a [@@deprecated "[since 2019-11] Use [peek_exn] instead."]

(** Like [peek_exn], but returns its result optionally, without exception.  Complexity:
    O(1). *)
val peek : 'a t -> 'a option

val top : 'a t -> 'a option [@@deprecated "[since 2019-11] Use [peek] instead."]

(** [dequeue_exn t] removes and returns the front of [t], raising [Empty] if [t] is empty.
    Complexity: amortized O(1). *)
val dequeue_exn : 'a t -> 'a * 'a t

(** Like [dequeue_exn], but returns result optionally, without exception.  Complexity:
    amortized O(1). *)
val dequeue : 'a t -> ('a * 'a t) option

(** Returns version of queue with front element removed.  Complexity: amortized O(1). *)
val drop_exn : 'a t -> 'a t

val discard_exn : 'a t -> 'a t [@@deprecated "[since 2019-11] Use [drop_exn] instead."]

(** [to_list t] returns a list of the elements in [t] in order from least-recently-added
    (at the head) to most-recently-added (at the tail).  Complexity: O(n). *)
val to_list : 'a t -> 'a list

(** [of_list] is the inverse of [to_list].  Complexity: O(n). *)
val of_list : 'a list -> 'a t

(** [to_sequence] returns a [Sequence.t] of the elements in [t] in order from
    from least-recently-added (at the head) to most-recently-added (at the
    tail). Complexity (if the sequence is fully traversed): O(n).

    {[to_list t = Sequence.to_list (to_sequence t)]}
*)
val to_sequence : 'a t -> 'a Sequence.t

(** [of_sequence] is the inverse of [to_sequence]. Complexity (if the sequence
    is fully traversed): O(n). *)
val of_sequence : 'a Sequence.t -> 'a t

(** Complexity: O(1). *)
val length : 'a t -> int

(** Complexity: O(1). *)
val is_empty : 'a t -> bool

val singleton : 'a -> 'a t

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving equal]

    include Stable_module_types.With_stable_witness.S1 with type 'a t := 'a t
  end
end
