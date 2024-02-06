open! Import

(** The key is used for the hashtable of queue elements. *)
module type Key = Hashtbl.Key_plain

module type S1 = sig
  type 'key create_arg
  type 'key create_key

  (** A hash-queue, where the values are of type ['data]. *)
  type ('key, 'data) t [@@deriving sexp_of]

  include Container.S1_phantom with type ('data, 'key) t := ('key, 'data) t

  (** [invariant t] checks the invariants of the queue. *)

  val invariant : ('key, 'data) t -> unit

  (** [create ()] returns an empty queue.  The arguments [growth_allowed] and [size] are
      referring to the underlying hashtable.

      @param growth_allowed defaults to true
      @param size initial size -- default to 16
  *)
  val create
    :  ?growth_allowed:bool
    -> ?size:int
    -> 'key create_arg
    -> ('key create_key, 'data) t

  (** Clears the queue. *)
  val clear : ('key, 'data) t -> unit

  (** Makes a fresh copy of the queue with identical contents to the original. *)
  val copy : ('key, 'data) t -> ('key, 'data) t

  (** {2 Finding elements} *)

  (** [mem q k] returns true iff there is some (k, v) in the queue. *)
  val mem : ('key, 'data) t -> 'key -> bool

  (** [lookup t k] returns the value of the key-value pair in the queue with
      key k, if there is one. *)
  val lookup : ('key, 'data) t -> 'key -> 'data option

  val lookup_exn : ('key, 'data) t -> 'key -> 'data

  (** {2 Adding, removing, and replacing elements}

      Note that even the non-[*_exn] versions can raise, but only if there is an ongoing
      iteration. *)

  (** [enqueue t back_or_front k v] adds the key-value pair (k, v) to the front or back of
      the queue, returning [`Ok] if the pair was added, or [`Key_already_present] if there
      is already a (k, v') in the queue.
  *)
  val enqueue
    :  ('key, 'data) t
    -> [ `back | `front ]
    -> 'key
    -> 'data
    -> [ `Ok | `Key_already_present ]

  (** Like {!enqueue}, but it raises in the [`Key_already_present] case *)
  val enqueue_exn : ('key, 'data) t -> [ `back | `front ] -> 'key -> 'data -> unit

  (** See {!enqueue}. [enqueue_back t k v] is the same as [enqueue t `back k v]  *)
  val enqueue_back : ('key, 'data) t -> 'key -> 'data -> [ `Ok | `Key_already_present ]

  (** See {!enqueue_exn}. [enqueue_back_exn t k v] is the same as [enqueue_exn t `back k v] *)
  val enqueue_back_exn : ('key, 'data) t -> 'key -> 'data -> unit

  (** See {!enqueue}. [enqueue_front t k v] is the same as [enqueue t `front k v]  *)
  val enqueue_front : ('key, 'data) t -> 'key -> 'data -> [ `Ok | `Key_already_present ]

  (** See {!enqueue_exn}. [enqueue_front_exn t k v] is the same as [enqueue_exn t `front k
      v] *)
  val enqueue_front_exn : ('key, 'data) t -> 'key -> 'data -> unit

  (** [lookup_and_move_to_back] finds the key-value pair (k, v) and moves it to the
      back of the queue if it exists, otherwise returning [None].

      The [_exn] versions of these functions raise if key-value pair does not exist.
  *)
  val lookup_and_move_to_back : ('key, 'data) t -> 'key -> 'data option

  (** Like {!lookup_and_move_to_back}, but raises instead of returning an option *)
  val lookup_and_move_to_back_exn : ('key, 'data) t -> 'key -> 'data

  (** Like {!lookup_and_move_to_back}, but moves element to the front of the queue *)
  val lookup_and_move_to_front : ('key, 'data) t -> 'key -> 'data option

  (** Like {!lookup_and_move_to_front}, but raises instead of returning an option *)
  val lookup_and_move_to_front_exn : ('key, 'data) t -> 'key -> 'data

  (** [last t] returns the last element of the queue, without removing it. *)
  val last : ('key, 'data) t -> 'data option

  (** [last_with_key t] returns the last element of the queue and its key, without
      removing it. *)
  val last_with_key : ('key, 'data) t -> ('key * 'data) option

  (** [first t] returns the front element of the queue, without removing it. *)
  val first : ('key, 'data) t -> 'data option

  (** [first_with_key t] returns the front element of the queue and its key, without
      removing it. *)
  val first_with_key : ('key, 'data) t -> ('key * 'data) option

  (** [keys t] returns the keys in the order of the queue. *)
  val keys : ('key, 'data) t -> 'key list

  (** [to_alist t] returns the elements of the queue with their keys in the order of the
      queue. *)
  val to_alist : ('key, 'data) t -> ('key * 'data) list

  (** [dequeue t front_or_back] returns the front or back element of the queue. *)
  val dequeue : ('key, 'data) t -> [ `back | `front ] -> 'data option

  (** Like {!dequeue}, but it raises if the queue is empty. *)
  val dequeue_exn : ('key, 'data) t -> [ `back | `front ] -> 'data

  (** [dequeue_back t] returns the back element of the queue. *)
  val dequeue_back : ('key, 'data) t -> 'data option

  (** Like {!dequeue_back}, but it raises if the queue is empty. *)
  val dequeue_back_exn : ('key, 'data) t -> 'data

  (** [dequeue_front t] returns the front element of the queue. *)
  val dequeue_front : ('key, 'data) t -> 'data option

  (** Like {!dequeue_front}, but it raises if the queue is empty. *)
  val dequeue_front_exn : ('key, 'data) t -> 'data

  (** [dequeue_with_key t] returns the front or back element of the queue and its key. *)
  val dequeue_with_key : ('key, 'data) t -> [ `back | `front ] -> ('key * 'data) option

  (** Like {!dequeue_with_key}, but it raises if the queue is empty. *)
  val dequeue_with_key_exn : ('key, 'data) t -> [ `back | `front ] -> 'key * 'data

  (** [dequeue_back_with_key t] returns the back element of the queue and its key. *)
  val dequeue_back_with_key : ('key, 'data) t -> ('key * 'data) option

  (** Like {!dequeue_back_with_key}, but it raises if the queue is empty. *)
  val dequeue_back_with_key_exn : ('key, 'data) t -> 'key * 'data

  (** [dequeue_front_with_key t] returns the front element of the queue and its key. *)
  val dequeue_front_with_key : ('key, 'data) t -> ('key * 'data) option

  (** Like {!dequeue_front_with_key}, but it raises if the queue is empty. *)
  val dequeue_front_with_key_exn : ('key, 'data) t -> 'key * 'data

  (** [dequeue_all t ~f] dequeues every element of the queue and applies [f] to each one.
      The dequeue order is from front to back. *)
  val dequeue_all : ('key, 'data) t -> f:('data -> unit) -> unit

  (** [remove q k] removes the key-value pair with key [k] from the queue. *)
  val remove : ('key, 'data) t -> 'key -> [ `Ok | `No_such_key ]

  val remove_exn : ('key, 'data) t -> 'key -> unit

  (** like {!remove}, but returns the removed element *)
  val lookup_and_remove : ('key, 'data) t -> 'key -> 'data option

  (** [replace q k v] changes the value of key [k] in the queue to [v]. *)
  val replace : ('key, 'data) t -> 'key -> 'data -> [ `Ok | `No_such_key ]

  (** [replace_or_enqueue q back_or_front k v] changes the value of key [k] in the queue
      to [v]. If the key [k] does not exist in the queue, it is added to the front or back
      with the value [v]. *)
  val replace_or_enqueue : ('key, 'data) t -> [ `back | `front ] -> 'key -> 'data -> unit

  (** See {!replace_or_enqueue}. [replace_or_enqueue_front t k v] is the same as
      [replace_or_enqueue t `front k v] *)
  val replace_or_enqueue_front : ('key, 'data) t -> 'key -> 'data -> unit

  (** See {!replace_or_enqueue}. [replace_or_enqueue_back t k v] is the same as
      [replace_or_enqueue t `back k v] *)
  val replace_or_enqueue_back : ('key, 'data) t -> 'key -> 'data -> unit

  val replace_exn : ('key, 'data) t -> 'key -> 'data -> unit

  (** [drop ?n q back_or_front] drops [n] elements (default 1) from the back or front of
      the queue. If the queue has fewer than [n] elements then it is cleared. *)
  val drop : ?n:int -> ('key, 'data) t -> [ `back | `front ] -> unit

  (** Equivalent to [drop ?n q `front]. *)
  val drop_front : ?n:int -> ('key, 'data) t -> unit

  (** Equivalent to [drop ?n q `back]. *)
  val drop_back : ?n:int -> ('key, 'data) t -> unit

  (** {2 Iterating over elements} *)

  (** [iter t ~f] applies [f] to each key and element of the queue. *)
  val iteri : ('key, 'data) t -> f:(key:'key -> data:'data -> unit) -> unit

  val foldi
    :  ('key, 'data) t
    -> init:'acc
    -> f:('acc -> key:'key -> data:'data -> 'acc)
    -> 'acc
end

module type S0 = sig
  type ('key, 'data) hash_queue
  type key

  include
    S1
      with type 'key create_key := key
      with type 'key create_arg := unit
      with type ('key, 'data) t := ('key, 'data) hash_queue

  type 'data t = (key, 'data) hash_queue [@@deriving sexp_of]
end

module type S_backend = sig
  include
    S1
      with type 'key create_arg := 'key Hashtbl.Hashable.t
      with type 'key create_key := 'key

  module type S = S0 with type ('key, 'data) hash_queue := ('key, 'data) t

  module Make (Key : Key) : S with type key = Key.t

  module Make_with_hashable (T : sig
    module Key : Key

    val hashable : Key.t Hashtbl.Hashable.t
  end) : S with type key = T.Key.t
end

(** A hash-queue is a combination of a queue and a hashtable that
    supports constant-time lookup and removal of queue elements in addition to
    the usual queue operations (enqueue, dequeue). The queue elements are
    key-value pairs. The hashtable has one entry for each element of the queue.

    Calls to functions that would modify a hash-queue (e.g. [enqueue], [dequeue],
    [remove], [replace]) detect if a client is in the middle of iterating over the
    queue (e.g., [iter], [fold], [for_all], [exists]) and if so, raise an exception.
*)
module type Hash_queue = sig
  module type Key = Key
  module type S_backend = S_backend

  module Make_backend (Table : Hashtbl_intf.Hashtbl) : S_backend

  (** equivalent to [Make_backend (Hashtbl)] *)
  include S_backend
end
