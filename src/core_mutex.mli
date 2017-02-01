open! Import

type t = Mutex.t

val create : unit -> t
val equal : t -> t -> bool

(** [lock mtx] locks [mtx], possibly waiting for it to be released
    first by another thread.

    @raise Unix_error if [lock] attempts to acquire [mtx] recursively.
*)
val lock : t -> unit

(** [try_lock] is like [lock], but always returns immediately.  If the calling thread or
    another one already has the mutex it returns [`Already_held_by_me_or_other], otherwise
    it locks it and returns [`Acquired]. *)
val try_lock : t -> [ `Already_held_by_me_or_other | `Acquired ]

(** [timedlock mtx timeout] like [lock], but takes a [timeout] parameter.
    @return [true] if the mutex was acquired, or [false] when [timeout]
    expires otherwise.

    @raise Unix_error if [timedlock] attempts to acquire [mtx] recursively.
*)
val timedlock : (t -> Time.t -> bool) Or_error.t

(** [unlock mtx] unlocks [mtx].

    @raise Unix_error if [unlock] attempts to release an unacquired
    mutex or a mutex held by another thread.
*)
val unlock : t -> unit

(** [critical_section t ~f] locks [t], runs [f], unlocks [t], and returns the result of
    [f] (or raises if [f] raised). *)
val critical_section : t -> f:(unit -> 'a) -> 'a

(** [synchronize f] creates a mutex and returns a new function that is identical to [f]
    except that the mutex is held during its execution. *)
val synchronize : ('a -> 'b) -> ('a -> 'b)

(** [update_signal mtx cnd ~f] updates some state within a critical
    section protected by mutex [mtx] using function [f] and signals
    condition variable [cnd] after finishing.  If [f] raises an exception,
    the condition will NOT be signaled! *)
val update_signal : t -> Condition.t -> f:(unit -> 'a) -> 'a

(** [update_broadcast mtx cnd ~f] updates some state within a critical
    section protected by mutex [mtx] using function [f] and broadcasts
    condition variable [cnd] after finishing.  If [f] raises an exception,
    the condition will NOT be broadcast! *)
val update_broadcast : t -> Condition.t -> f:(unit -> 'a) -> 'a
