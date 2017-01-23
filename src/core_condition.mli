open! Import
open Import_time

type t = Condition.t

val create : unit -> t
val equal : t -> t -> bool
val wait : t -> Mutex.t -> unit

(** [timedwait cond mtx timeout] waits on condition variable [cond]
    with mutex [mtx] until either the condition is signalled, or until
    [timeout] expires.  Note that [timeout] is an absolute Unix-time to
    prevent time-related race conditions.

    @return [false] iff the timer expired, but this does not mean that
    the condition is not true due to an unavoidable race condition in
    the system call.

    See [man pthread_cond_timedwait] for details.
*)
val timedwait : t -> Mutex.t -> Time.t -> bool

val signal : t -> unit
val broadcast : t -> unit
