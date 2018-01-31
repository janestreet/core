(** A nano-mutex is a lightweight mutex that can be used only within a single OCaml
    runtime.

    {2 Performance}

    Nano-mutexes are intended to be significantly cheaper than OS-level mutexes.  Creating
    a nano-mutex allocates a single OCaml record.  Locking and unlocking an uncontested
    nano-mutex each take a handful of instructions.  Only if a nano-mutex is contested
    will it fall back to using an OS-level mutex.  If a nano-mutex becomes uncontested
    again, it will switch back to using an OCaml-only lock.

    Nano-mutexes can be faster than using OS-level mutexes because OCaml uses a global
    lock on the runtime, and requires all running OCaml code to hold the lock.  The OCaml
    compiler only allows thread switches at certain points, and we can use that fact to
    get the atomic test-and-set used in the core of our implementation without needing any
    primitive locking, essentially because we're protected by the OCaml global lock.

    Here are some benchmarks comparing various mutexes available in OCaml:

    {v
      |-------------------------------------------------------------|
      |                       Name | Run time | S. dev. | Allocated |
      |----------------------------+----------+---------+-----------+
      |          Caml.Mutex create |   247 ns |    0 ns |         3 |
      |     Caml.Mutex lock/unlock |    49 ns |    0 ns |         0 |
      |          Core.Mutex create |   698 ns |    0 ns |         3 |
      |     Core.Mutex lock/unlock |    49 ns |    0 ns |         0 |
      |          Nano_mutex create |    10 ns |    0 ns |         4 |
      |     Nano_mutex lock/unlock |    28 ns |    0 ns |         0 |
      |-------------------------------------------------------------|
    v}

    The benchmark code is in core/extended/lib_test/bench_nano_mutex.ml.

    {2 Error handling}

    For any mutex, there are design choices as to how to behave in certain situations:

    - recursive locking (when a thread locks a mutex it already has)
    - unlocking an unlocked mutex
    - unlocking a mutex held by another thread

    Here is a table comparing how the various mutexes behave:

    {v
      |--------------------+------------+------------+------------+
      |                    | Caml.Mutex | Core.Mutex | Nano_mutex |
      |--------------------+------------+------------+------------+
      | recursive lock     | undefined  | error      | error      |
      | unlocking unlocked | undefined  | error      | error      |
      | t1:lock  t2:unlock | undefined  | error      | error      |
      |--------------------+------------+------------+------------+
    v}
*)

open! Import

type t [@@deriving sexp_of]

val invariant : t -> unit

(** [create ()] returns a new, unlocked mutex. *)
val create : unit -> t

(** [equal] is [phys_equal] *)
val equal : t -> t -> bool

(** [current_thread_has_lock t] returns [true] iff the current thread has [t] locked. *)
val current_thread_has_lock : t -> bool

(** [lock t] locks the mutex [t], blocking until it can be locked.  [lock] immediately
    returns [Error] if the current thread already holds [t]. *)
val lock     : t -> unit Or_error.t
val lock_exn : t -> unit

(** [try_lock t] locks [t] if it can immediately do so.  The result indicates whether
    [try_lock] succeeded in acquiring the lock.  [try_lock] returns [Error] if the current
    thread already holds [t]. *)
val try_lock     : t -> [ `Acquired | `Not_acquired ] Or_error.t
val try_lock_exn : t -> [ `Acquired | `Not_acquired ]

(** [unlock t] unlocks [t], if the current thread holds it.  [unlock] returns [Error] if
    the lock is not held by the calling thread. *)
val unlock     : t -> unit Or_error.t
val unlock_exn : t -> unit

val critical_section : t -> f:(unit -> 'a) -> 'a

