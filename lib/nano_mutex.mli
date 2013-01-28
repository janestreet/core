(* A nano-mutex is a lightweight mutex that can be used only within a single OCaml
   runtime.

   Performance
   ===========
   Nano-mutexes are intended to be significantly cheaper than OS-level mutexes.  Creating
   a nano-mutex allocates a single OCaml record.  Locking and unlocking an uncontested
   nano-mutex take a handful of instructions.  Only if a nano-mutex is contested will it
   fall back to using an OS-level mutex.  If a nano-mutex becomes uncontested again, it
   will switch back to using an OCaml-only lock.

   Nano-mutexes can be faster than using OS-level mutexes because OCaml uses a global lock
   on the runtime, and requires all running OCaml code to hold the lock.  The OCaml
   compiler only allows thread switches at certain points, and we can use that fact to get
   the atomic test-and-set used in the core of our implementaion without needing any
   primitive locking, essentially because we're protected by the OCaml global lock.

   Here are some benchmarks comparing various mutexes available in OCaml:

   |-------------------------------------------------------------|
   |                       Name | Run time | S. dev. | Allocated |
   |----------------------------+----------+---------+-----------+
   |          Caml.Mutex create |   247 ns |    0 ns |         3 |
   |     Caml.Mutex lock/unlock |    49 ns |    0 ns |         0 |
   |          Core.Mutex create |   698 ns |    0 ns |         3 |
   |     Core.Mutex lock/unlock |    49 ns |    0 ns |         0 |
   |      Agnostic_mutex create |   534 ns |    0 ns |        10 |
   | Agnostic_mutex lock/unlock |   188 ns |    0 ns |        16 |
   |          Nano_mutex create |    10 ns |    0 ns |         4 |
   |     Nano_mutex lock/unlock |    28 ns |    0 ns |         0 |
   |-------------------------------------------------------------|

   The benchmark code is in core/extended/lib_test/bench_nano_mutex.ml.

   Error handling
   ==============
   For any mutex, there are design choices as to how to behave in certain situations:

   * recursive locking (when a thread locks a mutex it already has)
   * unlocking an unlocked mutex
   * unlocking a mutex held by another thread

   For those design choices, Nano_mutex is most similar in semantics to Agnostic_mutex.
   Here is a table comparing how the various mutexes behave:

   |--------------------+------------+------------+----------------+------------|
   |                    | Caml.Mutex | Core.Mutex | Agnostic_mutex | Nano_mutex |
   |--------------------+------------+------------+----------------+------------|
   | recursive lock     | undefined  | error      | deadlock       | error      |
   | unlocking unlocked | undefined  | error      | error          | error      |
   | t1:lock  t2:unlock | undefined  | error      | ok             | error/ok   |
   |--------------------+------------+------------+----------------+------------|

   [Nano_mutex.unlock] fails if another thread holds the lock; however, one can supply
   [~allow_from_any_thread:true] to allow unlocking a lock held by another thread, like
   [Agnostic_mutex]. *)

type t with sexp_of

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
