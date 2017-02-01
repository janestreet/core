(** Lightweight threads. *)

open! Import

(** The type of thread handles. *)
type t [@@deriving sexp_of]

(** {6 Thread creation and termination} *)

val create : ('a -> 'b) -> 'a -> t
(** [Thread.create funct arg] creates a new thread of control,
    in which the function application [funct arg]
    is executed concurrently with the other threads of the program.
    The application of [Thread.create]
    returns the handle of the newly created thread.
    The new thread terminates when the application [funct arg]
    returns, either normally or by raising an uncaught exception.
    In the latter case, the exception is printed on standard error,
    but not propagated back to the parent thread. Similarly, the
    result of the application [funct arg] is discarded and not
    directly accessible to the parent thread. *)

val self : unit -> t
(** Return the thread currently executing. *)

val id : t -> int
(** Return the identifier of the given thread. A thread identifier
    is an integer that identifies uniquely the thread.
    It can be used to build data structures indexed by threads. *)

val exit : unit -> unit
(** Terminate prematurely the currently executing thread. *)

(** This has been deliberately removed from the interface because it is an inherently
    unsafe operation and is never required.

    {[
      (** Terminate prematurely the thread whose handle is given.
          This functionality is available only with bytecode-level threads. *)
      val kill : t -> unit
    ]}
*)

(** {6 Suspending threads} *)

val delay : float -> unit
(** [delay d] suspends the execution of the calling thread for
    [d] seconds. The other program threads continue to run during
    this time. *)

val join : t -> unit
(** [join th] suspends the execution of the calling thread
    until the thread [th] has terminated. *)

val wait_read : Unix.file_descr -> unit
(** See {!Thread.wait_write}.*)

val wait_write : Unix.file_descr -> unit
(** Suspend the execution of the calling thread until at least
    one character is available for reading ({!Thread.wait_read}) or
    one character can be written without blocking ([wait_write])
    on the given Unix file descriptor. *)

val wait_timed_read : Unix.file_descr -> float -> bool
(** See {!Thread.wait_timed_write}.*)

val wait_timed_write : Unix.file_descr -> float -> bool
(** Same as {!Thread.wait_read} and {!Thread.wait_write}, but wait for at most
    the amount of time given as second argument (in seconds).
    Return [true] if the file descriptor is ready for input/output
    and [false] if the timeout expired. *)

val yield : unit -> unit
(** Re-schedule the calling thread without suspending it.
    This function can be used to give scheduling hints,
    telling the scheduler that now is a good time to
    switch to other threads. *)

(** {6 Management of signals} *)

(** Signal handling follows the POSIX thread model: signals generated
    by a thread are delivered to that thread; signals generated externally
    are delivered to one of the threads that does not block it.
    Each thread possesses a set of blocked signals, which can be modified
    using {!Thread.sigmask}.  This set is inherited at thread creation time.
    Per-thread signal masks are supported only by the system thread library
    under Unix, but not under Win32, nor by the VM thread library. *)

val sigmask : Signal.sigprocmask_command -> Signal.t list -> Signal.t list
(** [sigmask cmd sigs] changes the set of blocked signals for the
    calling thread.
    If [cmd] is [`Set], blocked signals are set to those in
    the list [sigs].
    If [cmd] is [`Block], the signals in [sigs] are added to
    the set of blocked signals.
    If [cmd] is [`Unblock], the signals in [sigs] are removed
    from the set of blocked signals.
    [sigmask] returns the set of previously blocked signals for the thread. *)

val wait_signal : Signal.t list -> int
(** [wait_signal sigs] suspends the execution of the calling thread
    until the process receives one of the signals specified in the
    list [sigs].  It then returns the number of the signal received.
    Signal handlers attached to the signals in [sigs] will not
    be invoked.  The signals [sigs] are expected to be blocked before
    calling [wait_signal]. *)


(** Jane Street extensions *)

(** [true] iff Thread.create has ever been called, even if there is
    currently only one running thread. *)
val threads_have_been_created : unit -> bool

(** [num_threads ()] attempts to return the number of currently running
    threads by parsing /proc.  Since this is an operation frought with
    potential failure, we return an option in cases of failure *)
val num_threads : unit -> int option

(** [block_forever ()] will block the calling thread forever. *)
val block_forever : unit -> 'a
