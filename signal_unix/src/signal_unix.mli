(** Signal handlers. *)

open! Core
open! Import
open Signal

(** [of_system_int] and [to_system_int] return and take respectively a signal number
    corresponding to those in the system's /usr/include/bits/signum.h (or equivalent).  It
    is not guaranteed that these numbers are portable across any given pair of systems --
    although some are defined as standard by POSIX. *)
val of_system_int : int -> t
val to_system_int : t -> int

type pid_spec = [ `Pid of Pid.t | `My_group | `Group of Pid.t ] [@@deriving sexp_of]

(** [send signal pid_spec] sends [signal] to the processes specified by [pid_spec].

    [send_i] is like [send], except that it silently returns if the specified processes
    don't exist.

    [send_exn] is like [send], except that it raises if the specified processes
    don't exist.

    All of [send], [send_i], and [send_exn] raise if you don't have permission to send the
    signal to the specified processes or if [signal] is unknown. *)
val send     : t -> pid_spec -> [ `Ok | `No_such_process ]
val send_i   : t -> pid_spec -> unit
val send_exn : t -> pid_spec -> unit

(** [can_send_to pid] returns true if [pid] is running and the current process has
    permission to send it signals. *)
val can_send_to : Pid.t -> bool

type sigprocmask_command = [ `Set | `Block | `Unblock ]

(** [sigprocmask cmd sigs] changes the set of blocked signals.

    - If [cmd] is [`Set], blocked signals are set to those in the list [sigs].
    - If [cmd] is [`Block], the signals in [sigs] are added to the set of blocked signals.
    - If [cmd] is [`Unblock], the signals in [sigs] are removed from the set of blocked
      signals.

    [sigprocmask] returns the set of previously blocked signals.
*)
val sigprocmask : sigprocmask_command -> t list -> t list

(** [sigpending ()] returns the set of blocked signals that are currently pending.
*)
val sigpending : unit -> t list

(** [sigsuspend sigs] atomically sets the blocked signals to [sigs] and waits for
 * a non-ignored, non-blocked signal to be delivered.  On return, the blocked
 * signals are reset to their initial value.
*)
val sigsuspend : t list -> unit
