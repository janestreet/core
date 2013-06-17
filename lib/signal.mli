open Core_kernel.Std

type t with bin_io, sexp

include Comparable.S with type t := t
include Hashable.S with type t := t
include Stringable.S with type t := t

val equal : t -> t -> bool

(** [of_system_int] and [to_system_int] return and take respectively a signal number
    corresponding to those in the system's /usr/include/bits/signum.h (or equivalent).  It
    is not guaranteed that these numbers are portable across any given pair of systems --
    although some are defined as standard by POSIX. *)
val of_system_int : int -> t
val to_system_int : t -> int

(** [of_caml_int] constructs a Signal.t given an O'Caml internal signal number.  This is
    only for the use of the Core_unix module. *)
val of_caml_int : int -> t
val to_caml_int : t -> int

(** [to_string t] returns a human-readable name: "sigabrt", "sigalrm", ... *)
val to_string : t -> string

(** The default behaviour of the system if these signals trickle to the top level of a
    program.  See include/linux/kernel.h in the Linux kernel source tree (not the file
    /usr/include/linux/kernel.h). *)
type sys_behavior = [
| `Continue  (** Continue the process if it is currently stopped*)
| `Dump_core (** Terminate the process and dump core *)
| `Ignore    (** Ignore the signal*)
| `Stop      (** Stop the process *)
| `Terminate (** Terminate the process *)
]
with sexp

(** [default_sys_behavior t]
    Query the default system behavior for a signal.
*)
val default_sys_behavior : t -> sys_behavior

(** [handle_default t] is [set t `Default]. *)
val handle_default : t -> unit

(** [ignore t] is [set t `Ignore]. *)
val ignore : t -> unit

type pid_spec = [ `Pid of Pid.t | `My_group | `Group of Pid.t ] ;;

(** [send signal pid] sends [signal] to the process whose process id is [pid]. *)
val send : t -> pid_spec -> [ `Ok | `No_such_process ]

(** [send_i signal ~pid] sends [signal] to the process whose process id is [pid].
 * No exception will be raised if [pid] is a zombie or nonexistent.
 *)
val send_i : t -> pid_spec -> unit

(** [send_exn signal ~pid] sends [signal] to the process whose process id is
 * [pid].  In Caml's standard library, this is called [Unix.kill].  Sending a
 * signal to a zombie and/or nonexistent process will raise an exception.
 *)
val send_exn : t -> pid_spec -> unit

(** [can_send_to pid] returns true if [pid] is running and the current process has
    permission to send it signals. *)
val can_send_to : Pid.t -> bool

type sigprocmask_command = [ `Set | `Block | `Unblock ]

(** [sigprocmask cmd sigs] changes the set of blocked signals.
 * If [cmd] is [`Set], blocked signals are set to those in the list [sigs].
 * If [cmd] is [`Block], the signals in [sigs] are added to the set of blocked
 *   signals.
 * If [cmd] is [`Unblock], the signals in [sigs] are removed from the set of
 *   blocked signals.
 * [sigprocmask] returns the set of previously blocked signals.
 *)
val sigprocmask : sigprocmask_command -> t list -> t list

(** [sigpending ()] returns the set of blocked signals that are currently
 * pending.
 *)
val sigpending : unit -> t list

(** [sigsuspend sigs] atomically sets the blocked signals to [sigs] and waits for
 * a non-ignored, non-blocked signal to be delivered.  On return, the blocked
 * signals are reset to their initial value.
 *)
val sigsuspend : t list -> unit


(** Specific signals, along with their default behavior and meaning. *)

val abrt   : t  (** [Dump_core]  Abnormal termination                           *)
val alrm   : t  (** [Terminate]  Timeout                                        *)
val chld   : t  (** [Ignore]     Child process terminated                       *)
val cont   : t  (** [Continue]   Continue                                       *)
val fpe    : t  (** [Dump_core]  Arithmetic exception                           *)
val hup    : t  (** [Terminate]  Hangup on controlling terminal                 *)
val ill    : t  (** [Dump_core]  Invalid hardware instruction                   *)
val int    : t  (** [Terminate]  Interactive interrupt (ctrl-C)                 *)
val kill   : t  (** [Terminate]  Termination (cannot be ignored)                *)
val pipe   : t  (** [Terminate]  Broken pipe                                    *)
val prof   : t  (** [Terminate]  Profiling interrupt                            *)
val quit   : t  (** [Dump_core]  Interactive termination                        *)
val segv   : t  (** [Dump_core]  Invalid memory reference                       *)
val stop   : t  (** [Stop]       Stop                                           *)
val term   : t  (** [Terminate]  Termination                                    *)
val tstp   : t  (** [Stop]       Interactive stop                               *)
val ttin   : t  (** [Stop]       Terminal read from background process          *)
val ttou   : t  (** [Stop]       Terminal write from background process         *)
val usr1   : t  (** [Terminate]  Application-defined signal 1                   *)
val usr2   : t  (** [Terminate]  Application-defined signal 2                   *)
val vtalrm : t  (** [Terminate]  Timeout in virtual time                        *)
val zero   : t  (** [Ignore]     No-op; can be used to test whether the target
                                 process exists and the current process has
                                 permission to signal it                        *)

(** The [Expert] module contains functions that novice users should avoid, due to their
    complexity.

    An OCaml signal handler can run at any time, which introduces all the semantic
    complexities of multithreading.  It is much easier to use async signal handling, see
    {!Async_unix.Signal}, which does not involve multithreading, and runs user code as
    ordinary async jobs.  Also, beware that there can only be a single OCaml signal
    handler for any signal, so handling a signal with a [Core] signal handler will
    interfere if async is attempting to handle the same signal.

    If you do use [Core] signal handlers, you should strive to make the signal handler
    perform a simple idempotent action, like setting a ref. *)
module Expert : sig

  type behavior =
    [ `Default
    | `Ignore
    | `Handle of t -> unit
    ]

  (** [signal t] sets the behavior of the system on receipt of signal [t] and returns the
      behavior previously associated with [t].  If [t] is not available on your system,
      [signal] raises. *)
  val signal : t -> behavior -> behavior


  (** [set t b] is [ignore (signal t b)] *)
  val set : t -> behavior -> unit

  (** [handle t f] is [set t (`Handle f)]. *)
  val handle : t -> (t -> unit) -> unit

end
