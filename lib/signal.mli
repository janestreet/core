type t

include Comparable.S with type t := t
include Hashable.S with type t := t
include Sexpable.S with type t := t
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

type behavior = [
| `Default
| `Ignore
| `Handle of t -> unit
]

(** [default_sys_behavior t]
    Query the default system behavior for a signal.
*)
val default_sys_behavior : t -> sys_behavior

(** [signal t]
    Set the behavior of the system on receipt of a given signal.  The
   first argument is the signal number.  Return the behavior
   previously associated with the signal. If the signal number is
   invalid (or not available on your system), an [Invalid_argument]
   exception is raised. *)
val signal : t -> behavior -> behavior

(** [set t b] is [ignore (signal t b)] *)
val set : t -> behavior -> unit

(** [handle t f] is [set t (`Handle f)]. *)
val handle : t -> (t -> unit) -> unit

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


(** Specific signals *)

val abrt : t (** Abnormal termination *)
val alrm : t (** Timeout *)
val chld : t (** Child process terminated *)
val cont : t (** Continue *)
val fpe : t (** Arithmetic exception *)
val hup : t (** Hangup on controlling terminal *)
val ill : t (** Invalid hardware instruction *)
val int : t (** Interactive interrupt (ctrl-C) *)
val kill : t (** Termination (cannot be ignored) *)
val pipe : t (** Broken pipe *)
val prof : t (** Profiling interrupt *)
val quit : t (** Interactive termination *)
val segv : t (** Invalid memory reference *)
val stop : t (** Stop *)
val term : t (** Termination *)
val tstp : t (** Interactive stop *)
val ttin : t (** Terminal read from background process *)
val ttou : t (** Terminal write from background process *)
val usr1 : t (** Application-defined signal 1 *)
val usr2 : t (** Application-defined signal 2 *)
val vtalrm : t (** Timeout in virtual time *)
val zero : t (** No-op; can be used to test whether the target process exists and the
                 current process has permission to signal it *)
