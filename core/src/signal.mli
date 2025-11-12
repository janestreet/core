@@ portable

(** Signal handlers. *)

open! Import

type t : immediate [@@deriving bin_io, sexp]

include%template Comparable.S [@mode local portable] with type t := t

include Hashable.S with type t := t
include Stringable.S with type t := t

val equal : t -> t -> bool

(** [of_caml_int] constructs a [Signal.t] given an OCaml internal signal number. This is
    only for the use of the [Core_unix] module. *)
val of_caml_int : int -> t

val to_caml_int : t -> int

(** [to_string t] returns a human-readable name: "sigabrt", "sigalrm", ... *)
val to_string : t -> string

(** The behaviour of the system if a signal is received by a process. See
    include/linux/kernel.h in the Linux kernel source tree (not the file
    /usr/include/linux/kernel.h). *)
type sys_behavior =
  [ `Continue (** Continue the process if it is currently stopped *)
  | `Dump_core (** Terminate the process and dump core *)
  | `Ignore (** Ignore the signal *)
  | `Stop (** Stop (suspend) the process *)
  | `Terminate (** Terminate the process *)
  ]
[@@deriving sexp, sexp_grammar]

(** Queries the default system behavior for a signal. *)
val default_sys_behavior : t -> sys_behavior

(** [handle_default t] is [set t `Default]. *)
val handle_default : t -> unit @@ nonportable

(** [ignore t] is [set t `Ignore]. *)
val ignore : t -> unit @@ nonportable

(** Specific signals, along with their default behavior and meaning. *)

(** [Dump_core] Abnormal termination *)
val abrt : t

(** [Terminate] Timeout *)
val alrm : t

(** [Dump_core] Bus error *)
val bus : t

(** {v  [Ignore]     Child process terminated v} *)
val chld : t

(** {v  [Continue]   Continue v} *)
val cont : t

(** [Dump_core] Arithmetic exception *)
val fpe : t

(** [Terminate] Hangup on controlling terminal *)
val hup : t

(** [Dump_core] Invalid hardware instruction *)
val ill : t

(** [Terminate] Interactive interrupt (ctrl-C) *)
val int : t

(** [Terminate] Termination (cannot be ignored) *)
val kill : t

(** [Terminate] Broken pipe *)
val pipe : t

(** [Terminate] Pollable event *)
val poll : t

(** [Terminate] Profiling interrupt *)
val prof : t

(** [Dump_core] Interactive termination *)
val quit : t

(** [Dump_core] Invalid memory reference *)
val segv : t

(** [Dump_core] Bad argument to routine *)
val sys : t

(** {v  [Stop]       Stop v} *)
val stop : t

(** [Terminate] Termination *)
val term : t

(** [Dump_core] Trace/breakpoint trap *)
val trap : t

(** {v  [Stop]       Interactive stop v} *)
val tstp : t

(** {v  [Stop]       Terminal read from background process v} *)
val ttin : t

(** {v  [Stop]       Terminal write from background process v} *)
val ttou : t

(** {v  [Ignore]     Urgent condition on socket v} *)
val urg : t

(** [Terminate] Application-defined signal 1 *)
val usr1 : t

(** [Terminate] Application-defined signal 2 *)
val usr2 : t

(** [Terminate] Timeout in virtual time *)
val vtalrm : t

(** [Dump_core] Timeout in cpu time *)
val xcpu : t

(** [Dump_core] File size limit exceeded *)
val xfsz : t

(** {v
 [Ignore]     No-op; can be used to test whether the target
    process exists and the current process has
    permission to signal it
    v} *)
val zero : t

(** All known POSIX signals. *)
val all_posix : t list

type pid_spec = [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]

type sigprocmask_command = [ `Use_Signal_unix ]
[@@deprecated "[since 2021-04] Use [Signal_unix]"]

val can_send_to : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]

val of_system_int : [ `Use_Signal_unix ]
[@@deprecated "[since 2021-04] Use [Signal_unix]"]

val send : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]
val send_exn : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]
val send_i : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]

val sexp_of_pid_spec : [ `Use_Signal_unix ]
[@@deprecated "[since 2021-04] Use [Signal_unix]"]

val sigpending : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]
val sigprocmask : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]
val sigsuspend : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]

val to_system_int : [ `Use_Signal_unix ]
[@@deprecated "[since 2021-04] Use [Signal_unix]"]

(** The [Expert] module contains functions that novice users should avoid, due to their
    complexity.

    An OCaml signal handler can run at any time, which introduces all the semantic
    complexities of multithreading. It is much easier to use Async's signal handling, see
    {!Async_unix.Signal}, which does not involve multithreading, and runs user code as
    ordinary Async jobs. Also, beware that there can only be a single OCaml signal handler
    for any signal, so handling a signal with a [Core] signal handler will interfere if
    Async is attempting to handle the same signal.

    All signal handler functions are called with
    [Exn.handle_uncaught_and_exit_immediately], to prevent the signal handler from
    raising, because raising from a signal handler could raise to any allocation or GC
    point in any thread, which would be impossible to reason about. Note that since it
    calls [exit_immediately], uncaught exceptions will lead to any registered [at_exit]
    functions not being run.

    If you do use [Core] signal handlers, you should strive to make the signal handler
    perform a simple idempotent action, like setting a ref. *)
module Expert : sig
  type behavior =
    | Default
    | Ignore
    | Handle of (t -> unit) @@ portable
  [@@deriving sexp_of]

  (** [signal t] sets the behavior of the system on receipt of signal [t] and returns the
      behavior previously associated with [t]. If [t] is not available on your system,
      [signal] raises. *)
  val signal : t -> behavior -> behavior

  (** [set t b] is [ignore (signal t b)]. *)
  val set : t -> behavior -> unit

  (** [handle t f] is [set t (`Handle f)]. *)
  val handle : t -> (t -> unit) @ portable -> unit
end

module Stable : sig
  module V2 : sig
    type nonrec t = t [@@deriving bin_io, compare ~localize, sexp]
  end

  module V1 : sig
    type nonrec t = t [@@deriving bin_io, compare ~localize, sexp]
  end
end
