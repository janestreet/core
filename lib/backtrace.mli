(** Dealing with stack backtraces.

    The [Backtrace] module deals with two different kinds of backtraces:

    1. snapshots of the stack obtained on demand ([Backtrace.get])
    2. the stack frames unwound when an exception is raised ([Backtrace.Exn])
*)

open Core_kernel.Std

(** A [Backtrace.t] is a snapshot of the stack obtained by calling [Backtrace.get].  It is
    represented as a string with newlines separating the frames.  [sexp_of_t] splits the
    string at newlines and removes some of the cruft, leaving a human-friendly list of
    frames, but [to_string] does not. *)
type t with sexp_of

val get : (unit -> t) Or_error.t

(** [get_opt ()] returns a backtrace if [get] is implemented. *)
val get_opt : unit -> t option

val to_string : t -> string

(** [Backtrace.Exn] has functions for controlling and printing the backtrace of the most
    recently raised exception.

    When an exception is raised, the runtime "unwinds" the stack, i.e. removes stack
    frames, until it reaches a frame with an exception handler.  It then matches the
    exception against the patterns in the handler.  If the exception matches, then the
    program continues.  If not, then the runtime continues unwinding the stack to the
    next handler.

    If [am_recording () = true], then the runtime, while it is unwinding the stack, keeps
    track of the part of the stack that is unwound.  This is available as a human-readable
    string via [most_recent ()].  Calling [most_recent] if [am_recording () = false] will
    yield the empty string.

    With [am_recording () = true], OCaml keeps only a backtrace for the most recently
    raised exception.  When one raises an exception, OCaml checks if it is physically
    equal to the most recently raised exception.  If they are equal, then OCaml appends
    the string representation of the stack unwound by the current raise to the stored
    backtrace.  If the exception being raised is not physically equally to the most
    recently raised exception, then OCaml starts recording a new backtrace.  Thus one must
    call [most_recent] before a subsequent [raise] of a (physically) distinct exception,
    or the backtrace is lost.

    The initial value of [am_recording ()] is determined by the setting of the
    environment variable OCAMLRUNPARAM.  If OCAMLRUNPARAM is set, then [am_recording ()
    = true] iff the character "b" occurs in OCAMLRUNPARAM.  If OCAMLRUNPARAM is not set,
    then [am_recording ()] is initially true.

    This is the same functionality as provided by the OCaml stdlib [Printexc] functions
    [backtrace_status], [record_backtraces], [get_backtrace]. *)
module Exn : sig

  val am_recording  : unit -> bool
  val set_recording : bool -> unit

  (** [most_recent ()] returns a string containing the stack that was unwound by the
      most recently raised exception. *)
  val most_recent : unit -> string
end
