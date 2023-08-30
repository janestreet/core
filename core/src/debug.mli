(** Utilities for printing debug messages. *)

open! Import

(** [eprint message] prints to stderr [message], followed by a newline and flush.  This is
    the same as [prerr_endline]. *)
val eprint : string -> unit

(** [eprints message a sexp_of_a] prints to stderr [message] and [a] as a sexp, followed
    by a newline and flush. *)
val eprints : string -> 'a -> ('a -> Sexp.t) -> unit

(** [eprint_s sexp] prints [sexp] to stderr, followed by a newline and a flush. *)
val eprint_s : Sexp.t -> unit

(** [eprintf message arg1 ... argn] prints to stderr [message], with sprintf-style format
    characters instantiated, followed by a newline and flush. *)
val eprintf : ('r, unit, string, unit) format4 -> 'r

(** [Debug.Make] produces a [debug] function used to wrap a function to display arguments
    before calling and display results after returning.  Intended usage is:

    {[
      module Foo = struct
        type t = ...
        let invariant = ...
          let bar t x y : Result.t = ...
      end
      module Foo_debug = struct
        open Foo
        include Debug.Make ()
        let debug x = debug invariant ~module_name:"Foo" x
        let bar t x y =
          debug "bar" [t] (t, x, y) [%sexp_of: t * X.t * Y.t] [%sexp_of: Result.t]
            (fun () -> bar t x y)
      end
    ]}
*)
module Make () : sig
  (** Whether the invariants are called on each invocation. *)
  val check_invariant : bool ref

  (** If true, you get a message on stderr every time [debug] is called. *)
  val show_messages : bool ref

  (** We avoid labels so that the applications are more concise -- see example above. *)
  val debug
    :  't Invariant.t
    -> module_name:string (** module_name appears on messages *)
    -> string (** string name of function [f], also appears on messages *)
    -> 't list (** args of type [t], to have invariant checked iff [check_invariant] *)
    -> 'args (** arguments to the function we're debugging *)
    -> ('args -> Sexp.t)
    -> ('result -> Sexp.t)
    -> (unit -> 'result) (** should call [f] with ['args], exn's re-raised *)
    -> 'result
end

(** [am], [ams], and [amf] output a source code position and backtrace to stderr.  [amf]
    accepts a printf-style format string.  [ams] accepts a message, value, and sexp
    converter for that value.  Typical usage looks like:

    {[
      ...;
    Debug.am [%here];
      ...;
      Debug.amf [%here] "hello (%s, %s)" (X.to_string x) (Y.to_string y);
      ...;
      Debug.ams [%here] "hello" (x, y) [%sexp_of: X.t * Y.t];
      ...;
    ]}

    The [am*] functions output source code positions in the standard format
    "FILE:LINE:COL", which means that one can use a tool like emacs grep-mode on a buffer
    containing debug messages to step through one's code by stepping through the
    messages. *)
val am : Source_code_position.t -> unit

val ams : Source_code_position.t -> string -> 'a -> ('a -> Sexp.t) -> unit
val amf : Source_code_position.t -> ('r, unit, string, unit) format4 -> 'r

(** [should_print_backtrace] governs whether the [am*] functions print a backtrace. *)
val should_print_backtrace : bool ref
