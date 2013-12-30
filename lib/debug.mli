open Core_kernel.Std

include module type of Core_kernel.Debug

(** [am], [ams], and [amf] output a source code position and backtrace to stderr.  [amf]
    accepts a printf-style format string.  [ams] accepts a message, value, and sexp
    converter for that value.  Typical usage looks like:

    {[
      ...;
      Debug.am _here_;
      ...;
      Debug.amf _here_ "hello (%s, %s)" (X.to_string x) (Y.to_string y);
      ...;
      Debug.ams _here_ "hello" (x, y) <:sexp_of< X.t * Y.t >>;
      ...;
    ]}

    The [am*] functions output source code positions in the standard format
    "FILE:LINE:COL", which means that one can use a tool like emacs grep-mode on a buffer
    containing debug messages to step through one's code by stepping through the
    messages. *)
val am  : Source_code_position.t -> unit
val ams : Source_code_position.t -> string -> 'a -> ('a -> Sexp.t) -> unit
val amf : Source_code_position.t -> ('r, unit, string, unit) format4 -> 'r


(** [should_print_backtrace] governs whether the [am*] functions print a backtrace. *)
val should_print_backtrace : bool ref
