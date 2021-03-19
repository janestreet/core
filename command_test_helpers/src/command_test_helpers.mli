(** Functions to help test [Command]. *)

open! Core
open! Import

(** [parse_command_line param] returns a function which evaluates [param] against a string
    list as if those were the arguments passed to [param]. No shelling out takes place.
    However, the [param] is evaluated, and side effects of that evaluation do occur.

    See [validate_command_line] below for a less accurate but generally safer test that
    does not evaluate the param.  (Of course if your param is side-effect free, there's no
    reason to shy away from this one.)

    If the command-line fails to parse, an error will be printed.  If the command-line
    parsing code exits for any reason (e.g. you passed "-help"), the exit code is printed.
*)
val parse_command_line
  :  ?path:string list
  -> ?summary:string
  -> 'a Command.Param.t
  -> (?on_error:(unit -> unit) -> ?on_success:('a -> unit) -> string list -> unit)
       Staged.t

module On_exec : sig
  (** We don't follow execs unless the caller promises it's safe.

      Tests should only depend in binaries that will be available and unchanged forever.
      Typically, that means binaries built in the repo. *)

  type t =
    | Fail
    | Trust_me__I_understand_the_consequences_of_external_dependencies
  [@@deriving compare, sexp_of]
end

(** [validate_command_line shape] provides a function [f] s.t. [f args] is best-effort
    check of [args] against the command described by [shape], without actual execution of
    that command.

    What we check:

    1. [args] refers to a valid subcommand of [shape].

    2. [args] passes an acceptable number of anonymous arguments.

    3. [args] passes flags that exist, an acceptable number of times, and with arguments
    where they are expected.

    What we do not check:

    1. Whether argument have acceptable values. E.g., it falsely accepts floats where ints
    are expected.

    2. Side effects during argument parsing, including aborting further parsing of the
    command line.  E.g., it does not handle [-help] or [escape] flags correctly.

    3. Aliases excluded from help.  E.g., [--help].

    4. [full_flag_required].  We assume every flag can be passed by prefix.

*)
val validate_command_line
  :  ?on_exec:On_exec.t (** default: [Fail] *)
  -> Command.Shape.t
  -> (string list -> unit Or_error.t) Or_error.t

(** [complete ?which_arg param ~args] prints the completion suggestions to stderr.

    Thread safety:

    [complete] is not in general thread-safe. It sets and then restores the environment
    variable [COMP_CWORD]. However, the cooperative multi-threading semantics of [Async]
    mean that other async jobs will not see the altered environments.

    Side effects:

    [complete] will not perform the side effects of the param proper (e.g., due to the [f]
    of a [Param.map ~f]).

    [complete] will perform side effects of completion (e.g., due to the [complete] of
    [Arg_type.create ~complete]).
*)
val complete
  :  ?which_arg:int (** zero-indexed. Default: the last arg *)
  -> _ Command.Param.t
  -> args:string list
  -> unit

(** As [complete] but applies to an intact [Command]. *)
val complete_command
  :  ?complete_subcommands:
    (path:string list -> part:string -> string list list -> string list option)
  -> ?which_arg:int
  -> Command.t
  -> args:string list
  -> unit
