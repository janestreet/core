open! Base
open! Import

(** [Command_env_var] collects all the environment variables used by [Command].

    We define them centrally because some services that wrap [Command] calls need to know
    to special case them. *)
type t =
  | COMMAND_OUTPUT_INSTALLATION_BASH
  | COMMAND_OUTPUT_HELP_SEXP
  | COMP_CWORD
[@@deriving compare, enumerate, sexp_of]

val to_string : t -> string
