open! Base
open! Import

type t =
  | COMMAND_OUTPUT_INSTALLATION_BASH
  | COMMAND_OUTPUT_HELP_SEXP
  | COMP_CWORD
[@@deriving compare, enumerate, sexp_of]

let to_string t = Sexp.to_string (sexp_of_t t)
