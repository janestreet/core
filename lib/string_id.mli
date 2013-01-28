open Std_internal

(* Disallows whitespace around the edges in of_string and t_of_sexp, but doesn't check
   when reading from bin_io. *)
include Identifier with type t = private string
