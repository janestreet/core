open Sexplib

type t = Lexing.position with bin_io, sexp

val to_sexp_hum : t -> Sexp.t
