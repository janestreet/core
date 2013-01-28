open Sexplib

type t = Lexing.position with bin_io, sexp

include Comparable.S with type t := t
include Hashable.S   with type t := t

val to_sexp_hum : t -> Sexp.t
