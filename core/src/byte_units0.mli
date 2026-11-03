open! Import
module Repr = Int63

type t [@@deriving compare, hash, sexp_of, typerep] [@@immediate64]

val to_string : t -> string
val to_string_hum : t -> string
val of_repr : Repr.t -> t
val to_repr : t -> Repr.t
val bytes_int_exn : t -> int
