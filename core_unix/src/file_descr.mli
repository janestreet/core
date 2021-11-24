open! Core
open! Import

type t = Unix.file_descr [@@deriving sexp]

include Equal.S with type t := t
include Hashable.S_plain with type t := t
include Stringable.S with type t := t

val of_int : int -> t
val of_int_exn : int -> t
val to_int : t -> int
