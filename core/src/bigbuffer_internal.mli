open! Import

type t =
  { mutable bstr : Bigstring.t
  ; mutable pos : int
  ; mutable len : int
  ; init : Bigstring.t
  }
[@@deriving sexp_of]

val resize : t -> int -> unit
