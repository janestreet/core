open! Core

exception E of int [@@deriving sexp]

let () = raise (E 42)
