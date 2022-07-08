(* This executable exercises the behavior of uncaught exceptions at module init, when Core
   is opened. *)
open! Core

exception E of int [@@deriving sexp]

let () = raise (E 42)
