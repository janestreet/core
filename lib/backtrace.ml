open Std_internal

module Sexp = Sexplib.Sexp

type t = string

let to_string t = t

let sexp_of_t t =
  let l =
    List.map (String.split t ~on:'\n') ~f:(fun s ->
      Sexp.Atom
        (match String.index s ':' with
        | None -> s
        | Some i -> try String.slice s (i + 2) 0 with _ -> s))
  in
  Sexp.List (List.drop l 2)
;;


INCLUDE "config.mlh"
IFDEF ARCH_x86_64 THEN
external backtrace_get : unit -> string = "backtrace_get"
let get = Ok backtrace_get
ELSE
let get = Or_error.error "unimplemented" "Backtrace.get" <:sexp_of< string >>
ENDIF
