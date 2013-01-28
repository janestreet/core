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
let get = unimplemented "Backtrace.get"
ENDIF

TEST_UNIT =
  match get with
  | Error _ -> ()
  | Ok get ->
    let s = get () in
    assert (String.length s > 0);
;;
