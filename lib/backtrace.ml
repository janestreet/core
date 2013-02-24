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
IFDEF LINUX_EXT THEN
external backtrace_get : unit -> string = "backtrace_get"
let get = Ok backtrace_get
ELSE
let get = unimplemented "Backtrace.get"
ENDIF
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

module Exn = struct
  let set_recording = Printexc.record_backtrace
  let am_recording  = Printexc.backtrace_status
  let most_recent   = Printexc.get_backtrace

  (* We turn on backtraces by default if OCAMLRUNPARAM isn't set. *)
  let () =
    match Core_sys.getenv "OCAMLRUNPARAM" with
    | None   -> set_recording true
    | Some _ -> ()  (* the caller set something, they are responsible *)
  ;;

  let with_recording b ~f =
    let saved = am_recording () in
    set_recording b;
    protect ~f ~finally:(fun () -> set_recording saved)
  ;;

  TEST = "" = with_recording false ~f:most_recent
end
