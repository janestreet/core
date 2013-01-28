open Sexplib.Std
open Bin_prot.Std
open Result

exception Already_set with sexp

type 'a t = 'a option ref with sexp

let create () = ref None

let set_exn t v =
  match !t with
  | None -> t := Some v
  | Some _ -> raise Already_set

let set t v =
  try Ok (set_exn t v)
  with Already_set -> Error "already set"

let get t = !t

let get_exn t = Option.value_exn !t
