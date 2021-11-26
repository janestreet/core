open! Import
open Std_internal
module Ephemeron = Caml.Ephemeron.K1

type ('a, 'b) t = ('a Heap_block.t, 'b Heap_block.t) Ephemeron.t

let create = Ephemeron.create

let set_key t = function
  | None -> Ephemeron.unset_key t
  | Some v -> Ephemeron.set_key t v
;;

let get_key = Ephemeron.get_key

let set_data t = function
  | None -> Ephemeron.unset_data t
  | Some v -> Ephemeron.set_data t v
;;

let get_data = Ephemeron.get_data
let is_key_some t = Ephemeron.check_key t
let is_key_none t = not (is_key_some t)
let is_data_some t = Ephemeron.check_data t
let is_data_none t = not (is_data_some t)

let sexp_of_t sexp_of_a sexp_of_b t =
  [%sexp_of: a Heap_block.t option * b Heap_block.t option] (get_key t, get_data t)
;;
