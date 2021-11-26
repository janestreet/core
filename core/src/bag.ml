open! Import
include Bag_intf
include (Doubly_linked : Doubly_linked.S)

let add = insert_first
let add_unit t v = add t v |> (ignore : _ Elt.t -> unit)
let elts t = fold_elt t ~init:[] ~f:(fun acc elt -> elt :: acc)
let remove_one = remove_first
let choose = first_elt

let until_empty t f =
  let rec loop () =
    Option.iter (remove_one t) ~f:(fun v ->
      f v;
      loop ())
  in
  loop ()
;;
