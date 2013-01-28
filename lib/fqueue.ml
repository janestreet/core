(** Simple implementation of a polymorphic functional queue *)

(** Invariants:

      - iff queue is not empty, outlist is not empty
      - iff queue has more than 1 element, then inlist is not empty
      - queue.length = List.length queue.outlist + List.length queue.inlist
*)

open Std_internal

exception Empty with sexp

type 'a t = { inlist : 'a list; outlist : 'a list; length : int } with bin_io, sexp

let test_invariants queue =
  let n_out = List.length queue.outlist in
  let n_in = List.length queue.inlist in
  assert (queue.length = n_out + n_in);
  assert (queue.length = 0 || n_out <> 0);
  assert (queue.length <= 1 || n_in <> 0)

let empty = { inlist = []; outlist = []; length = 0 }

let enqueue queue el =
  let inlist, outlist =
    if queue.length = 0 then [], [el]
    else el :: queue.inlist, queue.outlist
  in
  { inlist = inlist; outlist = outlist; length = queue.length + 1 }

(** enqueue el on the top of the queue, effectively making it
    the least recently enqueued element *)
let enqueue_top queue el =
  let inlist, outlist =
    if queue.inlist = [] then List.rev queue.outlist, [el]
    else queue.inlist, el :: queue.outlist
  in
  { inlist = inlist; outlist = outlist; length = queue.length + 1 }

(** returns bottom (most-recently enqueued) item  *)
let bot_exn queue =
  match queue.inlist, queue.outlist with
  | [], [x] | x :: _, _ -> x
  | [], [] -> raise Empty
  | [], _ :: _ :: _ ->
      raise (Bug "Fqueue.bot_exn: empty inlist and outlist with len > 1")

let bot queue = try Some (bot_exn queue) with Empty -> None

(** returns top (least-recently enqueued) item  *)
let top_exn queue =
  match queue.outlist with
  | x :: _ -> x
  | [] -> raise Empty

let top queue = try Some (top_exn queue) with Empty -> None

(** returns top of queue and queue with top removed  *)
let dequeue_exn queue =
  let x, inlist, outlist =
    match queue.inlist, queue.outlist with
    | [_] as inlist, [x] -> x, [], inlist
    | y :: ytl, [x] -> x, [y], List.rev ytl
    | inlist, x :: xtl -> x, inlist, xtl
    | [], [] -> raise Empty
    | _ :: _, [] -> raise (Bug "Fqueue.dequeue_exn: outlist empty, inlist not")
  in
  x, { inlist = inlist; outlist = outlist; length = queue.length - 1 }

let dequeue queue = try Some (dequeue_exn queue) with Empty -> None

(** returns queue with top removed *)
let discard_exn queue = snd (dequeue_exn queue)

let to_list queue = List.append queue.outlist (List.rev queue.inlist)

let sexp_of_t sexp_of_a q = Sexplib.Conv.sexp_of_list sexp_of_a (to_list q)

let length queue = queue.length

let is_empty queue = queue.length = 0
