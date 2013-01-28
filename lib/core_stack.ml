open Bin_prot.Std
module List = Core_list

exception Empty with sexp

type 'a t = {
  mutable elts : 'a list;
  mutable length : int;
} with bin_io

let invariant t =
  assert (t.length = List.length t.elts);
;;

let create () = { elts = []; length = 0; }

(* We always want to set elts and length at the same time.  Having a function
 * to do so helps us to remember.
 *)
let set t elts length = t.elts <- elts; t.length <- length

let push t x = set t (x :: t.elts) (t.length + 1)

let pop_exn t =
  match t.elts with
  | [] -> raise Empty
  | x :: l -> set t l (t.length - 1); x
;;

let pop t = try Some (pop_exn t) with Empty -> None

let top_exn t =
  match t.elts with
  | [] -> raise Empty
  | x :: _ -> x
;;

let top t = try Some (top_exn t) with Empty -> None

let clear t = set t [] 0

let copy t = { elts = t.elts; length = t.length; }

let length t = t.length

let is_empty t = t.length = 0

let iter t ~f = List.iter t.elts ~f

let fold t ~init ~f = List.fold t.elts ~init ~f

let count t ~f = List.count t.elts ~f

let exists t ~f = List.exists t.elts ~f

let mem ?equal t a = List.mem ?equal t.elts a

let for_all t ~f = List.for_all t.elts ~f

let find t ~f = List.find t.elts ~f

let find_map t ~f = List.find_map t.elts ~f

let to_list t = t.elts

let of_list l = { elts = l; length = List.length l }

let to_array t = Array.of_list t.elts

let sexp_of_t sexp_of_a t = Sexplib.Conv.sexp_of_list sexp_of_a (to_list t)

let t_of_sexp a_of_sexp sexp =
  let elts = Sexplib.Conv.list_of_sexp a_of_sexp sexp in
  { elts = elts; length = List.length elts; }
;;

let until_empty t f =
  let rec loop () = if t.length > 0 then (f (pop_exn t); loop ()) in
  loop ()
;;

TEST_MODULE = struct
  let empty = create ()

  TEST = is_empty empty

  TEST = length empty = 0

  TEST =
    try ignore (top_exn empty); false
    with _ -> true
  ;;

  TEST =
    try ignore (pop_exn empty); false
    with _ -> true
  ;;

  TEST = pop empty = None
  TEST = top empty = None

  let t =
    let t = create () in
    push t 0;
    push t 1;
    push t 2;
    t
  ;;

  TEST = not (is_empty t)
  TEST = length t = 3
  TEST = top t = Some 2
  TEST = top_exn t = 2

  let t' = copy t

  TEST = pop_exn t' = 2
  TEST = pop_exn t' = 1
  TEST = pop_exn t' = 0

  TEST = length t' = 0
  TEST = is_empty t'

  let t' = copy t

  TEST = pop t' = Some 2
  TEST = pop t' = Some 1
  TEST = pop t' = Some 0

  TEST = length t' = 0
  TEST = is_empty t'

  (* test that t was not modified by pops applied to copies *)
  TEST = length t = 3
  TEST = top_exn t = 2

  TEST =
    let n = ref 0 in
    iter t ~f:(fun x -> n := !n + x);
    !n = 3
  ;;

  TEST = fold t ~init:0 ~f:(+) = 3
  TEST = count t ~f:(fun x -> x > 1) = 1
  TEST = mem t 0
  TEST = not (mem t 5)
  TEST = for_all t ~f:(fun x -> x >= 0)
  TEST = not (for_all t ~f:(fun x -> x > 0))
  TEST = find t ~f:(fun x -> x < 2) = Some 1
  TEST = find t ~f:(fun x -> x < 0) = None
  TEST = find_map t ~f:(fun x -> if x < 2 then Some (x + 5) else None) = Some 6
  TEST = find_map t ~f:(fun x -> if x < 0 then Some (x + 5) else None) = None
  TEST = to_list t = [2; 1; 0]
  TEST = to_array t = [|2; 1; 0|]

  TEST = length t = 3
  TEST = top_exn t = 2

  let t' = copy t

  TEST =
    let n = ref 0 in
    until_empty t' (fun x -> n := !n + x);
    !n = 3
  ;;

  TEST = is_empty t'
  TEST = length t' = 0
end
