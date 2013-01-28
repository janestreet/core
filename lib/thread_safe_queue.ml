(* This module exploits the fact that OCaml does not perform context-switches
   under certain conditions.  It can therefore avoid using mutexes.

   Given the semantics of the current OCaml runtime (and for the foreseeable
   future), code sections documented as atomic below will never contain a
   context-switch.  The deciding criterion is whether they contain allocations
   or calls to external/builtin functions.  If there is none, a context-switch
   cannot happen.  Assignments without allocations, field access,
   pattern-matching, etc., do not trigger context-switches.

   Code reviewers should therefore make sure that the sections documented
   as atomic below do not violate the above assumptions.  It is prudent to
   disassemble the .o file (using objdump -dr) and examine it.
*)

module List = Core_list

type 'a queue_end = 'a z option ref
and 'a z =
  { value : 'a;
    next : 'a queue_end;
  }

let queue_end_to_list queue_end =
  let rec loop queue_end ac =
    match !queue_end with
    | None -> List.rev ac
    | Some z -> loop z.next (z.value :: ac)
  in
  loop queue_end []
;;


type 'a t =
  { mutable front : 'a queue_end;
    mutable back : 'a queue_end;
    mutable length : int;
  } with fields

let to_list t = queue_end_to_list t.front

let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (to_list t)

let create () =
  let queue_end = ref None in
  { front = queue_end; back = queue_end; length = 0 }

let enqueue t a =
  let next = ref None in
  let el = Some { value = a; next = next } in
  (* BEGIN ATOMIC SECTION *)
  t.length <- t.length + 1;
  t.back := el;
  t.back <- next;
  (* END ATOMIC SECTION *)
;;

let dequeue t =
  (* BEGIN ATOMIC SECTION *)
  match !(t.front) with
  | None -> None
  | Some el ->
    t.front <- el.next;
    t.length <- t.length - 1;
    (* END ATOMIC SECTION *)
    Some el.value
;;

let rec dequeue_until_empty t f =
  match dequeue t with
  | None -> ()
  | Some x -> (
    f x;
    dequeue_until_empty t f
  )
;;

let create' () =
  let t = create () in
  ((fun () -> dequeue t), fun a -> enqueue t a)
;;
