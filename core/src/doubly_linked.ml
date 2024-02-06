open! Import
include Doubly_linked_intf

(* INVARIANT: This exception is raised if a list is mutated during a pending iteration.

   This invariant is guaranteed by the Header and Elt modules in conjunction.  All
   downstream code in this module need not be concerned with this invariant. *)
exception Attempt_to_mutate_list_during_iteration

let phys_equal = ( == )

module Header : sig
  type t

  val create : unit -> t
  val length : t -> int
  val equal : t -> t -> bool
  val incr_length : by:int -> t -> unit
  val check_no_pending_iterations : t -> unit

  (* Unfortunate, but by specializing [with_iteration] for different arities, a large
     amount of allocation during folds and iterations is avoided.

     The original type of [with_iteration] was
     [val with_iteration : t -> (unit -> 'a) -> 'a]

     The difference between
     {[
       let x = e in
       let f () = g x in
       f ()
     ]}
     and
     {[
       let x = e in
       let f x = g x in
       f x
     ]}
     is that in the first case the closure for [f] contains a pointer to [x],
     and in the second case it doesn't. A closure without pointers to enclosing
     environment is implemented as a naked function pointer, so we don't
     allocate at all.

     For the same reason we make sure not to call [Result.try_with (fun () -> ...)]
     inside [with_iteration] and do an explicit match statement instead. *)

  val with_iteration_2l : t -> 'a -> 'b -> ('a -> 'b -> 'c) -> 'c
  val with_iteration_3l : t -> 'a -> 'b -> 'c -> ('a -> 'b -> 'c -> 'd) -> 'd
  val with_iteration_4 : t -> 'a -> 'b -> 'c -> 'd -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'e
  val with_iteration_4l : t -> 'a -> 'b -> 'c -> 'd -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'e
  val merge : t -> t -> [ `Same_already | `Merged ]
end = struct
  type s =
    { mutable length : int
    ; mutable pending_iterations : int
    }

  type t = s Union_find.t

  let create () = Union_find.create { length = 1; pending_iterations = 0 }
  let equal (t1 : t) t2 = Union_find.same_class t1 t2
  let length t = (Union_find.get t).length

  let union_find_get__check_no_pending_iterations t =
    let s = Union_find.get t in
    if s.pending_iterations > 0 then raise Attempt_to_mutate_list_during_iteration else s
  ;;

  let check_no_pending_iterations t =
    ignore (union_find_get__check_no_pending_iterations t : s)
  ;;

  let incr_length ~by:n t =
    let s = union_find_get__check_no_pending_iterations t in
    s.length <- s.length + n
  ;;

  (* Care is taken not to allocate in [with_iteration_*], since it is called every second
     by [every_second] in [writer0.ml] *)

  let incr_pending_iters s = s.pending_iterations <- s.pending_iterations + 1
  let decr_pending_iters s = s.pending_iterations <- s.pending_iterations - 1

  let with_iteration_2l t a b f =
    let s = Union_find.get t in
    incr_pending_iters s;
    match f a b with
    | exception exn ->
      decr_pending_iters s;
      raise exn
    | r ->
      decr_pending_iters s;
      r
  ;;

  let with_iteration_3l t a b c f =
    let s = Union_find.get t in
    incr_pending_iters s;
    match f a b c with
    | exception exn ->
      decr_pending_iters s;
      raise exn
    | r ->
      decr_pending_iters s;
      r
  ;;

  let with_iteration_4 t a b c d f =
    let s = Union_find.get t in
    incr_pending_iters s;
    match f a b c d with
    | exception exn ->
      decr_pending_iters s;
      raise exn
    | r ->
      decr_pending_iters s;
      r
  ;;

  let with_iteration_4l t a b c d f =
    let s = Union_find.get t in
    incr_pending_iters s;
    match f a b c d with
    | exception exn ->
      decr_pending_iters s;
      raise exn
    | r ->
      decr_pending_iters s;
      r
  ;;

  let merge (t1 : t) t2 =
    if Union_find.same_class t1 t2
    then `Same_already
    else (
      let n1 = (union_find_get__check_no_pending_iterations t1).length in
      let n2 = (union_find_get__check_no_pending_iterations t2).length in
      with_iteration_4 t1 t1 t2 n1 n2 (fun t1 t2 n1 n2 ->
        with_iteration_4 t2 t1 t2 n1 n2 (fun t1 t2 n1 n2 ->
          Union_find.union t1 t2;
          Union_find.set t1 { length = n1 + n2; pending_iterations = 0 }));
      `Merged)
  ;;
end

module Elt : sig
  type 'a t [@@deriving sexp_of]

  val header : 'a t -> Header.t
  val equal : 'a t -> 'a t -> bool
  val create : 'a -> 'a t
  val value : 'a t -> 'a
  val set : 'a t -> 'a -> unit
  val unlink : 'a t -> unit
  val split_or_splice_before : 'a t -> 'a t -> unit
  val split_or_splice_after : 'a t -> 'a t -> unit
  val insert_after : 'a t -> 'a -> 'a t
  val insert_before : 'a t -> 'a -> 'a t
  val unlink_before : 'a t -> 'a t
  val next : 'a t -> 'a t
  val prev : 'a t -> 'a t
end = struct
  type 'a t =
    { mutable value : 'a
    ; mutable prev : 'a t
    ; mutable next : 'a t
    ; mutable header : Header.t
    }

  let equal = phys_equal
  let next t = t.next
  let prev t = t.prev
  let header t = t.header

  let create_aux v header =
    let rec t = { value = v; prev = t; next = t; header } in
    t
  ;;

  let is_singleton t = equal t t.prev
  let sexp_of_t sexp_of_a t = sexp_of_a t.value
  let create v = create_aux v (Header.create ())
  let value t = t.value
  let set t v = t.value <- v

  (*
     [split_or_splice] is sufficient as the lone primitive for
     accomplishing all pointer updates on cyclic loops of list nodes.
     It takes two "gaps" between adjacent linked list nodes.  If the gaps
     point into the same list, the result is that it will be split into
     two lists afterwards.  If the gaps point into different lists, the
     result is that they will be spliced together into one list afterwards.

     {v
       Before                      After
           -----+        +-----         -----+               +-----
              A |  <-->  | B               A |  <---   --->  | B
           -----+        +-----         -----+      \ /      +-----
                                                     X
           -----+        +-----         -----+      / \      +-----
              C |  <-->  | D               C |  <---   --->  | D
           -----+        +-----         -----+               +-----
     v} *)

  let unsafe_split_or_splice ~prev1:a ~next1:b ~prev2:c ~next2:d =
    a.next <- d;
    d.prev <- a;
    c.next <- b;
    b.prev <- c
  ;;

  let unsafe_split_or_splice_after t1 t2 =
    unsafe_split_or_splice
      ~next1:t1.next
      ~prev1:t1.next.prev
      ~next2:t2.next
      ~prev2:t2.next.prev
  ;;

  let unsafe_split_or_splice_before t1 t2 =
    unsafe_split_or_splice
      ~prev1:t1.prev
      ~next1:t1.prev.next
      ~prev2:t2.prev
      ~next2:t2.prev.next
  ;;

  let check_two_nodes_no_pending_iterations t1 t2 =
    Header.check_no_pending_iterations t1.header;
    if not (Header.equal t1.header t2.header)
    then Header.check_no_pending_iterations t2.header
  ;;

  (* We redefine safe versions for export *)
  let split_or_splice_after t1 t2 =
    check_two_nodes_no_pending_iterations t1 t2;
    unsafe_split_or_splice_after t1 t2
  ;;

  let split_or_splice_before t1 t2 =
    check_two_nodes_no_pending_iterations t1 t2;
    unsafe_split_or_splice_before t1 t2
  ;;

  let insert_before t v =
    Header.incr_length t.header ~by:1;
    let node = create_aux v t.header in
    unsafe_split_or_splice_before t node;
    node
  ;;

  let insert_after t v =
    Header.incr_length t.header ~by:1;
    let node = create_aux v t.header in
    unsafe_split_or_splice_after t node;
    node
  ;;

  let dummy_header = Header.create ()

  let unlink_before t =
    let node = t.prev in
    if is_singleton node
    then node
    else (
      Header.incr_length t.header ~by:(-1);
      unsafe_split_or_splice_before t node;
      node.header <- dummy_header;
      node)
  ;;

  let unlink_after t =
    let node = t.next in
    if is_singleton node
    then node
    else (
      Header.incr_length t.header ~by:(-1);
      unsafe_split_or_splice_after t node;
      node.header <- dummy_header;
      node)
  ;;

  let unlink t = ignore (unlink_after t.prev : _ t)
end

type 'a t = 'a Elt.t option ref

let invariant invariant_a t =
  match !t with
  | None -> ()
  | Some head ->
    let header = Elt.header head in
    let rec loop n elt =
      let next_elt = Elt.next elt in
      let prev_elt = Elt.prev elt in
      assert (Elt.equal elt (Elt.prev next_elt));
      assert (Elt.equal elt (Elt.next prev_elt));
      assert (Header.equal (Elt.header elt) header);
      invariant_a (Elt.value elt);
      if Elt.equal next_elt head then n else loop (n + 1) next_elt
    in
    let len = loop 1 head in
    assert (len = Header.length header)
;;

let create (type a) () : a t = ref None
let equal (t : _ t) t' = phys_equal t t'

let of_list = function
  | [] -> create ()
  | x :: xs ->
    let first = Elt.create x in
    let _last = List.fold xs ~init:first ~f:(fun a b -> Elt.insert_after a b) in
    ref (Some first)
;;

let of_array = function
  | [||] -> create ()
  | arr ->
    let first = Elt.create arr.(0) in
    let rec loop arr elt i =
      if i < Array.length arr then loop arr (Elt.insert_after elt arr.(i)) (i + 1)
    in
    loop arr first 1;
    ref (Some first)
;;

let map t ~f =
  match !t with
  | None -> create ()
  | Some first ->
    let new_first = Elt.create (f (Elt.value first)) in
    Header.with_iteration_3l
      (Elt.header first)
      f
      new_first
      first
      (fun f new_first first ->
      let rec loop f acc first elt =
        let acc = Elt.insert_after acc (f (Elt.value elt)) in
        let next = Elt.next elt in
        if not (phys_equal next first) then loop f acc first next
      in
      (* unroll and skip first elt *)
      let next = Elt.next first in
      if not (phys_equal next first) then loop f new_first first next);
    ref (Some new_first)
;;

let mapi t ~f =
  match !t with
  | None -> create ()
  | Some first ->
    let new_first = Elt.create (f 0 (Elt.value first)) in
    Header.with_iteration_3l
      (Elt.header first)
      f
      new_first
      first
      (fun f new_first first ->
      let rec loop f i acc first elt =
        let acc = Elt.insert_after acc (f i (Elt.value elt)) in
        let next = Elt.next elt in
        if not (phys_equal next first) then loop f (i + 1) acc first next
      in
      (* unroll and skip first elt *)
      let next = Elt.next first in
      if not (phys_equal next first) then loop f 1 new_first first next);
    ref (Some new_first)
;;

let fold_elt t ~init ~f =
  match !t with
  | None -> init
  | Some first ->
    Header.with_iteration_3l (Elt.header first) f init first (fun f init first ->
      let rec loop f acc first elt =
        let acc = f acc elt in
        let next = Elt.next elt in
        if phys_equal next first then acc else loop f acc first next
      in
      loop f init first first)
;;

let foldi_elt t ~init ~f =
  match !t with
  | None -> init
  | Some first ->
    Header.with_iteration_3l (Elt.header first) f init first (fun f init first ->
      let rec loop f i acc first elt =
        let acc = f i acc elt in
        let next = Elt.next elt in
        if phys_equal next first then acc else loop f (i + 1) acc first next
      in
      loop f 0 init first first)
;;

let fold_elt_1 t ~init ~f a =
  match !t with
  | None -> init
  | Some first ->
    Header.with_iteration_4l (Elt.header first) f a init first (fun f a init first ->
      let rec loop f a acc first elt =
        let acc = f a acc elt in
        let next = Elt.next elt in
        if phys_equal next first then acc else loop f a acc first next
      in
      loop f a init first first)
;;

let foldi_elt_1 t ~init ~f a =
  match !t with
  | None -> init
  | Some first ->
    Header.with_iteration_4 (Elt.header first) f a init first (fun f a init first ->
      let rec loop f i a acc first elt =
        let acc = f i a acc elt in
        let next = Elt.next elt in
        if phys_equal next first then acc else loop f (i + 1) a acc first next
      in
      loop f 0 a init first first)
;;

let iter_elt t ~f = fold_elt_1 t ~init:() ~f:(fun f () elt -> f elt) f
let iteri_elt t ~f = foldi_elt t ~init:() ~f:(fun i () elt -> f i elt)

open With_return

let find_elt t ~f =
  with_return (fun r ->
    fold_elt_1 t f ~init:() ~f:(fun f () elt ->
      if f (Elt.value elt) then r.return (Some elt));
    None)
;;

let findi_elt t ~f =
  with_return (fun r ->
    foldi_elt_1 t f ~init:() ~f:(fun i f () elt ->
      if f i (Elt.value elt) then r.return (Some (i, elt)));
    None)
;;

(* this function is lambda lifted for performance, to make direct recursive calls instead
   of calls through its closure. It also avoids the initial closure allocation. *)
let rec iter_loop first f elt =
  f (Elt.value elt);
  let next = Elt.next elt in
  if not (phys_equal next first) then iter_loop first f next
;;

let iter t ~f =
  match !t with
  | None -> ()
  | Some first ->
    Header.with_iteration_2l (Elt.header first) first f (fun first f ->
      iter_loop first f first)
;;

let length t =
  match !t with
  | None -> 0
  | Some first -> Header.length (Elt.header first)
;;

let rec iteri_loop first f i elt =
  f i (Elt.value elt);
  let next = Elt.next elt in
  if not (phys_equal next first) then iteri_loop first f (i + 1) next
;;

let iteri t ~f =
  match !t with
  | None -> ()
  | Some first ->
    Header.with_iteration_2l (Elt.header first) first f (fun first f ->
      iteri_loop first f 0 first)
;;

let foldi t ~init ~f =
  foldi_elt_1 t ~init f ~f:(fun i f acc elt -> f i acc (Elt.value elt))
;;

module C = Container.Make (struct
  type nonrec 'a t = 'a t

  let fold t ~init ~f =
    let r = fold_elt_1 t ~init f ~f:(fun f acc elt -> f acc (Elt.value elt)) in
    r
  ;;

  let iter = `Custom iter
  let length = `Custom length
end)

let count = C.count
let sum = C.sum
let exists = C.exists
let find = C.find
let find_map = C.find_map
let fold = C.fold
let for_all = C.for_all
let mem = C.mem
let to_array = C.to_array
let min_elt = C.min_elt
let max_elt = C.max_elt
let fold_result = C.fold_result
let fold_until = C.fold_until

let unchecked_iter t ~f =
  match !t with
  | None -> ()
  | Some first ->
    let rec loop t f elt =
      f (Elt.value elt);
      let next = Elt.next elt in
      match !t with
      (* the first element of the bag may have been changed by [f] *)
      | None -> ()
      | Some first -> if not (phys_equal first next) then loop t f next
    in
    loop t f first
;;

let is_empty t = Option.is_none !t

(* more efficient than what Container.Make returns *)

let fold_right t ~init ~f =
  match !t with
  | None -> init
  | Some first ->
    Header.with_iteration_3l (Elt.header first) f init first (fun f init first ->
      let rec loop f acc elt =
        let prev = Elt.prev elt in
        let acc = f (Elt.value prev) acc in
        if phys_equal prev first then acc else loop f acc prev
      in
      loop f init first)
;;

let fold_right_elt t ~init ~f =
  match !t with
  | None -> init
  | Some first ->
    Header.with_iteration_3l (Elt.header first) f init first (fun f init first ->
      let rec loop f acc elt =
        let prev = Elt.prev elt in
        let acc = f prev acc in
        if phys_equal prev first then acc else loop f acc prev
      in
      loop f init first)
;;

let to_list t = fold_right t ~init:[] ~f:(fun x tl -> x :: tl)
let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (to_list t)
let t_of_sexp a_of_sexp s = of_list (List.t_of_sexp a_of_sexp s)

let t_sexp_grammar elt_grammar =
  Sexplib.Sexp_grammar.coerce (Base.List.t_sexp_grammar elt_grammar)
;;

let copy t = of_list (to_list t)
let clear t = t := None

let compare compare_elt t1 t2 =
  match !t1, !t2 with
  | None, None -> 0
  | None, _ -> -1
  | _, None -> 1
  | Some f1, Some f2 ->
    Header.with_iteration_3l (Elt.header f1) compare_elt f1 f2 (fun compare_elt f1 f2 ->
      Header.with_iteration_3l (Elt.header f2) compare_elt f1 f2 (fun compare_elt f1 f2 ->
        let rec loop compare_elt elt1 f1 elt2 f2 =
          let compare_result = compare_elt (Elt.value elt1) (Elt.value elt2) in
          if compare_result <> 0
          then compare_result
          else (
            let next1 = Elt.next elt1 in
            let next2 = Elt.next elt2 in
            match phys_equal next1 f1, phys_equal next2 f2 with
            | true, true -> 0
            | true, false -> -1
            | false, true -> 1
            | false, false -> loop compare_elt next1 f1 next2 f2)
        in
        loop compare_elt f1 f1 f2 f2))
;;

exception Transfer_src_and_dst_are_same_list

let transfer ~src ~dst =
  if phys_equal src dst then raise Transfer_src_and_dst_are_same_list;
  match !src with
  | None -> ()
  | Some src_head ->
    (match !dst with
     | None ->
       dst := Some src_head;
       clear src
     | Some dst_head ->
       (match Header.merge (Elt.header src_head) (Elt.header dst_head) with
        | `Same_already -> raise Transfer_src_and_dst_are_same_list
        | `Merged ->
          Elt.split_or_splice_before dst_head src_head;
          clear src))
;;

let map_inplace t ~f = iter_elt t ~f:(fun elt -> Elt.set elt (f (Elt.value elt)))
let mapi_inplace t ~f = iteri_elt t ~f:(fun i elt -> Elt.set elt (f i (Elt.value elt)))

let remove_list t to_remove =
  List.iter to_remove ~f:(fun elt ->
    (match !t with
     | None -> ()
     | Some head ->
       if Elt.equal head elt
       then (
         let next_elt = Elt.next elt in
         t := if Elt.equal head next_elt then None else Some next_elt));
    Elt.unlink elt)
;;

let filter_inplace t ~f =
  let to_remove =
    List.rev
      (fold_elt t ~init:[] ~f:(fun elts elt ->
         if f (Elt.value elt) then elts else elt :: elts))
  in
  remove_list t to_remove
;;

let filteri_inplace t ~f =
  let to_remove =
    List.rev
      (foldi_elt t ~init:[] ~f:(fun i elts elt ->
         if f i (Elt.value elt) then elts else elt :: elts))
  in
  remove_list t to_remove
;;

let filter_map_inplace t ~f =
  let to_remove =
    List.rev
      (fold_elt t ~init:[] ~f:(fun elts elt ->
         match f (Elt.value elt) with
         | None -> elt :: elts
         | Some value ->
           Elt.set elt value;
           elts))
  in
  remove_list t to_remove
;;

let filter_mapi_inplace t ~f =
  let to_remove =
    List.rev
      (foldi_elt t ~init:[] ~f:(fun i elts elt ->
         match f i (Elt.value elt) with
         | None -> elt :: elts
         | Some value ->
           Elt.set elt value;
           elts))
  in
  remove_list t to_remove
;;

exception Elt_does_not_belong_to_list

let first_elt t = !t
let last_elt t = Option.map ~f:Elt.prev !t
let first t = Option.map ~f:Elt.value (first_elt t)
let last t = Option.map ~f:Elt.value (last_elt t)
let first_exn (t : 'a t) = Option.value_exn !t |> Elt.value
let last_exn (t : 'a t) = Option.value_exn !t |> Elt.prev |> Elt.value

let is_first t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Header.equal (Elt.header first) (Elt.header elt)
    then Elt.equal elt first
    else raise Elt_does_not_belong_to_list
;;

let is_last t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Header.equal (Elt.header first) (Elt.header elt)
    then (
      let last = Elt.prev first in
      Elt.equal elt last)
    else raise Elt_does_not_belong_to_list
;;

let mem_elt t elt =
  match !t with
  | None -> false
  | Some first -> Header.equal (Elt.header first) (Elt.header elt)
;;

let prev t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Elt.equal elt first
    then None
    else if Header.equal (Elt.header first) (Elt.header elt)
    then Some (Elt.prev elt)
    else raise Elt_does_not_belong_to_list
;;

let next t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    let last = Elt.prev first in
    if Elt.equal elt last
    then None
    else if Header.equal (Elt.header first) (Elt.header elt)
    then Some (Elt.next elt)
    else raise Elt_does_not_belong_to_list
;;

let insert_after t elt v =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Header.equal (Elt.header first) (Elt.header elt)
    then Elt.insert_after elt v
    else raise Elt_does_not_belong_to_list
;;

let insert_before t elt v =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Elt.equal elt first
    then (
      let new_elt = Elt.insert_before first v in
      t := Some new_elt;
      new_elt)
    else if Header.equal (Elt.header first) (Elt.header elt)
    then Elt.insert_before elt v
    else raise Elt_does_not_belong_to_list
;;

let insert_empty t v =
  let new_elt = Elt.create v in
  t := Some new_elt;
  new_elt
;;

let insert_last t v =
  match !t with
  | None -> insert_empty t v
  | Some first -> Elt.insert_before first v
;;

let insert_first t v =
  match !t with
  | None -> insert_empty t v
  | Some first ->
    let new_elt = Elt.insert_before first v in
    t := Some new_elt;
    new_elt
;;

let remove_last t =
  match !t with
  | None -> None
  | Some first ->
    let last = Elt.unlink_before first in
    if Elt.equal first last then t := None;
    Some (Elt.value last)
;;

let remove_first t =
  match !t with
  | None -> None
  | Some first ->
    let second = Elt.next first in
    Elt.unlink first;
    t := if Elt.equal first second then None else Some second;
    Some (Elt.value first)
;;

let remove t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    if Elt.equal elt first
    then ignore (remove_first t : _ option)
    else if Header.equal (Elt.header first) (Elt.header elt)
    then Elt.unlink elt
    else raise Elt_does_not_belong_to_list
;;

let filter t ~f =
  let new_t = create () in
  (match !t with
   | None -> ()
   | Some first ->
     Header.with_iteration_3l (Elt.header first) f new_t first (fun f new_t first ->
       let rec loop f new_t first elt =
         if f (Elt.value elt)
         then insert_last new_t (Elt.value elt) |> (ignore : _ Elt.t -> unit);
         let next = Elt.next elt in
         if not (phys_equal next first) then loop f new_t first next
       in
       loop f new_t first first));
  new_t
;;

let filteri t ~f =
  let new_t = create () in
  (match !t with
   | None -> ()
   | Some first ->
     Header.with_iteration_3l (Elt.header first) f new_t first (fun f new_t first ->
       let rec loop f i new_t first elt =
         if f i (Elt.value elt)
         then insert_last new_t (Elt.value elt) |> (ignore : _ Elt.t -> unit);
         let next = Elt.next elt in
         if not (phys_equal next first) then loop f (i + 1) new_t first next
       in
       loop f 0 new_t first first));
  new_t
;;

let filter_map t ~f =
  let new_t = create () in
  (match !t with
   | None -> ()
   | Some first ->
     Header.with_iteration_3l (Elt.header first) f new_t first (fun f new_t first ->
       let rec loop f new_t first elt =
         (match f (Elt.value elt) with
          | None -> ()
          | Some value -> insert_last new_t value |> (ignore : _ Elt.t -> unit));
         let next = Elt.next elt in
         if not (phys_equal next first) then loop f new_t first next
       in
       loop f new_t first first));
  new_t
;;

let filter_mapi t ~f =
  let new_t = create () in
  (match !t with
   | None -> ()
   | Some first ->
     Header.with_iteration_3l (Elt.header first) f new_t first (fun f new_t first ->
       let rec loop f i new_t first elt =
         (match f i (Elt.value elt) with
          | None -> ()
          | Some value -> insert_last new_t value |> (ignore : _ Elt.t -> unit));
         let next = Elt.next elt in
         if not (phys_equal next first) then loop f (i + 1) new_t first next
       in
       loop f 0 new_t first first));
  new_t
;;

let partition_tf t ~f =
  let t1 = create () in
  let t2 = create () in
  (match !t with
   | None -> ()
   | Some first ->
     Header.with_iteration_4 (Elt.header first) f t1 t2 first (fun f t1 t2 first ->
       let rec loop f t1 t2 first elt =
         insert_last (if f (Elt.value elt) then t1 else t2) (Elt.value elt)
         |> (ignore : _ Elt.t -> unit);
         let next = Elt.next elt in
         if not (phys_equal next first) then loop f t1 t2 first next
       in
       loop f t1 t2 first first));
  t1, t2
;;

let partitioni_tf t ~f =
  let t1 = create () in
  let t2 = create () in
  (match !t with
   | None -> ()
   | Some first ->
     Header.with_iteration_4 (Elt.header first) f t1 t2 first (fun f t1 t2 first ->
       let rec loop f i t1 t2 first elt =
         insert_last (if f i (Elt.value elt) then t1 else t2) (Elt.value elt)
         |> (ignore : _ Elt.t -> unit);
         let next = Elt.next elt in
         if not (phys_equal next first) then loop f (i + 1) t1 t2 first next
       in
       loop f 0 t1 t2 first first));
  t1, t2
;;

let partition_map t ~f =
  let t1 = create () in
  let t2 = create () in
  (match !t with
   | None -> ()
   | Some first ->
     Header.with_iteration_4 (Elt.header first) f t1 t2 first (fun f t1 t2 first ->
       let rec loop f t1 t2 first elt =
         (match (f (Elt.value elt) : (_, _) Either.t) with
          | First value -> insert_last t1 value |> (ignore : _ Elt.t -> unit)
          | Second value -> insert_last t2 value |> (ignore : _ Elt.t -> unit));
         let next = Elt.next elt in
         if not (phys_equal next first) then loop f t1 t2 first next
       in
       loop f t1 t2 first first));
  t1, t2
;;

let partition_mapi t ~f =
  let t1 = create () in
  let t2 = create () in
  (match !t with
   | None -> ()
   | Some first ->
     Header.with_iteration_4 (Elt.header first) f t1 t2 first (fun f t1 t2 first ->
       let rec loop f i t1 t2 first elt =
         (match (f i (Elt.value elt) : (_, _) Either.t) with
          | First value -> insert_last t1 value |> (ignore : _ Elt.t -> unit)
          | Second value -> insert_last t2 value |> (ignore : _ Elt.t -> unit));
         let next = Elt.next elt in
         if not (phys_equal next first) then loop f (i + 1) t1 t2 first next
       in
       loop f 0 t1 t2 first first));
  t1, t2
;;

exception Invalid_move__elt_equals_anchor

let move_before t elt ~anchor =
  if Elt.equal anchor elt then raise Invalid_move__elt_equals_anchor;
  if Header.equal (Elt.header anchor) (Elt.header elt)
  then (
    match !t with
    | None -> raise Elt_does_not_belong_to_list
    | Some first ->
      if Header.equal (Elt.header first) (Elt.header elt)
      then (
        (* unlink [elt] *)
        let after_elt = Elt.next elt in
        Elt.split_or_splice_before elt after_elt;
        let first =
          if Elt.equal first elt
          then (
            t := Some after_elt;
            after_elt)
          else first
        in
        (* splice [elt] in before [anchor] *)
        Elt.split_or_splice_before anchor elt;
        if Elt.equal first anchor then t := Some elt)
      else raise Elt_does_not_belong_to_list)
  else raise Elt_does_not_belong_to_list
;;

let move_to_front t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first -> if not (Elt.equal elt first) then move_before t elt ~anchor:first
;;

let move_after t elt ~anchor =
  if Elt.equal anchor elt then raise Invalid_move__elt_equals_anchor;
  if Header.equal (Elt.header anchor) (Elt.header elt)
  then (
    match !t with
    | None -> raise Elt_does_not_belong_to_list
    | Some first ->
      if Header.equal (Elt.header first) (Elt.header elt)
      then (
        (* unlink [elt] *)
        let after_elt = Elt.next elt in
        Elt.split_or_splice_before elt after_elt;
        if Elt.equal first elt then t := Some after_elt;
        (* splice [elt] in after [anchor] *)
        Elt.split_or_splice_after anchor elt)
      else raise Elt_does_not_belong_to_list)
  else raise Elt_does_not_belong_to_list
;;

let move_to_back t elt =
  match !t with
  | None -> raise Elt_does_not_belong_to_list
  | Some first ->
    let last = Elt.prev first in
    if not (Elt.equal elt last) then move_after t elt ~anchor:last
;;

let to_sequence t = to_list t |> Sequence.of_list
