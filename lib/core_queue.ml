open Bin_prot.Std
open Sexplib.Conv

open Common

module Array = Core_array
module List = Core_list
module Queue = Caml.Queue

exception Empty = Queue.Empty

type 'a t = 'a Queue.t

let create = Queue.create

let enqueue t x = Queue.push x t

let is_empty = Queue.is_empty

let dequeue t = if is_empty t then None else Some (Queue.pop t)

let dequeue_exn = Queue.pop

let peek t = if is_empty t then None else Some (Queue.peek t)

let peek_exn = Queue.peek

let clear = Queue.clear

let copy = Queue.copy

let length = Queue.length

let iter t ~f = Queue.iter f t

let fold t ~init ~f = Queue.fold f init t

let count t ~f = Container.fold_count fold t ~f

let transfer ~src ~dst = Queue.transfer src dst

let concat_map t ~f =
  let res = create () in
  iter t ~f:(fun a ->
    List.iter (f a) ~f:(fun b -> enqueue res b));
  res
;;

let filter_map t ~f =
  let res = create () in
  iter t ~f:(fun a ->
    match f a with
    | None -> ()
    | Some b -> enqueue res b);
  res;
;;

let map t ~f =
  let res = create () in
  iter t ~f:(fun a -> enqueue res (f a));
  res;
;;

let filter_inplace q ~f =
  let q' = create () in
  transfer ~src:q ~dst:q';
  iter q' ~f:(fun x -> if f x then enqueue q x)

let to_list t = List.rev (fold t ~init:[] ~f:(fun acc elem -> elem::acc))

let of_list list =
  let t = create () in
  List.iter list ~f:(fun x -> enqueue t x);
  t

let of_array array =
  let t = create () in
  Array.iter array ~f:(fun x -> enqueue t x);
  t
;;

let to_array t =
  match length t with
  | 0 -> [||]
  | len ->
    let arr = Array.create len (peek_exn t) in
    let i = ref 0 in
    iter t ~f:(fun v ->
      arr.(!i) <- v;
      incr i);
    arr

let find t ~f =
  with_return (fun r ->
    iter t ~f:(fun x -> if f x then r.return (Some x));
    None)
;;

let find_map t ~f =
  with_return (fun r ->
    iter t ~f:(fun x -> match f x with None -> () | Some _ as res -> r.return res);
    None)
;;

let exists t ~f = Option.is_some (find t ~f)
let for_all t ~f = not (exists t ~f:(fun x -> not (f x)))

let mem ?(equal = (=)) t a = exists t ~f:(equal a)

let partial_iter t ~f =
  with_return (fun r ->
    iter t ~f:(fun x ->
      match f x with
      | `Continue -> ()
      | `Stop -> r.return ()))
;;

let t_of_sexp a_of_sexp sexp = of_list (list_of_sexp a_of_sexp sexp)
let sexp_of_t sexp_of_a t = sexp_of_list sexp_of_a (to_list t)

let fold t ~init ~f = Queue.fold f init t

let to_list t = List.rev (fold t ~init:[] ~f:(fun l x -> x::l))

let singleton a =
  let t = create () in
  enqueue t a;
  t
;;

include Bin_prot.Utils.Make_iterable_binable1 (struct

  type 'a t = 'a Queue.t
  type 'a el = 'a
  type 'a acc = 'a t

  let module_name = Some "Queue"

  let length = length

  let iter = iter

  let init _ = create ()

  (* Bin_prot reads the elements in the same order they were written out, as determined by
     [iter].  So, we can ignore the index and just enqueue each element as it is read
     in. *)
  let insert t x _i = enqueue t x; t

  let finish t = t

  let bin_size_el sizer = sizer

  let bin_write_el_ writer = writer

  let bin_read_el_ reader = reader

end)

TEST_MODULE = struct
  let m =
    let module M  = struct
      type 'a u = 'a t with bin_io
      type t = int u with bin_io
    end
    in
    (module M : Binable.S with type t = M.t)
  ;;

  let test list =
    let t = of_list list in
    let bigstring = Binable.to_bigstring m t in
    let list' = to_list (Binable.of_bigstring m bigstring) in
    list = list'
  ;;

  TEST = test []
  TEST = test [ 1 ]
  TEST = test [ 1; 2; 3 ]
  TEST = test (List.init 10_000 ~f:Fn.id)

end
