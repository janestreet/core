module Array = StdLabels.Array
open Sexplib.Std
open Bin_prot.Std

let invalid_argf = Core_printf.invalid_argf

type 'a t = 'a array with sexp, bin_io

let max_length = Caml.Sys.max_array_length

(* Standard functions *)
let append = Array.append
let blit = Array.blit
let concat = Array.concat
let copy = Array.copy
let create_matrix = Array.create_matrix
let fill = Array.fill
let fold_right = Array.fold_right
let init = Array.init
let iteri = Array.iteri
let make_matrix = Array.make_matrix
let map = Array.map
let mapi = Array.mapi
let of_list = Array.of_list
(* Note that Ocaml's stable_sort and fast_sort are the same. Regular sort is unstable and
   slower, but uses constant heap space. *)
let sort = Array.sort
let stable_sort = Array.stable_sort
let sub = Array.sub
let to_list = Array.to_list

external create : int -> 'a -> 'a array = "caml_make_vect"

let create i x =
  try create i x
  with Invalid_argument _ ->
    invalid_argf "Array.create %d: invalid length" i ()
;;

external get : 'a array -> int -> 'a = "%array_safe_get"
external set : 'a array -> int -> 'a -> unit = "%array_safe_set"
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external length : 'a array -> int = "%array_length"

let max_length = Sys.max_array_length

let to_array t = t

let is_empty t = length t = 0

let is_sorted t ~cmp =
  let rec loop i =
    if i < 1 then
      true
    else
      cmp t.(i - 1) t.(i) <= 0 && loop (i - 1)
  in
  loop (length t - 1)

TEST = is_sorted [||] ~cmp:compare
TEST = is_sorted [|0|] ~cmp:compare
TEST = is_sorted [|0;1;2;2;4|] ~cmp:compare
TEST = not (is_sorted [|0;1;2;3;2|] ~cmp:compare)

let fold t ~init ~f = Array.fold_left t ~init ~f

let count t ~f = Container.fold_count fold t ~f

let foldi t ~init ~f =
  let rec loop i ac =
    if i = length t then
      ac
    else loop (i + 1) (f i ac t.(i))
  in
  loop 0 init
;;

TEST = foldi [||] ~init:13 ~f:(fun _ _ _ -> failwith "bad") = 13
TEST = foldi [| 13 |] ~init:17 ~f:(fun i ac x -> ac + i + x) = 30
TEST = foldi [| 13; 17 |] ~init:19 ~f:(fun i ac x -> ac + i + x) = 50

let iter t ~f = Array.iter t ~f

let concat_map t ~f = concat (to_list (map ~f t))

(** [normalize array index] returns a new index into the array such that if index is less
    than zero, the returned index will "wrap around" -- i.e. array.(normalize array (-1))
   returns the last element of the array. *)
let normalize t i =
  Ordered_collection_common.normalize ~length_fun:length t i

(** [slice array start stop] returns a fresh array including elements [array.(start)]
    through [array.(stop-1)] with the small tweak that the start and stop positions are
    normalized and a stop index of 0 means the same thing a stop index of
    [Array.length array].  In summary, it's like the slicing in Python or Matlab. *)
let slice t start stop =
  Ordered_collection_common.slice ~length_fun:length ~sub_fun:sub
    t start stop

(** [nget array index] "normalizes" the index to {!Array.get} -- see normalize *)
let nget t i =
  t.(normalize t i)

(** [nset array index value] "normalizes" the index to {!Array.set} -- see normalize *)
let nset t i v =
  t.(normalize t i) <- v

let swap = Array_permute.swap;;

(** reverses an array in place. *)
let rev_inplace t =
  let i = ref 0 in
  let j = ref (length t - 1) in
  while !i < !j; do
    swap t !i !j;
    incr i;
    decr j;
  done
;;

let of_list_rev l =
  let t = of_list l in
  rev_inplace t;
  t

(* [list_length] and [of_list_rev_map] are based on functions from the
   OCaml distribution. *)

(* Cannot use List.length here because the List module depends on Array. *)
let rec list_length accu = function
  | [] -> accu
  | _h::t -> list_length (succ accu) t

let of_list_map xs ~f =
  match xs with
  | [] -> [||]
  | hd::tl as l ->
      let a = create (list_length 0 l) (f hd) in
      let rec fill i = function
        | [] -> a
        | hd::tl -> unsafe_set a i (f hd); fill (i+1) tl in
      fill 1 tl

let of_list_rev_map xs ~f =
  let t = of_list_map xs ~f in
  rev_inplace t;
  t

(** [filter_opt array] returns a new array where [None] entries are omitted and [Some x]
    entries are replaced with [x]. Note that this changes the index at which elements
    will appear. *)
let filter_opt t =
  let n = length t in
  let res_size = ref 0 in
  let first_some = ref None in
  for i = 0 to n - 1 do
    begin match t.(i) with
    | None -> ()
    | Some _ as s ->
      if !res_size = 0 then first_some := s;
      incr res_size;
    end;
  done;
  match !first_some with
  | None -> [||]
  | Some el ->
    let result = create !res_size el in
    let pos = ref 0 in
    for i = 0 to n - 1 do
      begin match t.(i) with
      | None -> ()
      | Some x ->
        result.(!pos) <- x;
        incr pos;
      end;
    done;
    result

TEST = filter_opt [|Some 1; None; Some 2; None; Some 3|] = [|1; 2; 3|]
TEST = filter_opt [|Some 1; None; Some 2|] = [|1; 2|]
TEST = filter_opt [|Some 1|] = [|1|]
TEST = filter_opt [|None|] = [||]
TEST = filter_opt [||] = [||]

(** [filter_map ~f array] maps [f] over [array] and filters [None] out of the results. *)
let filter_map t ~f = filter_opt (map t ~f)
(** Same as {!filter_map} but uses {!Array.mapi}. *)
let filter_mapi t ~f = filter_opt (mapi t ~f)

let iter2 t1 t2 ~f =
  if length t1 <> length t2 then invalid_arg "Array.iter2";
  iteri t1 ~f:(fun i x1 -> f x1 t2.(i))

let map2 t1 t2 ~f =
  let len = length t1 in
  if length t2 <> len then invalid_arg "Array.map2";
  init len ~f:(fun i -> f t1.(i) t2.(i))

let fold2_exn t1 t2 ~init ~f =
  if length t1 <> length t2 then invalid_arg "Array.fold2_exn";
  foldi t1 ~init ~f:(fun i ac x -> f ac x t2.(i))
;;

TEST = fold2_exn [||] [||] ~init:13 ~f:(fun _ -> failwith "fail") = 13
TEST = fold2_exn [| 1 |] [| "1" |] ~init:[] ~f:(fun ac a b -> (a, b) :: ac) = [ 1, "1" ]

(** [filter ~f array] removes the elements for which [f] returns false.  *)
let filter ~f =  filter_map ~f:(fun x -> if f x then Some x else None)
(** Like {!filter} except [f] also receives the index. *)
let filteri ~f = filter_mapi ~f:(fun i x -> if f i x then Some x else None)

let exists t ~f =
  let rec loop i =
    if i < 0
    then false
    else if f t.(i)
    then true
    else loop (i - 1)
  in
  loop (length t - 1)

let mem ?(equal = (=)) t a = exists t ~f:(equal a)

let for_all t ~f =
  let rec loop i =
    if i < 0
    then true
    else if f t.(i)
    then loop (i - 1)
    else false
  in
  loop (length t - 1)

let for_all2 t1 t2 ~f =
  let len = length t1 in
  if length t2 <> len then invalid_arg "Array.for_all2";
  let rec loop i =
    if i < 0
    then true
    else if f t1.(i) t2.(i)
    then loop (i - 1)
    else false
  in
  loop (len - 1)

let equal t1 t2 ~equal = length t1 = length t2 && for_all2 t1 t2 ~f:equal

TEST = equal [||] [||] ~equal:(=)
TEST = equal [| 1 |] [| 1 |] ~equal:(=)
TEST = equal [| 1; 2 |] [| 1; 2 |] ~equal:(=)
TEST = not (equal [||] [| 1 |] ~equal:(=))
TEST = not (equal [| 1 |] [||] ~equal:(=))
TEST = not (equal [| 1 |] [| 1; 2 |] ~equal:(=))
TEST = not (equal [| 1; 2 |] [| 1; 3 |] ~equal:(=))

let replace t i ~f = t.(i) <- f t.(i)

(** modifies an array in place -- [t.(i)] will be set to [f(t.(i))] *)
let replace_all t ~f =
  for i = 0 to length t - 1 do
    t.(i) <- f t.(i)
  done

let findi t ~f =
  let length = length t in
  let rec loop i =
    if i >= length then None
    else if f i t.(i) then Some (i, t.(i))
    else loop (i + 1)
  in
  loop 0
;;

let findi_exn t ~f =
  match findi t ~f with
  | None -> raise Not_found
  | Some x -> x
;;

let find_exn t ~f =
  match findi t ~f:(fun _i x -> f x) with
  | None -> raise Not_found
  | Some (_i, x) -> x
;;

let find t ~f = Option.map (findi t ~f:(fun _i x -> f x)) ~f:(fun (_i, x) -> x)

let find_map t ~f =
  let length = length t in
  let rec loop i =
    if i >= length then None
    else
      match f t.(i) with
      | None -> loop (i + 1)
      | Some _ as res -> res
  in
  loop 0
;;


let reduce t ~f =
  if length t = 0 then None
  else begin
    let r = ref t.(0) in
    for i = 1 to length t - 1 do
      r := f !r t.(i)
    done;
    Some !r
  end

let reduce_exn t ~f =
  match reduce t ~f with
  | None -> invalid_arg "Array.reduce_exn"
  | Some v -> v

let permute = Array_permute.permute

let combine t1 t2 =
  if length t1 <> length t2 then failwith "Array.combine"
  else map2 t1 t2 ~f:(fun x1 x2 -> x1, x2)

let split t =
  let n = length t in
  if n = 0 then [||], [||]
  else
    let x, y = t.(0) in
    let res1 = create n x in
    let res2 = create n y in
    for i = 1 to n - 1 do
      let x, y = t.(i) in
      res1.(i) <- x;
      res2.(i) <- y;
    done;
    res1, res2

let sorted_copy t ~cmp =
  let t1 = copy t in
  sort t1 ~cmp;
  t1

let partitioni_tf t ~f =
  let (trues, falses) =
    mapi t ~f:(fun i x -> if f i x then (Some x, None) else (None, Some x)) |! split
  in
  (filter_opt trues, filter_opt falses)

let partition_tf t ~f =
  partitioni_tf t ~f:(fun _i x -> f x)

let last t = t.(length t - 1)

module Infix = struct
  let ( <|> ) t (start,stop) = slice t start stop
end

(* We use [init 0] rather than [||] because all [||] are physically equal, and
   we want [empty] to create a new array. *)
let empty () = init 0 ~f:(fun _ -> assert false)

let cartesian_product t1 t2 =
  if is_empty t1 || is_empty t2 then
    empty ()
  else
    let n1 = length t1 in
    let n2 = length t2 in
    let t = create (n1 * n2) (t1.(0), t2.(0)) in
    let r = ref 0 in
    for i1 = 0 to n1 - 1 do
      for i2 = 0 to n2 - 1 do
        t.(!r) <- (t1.(i1), t2.(i2));
        incr r;
      done
    done;
    t
;;

