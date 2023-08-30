open! Import
open Std_internal

type 'a t =
  { (* [arr] is a cyclic buffer *)
    mutable arr : 'a Option_array.t
  ; (* [front_index] and [back_index] are the positions in which new elements may be
       enqueued.  This makes the active part of [arr] the range from [front_index+1] to
       [back_index-1] (modulo the length of [arr] and wrapping around if necessary).  Note
       that this means the active range is maximized when [front_index = back_index], which
       occurs when there are [Array.length arr - 1] active elements. *)
    mutable front_index : int
  ; mutable back_index : int
  ; (* apparent_front_index is what is exposed as the front index externally.  It has no
       real relation to the array -- every enqueue to the front decrements it and every
       dequeue from the front increments it. *)
    mutable apparent_front_index : int
  ; mutable length : int
  ; (* We keep arr_length here as a speed hack.  Calling Array.length on arr is actually
       meaningfully slower. *)
    mutable arr_length : int
  ; never_shrink : bool
  }

let create ?initial_length ?never_shrink () =
  let never_shrink =
    match never_shrink with
    | None -> Option.is_some initial_length
    | Some b -> b
  in
  let initial_length = Option.value ~default:7 initial_length in
  if initial_length < 0
  then invalid_argf "passed negative initial_length to Deque.create: %i" initial_length ();
  (* Make the initial array length be [initial_length + 1] so we can fit [initial_length]
     elements without growing.  We never quite use the whole array. *)
  let arr_length = initial_length + 1 in
  { arr = Option_array.create ~len:arr_length
  ; front_index = 0
  ; back_index = 1
  ; apparent_front_index = 0
  ; length = 0
  ; arr_length
  ; never_shrink
  }
;;

let length t = t.length
let is_empty t = length t = 0

(* We keep track of the length in a mutable field for speed, but this calculation should
   be correct by construction, and can be used for testing. *)
let _invariant_length t =
  let constructed_length =
    if t.front_index < t.back_index
    then t.back_index - t.front_index - 1
    else t.back_index - t.front_index - 1 + t.arr_length
  in
  assert (length t = constructed_length)
;;

(* The various "when_not_empty" functions return misleading numbers when the dequeue is
   empty.  They are safe to call if it is known that the dequeue is non-empty. *)
let apparent_front_index_when_not_empty t = t.apparent_front_index
let apparent_back_index_when_not_empty t = t.apparent_front_index + length t - 1

let actual_front_index_when_not_empty t =
  if t.front_index = t.arr_length - 1 then 0 else t.front_index + 1
;;

let actual_back_index_when_not_empty t =
  if t.back_index = 0 then t.arr_length - 1 else t.back_index - 1
;;

let checked t f = if is_empty t then None else Some (f t)
let apparent_front_index t = checked t apparent_front_index_when_not_empty
let apparent_back_index t = checked t apparent_back_index_when_not_empty

let foldi' t dir ~init ~f =
  if is_empty t
  then init
  else (
    let apparent_front = apparent_front_index_when_not_empty t in
    let apparent_back = apparent_back_index_when_not_empty t in
    let actual_front = actual_front_index_when_not_empty t in
    let actual_back = actual_back_index_when_not_empty t in
    let rec loop acc ~apparent_i ~real_i ~stop_pos ~step =
      if real_i = stop_pos
      then acc, apparent_i
      else
        loop
          (f apparent_i acc (Option_array.get_some_exn t.arr real_i))
          ~apparent_i:(apparent_i + step)
          ~real_i:(real_i + step)
          ~stop_pos
          ~step
    in
    (* We want to iterate from actual_front to actual_back (or vice versa), but we may
       need to wrap around the array to do so.  Thus we do the following:
       1.  If the active range is contiguous (i.e. actual_front <= actual_back), then loop
       starting at the appropriate end of the active range until we reach the first
       element outside of it.
       2.  If it is not contiguous (actual_front > actual_back), then first loop from the
       appropriate end of the active range to the end of the array.  Then, loop from
       the opposite end of the array to the opposite end of the active range.
    *)
    match dir with
    | `front_to_back ->
      if actual_front <= actual_back
      then (
        let acc, _ =
          loop
            init
            ~apparent_i:apparent_front
            ~real_i:actual_front
            ~stop_pos:(actual_back + 1)
            ~step:1
        in
        acc)
      else (
        let acc, apparent_i =
          loop
            init
            ~apparent_i:apparent_front
            ~real_i:actual_front
            ~stop_pos:t.arr_length
            ~step:1
        in
        let acc, _ = loop acc ~apparent_i ~real_i:0 ~stop_pos:(actual_back + 1) ~step:1 in
        acc)
    | `back_to_front ->
      if actual_front <= actual_back
      then (
        let acc, _ =
          loop
            init
            ~apparent_i:apparent_back
            ~real_i:actual_back
            ~stop_pos:(actual_front - 1)
            ~step:(-1)
        in
        acc)
      else (
        let acc, apparent_i =
          loop
            init
            ~apparent_i:apparent_back
            ~real_i:actual_back
            ~stop_pos:(-1)
            ~step:(-1)
        in
        let acc, _ =
          loop
            acc
            ~apparent_i
            ~real_i:(t.arr_length - 1)
            ~stop_pos:(actual_front - 1)
            ~step:(-1)
        in
        acc))
;;

let fold' t dir ~init ~f = foldi' t dir ~init ~f:(fun _ acc v -> f acc v) [@nontail]
let iteri' t dir ~f = foldi' t dir ~init:() ~f:(fun i () v -> f i v)
let iter' t dir ~f = foldi' t dir ~init:() ~f:(fun _ () v -> f v)
let fold t ~init ~f = fold' t `front_to_back ~init ~f
let foldi t ~init ~f = foldi' t `front_to_back ~init ~f
let iteri t ~f = iteri' t `front_to_back ~f

let iteri_internal t ~f =
  if not (is_empty t)
  then (
    let actual_front = actual_front_index_when_not_empty t in
    let actual_back = actual_back_index_when_not_empty t in
    let rec loop ~real_i ~stop_pos =
      if real_i < stop_pos
      then (
        f t.arr real_i;
        loop ~real_i:(real_i + 1) ~stop_pos)
    in
    if actual_front <= actual_back
    then loop ~real_i:actual_front ~stop_pos:(actual_back + 1) [@nontail]
    else (
      loop ~real_i:actual_front ~stop_pos:t.arr_length;
      loop ~real_i:0 ~stop_pos:(actual_back + 1) [@nontail]))
;;

let iter t ~f =
  iteri_internal t ~f:(fun arr i -> Option_array.get_some_exn arr i |> f) [@nontail]
;;

let clear t =
  if t.never_shrink
  then
    (* clear the array to allow elements to be garbage collected *)
    iteri_internal t ~f:Option_array.unsafe_set_none
  else t.arr <- Option_array.create ~len:8;
  t.front_index <- 0;
  t.back_index <- 1;
  t.length <- 0;
  t.arr_length <- Option_array.length t.arr
;;

(* We have to be careful here, importing all of Container.Make would change the runtime of
   some functions ([length] minimally) silently without changing the semantics.  We get
   around that by importing things explicitly.  *)
module C = Container.Make (struct
  type nonrec 'a t = 'a t

  let fold = fold
  let iter = `Custom iter
  let length = `Custom length
end)

let count = C.count
let sum = C.sum
let exists = C.exists
let mem = C.mem
let for_all = C.for_all
let find_map = C.find_map
let find = C.find
let to_list = C.to_list
let min_elt = C.min_elt
let max_elt = C.max_elt
let fold_result = C.fold_result
let fold_until = C.fold_until

let blit new_arr t =
  assert (not (is_empty t));
  let actual_front = actual_front_index_when_not_empty t in
  let actual_back = actual_back_index_when_not_empty t in
  let old_arr = t.arr in
  if actual_front <= actual_back
  then
    Option_array.blit
      ~src:old_arr
      ~dst:new_arr
      ~src_pos:actual_front
      ~dst_pos:0
      ~len:(length t)
  else (
    let break_pos = Option_array.length old_arr - actual_front in
    Option_array.blit
      ~src:old_arr
      ~dst:new_arr
      ~src_pos:actual_front
      ~dst_pos:0
      ~len:break_pos;
    Option_array.blit
      ~src:old_arr
      ~dst:new_arr
      ~src_pos:0
      ~dst_pos:break_pos
      ~len:(actual_back + 1));
  (* length depends on t.arr and t.front_index, so this needs to be first *)
  t.back_index <- length t;
  t.arr <- new_arr;
  t.arr_length <- Option_array.length new_arr;
  t.front_index <- Option_array.length new_arr - 1;
  (* Since t.front_index = Option_array.length new_arr - 1, this is asserting that t.back_index
     is a valid index in the array and that the array can support at least one more
     element -- recall, if t.front_index = t.back_index then the array is full.

     Note that this is true if and only if Option_array.length new_arr > length t + 1.
  *)
  assert (t.front_index > t.back_index)
;;

let maybe_shrink_underlying t =
  if (not t.never_shrink) && t.arr_length > 10 && t.arr_length / 3 > length t
  then (
    let new_arr = Option_array.create ~len:(t.arr_length / 2) in
    blit new_arr t)
;;

let grow_underlying t =
  let new_arr = Option_array.create ~len:(t.arr_length * 2) in
  blit new_arr t
;;

let enqueue_back t v =
  if t.front_index = t.back_index then grow_underlying t;
  Option_array.set_some t.arr t.back_index v;
  t.back_index <- (if t.back_index = t.arr_length - 1 then 0 else t.back_index + 1);
  t.length <- t.length + 1
;;

let enqueue_front t v =
  if t.front_index = t.back_index then grow_underlying t;
  Option_array.set_some t.arr t.front_index v;
  t.front_index <- (if t.front_index = 0 then t.arr_length - 1 else t.front_index - 1);
  t.apparent_front_index <- t.apparent_front_index - 1;
  t.length <- t.length + 1
;;

let enqueue t back_or_front v =
  match back_or_front with
  | `back -> enqueue_back t v
  | `front -> enqueue_front t v
;;

let peek_front_nonempty t =
  Option_array.get_some_exn t.arr (actual_front_index_when_not_empty t)
;;

let peek_front_exn t =
  if is_empty t
  then failwith "Deque.peek_front_exn passed an empty queue"
  else peek_front_nonempty t
;;

let peek_front t = if is_empty t then None else Some (peek_front_nonempty t)

let peek_back_nonempty t =
  Option_array.get_some_exn t.arr (actual_back_index_when_not_empty t)
;;

let peek_back_exn t =
  if is_empty t
  then failwith "Deque.peek_back_exn passed an empty queue"
  else peek_back_nonempty t
;;

let peek_back t = if is_empty t then None else Some (peek_back_nonempty t)

let peek t back_or_front =
  match back_or_front with
  | `back -> peek_back t
  | `front -> peek_front t
;;

let dequeue_front_nonempty t =
  let i = actual_front_index_when_not_empty t in
  let res = Option_array.get_some_exn t.arr i in
  Option_array.set_none t.arr i;
  t.front_index <- i;
  t.apparent_front_index <- t.apparent_front_index + 1;
  t.length <- t.length - 1;
  maybe_shrink_underlying t;
  res
;;

let dequeue_front_exn t =
  if is_empty t
  then failwith "Deque.dequeue_front_exn passed an empty queue"
  else dequeue_front_nonempty t
;;

let dequeue_front t = if is_empty t then None else Some (dequeue_front_nonempty t)

let dequeue_back_nonempty t =
  let i = actual_back_index_when_not_empty t in
  let res = Option_array.get_some_exn t.arr i in
  Option_array.set_none t.arr i;
  t.back_index <- i;
  t.length <- t.length - 1;
  maybe_shrink_underlying t;
  res
;;

let dequeue_back_exn t =
  if is_empty t
  then failwith "Deque.dequeue_back_exn passed an empty queue"
  else dequeue_back_nonempty t
;;

let dequeue_back t = if is_empty t then None else Some (dequeue_back_nonempty t)

let dequeue_exn t back_or_front =
  match back_or_front with
  | `front -> dequeue_front_exn t
  | `back -> dequeue_back_exn t
;;

let dequeue t back_or_front =
  match back_or_front with
  | `front -> dequeue_front t
  | `back -> dequeue_back t
;;

let drop_gen ?(n = 1) ~dequeue t =
  if n < 0 then invalid_argf "Deque.drop:  negative input (%d)" n ();
  let rec loop n =
    if n > 0
    then (
      match dequeue t with
      | None -> ()
      | Some _ -> loop (n - 1))
  in
  loop n
;;

let drop_front ?n t = drop_gen ?n ~dequeue:dequeue_front t
let drop_back ?n t = drop_gen ?n ~dequeue:dequeue_back t

let drop ?n t back_or_front =
  match back_or_front with
  | `back -> drop_back ?n t
  | `front -> drop_front ?n t
;;

let assert_not_empty t name = if is_empty t then failwithf "%s: Deque.t is empty" name ()

let true_index_exn t i =
  let i_from_zero = i - t.apparent_front_index in
  if i_from_zero < 0 || length t <= i_from_zero
  then (
    assert_not_empty t "Deque.true_index_exn";
    let apparent_front = apparent_front_index_when_not_empty t in
    let apparent_back = apparent_back_index_when_not_empty t in
    invalid_argf
      "invalid index: %i for array with indices (%i,%i)"
      i
      apparent_front
      apparent_back
      ());
  let true_i = t.front_index + 1 + i_from_zero in
  if true_i >= t.arr_length then true_i - t.arr_length else true_i
;;

let get t i = Option_array.get_some_exn t.arr (true_index_exn t i)

let get_opt t i =
  try Some (get t i) with
  | _ -> None
;;

let set_exn t i v = Option_array.set_some t.arr (true_index_exn t i) v

let to_array t =
  match peek_front t with
  | None -> [||]
  | Some front ->
    let arr = Array.create ~len:(length t) front in
    ignore
      (fold t ~init:0 ~f:(fun i v ->
         arr.(i) <- v;
         i + 1)
        : int);
    arr
;;

let of_array arr =
  let t = create ~initial_length:(Array.length arr + 1) () in
  Array.iter arr ~f:(fun v -> enqueue_back t v);
  t
;;

include Bin_prot.Utils.Make_iterable_binable1 (struct
  type nonrec 'a t = 'a t
  type 'a el = 'a [@@deriving bin_io]

  let caller_identity =
    Bin_prot.Shape.Uuid.of_string "34c1e9ca-4992-11e6-a686-8b4bd4f87796"
  ;;

  let module_name = Some "Core.Deque"
  let length = length
  let iter t ~f = iter t ~f

  let init ~len ~next =
    let t = create ~initial_length:len () in
    for _i = 0 to len - 1 do
      let x = next () in
      enqueue_back t x
    done;
    t
  ;;
end)

let t_of_sexp f sexp = of_array (Array.t_of_sexp f sexp)
let sexp_of_t f t = Array.sexp_of_t f (to_array t)

let t_sexp_grammar elt_grammar =
  Sexplib.Sexp_grammar.coerce (Array.t_sexp_grammar elt_grammar)
;;

(* re-expose these here under a different name to avoid internal confusion *)
let back_index = apparent_back_index
let front_index = apparent_front_index

let back_index_exn t =
  assert_not_empty t "Deque.back_index_exn";
  apparent_back_index_when_not_empty t
;;

let front_index_exn t =
  assert_not_empty t "Deque.front_index_exn";
  apparent_front_index_when_not_empty t
;;

module Binary_searchable = Test_binary_searchable.Make1_and_test (struct
  type nonrec 'a t = 'a t

  let get t i = get t (front_index_exn t + i)
  let length = length

  module For_test = struct
    let of_array = of_array
  end
end)

(* The "stable" indices used in this module make the application of the
   [Binary_searchable] functor awkward.  We need to be sure to translate incoming
   positions from stable space to the expected 0 -> length - 1 space and then we need to
   translate them back on return. *)
let binary_search ?pos ?len t ~compare how v =
  let pos =
    match pos with
    | None -> None
    | Some pos -> Some (pos - t.apparent_front_index)
  in
  match Binary_searchable.binary_search ?pos ?len t ~compare how v with
  | None -> None
  | Some untranslated_i -> Some (t.apparent_front_index + untranslated_i)
;;

let binary_search_segmented ?pos ?len t ~segment_of how =
  let pos =
    match pos with
    | None -> None
    | Some pos -> Some (pos - t.apparent_front_index)
  in
  match Binary_searchable.binary_search_segmented ?pos ?len t ~segment_of how with
  | None -> None
  | Some untranslated_i -> Some (t.apparent_front_index + untranslated_i)
;;
