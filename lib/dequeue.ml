(* Double-ended index-able queue *)

(* the user's index is called the "conceptual" index (variable names are index or i),
   while the corresponding index into the physical array is called the "physical" index
   (variable names are pindex or p)
   the physical length is "Array.length buf.data" (variable name plength) while the
   conceptual length is buf.length (variable name length)
   invariant: physical_index = conceptual_index mod physical_length
   the "front" has smaller indices
   physical array lengths are always powers of two
   maybe I should insert dummy values into dropped indices to g.c. junk sooner? *)

open Std_internal


type 'a t = {
  mutable data: 'a array;
  mutable min_index: int; (* conceptual *)
  mutable length: int;    (* conceptual: max_index = min_index + length - 1 *)
  never_shrink: bool;
  dummy: 'a;
} with bin_io, sexp

let create ?(never_shrink=false) ?(initial_index=0) ~dummy () = {
  data = Array.create (1 lsl 3) dummy;  (* (1 lsl 3) = 8, must be power of 2! *)
  min_index = initial_index;
  length = 0;
  never_shrink = never_shrink;
  dummy = dummy;
}

let length buf = buf.length

let is_empty buf = buf.length = 0

let front_index buf = buf.min_index

let back_index buf = buf.min_index + buf.length - 1

let is_full buf = buf.length = Array.length buf.data

let invariant buf =
  assert (buf.length <= Array.length buf.data)

let fast_double x = x lsl 1             (* x * 2 *)
let fast_half x = x asr 1               (* x / 2 *)
let fast_quarter x = x asr 2            (* x / 4 *)
let fast_mod x l = x land (l-1)         (* x % l (works when l is power of 2) *)
let fast_is_power_2 x = x land (x-1) = 0(* x=2^n for non-negative integer n or x=0 or -max_int-1*)

let check_index fname buf i =
  if i < buf.min_index || i >= buf.min_index + buf.length then
    invalid_arg
      (sprintf "Dequeue.%s: index %i is not in [%d, %d]"
        fname i (front_index buf) (back_index buf))

let get_exn buf i =
  check_index "get" buf i;
  buf.data.(fast_mod i (Array.length buf.data))

let get_front_exn buf =
  get_exn buf buf.min_index

let get_back_exn buf =
  get_exn buf (back_index buf)

let get_front buf = try Some (get_front_exn buf) with _ -> None
let get_back  buf = try Some (get_back_exn  buf) with _ -> None

let set_exn buf i v =
  check_index "set" buf i;
  buf.data.(fast_mod i (Array.length buf.data)) <- v

let iteri ~f buf =
  for i=(front_index buf) to (back_index buf) do
    f i (get_exn buf i)
  done

let iter ~f buf = iteri ~f:(fun _ x -> f x) buf

let foldi ~f ~init buf =
  let acc = ref init in
  iteri ~f:(fun i a -> acc := f !acc i a) buf;
  !acc

let fold ~f ~init buf = foldi ~f:(fun acc _ a -> f acc a) ~init buf

let copy_data buf new_plength =         (* plength = physical array length *)
  let old_plength = Array.length buf.data in
  (* these invariants are maintained -- let's make them explicit *)
  assert (new_plength >= buf.length);
  assert (fast_double new_plength = old_plength ||
          fast_double old_plength = new_plength);
  assert (fast_is_power_2 old_plength && fast_is_power_2 new_plength);
  let newdata = Array.create new_plength buf.dummy in
  let src_min_pindex = fast_mod buf.min_index old_plength in
  let dst_min_pindex = fast_mod buf.min_index new_plength in
  let first_copy_length =
    let small_plength = Int.min old_plength new_plength in
    let small_min_pindex = fast_mod buf.min_index small_plength in
    small_plength - small_min_pindex
  in
  Array.blit ~src:buf.data ~dst:newdata
    ~src_pos:src_min_pindex
    ~dst_pos:dst_min_pindex
    ~len:first_copy_length;
  if first_copy_length < buf.length
  then begin
    let second_copy_length = buf.length - first_copy_length in
    let second_copy_start_index = buf.min_index + first_copy_length in
    let src_start_pindex = fast_mod second_copy_start_index old_plength in
    let dst_start_pindex = fast_mod second_copy_start_index new_plength in
    Array.blit ~src:buf.data ~dst:newdata
      ~src_pos:src_start_pindex
      ~dst_pos:dst_start_pindex
      ~len:second_copy_length
  end;
  newdata

let swap_array buf new_plength =
  let newdata = copy_data buf new_plength in
  buf.data <- newdata

let maybe_expand buf =
  if is_full buf
  then swap_array buf (fast_double (Array.length buf.data))

(* fast_quarter is used here instead of fast_half for performace reasons.  This is _not_
   known to optimize performance.
*)
let maybe_shrink buf =
  if not buf.never_shrink && buf.length < fast_quarter (Array.length buf.data)
  then swap_array buf (fast_half (Array.length buf.data))

let push_front buf v =
  maybe_expand buf;
  buf.min_index <- buf.min_index - 1;
  buf.length <- buf.length + 1;
  set_exn buf buf.min_index v

let push_back buf v =
  maybe_expand buf;
  buf.length <- buf.length + 1;
  set_exn buf (back_index buf) v

let drop_front_exn ?(n=1) buf =
  if n > buf.length || n < 0 then invalid_arg "Dequeue.drop_front";
  buf.min_index <- buf.min_index + n;
  buf.length <- buf.length - n;
  maybe_shrink buf

let drop_back_exn ?(n=1) buf =
  if n > buf.length || n < 0 then invalid_arg "Dequeue.drop_back";
  buf.length <- buf.length - n;
  maybe_shrink buf

let take_front_exn buf =
  let v = get_front_exn buf in
  drop_front_exn buf;
  v

let take_back_exn buf =
  let v = get_back_exn buf in
  drop_back_exn buf;
  v

let drop_indices_less_than_exn buf i =
  drop_front_exn ~n:(i - buf.min_index) buf

let drop_indices_greater_than_exn buf i =
  drop_back_exn ~n:(back_index buf - i) buf
