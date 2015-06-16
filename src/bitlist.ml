open Core_kernel

let num_bits = Core_int.num_bits
let all_ones = lnot 0

type t = {length: int; data: int list}

let make sz =
  let len = if sz mod num_bits = 0 then sz / num_bits else sz / num_bits + 1 in
  let rec _make = function
    | 0 -> []
    | n -> 0 :: _make (n - 1)
  in
  let dat = _make len in
  {length = sz; data = dat}

let set ls i v =
  if i >= ls.length || i < 0
  then raise Not_found
  else begin
    let rec _set ls i v =
      match ls with
      | hd :: tl when i >= num_bits ->
          hd :: (_set tl (i - num_bits) v)
      | hd :: tl when i < num_bits ->
          let crit = v lsl i in (* the critical bit *)
          let hd' = hd land (all_ones - crit) in
          (hd' + crit) :: tl (* only copy up to the element that is changed *)
      | [] -> raise Not_found
    in {length = ls.length; data = _set ls.data i (if v = true then 1 else 0)};
  end

let get ls i =
  if i >= ls.length || i < 0
  then raise Not_found
  else begin
    let rec _get ls i =
      match ls with
      | hd :: tl when i >= num_bits ->
          _get tl (i - num_bits)
      | hd :: tl when i < num_bits ->
          let crit = 1 lsl i in
          let hd' = hd land crit in
          if hd' = 0 then false else true
    in _get ls.data i
  end
