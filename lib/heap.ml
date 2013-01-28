
open Int_replace_polymorphic_compare
module OldArray = Caml.Array
module Array = Res.Array

let def_gfactor, def_sfactor, def_min_size = Res.DefStrat.default

exception Empty with sexp

type 'el t =
  {
    ar : 'el heap_el Array.t;
    cmp : 'el -> 'el -> int
  }

and 'el heap_el =
  {
    mutable heap : 'el t;
    mutable pos : int;
    mutable el : 'el;
  }

let heap_el_is_valid h_el = h_el.pos >= 0

let heap_el_get_el h_el = h_el.el

let length h = Array.length h.ar
let is_empty h = length h = 0
let get_cmp h = h.cmp

let calc_parent pos = (pos - 1) / 2
let calc_left pos = pos + pos + 1
let calc_right pos = pos + pos + 2

let invalidate h_el = h_el.pos <- -1

let set_pos ar pos = (Array.get ar pos).pos <- pos
let get_el ar i = (Array.get ar i).el

let exchange ar i1 i2 =
  Array.swap ar i1 i2;
  set_pos ar i1;
  set_pos ar i2

let rec heapify ar cmp len pos =
  let left = calc_left pos in
  let right = left + 1 in
  let largest =
    if left < len && cmp (get_el ar left) (get_el ar pos) < 0
    then left
    else pos
  in
  let largest =
    if right < len && cmp (get_el ar right) (get_el ar largest) < 0
    then right
    else largest
  in
  if pos <> largest then begin
    exchange ar pos largest;
    heapify ar cmp len largest
  end

let create ?(min_size = def_min_size) cmp =
  {
    ar = Array.sempty (def_gfactor, def_sfactor, min_size);
    cmp = cmp;
  }

let dummy_strat = 0.0, 0.0, 0

let make_dummy_heap cmp =
  {
    ar = Array.sempty dummy_strat;
    cmp = cmp;
  }

let of_array ?min_size cmp ar =
  let len = OldArray.length ar in
  if len = 0 then create ?min_size cmp
  else
    let dummy = make_dummy_heap cmp in
    (* Copied in from core_int, to avoid annoying loops *)
    let int_max (x : int) y = if x > y then x else y in
    let min_size =
      match min_size with
      | None -> int_max def_min_size (len / 2)
      | Some min_size -> min_size in
    let rar =
      Array.sinit (def_gfactor, def_sfactor, min_size) len (fun pos ->
        {
          heap = dummy;
          pos = pos;
          el = OldArray.get ar pos;
        }) in
    let res = { ar = rar; cmp = cmp } in
    for pos = 0 to len - 1 do
      (Array.get rar pos).heap <- res
    done;
    if len = 1 then res
    else
      let rec loop pos =
        if pos >= 0 then (
          heapify rar cmp len pos;
          loop (pos - 1)) in
      loop (calc_parent (len - 1));
      res
;;

let copy { ar; cmp } =
  let len = Array.length ar in
  let (_, _, min_size) as strat = Array.get_strategy ar in
  if len = 0 then create ~min_size cmp
  else
    let res_ar =
      Array.sinit strat len (fun ix ->
        let ar_ix = Array.get ar ix in
        { ar_ix with heap = ar_ix.heap }) in
    let res = { ar = res_ar; cmp = cmp } in
    for pos = 0 to len - 1 do
      (Array.get res_ar pos).heap <- res
    done;
    res
;;

let mem { ar; cmp } el =
  let len = Array.length ar in
  len > 0 &&
    let rec loop pos =
      let c = cmp (get_el ar pos) el in
      c = 0
      || c < 0
      &&
        let left = calc_left pos in
        left < len
        && (
          loop left
          ||
            let right = left + 1 in
            right < len
            && loop right
        )
    in
    loop 0
;;

let find_heap_el_exn { ar; cmp } el =
  let len = Array.length ar in
  let rec loop pos =
    if pos >= len then None
    else begin
      let h_el = Array.get ar pos in
      let c = cmp h_el.el el in
      if c = 0 then
        Some h_el
      else if c > 0 then
        None
      else begin
        let left = calc_left pos in
        match loop left with
        | Some _ as x -> x
        | None ->
          let right = left + 1 in
          loop right
      end
    end
  in
  match loop 0 with
  | Some el -> el
  | None -> raise Not_found
;;

let heap_el_mem heap h_el = heap_el_is_valid h_el && h_el.heap == heap

let top_heap_el_exn { ar; _ } =
  if Array.length ar = 0 then raise Empty;
  ar.(0)

let top_heap_el { ar; _ } =
  if Array.length ar = 0 then None
  else Some ar.(0)

let top_exn t = (top_heap_el_exn t).el

let top { ar; _ } =
  if Array.length ar = 0 then None
  else Some (get_el ar 0)

let iter t ~f =
  let ar = t.ar in
  let rec loop i =
    if i < Array.length ar then begin
      let el = ar.(i) in
      if el.pos >= 0 then begin
        f el.el;
        loop (i + 1);
      end
    end
  in
  loop 0
;;

let pop_heap_el_exn ({ ar; _ } as h) =
  let len = Array.length ar in
  if len = 0 then raise Empty;
  let min_h_el = Array.get ar 0 in
  invalidate min_h_el;
  if len = 1 then (Array.remove_one ar; min_h_el)
  else (
    Array.swap_in_last ar 0;
    set_pos ar 0;
    heapify ar h.cmp (len - 1) 0;
    min_h_el)

let pop_heap_el ({ ar; _ } as h) =
  if Array.length ar = 0 then None
  else Some (pop_heap_el_exn h)

let pop_exn h = (pop_heap_el_exn h).el

let pop ({ ar; _ } as h) =
  if Array.length ar = 0 then None
  else Some (pop_exn h)

let cond_pop_heap_el ({ ar; _ } as h) cond =
  if Array.length ar = 0 then None
  else
    let min_h_el = Array.get ar 0 in
    if cond min_h_el.el then Some (pop_heap_el_exn h)
    else None

let cond_pop ({ ar; _ } as h) cond =
  if Array.length ar = 0 then None
  else
    let min_el = get_el ar 0 in
    if cond min_el then Some (pop_exn h)
    else None

let rec move_up ar cmp el pos =
  if pos <= 0 then pos
  else
    let parent_pos = calc_parent pos in
    let parent = Array.get ar parent_pos in
    if cmp el parent.el >= 0 then pos
    else (
      Array.set ar pos parent;
      parent.pos <- pos;
      move_up ar cmp el parent_pos)

let move_up_h_el h_el ar cmp el pos =
  let pos = move_up ar cmp el pos in
  Array.set ar pos h_el;
  h_el.pos <- pos

let push_heap_el { ar; cmp } h_el =
  let len = Array.length ar in
  Array.add_one ar h_el;
  let pos = move_up ar cmp h_el.el len in
  Array.set ar pos h_el;
  h_el.pos <- pos

let push_heap_el h h_el =
  if heap_el_is_valid h_el then
    failwith "Heap.push_heap_el: heap element already in a heap";
  h_el.heap <- h;
  push_heap_el h h_el

let push h el =
  let h_el = { heap = h; pos = -1; el = el } in
  push_heap_el h h_el;
  h_el

let remove_move_down ar cmp len pos last =
  Array.set ar pos last;
  last.pos <- pos;
  heapify ar cmp len pos

let remove ({ heap = { ar; cmp }; pos; _ } as h_el) =
  assert (heap_el_is_valid h_el);
  invalidate h_el;
  let lix = Array.lix ar in
  if pos = lix then Array.remove_one ar
  else
    let { el = last_el; _ } as last = Array.get ar lix in
    Array.remove_one ar;
    if pos = 0 then remove_move_down ar cmp lix pos last
    else
      let parent_pos = calc_parent pos in
      let parent = Array.get ar parent_pos in
      if cmp last_el parent.el < 0 then move_up_h_el last ar cmp last_el pos
      else remove_move_down ar cmp lix pos last

let update ({ pos; _ } as h_el) el =
  assert (heap_el_is_valid h_el);
  h_el.el <- el;
  let { ar = ar; cmp = cmp } = h_el.heap in
  let len = Array.length ar in
  if pos = 0 then heapify ar cmp len pos
  else
    let parent_pos = calc_parent pos in
    let parent = Array.get ar parent_pos in
    if cmp el parent.el < 0 then move_up_h_el h_el ar cmp el pos
    else heapify ar cmp len pos

let check_heap_property { ar = ar; cmp = cmp } =
  let len = Array.length ar in
  try
    for i = 0 to (len - 3) / 2 do
      let el = ar.(i).el in
      let left_child = calc_left i in
      let right_child = calc_right i in
      if cmp ar.(left_child).el el < 0
        || cmp ar.(right_child).el el < 0
      then raise Exit;
    done;
    true
  with Exit -> false
