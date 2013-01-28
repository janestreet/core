(* A few small things copied from other parts of core because
   they depend on us, so we can't use them. *)
module Int = struct
  type t = int

  let max (x : t) y = if x > y then x else y
  let min (x : t) y = if x < y then x else y
end

open Sexplib.Std

let phys_equal = (==)

(* Its important that Empty have no args. It's tempting to make this type
   a record (e.g. to hold the compare function), but a lot of memory is saved
   by Empty being an immediate, since all unused buckets in the hashtbl don't
   use any memory (besides the array cell) *)
type ('k, 'v) t =
| Empty
| Node of ('k, 'v) t * 'k * 'v * int * ('k, 'v) t
| Leaf of 'k * 'v

(* We do this 'crazy' magic because we want to remove a level of
   indirection in the tree. If we didn't do this, we'd need to use a
   record, and then the variant would be a block with a pointer to
   the record. Where as now the 'record' is tagged with the
   constructor, thus removing a level of indirection. This is even
   reasonably safe, certainly no more dangerous than a C binding.
   The extra checking is probably free, since the block will already
   be in L1 cache, and the branch predictor is very likely to
   predict correctly. *)
module Update : sig
  val leaf_val    : ('k, 'v) t -> 'v -> unit
  val node_val    : ('k, 'v) t -> 'v -> unit
  val node_left   : ('k, 'v) t -> ('k, 'v) t -> unit
  val node_height : ('k, 'v) t -> int -> unit
  val node_right  : ('k, 'v) t -> ('k, 'v) t -> unit
end = struct

  let set_field (to_update: ('k, 'v) t) (n: int) v =
    Obj.set_field (Obj.repr to_update) n (Obj.repr v)

  let node_left to_update v =
    match to_update with
    | Node _ -> set_field to_update 0 v
    | _ -> assert false

  let leaf_val to_update v =
    match to_update with
    | Leaf _ -> set_field to_update 1 v
    | _ -> assert false

  let node_val to_update v =
    match to_update with
    | Node _ -> set_field to_update 2 v
    | _ -> assert false

  let node_height to_update v =
    match to_update with
    | Node _ -> set_field to_update 3 v
    | _ -> assert false

  let node_right to_update v =
    match to_update with
    | Node _ -> set_field to_update 4 v
    | _ -> assert false

end

let empty = Empty

let height = function
   | Empty -> 0
   | Leaf _ -> 1
   | Node (_l, _k, _v, height, _r) -> height

let invariant compare =
  let legal_left_key key = function
    | Empty -> ()
    | Leaf (left_key, _)
    | Node (_, left_key, _, _, _) ->
      assert (compare left_key key < 0)
  in
  let legal_right_key key = function
    | Empty -> ()
    | Leaf (right_key, _)
    | Node (_, right_key, _, _, _) ->
      assert (compare right_key key > 0)
  in
  let rec inv = function
    | Empty | Leaf _ -> ()
    | Node (left, k, _v, h, right) ->
      let (hl, hr) = (height left, height right) in
      inv left;
      inv right;
      legal_left_key k left;
      legal_right_key k right;
      assert (h = Int.max hl hr + 1);
      assert (abs (hl - hr) <= 2)
  in inv

let invariant t ~compare = invariant compare t

(* In the following comments,
   't is balanced' means that 'invariant t' does not
     raise an exception.  This implies of course that each node's height field is
     correct.
   't is balanceable' means that height of the left and right subtrees of t
     differ by at most 3.
*)

(* In the following comments,
   't is balanced' means that 'invariant t' does not
     raise an exception.  This implies of course that each node's height field is
     correct.
   't is balanceable' means that height of the left and right subtrees of t
     differ by at most 3.
*)

(* @pre: left and right subtrees have correct heights
   @post: output has the correct height *)
let update_height n =
  match n with
  | Node (left, _, _, old_height, right) ->
    let new_height = (Int.max (height left) (height right)) + 1 in
    if new_height <> old_height then Update.node_height n new_height
  | _ -> assert false

let balanceable = function
  | Empty | Leaf _ -> true
  | Node(l, _, _, _, r) -> abs (height l - height r) <= 3

(* @pre: left and right subtrees are balanced
   @pre: tree is balanceable
   @post: output is balanced (in particular, height is correct)
*)
let balance tree =
  (* assert (balanceable tree); *)
  match tree with
  | Empty | Leaf _ -> tree
  | Node (left, _k, _v, _h, right) as root_node ->
    let hl = height left and hr = height right in
    (* + 2 is critically important, lowering it to 1 will break the Leaf
       assumptions in the code below, and will force us to promote leaf nodes in
       the balance routine. It's also faster, since it will balance less often.
       Note that the following code is delicate.  The update_height calls must
       occur in the correct order, since update_height assumes its children have
       the correct heights.  *)
    if hl > hr + 2 then begin
      match left with
      (* It cannot be a leaf, because even if right is empty, a leaf
         is only height 1 *)
      | Empty | Leaf _ -> assert false
      | Node (left_node_left, _, _, _, left_node_right) as left_node ->
        if height left_node_left >= height left_node_right then begin
          Update.node_left root_node left_node_right;
          Update.node_right left_node root_node;
          update_height root_node;
          update_height left_node;
          left_node
        end else begin
          (* if right is a leaf, then left must be empty. That means
             height is 2. Even if hr is empty we still can't get here. *)
          match left_node_right with
          | Empty | Leaf _ -> assert false
          | Node (lr_left, _, _, _, lr_right) as lr_node ->
            Update.node_right left_node lr_left;
            Update.node_left root_node lr_right;
            Update.node_right lr_node root_node;
            Update.node_left lr_node left_node;
            update_height left_node;
            update_height root_node;
            update_height lr_node;
            lr_node
        end
    end else if hr > hl + 2 then begin
      (* see above for an explanation of why right cannot be a leaf *)
      match right with
      | Empty | Leaf _ -> assert false
      | Node (right_node_left, _, _, _, right_node_right) as right_node ->
        if height right_node_right >= height right_node_left then begin
          Update.node_right root_node right_node_left;
          Update.node_left right_node root_node;
          update_height root_node;
          update_height right_node;
          right_node
        end else begin
          (* see above for an explanation of why this cannot be a leaf *)
          match right_node_left with
          | Empty | Leaf _ -> assert false
          | Node (rl_left, _, _, _, rl_right) as rl_node ->
            Update.node_left right_node rl_right;
            Update.node_right root_node rl_left;
            Update.node_left rl_node root_node;
            Update.node_right rl_node right_node;
            update_height right_node;
            update_height root_node;
            update_height rl_node;
            rl_node
        end
    end else begin
          update_height tree;
          tree
    end
;;

(* @pre: tree is balanceable
   @pre: abs (height (right node) - height (balance tree)) <= 3
   @post: result is balanceable
*)

(* @pre: tree is balanceable
   @pre: abs (height (right node) - height (balance tree)) <= 3
   @post: result is balanceable
*)
let set_left node tree =
  let tree = balance tree in
  match node with
  | Node (left, _, _, _, _) ->
    if phys_equal left tree then ()
    else
      Update.node_left node tree;
    update_height node
  | _ -> assert false

(* @pre: tree is balanceable
   @pre: abs (height (left node) - height (balance tree)) <= 3
   @post: result is balanceable
*)
let set_right node tree =
  let tree = balance tree in
  match node with
  | Node (_, _, _, _, right) ->
    if phys_equal right tree then ()
    else
      Update.node_right node tree;
    update_height node
  | _ -> assert false

(* @pre: t is balanced.
   @post: result is balanced, with new node inserted
   @post: !added = true iff the shape of the input tree changed.  *)
let add =
  let rec add t replace added compare k v =
    match t with
    | Empty ->
      added := true;
      Leaf (k, v)
    | Leaf (k', _) ->
      let c = compare k' k in
      (* This compare is reversed on purpose, we are pretending
         that the leaf was just inserted instead of the other way
         round, that way we only allocate one node. *)
      if c = 0 then begin
        added := false;
        if replace then Update.leaf_val t v;
        t
      end else begin
        added := true;
        if c < 0 then
          Node(t, k, v, 2, Empty)
        else
          Node(Empty, k, v, 2, t)
      end
    | Node (left, k', _, _, right) ->
      let c = compare k k' in
      if c = 0 then begin
        added := false;
        if replace then Update.node_val t v;
      end else if c < 0 then
          set_left t (add left replace added compare k v)
        else
          set_right t (add right replace added compare k v);
      t
  in
  fun ?(replace=true) t ~compare ~added ~key ~data ->
    let t = add t replace added compare key data in
    if !added then balance t else t
;;



let rec find t ~compare k =
  (* A little manual unrolling of the recursion.
     This is really worth 5% on average *)
  match t with
  | Empty -> None
  | Leaf (k', v) ->
    if compare k k' = 0 then Some v
    else None
  | Node (left, k', v, _, right) ->
    let c = compare k k' in
    if c = 0 then Some v
    else if c < 0 then begin
      match left with
      | Empty -> None
      | Leaf (k', v) ->
        if compare k k' = 0 then Some v
        else None
      | Node (left, k', v, _, right) ->
        let c = compare k k' in
        if c = 0 then Some v
        else find (if c < 0 then left else right) ~compare k
    end else begin
      match right with
      | Empty -> None
      | Leaf (k', v) ->
        if compare k k' = 0 then Some v
        else None
      | Node (left, k', v, _, right) ->
        let c = compare k k' in
        if c = 0 then Some v
        else find (if c < 0 then left else right) ~compare k
    end
;;

let mem t ~compare k = Option.is_some (find t ~compare k)

let remove =
  let rec min_elt tree =
    match tree with
    | Empty -> Empty
    | Leaf _ -> tree
    | Node (Empty, _, _, _, _) -> tree
    | Node (left, _, _, _, _) -> min_elt left
  in
  let rec remove_min_elt tree =
    match tree with
    | Empty -> assert false
    | Leaf _ -> Empty (* This must be the root *)
    | Node (Empty, _, _, _, right) -> right
    | Node (Leaf _, k, v, _, Empty) -> Leaf (k, v)
    | Node (Leaf _, _, _, _, _) as node -> set_left node Empty; tree
    | Node (left, _, _, _, _) as node ->
      set_left node (remove_min_elt left); tree
  in
  let merge t1 t2 =
    match (t1, t2) with
    | (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
      let tree = min_elt t2 in
      match tree with
      | Empty -> assert false
      | Leaf (k, v) ->
        let t2 = balance (remove_min_elt t2) in
        Node (t1, k, v, Int.max (height t1) (height t2) + 1, t2)
      | Node _ as node ->
        set_right node (remove_min_elt t2);
        set_left node t1;
        node
  in
  let rec remove t removed compare k =
    match t with
    | Empty ->
      removed := false;
      Empty
    | Leaf (k', _) ->
      if compare k k' = 0 then begin
        removed := true;
        Empty
      end else begin
        removed := false;
        t
      end
    | Node (left, k', _, _, right) ->
      let c = compare k k' in
      if c = 0 then begin
        removed := true;
        merge left right
      end else if c < 0 then begin
        set_left t (remove left removed compare k);
        t
      end else begin
        set_right t (remove right removed compare k);
        t
      end
  in
  fun t ~removed ~compare k -> balance (remove t removed compare k)
;;

let rec fold t ~init ~f =
  match t with
  | Empty -> init
  | Leaf (key, data) -> f ~key ~data init
  | Node (Leaf (lkey, ldata), key, data, _, Leaf (rkey, rdata)) ->
    f ~key:rkey ~data:rdata (f ~key ~data (f ~key:lkey ~data:ldata init))
  | Node (Leaf (lkey, ldata), key, data, _, Empty) ->
    f ~key ~data (f ~key:lkey ~data:ldata init)
  | Node (Empty, key, data, _, Leaf (rkey, rdata)) ->
    f ~key:rkey ~data:rdata (f ~key ~data init)
  | Node (left, key, data, _, Leaf (rkey, rdata)) ->
    f ~key:rkey ~data:rdata (f ~key ~data (fold left ~init ~f))
  | Node (Leaf (lkey, ldata), key, data, _, right) ->
    fold right ~init:(f ~key ~data (f ~key:lkey ~data:ldata init)) ~f
  | Node (left, key, data, _, right) ->
    fold right ~init:(f ~key ~data (fold left ~init ~f)) ~f

let iter t ~f = fold t ~init:() ~f:(fun ~key ~data () -> f ~key ~data)
