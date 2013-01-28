open Core.Std
open Core_extended.Std

module Avltree = struct
  type ('k, 'v) t =
  | Empty
  | Node of ('k, 'v) node
  | Leaf of 'k * 'v

  (* This record adds an indirection, but it is necessary to make
     insert performance decent, otherwise we are killed by
     allocation. It IS possible to get rid of it by collapsing it's
     fields into a constructor. However, in order to set the fields
     we'd need to use the Obj module, and I wasn't willing to
     contamplate that just yet. (E.G. Obj.set_field ) *)
  and ('k, 'v) node = {
    key: 'k;
    mutable value: 'v;
    mutable left: ('k, 'v) t;
    mutable right: ('k, 'v) t;
    mutable height: int;
  }

  let invariant t compare =
    let rec binary_tree = function
      | Empty | Leaf _ -> ()
      | Node {left = left; right = right; key = key} ->
          begin match left with
          | Empty -> ()
          | Leaf (left_key, _)
          | Node {key = left_key} -> assert (compare left_key key < 0)
          end;
          begin match right with
          | Empty -> ()
          | Leaf (right_key, _)
          | Node {key = right_key} -> assert (compare right_key key > 0)
          end;
          assert (compare key key = 0);
          binary_tree left;
          binary_tree right
    in
    let rec height = function
      | Empty -> 0
      | Leaf _ -> 1
      | Node {left = left; right = right} ->
          Int.max (height left) (height right) + 1
    in
    let rec balanced = function
      | Empty | Leaf _ -> ()
      | Node {left = left; right = right} ->
          assert (abs (height left - height right) < 3);
          balanced left;
          balanced right
    in
    binary_tree t;
    balanced t

  let empty = Empty

  let height = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node n -> n.height

  let update_height n =
    let new_height = (Int.max (height n.left) (height n.right)) + 1 in
    n.height <- new_height

  let balance tree =
    match tree with
    | Empty | Leaf _ -> tree
    | Node ({left = left; right = right} as root_node) as root ->
        let hl = height left and hr = height right in
        (* + 2 is critically important, lowering it to 1 will break the Leaf
           assumptions in the code below, and will force us to promote leaf
           nodes in the balance routine. It's also faster, since it will
           balance less often. *)
        if hl > hr + 2 then begin
          match left with
          (* It cannot be a leaf, because even if right is empty, a leaf
             is only height 1 *)
          | Empty | Leaf _ -> assert false
          | Node left_node ->
              if height left_node.left >= height left_node.right then begin
                root_node.left <- left_node.right;
                left_node.right <- root;
                update_height root_node;
                update_height left_node;
                left
              end else begin
                (* if right is a leaf, then left must be empty. That means
                   height is 2. Even if hr is empty we still can't get here. *)
                match left_node.right with
                | Empty | Leaf _ -> assert false
                | Node lr_node as lr ->
                    left_node.right <- lr_node.left;
                    root_node.left <- lr_node.right;
                    lr_node.right <- root;
                    lr_node.left <- left;
                    update_height left_node;
                    update_height root_node;
                    update_height lr_node;
                    lr
              end
        end else if hr > hl + 2 then begin
          (* see above for an explanation of why right cannot be a leaf *)
          match right with
          | Empty | Leaf _ -> assert false
          | Node right_node ->
              if height right_node.right >= height right_node.left then begin
                root_node.right <- right_node.left;
                right_node.left <- root;
                update_height root_node;
                update_height right_node;
                right
              end else begin
                (* see above for an explanation of why this cannot be a leaf *)
                match right_node.left with
                | Empty | Leaf _ -> assert false
                | Node rl_node as rl ->
                    right_node.left <- rl_node.right;
                    root_node.right <- rl_node.left;
                    rl_node.left <- root;
                    rl_node.right <- right;
                    update_height right_node;
                    update_height root_node;
                    update_height rl_node;
                    rl
              end
        end else
            tree
  ;;

  let set_left node tree =
    let tree = balance tree in
    if phys_equal node.left tree then ()
    else
      node.left <- tree;
    update_height node

  let set_right node tree =
    let tree = balance tree in
    if phys_equal node.right tree then ()
    else
      node.right <- tree;
    update_height node

  let balance_root tree =
    let tree = balance tree in
    begin match tree with
    | Empty | Leaf _ -> ()
    | Node node -> update_height node
    end;
    tree

  let new_node k v =
    { key = k; value = v;
      left = Empty; right = Empty;
      height = 1 }

  let add =
    let rec add t added compare k v =
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
            Leaf (k, v)
          end else begin
            (* going to be the new root node *)
            let node = new_node k v in
            added := true;
            if c < 0 then set_left node t
            else set_right node t;
            Node node
          end
      | Node node ->
          let c = compare k node.key in
          if c = 0 then begin
            added := false;
            node.value <- v
          end else if c < 0 then
            set_left node (add node.left added compare k v)
          else
            set_right node (add node.right added compare k v);
          t
    in
    fun t compare ~added ~key ~data ->
      balance_root (add t added compare key data)

  let rec find t compare k =
    (* A little manual unrolling of the recursion.
       This is really worth 5% on average *)
    match t with
    | Empty -> None
    | Leaf (k', v) ->
        if compare k k' = 0 then Some v
        else None
    | Node node ->
        let c = compare k node.key in
        if c = 0 then Some node.value
        else if c < 0 then begin
          match node.left with
          | Empty -> None
          | Leaf (k', v) ->
              if compare k k' = 0 then Some v
              else None
          | Node node ->
              let c = compare k node.key in
              if c = 0 then Some node.value
              else find (if c < 0 then node.left else node.right) compare k
        end else begin
          match node.right with
          | Empty -> None
          | Leaf (k', v) ->
              if compare k k' = 0 then Some v
              else None
          | Node node ->
              let c = compare k node.key in
              if c = 0 then Some node.value
              else find (if c < 0 then node.left else node.right) compare k
        end
  ;;

  let mem t compare k = Option.is_some (find t compare k)

  let rec min_elt tree =
    match tree with
    | Empty -> Empty
    | Leaf _ -> tree
    | Node {left = Empty} -> tree
    | Node node -> min_elt node.left

  let rec remove_min_elt tree =
    match tree with
    | Empty -> assert false
    | Leaf _ -> Empty (* This must be the root *)
    | Node ({left = Empty} as node) -> node.right
    | Node ({left = Leaf _} as node) -> set_left node Empty; tree
    | Node node -> set_left node (remove_min_elt node.left); tree

  let merge =
    let do_merge t1 t2 tree node =
      set_right node (remove_min_elt t2);
      set_left node t1;
      tree
    in
    fun t1 t2 ->
    match (t1, t2) with
    | (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
        let tree = min_elt t2 in
        match tree with
        | Empty -> Empty
        | Leaf (k, v) ->
            let node = new_node k v in
            do_merge t1 t2 (Node node) node
        | Node node ->
            do_merge t1 t2 tree node

  let remove =
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
      | Node node ->
          let c = compare k node.key in
          if c = 0 then begin
            removed := true;
            merge node.left node.right
          end else if c < 0 then begin
            set_left node (remove node.left removed compare k);
            t
          end else begin
            set_right node (remove node.right removed compare k);
            t
          end
    in
    fun t ~removed ~compare k -> balance_root (remove t removed compare k)

  (* estokes: for a real tree implementation we probably want fold_right,
     that way the elements come in order, but this is a hashtbl,
     so we don't care. *)
  let rec fold t ~init ~f =
    match t with
    | Empty -> init
    | Leaf (key, data) -> f ~key ~data init
    | Node node ->
        let init = f ~key:node.key ~data:node.value init in
       fold node.right ~init:(fold node.left ~init ~f) ~f

  let iter t ~f = fold t ~init:() ~f:(fun ~key ~data () -> f ~key ~data)
end

module X = Table_new_intf

module T : X.Basic = struct
  type ('k, 'v) t = {
    mutable table : ('k, 'v) Avltree.t array;
    mutable array_length: int;
    mutable length : int;
    mutable params : X.params;
    added_or_removed : bool ref;
    hashable: 'k X.hashable;
  }


  let create ?(params = X.default_params) hashable =
    let size = Int.min (Int.max 1 params.X.initial_size) Sys.max_array_length in
    { table = Array.create size Avltree.empty;
      array_length = size;
      length = 0;
      params = params;
      added_or_removed = ref false;
      hashable = hashable; }
  ;;

  let hashable t = t.hashable
  let get_params t = t.params
  let set_params t p = t.params <- p

  let slot t key = t.hashable.X.hash key mod t.array_length

  let really_add t ~key ~data =
    let i = slot t key in
    let root = t.table.(i) in
    let new_root =
      (* The avl tree might replace the entry, in that case the table
         did not get bigger, so we should not increment length, we
         pass in the bool ref t.added so that it can tell us whether
         it added or replaced. We do it this way to avoid extra
         allocation. Since the bool is an immediate it does not go
         through the write barrier. *)
      Avltree.add root t.hashable.X.compare ~added:t.added_or_removed ~key ~data
    in
    if t.added_or_removed.contents then
      t.length <- t.length + 1;
    if not (phys_equal new_root root) then
      t.table.(i) <- new_root
  ;;

  let maybe_resize_table t =
    let should_grow =
      t.params.X.grow &&
        t.length >= t.array_length * t.params.X.load_factor
    in
    if should_grow then begin
      let new_array_length =
        Int.min (t.array_length * t.params.X.load_factor) Sys.max_array_length
      in
      if new_array_length > t.array_length then begin
        let new_table =
          Array.init new_array_length ~f:(fun _ -> Avltree.empty)
        in
        let old_table = t.table in
        t.array_length <- new_array_length;
        t.table <- new_table;
        t.length <- 0;
        for i = 0 to Array.length old_table - 1 do
          Avltree.iter old_table.(i) ~f:(fun ~key ~data ->
            really_add t ~key ~data)
        done
      end
    end
  ;;

  let add t ~key ~data =
    maybe_resize_table t;
    really_add t ~key ~data
  ;;

  let clear t =
    for i = 0 to t.array_length - 1 do
      t.table.(i) <- Avltree.empty;
    done;
    t.length <- 0
  ;;

  let find t key = Avltree.find t.table.(slot t key) t.hashable.X.compare key
  let mem t key = Avltree.mem t.table.(slot t key) t.hashable.X.compare key

  let remove t key =
    let i = slot t key in
    let root = t.table.(i) in
    let new_root =
      Avltree.remove root
        ~removed:t.added_or_removed ~compare:t.hashable.X.compare key
    in
    if not (phys_equal root new_root) then
      t.table.(i) <- new_root;
    if t.added_or_removed.contents then
      t.length <- t.length - 1
  ;;

  let length t = t.length

  let fold =
    (* this is done recursivly to avoid the write barrier in the case
       that the accumulator is a structured block, Array.fold does
       this with a for loop and a ref cell, when it is fixed, we can
       use it. *)
    let rec loop buckets i len init f =
      if i < len then
        loop buckets (i + 1) len (Avltree.fold buckets.(i) ~init ~f) f
      else
        init
    in
    fun t ~init ~f -> loop t.table 0 t.array_length init f
  ;;

  let invariant t =
    assert (Array.length t.table = t.array_length);
    let real_len = fold t ~init:0 ~f:(fun ~key:_ ~data:_ i -> i + 1) in
    assert (real_len = t.length);
    for i = 0 to t.array_length - 1 do
      Avltree.invariant t.table.(i) t.hashable.X.compare
    done
  ;;

end

include Table_new_intf.Make (T)
