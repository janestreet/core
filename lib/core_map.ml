open Sexplib
open Sexplib.Conv
open Core_map_intf
open With_return

let failwiths = Error.failwiths

module List = Core_list

open No_polymorphic_compare

let (= ) (x : int) y = Polymorphic_compare.(= ) x y
let (<>) (x : int) y = Polymorphic_compare.(<>) x y
let (< ) (x : int) y = Polymorphic_compare.(< ) x y
let (> ) (x : int) y = Polymorphic_compare.(> ) x y
let (>=) (x : int) y = Polymorphic_compare.(>=) x y

module Tree = struct
  type ('k, 'v) t =
  | Empty
  | Leaf of 'k * 'v
  | Node of ('k, 'v) t * 'k * 'v * ('k, 'v) t * int

  type ('k, 'v) tree = ('k, 'v) t

  let height = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node(_,_,_,_,h) -> h

  let create l x d r =
    let hl = height l and hr = height r in
    if hl = 0 && hr = 0 then
      Leaf (x, d)
    else
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))
  ;;

  let singleton key data = Leaf (key, data)

  let bal l x d r =
    let hl = height l in
    let hr = height r in
    if hl > hr + 2 then begin
      match l with
        Empty -> invalid_arg "Map.bal"
      | Leaf _ -> assert false (* height(Leaf) = 1 && 1 is not larger than hr + 2 *)
      | Node(ll, lv, ld, lr, _) ->
        if height ll >= height lr then
          create ll lv ld (create lr x d r)
        else begin
          match lr with
            Empty -> invalid_arg "Map.bal"
          | Leaf (lrv, lrd) ->
            create (create ll lv ld Empty) lrv lrd (create Empty x d r)
          | Node(lrl, lrv, lrd, lrr, _)->
            create (create ll lv ld lrl) lrv lrd (create lrr x d r)
        end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> invalid_arg "Map.bal"
      | Leaf _ -> assert false (* height(Leaf) = 1 && 1 is not larger than hl + 2 *)
      | Node(rl, rv, rd, rr, _) ->
        if height rr >= height rl then
          create (create l x d rl) rv rd rr
        else begin
          match rl with
            Empty -> invalid_arg "Map.bal"
          | Leaf (rlv, rld) ->
            create (create l x d Empty) rlv rld (create Empty rv rd rr)
          | Node(rll, rlv, rld, rlr, _) ->
            create (create l x d rll) rlv rld (create rlr rv rd rr)
        end
    end
    (* Inline expansion of [create] to save extracting the heights again. *)
    else if hl = 0 && hr = 0 then
        Leaf(x, d)
      else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))
  ;;

  let empty = Empty

  let is_empty = function Empty -> true | _ -> false

  let rec add t ~key:x ~data ~compare_key =
    match t with
    | Empty -> Leaf (x, data)
    | Leaf(v, d) ->
      let c = compare_key x v in
      if c = 0 then
        Leaf(x, data)
      else if c < 0 then
        Node(Leaf(x, data), v, d, Empty, 2)
      else
        Node(Empty, v, d, Leaf(x, data), 2)
    | Node(l, v, d, r, h) ->
        let c = compare_key x v in
        if c = 0 then
          Node(l, x, data, r, h)
        else if c < 0 then
          bal (add ~key:x ~data l ~compare_key) v d r
        else
          bal l v d (add ~key:x ~data r ~compare_key)
  ;;

  let rec find t x ~compare_key =
    match t with
    | Empty -> None
    | Leaf (v, d) -> if compare_key x v = 0 then Some d else None
    | Node(l, v, d, r, _) ->
      let c = compare_key x v in
      if c = 0 then Some d
      else find (if c < 0 then l else r) x ~compare_key
  ;;

  let add_multi t ~key ~data ~compare_key =
    let data = data :: Option.value (find t key ~compare_key) ~default:[] in
    add ~key ~data t ~compare_key
  ;;

  let rec find_exn t x ~compare_key =
    match find t x ~compare_key with
    | Some data -> data
    | None ->
      raise Not_found
  ;;

  let mem t x ~compare_key = Option.is_some (find t x ~compare_key)

  let rec min_elt = function
    | Empty -> None
    | Leaf (k, d) -> Some (k, d)
    | Node (Empty, k, d, _, _) -> Some (k, d)
    | Node (l, _, _, _, _) -> min_elt l
  ;;

  exception Map_min_elt_exn_of_empty_map with sexp
  exception Map_max_elt_exn_of_empty_map with sexp

  let rec min_elt_exn t =
    match min_elt t with
    | None -> raise Map_min_elt_exn_of_empty_map
    | Some v -> v
  ;;

  let rec max_elt = function
    | Empty -> None
    | Leaf (k, d) -> Some (k, d)
    | Node (_, k, d, Empty, _) -> Some (k, d)
    | Node (_, _, _, r, _) -> max_elt r
  ;;
  let rec max_elt_exn t =
    match max_elt t with
    | None -> raise Map_max_elt_exn_of_empty_map
    | Some v -> v
  ;;

  let rec remove_min_elt t =
    match t with
      Empty -> invalid_arg "Map.remove_min_elt"
    | Leaf _ -> Empty
    | Node(Empty, _, _, r, _) -> r
    | Node(l, x, d, r, _) -> bal (remove_min_elt l) x d r

  (* assumes that min <= max in the ordering given by compare_key *)
  let rec fold_range_inclusive t ~min ~max ~init ~f ~compare_key =
    match t with
    | Empty -> init
    | Leaf (k, d) ->
      if compare_key k min < 0 || compare_key k max > 0 then
        (* k < min || k > max *)
        init
      else
        f ~key:k ~data:d init
    | Node (l, k, d, r, _) ->
      let c_min = compare_key k min in
      if c_min < 0 then
        (* if k < min, then this node and its left branch are outside our range *)
        fold_range_inclusive r ~min ~max ~init ~f ~compare_key
      else if c_min = 0 then
        (* if k = min, then this node's left branch is outside our range *)
        fold_range_inclusive r ~min ~max ~init:(f ~key:k ~data:d init) ~f ~compare_key
      else (* k > min *)
        begin
          let z = fold_range_inclusive l ~min ~max ~init ~f ~compare_key in
          let c_max = compare_key k max in
          (* if k > max, we're done *)
          if c_max > 0 then z
          else
            let z = f ~key:k ~data:d z in
            (* if k = max, then we fold in this one last value and we're done *)
            if c_max = 0 then z
            else fold_range_inclusive r ~min ~max ~init:z ~f ~compare_key
        end
  ;;

  let range_to_alist t ~min ~max ~compare_key =
    List.rev
      (fold_range_inclusive t ~min ~max ~init:[] ~f:(fun ~key ~data l -> (key,data)::l)
         ~compare_key)
  ;;

  let merge t1 t2 =
    match (t1, t2) with
      (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) ->
      let (x, d) = min_elt_exn t2 in
      bal t1 x d (remove_min_elt t2)
  ;;

  let rec remove t x ~compare_key =
    match t with
    | Empty -> Empty
    | Leaf (v, _) -> if compare_key x v = 0 then Empty else t
    | Node(l, v, d, r, _) ->
      let c = compare_key x v in
      if c = 0 then
        merge l r
      else if c < 0 then
        bal (remove l x ~compare_key) v d r
      else
        bal l v d (remove r x ~compare_key)
  ;;

  (* Use exception to avoid tree-rebuild in no-op case *)
  exception Change_no_op

  let change t key f ~compare_key =
    let rec change_core t key f =
      match t with
      | Empty ->
        begin match (f None) with
          | None -> raise Change_no_op (* equivalent to returning: Empty *)
          | Some data -> Leaf(key, data)
        end
      | Leaf(v, d) ->
        let c = compare_key key v in
        if c = 0 then
          match f (Some d) with
          | None -> Empty
          | Some d' -> Leaf(v, d')
        else if c < 0 then
          bal (change_core Empty key f) v d Empty
        else
          bal Empty v d (change_core Empty key f)
      | Node(l, v, d, r, h) ->
        let c = compare_key key v in
        if c = 0 then
          begin match (f (Some d)) with
            | None -> merge l r
            | Some data -> Node(l, key, data, r, h)
          end
        else
          if c < 0 then
            bal (change_core l key f) v d r
          else
            bal l v d (change_core r key f)
    in
    try change_core t key f with Change_no_op -> t
  ;;

  let rec iter t ~f =
    match t with
    | Empty -> ()
    | Leaf(v, d) -> f ~key:v ~data:d
    | Node(l, v, d, r, _) -> iter ~f l; f ~key:v ~data:d; iter ~f r
  ;;

  let rec map t ~f =
    match t with
    | Empty               -> Empty
    | Leaf(v, d)          -> Leaf(v, f d)
    | Node(l, v, d, r, h) ->
        let l' = map ~f l in
        let d' = f d in
        let r' = map ~f r in
        Node(l', v, d', r', h)
  ;;

  let rec mapi t ~f =
    match t with
    | Empty               -> Empty
    | Leaf(v, d)          -> Leaf(v, f ~key:v ~data:d)
    | Node(l, v, d, r, h) ->
        let l' = mapi ~f l in
        let d' = f ~key:v ~data:d in
        let r' = mapi ~f r in
        Node(l', v, d', r', h)
  ;;

  let rec fold t ~init:accu ~f =
    match t with
    | Empty -> accu
    | Leaf(v, d) -> f ~key:v ~data:d accu
    | Node(l, v, d, r, _) -> fold ~f r ~init:(f ~key:v ~data:d (fold ~f l ~init:accu))
  ;;

  let rec fold_right t ~init:accu ~f =
    match t with
    | Empty -> accu
    | Leaf(v, d) -> f ~key:v ~data:d accu
    | Node(l, v, d, r, _) ->
        fold_right ~f l ~init:(f ~key:v ~data:d (fold_right ~f r ~init:accu))
  ;;

  let filter t ~f ~compare_key =
    fold ~init:Empty t ~f:(fun ~key ~data accu ->
      if f ~key ~data then add ~key ~data accu ~compare_key else accu)
  ;;

  let filter_map t ~f ~compare_key =
    fold ~init:Empty t ~f:(fun ~key ~data accu ->
      match f data with
      | None -> accu
      | Some b -> add ~key ~data:b accu ~compare_key)
  ;;

  let filter_mapi t ~f ~compare_key =
    fold ~init:Empty t ~f:(fun ~key ~data accu ->
      match f ~key ~data with
      | None -> accu
      | Some b -> add ~key ~data:b accu ~compare_key)
  ;;

  module Enum = struct
    type ('k, 'v) t =
    | End
    | More of 'k * 'v * ('k, 'v) tree * ('k, 'v) t

    let rec cons t e =
      match t with
        Empty -> e
      | Leaf (v, d) -> More(v, d, Empty, e)
      | Node(l, v, d, r, _) -> cons l (More(v, d, r, e))
    ;;

    let rec compare cmp t1 t2 ~compare_key =
      match t1, t2 with
      | (End, End) -> 0
      | (End, _)  -> -1
      | (_, End) -> 1
      | (More (v1, d1, r1, e1), More (v2, d2, r2, e2)) ->
        let c = compare_key v1 v2 in
        if c <> 0 then c else
          let c = cmp d1 d2 in
          if c <> 0 then c else compare cmp (cons r1 e1) (cons r2 e2) ~compare_key
    ;;

    let rec equal cmp t1 t2 ~compare_key =
      match t1, t2 with
      | (End, End) -> true
      | (End, _)  -> false
      | (_, End) -> false
      | (More (v1, d1, r1, e1), More (v2, d2, r2, e2)) ->
        compare_key v1 v2 = 0
        && cmp d1 d2
        && equal cmp (cons r1 e1) (cons r2 e2) ~compare_key
    ;;
  end

  let compare cmp t1 t2 ~compare_key =
    Enum.compare cmp (Enum.cons t1 Enum.End) (Enum.cons t2 Enum.End) ~compare_key
  ;;

  let equal cmp t1 t2 ~compare_key =
    Enum.equal cmp (Enum.cons t1 Enum.End) (Enum.cons t2 Enum.End) ~compare_key
  ;;

  let rec length = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node (l, _, _, r, _) -> length l + length r + 1
  ;;

  let of_alist_fold alist ~init ~f ~compare_key =
    List.fold alist ~init:empty
      ~f:(fun accum (key, data) ->
        let prev_data =
          match find accum key ~compare_key with
          | None -> init
          | Some prev -> prev
        in
        let data = f prev_data data in
        add accum ~key ~data ~compare_key)
  ;;

  let keys t = fold_right ~f:(fun ~key ~data:_ list -> key::list) t ~init:[]
  let has_key = mem
  let data t = fold_right ~f:(fun ~key:_ ~data list -> data::list) t ~init:[]

  let of_alist alist ~compare_key =
    with_return (fun r ->
      let map =
        List.fold alist ~init:empty ~f:(fun t (key,data) ->
          if mem t key ~compare_key then r.return (`Duplicate_key key)
          else add ~key ~data t ~compare_key)
      in
      `Ok map)

  let for_all t ~f =
    with_return (fun r ->
      iter t ~f:(fun ~key:_ ~data -> if not (f data) then r.return false);
      true)

  let exists t ~f =
    with_return (fun r ->
      iter t ~f:(fun ~key:_ ~data -> if f data then r.return true);
      false)

  let of_alist_exn alist ~comparator =
    match of_alist alist ~compare_key:comparator.Comparator.compare with
    | `Ok x -> x
    | `Duplicate_key key ->
      let sexp_of_key = comparator.Comparator.sexp_of_t in
      failwiths "Map.of_alist_exn: duplicate key" key <:sexp_of< key >>
  ;;

  let of_alist_multi alist ~compare_key =
    let alist = List.rev alist in
    of_alist_fold alist ~init:[] ~f:(fun l x -> x :: l) ~compare_key
  ;;

  let to_alist t =
    fold_right t ~init:[] ~f:(fun ~key ~data x -> (key,data)::x)
  ;;

  let merge t1 t2 ~f ~compare_key =
    let all_keys =
      Core_list.dedup ~compare:compare_key (Core_list.append (keys t1) (keys t2))
    in
    List.fold ~init:empty all_keys
      ~f:(fun t key ->
        let z =
          match find t1 key ~compare_key, find t2 key ~compare_key with
          | None, None -> assert false
          | None, Some v2 -> `Right v2
          | Some v1, None -> `Left v1
          | Some v1, Some v2 -> `Both (v1, v2)
        in
        match f ~key z with
        | None -> t
        | Some data -> add ~key ~data t ~compare_key)
  ;;

  let rec next_key t k ~compare_key =
    match t with
    | Empty -> None
    | Leaf (k', v') ->
      if compare_key k' k > 0 then
        Some (k', v')
      else
        None
    | Node (l, k', v', r, _) ->
      let c = compare_key k' k in
      if c = 0 then min_elt r
      else if c < 0 then next_key r k ~compare_key
      else begin match next_key l k ~compare_key with
      | None -> Some (k', v')
      | Some answer -> Some answer
      end
  ;;

  let rec prev_key t k ~compare_key =
    match t with
    | Empty -> None
    | Leaf (k', v') ->
      if compare_key k' k < 0 then
        Some (k', v')
      else
        None
    | Node (l, k', v', r, _) ->
      let c = compare_key k' k in
      if c = 0 then max_elt l
      else if c > 0 then prev_key l k ~compare_key
      else begin match prev_key r k ~compare_key with
      | None -> Some (k', v')
      | Some answer -> Some answer
      end
  ;;

  let rec rank t k ~compare_key =
    match t with
    | Empty -> None
    | Leaf (k', _) -> if compare_key k' k = 0 then Some 0 else None
    | Node (l, k', _, r, _) ->
      let c = compare_key k' k in
      if c = 0
      then Some (length l)
      else if c > 0
      then rank l k ~compare_key
      else Option.map (rank r k ~compare_key) ~f:(fun rank -> rank + 1 + (length l))
  ;;

  let t_of_sexp key_of_sexp value_of_sexp sexp ~comparator =
    let alist = <:of_sexp< (key * value) list >> sexp in
    of_alist_exn alist ~comparator
  ;;

  let sexp_of_t sexp_of_key sexp_of_value t =
    let f ~key ~data acc = Type.List [sexp_of_key key; sexp_of_value data] :: acc in
    Type.List (fold_right ~f t ~init:[])
  ;;
end

type ('k, 'v, 'comparator) t =
  { tree : ('k, 'v) Tree.t;
    comparator : ('k, 'comparator) Comparator.t;
  }

type ('k, 'v, 'comparator) tree = ('k, 'v) Tree.t

let compare_key t = t.comparator.Comparator.compare

type ('a, 'b, 'c) map = ('a, 'b, 'c) t


let like { tree = _; comparator } tree = { tree; comparator }

module Accessors = struct
  let to_tree t = t.tree
  let is_empty t = Tree.is_empty t.tree
  let length t = Tree.length t.tree
  let add t ~key ~data = like t (Tree.add t.tree ~key ~data ~compare_key:(compare_key t))
  let add_multi t ~key ~data =
    like t (Tree.add_multi t.tree ~key ~data ~compare_key:(compare_key t))
  ;;
  let change t key f = like t (Tree.change t.tree key f ~compare_key:(compare_key t))
  let find_exn t key = Tree.find_exn t.tree key ~compare_key:(compare_key t)
  let find t key = Tree.find t.tree key ~compare_key:(compare_key t)
  let remove t key = like t (Tree.remove t.tree key ~compare_key:(compare_key t))
  let mem t key = Tree.mem t.tree key ~compare_key:(compare_key t)
  let iter t ~f = Tree.iter t.tree ~f
  let map t ~f = like t (Tree.map t.tree ~f)
  let mapi t ~f = like t (Tree.mapi t.tree ~f)
  let fold t ~init ~f = Tree.fold t.tree ~f ~init
  let fold_right t ~init ~f = Tree.fold_right t.tree ~f ~init
  let filter t ~f = like t (Tree.filter t.tree ~f ~compare_key:(compare_key t))
  let filter_map t ~f = like t (Tree.filter_map t.tree ~f ~compare_key:(compare_key t))
  let filter_mapi t ~f = like t (Tree.filter_mapi t.tree ~f ~compare_key:(compare_key t))
  let compare f t1 t2 = Tree.compare f t1.tree t2.tree ~compare_key:(compare_key t1)
  let equal f t1 t2 = Tree.equal f t1.tree t2.tree ~compare_key:(compare_key t1)
  let keys t = Tree.keys t.tree
  let data t = Tree.data t.tree
  let to_alist t = Tree.to_alist t.tree
  let merge t1 t2 ~f =
    like t1 (Tree.merge t1.tree t2.tree ~f ~compare_key:(compare_key t1));
  ;;
  let min_elt t = Tree.min_elt t.tree
  let min_elt_exn t = Tree.min_elt_exn t.tree
  let max_elt t = Tree.max_elt t.tree
  let max_elt_exn t = Tree.max_elt_exn t.tree
  let for_all t ~f = Tree.for_all t.tree ~f
  let exists t ~f = Tree.exists t.tree ~f
  let fold_range_inclusive t ~min ~max ~init ~f =
    Tree.fold_range_inclusive t.tree ~min ~max ~init ~f ~compare_key:(compare_key t)
  ;;
  let range_to_alist t ~min ~max =
    Tree.range_to_alist t.tree ~min ~max ~compare_key:(compare_key t)
  ;;
  let prev_key t key = Tree.prev_key t.tree key ~compare_key:(compare_key t)
  let next_key t key = Tree.next_key t.tree key ~compare_key:(compare_key t)
  let rank t key = Tree.rank t.tree key ~compare_key:(compare_key t)
  let sexp_of_t sexp_of_k sexp_of_v t = Tree.sexp_of_t sexp_of_k sexp_of_v t.tree
end

type ('a, 'b, 'c) create_options = ('a, 'b, 'c) create_options_with_comparator

let of_tree ~comparator tree = { tree; comparator }

let empty ~comparator = { tree = Tree.empty; comparator }

let singleton ~comparator k v = { comparator; tree = Tree.singleton k v }

let of_alist ~comparator alist =
  match Tree.of_alist alist ~compare_key:comparator.Comparator.compare with
  | `Ok tree -> `Ok { comparator; tree }
  | `Duplicate_key _ as z -> z
;;

let of_alist_exn ~comparator alist =
  { comparator; tree = Tree.of_alist_exn alist ~comparator }
;;

let of_alist_multi ~comparator alist =
  { comparator;
    tree = Tree.of_alist_multi alist ~compare_key:comparator.Comparator.compare;
  }
;;

let of_alist_fold ~comparator alist ~init ~f =
  { comparator;
    tree = Tree.of_alist_fold alist ~init ~f ~compare_key:comparator.Comparator.compare;
  }
;;

let t_of_sexp ~comparator k_of_sexp v_of_sexp sexp =
  { comparator; tree = Tree.t_of_sexp k_of_sexp v_of_sexp sexp ~comparator }
;;

module Creators (Key : Comparator.S1) : sig

  type ('a, 'b, 'c) t_ = ('a Key.t, 'b, Key.comparator) t
  type ('a, 'b, 'c) tree = ('a, 'b) Tree.t
  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) create_options_without_comparator

  val t_of_sexp : (Sexp.t -> 'a Key.t) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b, _) t_

  include Creators
    with type ('a, 'b, 'c) t := ('a, 'b, 'c) t_
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree
    with type 'a key := 'a Key.t
    with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) create_options

end = struct

  type ('a, 'b, 'c) create_options = ('a, 'b, 'c) create_options_without_comparator

  let comparator = Key.comparator

  type ('a, 'b, 'c) t_ = ('a Key.t, 'b, Key.comparator) t

  type ('a, 'b, 'c) tree = ('a, 'b) Tree.t

  let empty = { tree = Tree.empty; comparator }

  let of_tree tree = of_tree ~comparator tree

  let singleton k v = singleton ~comparator k v

  let of_alist alist = of_alist ~comparator alist

  let of_alist_exn alist = of_alist_exn ~comparator alist

  let of_alist_multi alist = of_alist_multi ~comparator alist

  let of_alist_fold alist ~init ~f = of_alist_fold ~comparator alist ~init ~f

  let t_of_sexp k_of_sexp v_of_sexp sexp = t_of_sexp ~comparator k_of_sexp v_of_sexp sexp

end

type 'a key = 'a

include Accessors

module Poly_creators = Creators (Comparator.Poly)

module Poly = struct
  include Poly_creators

  type ('k, 'v) t = ('k, 'v, Comparator.Poly.comparator) map
  type 'a key = 'a

  include Accessors

  let sexp_of_t = sexp_of_t

  include Bin_prot.Utils.Make_iterable_binable2 (struct
    type ('a, 'b) acc = ('a , 'b) t
    type ('a, 'b) t = ('a, 'b) acc
    type ('a, 'b) el = 'a * 'b with bin_io
    let module_name = Some "Core.Core_map"
    let length = length
    let iter t ~f = iter t ~f:(fun ~key ~data -> f (key, data))
    let init _n = empty
    let insert acc (key, data) _i =
      if mem acc key
      then failwith "Map.bin_read_t_: duplicate element in map"
      else add ~key ~data acc
    ;;
    let finish t = t
  end)

end

module type Key = Key
module type Key_binable = Key_binable

module type S = S
   with type ('a, 'b, 'c) map := ('a, 'b, 'c) t
   with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree

module type S_binable = S_binable
   with type ('a, 'b, 'c) map := ('a, 'b, 'c) t
   with type ('a, 'b, 'c) tree := ('a, 'b, 'c) tree

module Make_using_comparator (Key : Comparator.S) = struct

  module Key = Key

  include Creators (Comparator.S_to_S1 (Key))

  type key = Key.t
  type ('a, 'b, 'c) map = ('a, 'b, 'c) t
  type 'v t = (key, 'v, Key.comparator) map
  type 'a key_ = key

  include Accessors

  let sexp_of_t sexp_of_v t = sexp_of_t Key.sexp_of_t sexp_of_v t

  let t_of_sexp v_of_sexp sexp = t_of_sexp Key.t_of_sexp v_of_sexp sexp

end

module Make (Key : Comparator.Pre) = Make_using_comparator (Comparator.Make (Key))

module Make_binable_using_comparator (Key' : Comparator.S_binable) = struct

  include Make_using_comparator (Key')

  include Bin_prot.Utils.Make_iterable_binable1 (struct
    type 'v acc = 'v t
    type 'v t = 'v acc
    type 'v el = Key'.t * 'v with bin_io
    let module_name = Some "Core.Core_map"
    let length = length
    let iter t ~f = iter t ~f:(fun ~key ~data -> f (key, data))
    let init _n = empty
    let insert acc (key, data) _i =
      if mem acc key
      then failwith "Map.bin_read_t_: duplicate element in map"
      else add ~key ~data acc
    ;;
    let finish t = t
  end)

end

module Make_binable (Key : Comparator.Pre_binable) =
  Make_binable_using_comparator (Comparator.Make_binable (Key))
