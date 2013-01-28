(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)


(* $Id: pSet.ml,v 1.2 2003/09/10 15:40:01 sandor Exp $ *)

(* Sets over ordered types *)

open Sexplib
open T
open Core_set_intf

module Array = Core_array
module List = Core_list

open No_polymorphic_compare

let (= ) (x : int) y = Polymorphic_compare.(= ) x y
let (<>) (x : int) y = Polymorphic_compare.(<>) x y
let (< ) (x : int) y = Polymorphic_compare.(< ) x y
let (> ) (x : int) y = Polymorphic_compare.(> ) x y
let (>=) (x : int) y = Polymorphic_compare.(>=) x y


module type Elt = Elt
module type Elt_binable = Elt_binable

module Tree = struct
  type 'a t =
  | Empty
  (* (Leaf x) is the same as (Node (Empty, x, Empty, 1, 1)) but uses less space. *)
  | Leaf of 'a
  (* first int is height, second is sub-tree size *)
  | Node of 'a t * 'a * 'a t * int * int

  type 'a tree = 'a t

  (* Sets are represented by balanced binary trees (the heights of the children differ by
     at most 2. *)
  let height = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node(_, _, _, h, _) -> h
  ;;

  let length = function
    | Empty -> 0
    | Leaf _ -> 1
    | Node(_, _, _, _, s) -> s
  ;;

  let is_empty = function Empty -> true | Leaf _ | Node _ -> false

  (* Creates a new node with left son l, value v and right son r.
     We must have all elements of l < v < all elements of r.
     l and r must be balanced and | height l - height r | <= 2.
     Inline expansion of height for better speed. *)

  let create l v r =
    let hl = match l with Empty -> 0 | Leaf _ -> 1 | Node(_,_,_,h,_) -> h in
    let hr = match r with Empty -> 0 | Leaf _ -> 1 | Node(_,_,_,h,_) -> h in
    let h = if hl >= hr then hl + 1 else hr + 1 in
    if h = 1
    then Leaf v
    else begin
      let sl = match l with Empty -> 0 | Leaf _ -> 1 | Node(_,_,_,_,s) -> s in
      let sr = match r with Empty -> 0 | Leaf _ -> 1 | Node(_,_,_,_,s) -> s in
      Node (l, v, r, h, sl + sr + 1)
    end

  (* Same as create, but performs one step of rebalancing if necessary.
     Assumes l and r balanced and | height l - height r | <= 3.
     Inline expansion of create for better speed in the most frequent case
     where no rebalancing is required. *)

  let bal l v r =
    let hl = match l with Empty -> 0 | Leaf _ -> 1 | Node(_,_,_,h,_) -> h in
    let hr = match r with Empty -> 0 | Leaf _ -> 1 | Node(_,_,_,h,_) -> h in
    if hl > hr + 2 then begin
      match l with
      | Empty -> assert false
      | Leaf _ -> assert false          (* because h(l)>h(r)+2 and h(leaf)=1 *)
      | Node (ll, lv, lr, _, _) ->
        if height ll >= height lr then
          create ll lv (create lr v r)
        else begin
          match lr with
          | Empty -> assert false
          | Leaf lrv ->
            assert (is_empty ll);
            create (create ll lv Empty) lrv (create Empty v r)
          | Node(lrl, lrv, lrr, _, _)->
            create (create ll lv lrl) lrv (create lrr v r)
        end
    end else if hr > hl + 2 then begin
      match r with
        Empty -> assert false
      | Leaf rv -> create (create l v Empty) rv Empty
      | Node(rl, rv, rr, _, _) ->
        if height rr >= height rl then
          create (create l v rl) rv rr
        else begin
          match rl with
            Empty -> assert false
          | Leaf rlv ->
            assert (is_empty rr);
            create (create l v Empty) rlv (create Empty rv rr)
          | Node(rll, rlv, rlr, _, _) ->
            create (create l v rll) rlv (create rlr rv rr)
        end
    end else begin
      let h = if hl >= hr then hl + 1 else hr + 1 in
      let sl = match l with Empty -> 0 | Leaf _ -> 1 | Node(_,_,_,_,s) -> s in
      let sr = match r with Empty -> 0 | Leaf _ -> 1 | Node(_,_,_,_,s) -> s in
      if h = 1
      then Leaf v
      else Node (l, v, r, h, sl + sr + 1)
    end

  (* Insertion of one element *)

  let rec add t x ~compare_elt =
    match t with
    | Empty -> Leaf x
    | (Leaf v) as t ->
      let c = compare_elt x v in
      if c = 0 then t else
        if c < 0 then bal (Leaf x) v Empty else bal Empty v (Leaf x)
    | Node(l, v, r, _, _) as t ->
      let c = compare_elt x v in
      if c = 0 then t else
        if c < 0 then bal (add l x ~compare_elt) v r else bal l v (add r x ~compare_elt)
  ;;

  (* Same as create and bal, but no assumptions are made on the relative heights of l and
     r. *)
  let rec join l v r ~compare_elt =
    match (l, r) with
    | (Empty, _) -> add r v ~compare_elt
    | (_, Empty) -> add l v ~compare_elt
    | (Leaf lv, _) -> add (add r v ~compare_elt) lv ~compare_elt
    | (_, Leaf rv) -> add (add l v ~compare_elt) rv ~compare_elt
    | (Node (ll, lv, lr, lh, _), Node (rl, rv, rr, rh, _)) ->
      if lh > rh + 2 then bal ll lv (join lr v r ~compare_elt) else
        if rh > lh + 2 then bal (join l v rl ~compare_elt) rv rr else
          create l v r
  ;;

  (* Smallest and greatest element of a set *)
  let rec min_elt = function
    | Empty -> None
    | Leaf v
    | Node(Empty, v, _, _, _) -> Some v
    | Node(l, _, _, _, _) -> min_elt l
  ;;

  exception Set_min_elt_exn_of_empty_set with sexp
  exception Set_max_elt_exn_of_empty_set with sexp

  let rec min_elt_exn t =
    match min_elt t with
    | None -> raise Set_min_elt_exn_of_empty_set
    | Some v -> v
  ;;

  let fold_until t ~init ~f =
    let rec fold_until_helper ~f t acc =
      match t with
      | Empty -> `Continue acc
      | Leaf value -> f acc value
      | Node(left, value, right, _, _) ->
          match fold_until_helper ~f left acc with
          | `Stop _a as x -> x
          | `Continue acc ->
              match f acc value with
              | `Stop _a as x -> x
              | `Continue a -> fold_until_helper ~f right a
    in
    match fold_until_helper ~f t init with
    | `Stop a -> a
    (* `Continue case is reached if Set is exhausted without `Stop being returned.
       This will happen if t is empty, for example. *)
    | `Continue a -> a
  ;;

  let rec max_elt = function
    | Empty -> None
    | Leaf v
    | Node(_, v, Empty, _, _) -> Some v
    | Node(_, _, r, _, _) -> max_elt r
  ;;

  let rec max_elt_exn t =
    match max_elt t with
    | None -> raise Set_max_elt_exn_of_empty_set
    | Some v -> v
  ;;

  (* Remove the smallest element of the given set *)

  let rec remove_min_elt = function
    | Empty -> invalid_arg "Set.remove_min_elt"
    | Leaf _ -> Empty
    | Node(Empty, _, r, _, _) -> r
    | Node(l, v, r, _, _) -> bal (remove_min_elt l) v r
  ;;

  (* Merge two trees l and r into one.  All elements of l must precede the elements of r.
     Assume | height l - height r | <= 2. *)
  let merge t1 t2 =
    match (t1, t2) with
    | (Empty, t) -> t
    | (t, Empty) -> t
    | (_, _) -> bal t1 (min_elt_exn t2) (remove_min_elt t2)
  ;;

  (* Merge two trees l and r into one.  All elements of l must precede the elements of r.
     No assumption on the heights of l and r. *)
  let concat t1 t2 ~compare_elt =
    match (t1, t2) with
    | Empty, t | t, Empty -> t
    | (_, _) -> join t1 (min_elt_exn t2) (remove_min_elt t2) ~compare_elt
  ;;

  (* Splitting.  split x s returns a triple (l, present, r) where
     - l is the set of elements of s that are < x
     - r is the set of elements of s that are > x
     - present is false if s contains no element equal to x,
     or true if s contains an element equal to x. *)

  let split t x ~compare_elt =
    let rec split t =
      match t with
      | Empty -> (Empty, false, Empty)
      | Leaf v ->
        let c = compare_elt x v in
        if c = 0 then (Empty, true, Empty)
        else if c < 0 then (Empty, false, Leaf v)
        else (Leaf v, false, Empty)
      | Node (l, v, r, _, _) ->
        let c = compare_elt x v in
        if c = 0 then (l, true, r)
        else if c < 0 then
          let (ll, pres, rl) = split l in
          (ll, pres, join rl v r ~compare_elt)
        else
          let (lr, pres, rr) = split r in
          (join l v lr ~compare_elt, pres, rr)
    in
    split t
  ;;

  (* Implementation of the set operations *)

  let empty = Empty

  let rec mem t x ~compare_elt =
    match t with
    | Empty -> false
    | Leaf v ->
      let c = compare_elt x v in
      c = 0
    | Node(l, v, r, _, _) ->
      let c = compare_elt x v in
      c = 0 || mem (if c < 0 then l else r) x ~compare_elt
  ;;

  let singleton x = Leaf x

  let rec remove t x ~compare_elt =
    match t with
    | Empty -> Empty
    | Leaf v ->
      let c = compare_elt x v in
      if c = 0 then Empty else t
    | Node(l, v, r, _, _) ->
      let c = compare_elt x v in
      if c = 0
      then merge l r
      else
        if c < 0
        then bal (remove l x ~compare_elt) v r
        else bal l v (remove r x ~compare_elt)
  ;;

  let rec remove_index t i ~compare_elt =
    match t with
    | Empty -> Empty
    | Leaf _ -> if i = 0 then Empty else t
    | Node (l, v, r, _, _) ->
      let l_size = length l in
      let c = Pervasives.compare i l_size in
      if c = 0
      then merge l r
      else if c < 0
      then bal (remove_index l i ~compare_elt) v r
      else bal l v (remove_index r (i - l_size - 1) ~compare_elt)
  ;;

  let rec union s1 s2 ~compare_elt =
    let rec union s1 s2 =
      match s1, s2 with
      | Empty, t | t, Empty -> t
      | Leaf v1, _ -> union (Node(Empty, v1, Empty, 1, 1)) s2
      | _, Leaf v2 -> union s1 (Node(Empty, v2, Empty, 1, 1))
      | (Node(l1, v1, r1, h1, _), Node(l2, v2, r2, h2, _)) ->
        if h1 >= h2
        then
          if h2 = 1
          then add s1 v2 ~compare_elt
          else begin
            let (l2, _, r2) = split s2 v1 ~compare_elt in
            join (union l1 l2) v1 (union r1 r2) ~compare_elt
          end
        else
          if h1 = 1
          then add s2 v1 ~compare_elt
          else begin
            let (l1, _, r1) = split s1 v2 ~compare_elt in
            join (union l1 l2) v2 (union r1 r2) ~compare_elt
          end
    in
    union s1 s2
  ;;

  let inter s1 s2 ~compare_elt =
    let rec inter s1 s2 =
      match s1, s2 with
      | Empty, _ | _, Empty -> Empty
      | (Leaf v1, t2) -> inter (Node(Empty,v1,Empty,1,1)) t2
      | (Node (l1, v1, r1, _, _), t2) ->
        match split t2 v1 ~compare_elt with
        | (l2, false, r2) -> concat (inter l1 l2) (inter r1 r2) ~compare_elt
        | (l2, true, r2) -> join (inter l1 l2) v1 (inter r1 r2) ~compare_elt
    in
    inter s1 s2
  ;;

  let diff s1 s2 ~compare_elt =
    let rec diff s1 s2 =
      match s1, s2 with
      | (Empty, _) -> Empty
      | (t1, Empty) -> t1
      | (Leaf v1, t2) -> diff (Node(Empty, v1, Empty, 1, 1)) t2
      | (Node(l1, v1, r1, _, _), t2) ->
        match split t2 v1 ~compare_elt with
        | (l2, false, r2) ->
          join (diff l1 l2) v1 (diff r1 r2) ~compare_elt
        | (l2, true, r2) ->
          concat (diff l1 l2) (diff r1 r2) ~compare_elt
    in
    diff s1 s2
  ;;

  module Enum = struct
    type 'a t = End | More of 'a * 'a tree * 'a t

    let rec cons s e =
      match s with
      | Empty -> e
      | Leaf v -> (More (v, Empty, e))
      | Node (l, v, r, _, _) -> cons l (More (v, r, e))
    ;;

    let rec compare e1 e2 ~compare_elt =
      match (e1, e2) with
      | End, End -> 0
      | End, _  -> -1
      | _, End -> 1
      | More (v1, r1, e1), More (v2, r2, e2) ->
        let c = compare_elt v1 v2 in
        if c <> 0
        then c
        else compare (cons r1 e1) (cons r2 e2) ~compare_elt
    ;;

    let of_set s = cons s End
  end

  let compare s1 s2 ~compare_elt =
    Enum.compare (Enum.of_set s1) (Enum.of_set s2) ~compare_elt
  ;;

  let equal s1 s2 ~compare_elt = compare s1 s2 ~compare_elt = 0

  let subset s1 s2 ~compare_elt =
    let rec subset s1 s2 =
      match s1, s2 with
      | Empty, _ -> true
      | _, Empty -> false
      | Leaf v1, t2 -> subset (Node (Empty, v1, Empty, 1, 1)) t2
      | t1, Leaf v2 -> subset t1 (Node (Empty, v2, Empty, 1, 1))
      | Node (l1, v1, r1, _, _), (Node (l2, v2, r2, _, _) as t2) ->
        let c = compare_elt v1 v2 in
        if c = 0
        then subset l1 l2 && subset r1 r2
        (* Note that height and size don't matter here. *)
        else if c < 0 then
          subset (Node (l1, v1, Empty, 0, 0)) l2 && subset r1 t2
        else
          subset (Node (Empty, v1, r1, 0, 0)) r2 && subset l1 t2
    in
    subset s1 s2
  ;;

  let iter t ~f =
    let rec iter = function
      | Empty -> ()
      | Leaf v -> f v
      | Node(l, v, r, _, _) -> iter l; f v; iter r
    in
    iter t
  ;;

  let rec fold s ~init:accu ~f =
    match s with
    | Empty -> accu
    | Leaf v -> f accu v
    | Node(l, v, r, _, _) -> fold ~f r ~init:(f (fold ~f l ~init:accu) v)
  ;;

  let count t ~f = Container.fold_count fold t ~f

  let rec fold_right s ~init:accu ~f =
    match s with
    | Empty -> accu
    | Leaf v -> f v accu
    | Node(l, v, r, _, _) -> fold_right ~f l ~init:(f v (fold_right ~f r ~init:accu))
  ;;

  let rec for_all t ~f:p = match t with
    | Empty -> true
    | Leaf v -> p v
    | Node(l, v, r, _, _) -> p v && for_all ~f:p l && for_all ~f:p r
  ;;

  let rec exists t ~f:p = match t with
    | Empty -> false
    | Leaf v -> p v
    | Node(l, v, r, _, _) -> p v || exists ~f:p l || exists ~f:p r
  ;;

  let filter s ~f:p ~compare_elt =
    let rec filt accu = function
      | Empty -> accu
      | Leaf v -> if p v then add accu v ~compare_elt else accu
      | Node(l, v, r, _, _) ->
        filt (filt (if p v then add accu v ~compare_elt else accu) l) r
    in
    filt Empty s
  ;;

  let filter_map s ~f:p ~compare_elt =
    let rec filt accu = function
      | Empty -> accu
      | Leaf v ->
        (match p v with
        | None -> accu
        | Some v -> add accu v ~compare_elt)
      | Node(l, v, r, _, _) ->
          filt (filt (match p v with
                      | None -> accu
                      | Some v -> add accu v ~compare_elt) l) r
    in
    filt Empty s
  ;;

  let partition_tf s ~f:p ~compare_elt =
    let rec part ((t, f) as accu) = function
      | Empty -> accu
      | Leaf v -> if p v then (add t v ~compare_elt, f) else (t, add f v ~compare_elt)
      | Node(l, v, r, _, _) ->
        part (part (
          if p v
          then (add t v ~compare_elt, f)
          else (t, add f v ~compare_elt)) l) r
    in
    part (Empty, Empty) s
  ;;

  let rec elements_aux accu = function
    | Empty -> accu
    | Leaf v -> v :: accu
    | Node(l, v, r, _, _) -> elements_aux (v :: elements_aux accu r) l
  ;;

  let elements s = elements_aux [] s

  let choose t =
    match t with
    | Empty -> None
    | Leaf v -> Some v
    | Node (_, v, _, _, _) -> Some v
  ;;

  let choose_exn t =
    match choose t with
    | None ->
      raise Not_found
    | Some v -> v
  ;;

  let of_list lst ~compare_elt =
    List.fold lst ~init:empty ~f:(fun t x -> add t x ~compare_elt)
  ;;

  let to_list s = elements s

  let of_array a ~compare_elt =
    Array.fold a ~init:empty ~f:(fun t x -> add t x ~compare_elt)
  ;;

  (* faster but equivalent to [Array.of_list (to_list t)] *)
  let to_array = function
    | Empty -> [||]
    | Leaf v -> [| v |]
    | Node (l, v, r, _, s) ->
        let res = Array.create s v in
        let pos_ref = ref 0 in
        let rec loop = function
          (* Invariant: on entry and on exit to [loop], !pos_ref is the next
             available cell in the array. *)
          | Empty -> ()
          | Leaf v ->
            res.(!pos_ref) <- v;
            incr pos_ref
          | Node (l, v, r, _, _) ->
            loop l;
            res.(!pos_ref) <- v;
            incr pos_ref;
            loop r
        in
        loop l;
        (* res.(!pos_ref) is already initialized (by Array.create above). *)
        incr pos_ref;
        loop r;
        res
  ;;

  let map t ~f ~compare_elt = fold t ~init:empty ~f:(fun t x -> add t (f x) ~compare_elt)

  let group_by set ~equiv ~compare_elt =
    let rec loop set equiv_classes =
      if is_empty set
      then equiv_classes
      else
        let x = choose_exn set in
        let equiv_x, not_equiv_x =
          partition_tf set ~f:(fun elt -> x == elt || equiv x elt) ~compare_elt
        in
        loop not_equiv_x (equiv_x :: equiv_classes)
    in
    loop set []
  ;;

  let rec find t ~f =
    match t with
    | Empty -> None
    | Leaf v -> if f v then Some v else None
    | Node(l, v, r, _, _) ->
      if f v then Some v
      else
        match find l ~f with
        | None -> find r ~f
        | Some _ as r -> r
  ;;

  let rec find_map t ~f =
    match t with
    | Empty -> None
    | Leaf v -> f v
    | Node(l, v, r, _, _) ->
      match f v with
      | Some _ as r -> r
      | None ->
        match find_map l ~f with
        | None -> find_map r ~f
        | Some _ as r -> r
  ;;

  let find_exn t ~f =
    match find t ~f with
    | None -> failwith "Set.find_exn failed to find a matching element"
    | Some e -> e
  ;;

  let rec find_index t i =
    match t with
    | Empty -> None
    | Leaf v -> if i = 0 then Some v else None
    | Node (l, v, r, _, s) ->
      if i >= s then None
      else begin
        let l_size = length l in
        let c = Pervasives.compare i l_size in
        if c < 0 then find_index l i
        else if c = 0 then Some v
        else find_index r (i - l_size - 1)
      end
  ;;

  let setify l ~compare_elt = to_list (of_list l ~compare_elt)

  let stable_dedup_list xs ~compare_elt =
    let rec loop xs leftovers already_seen =
      match xs with
      | [] -> List.rev leftovers
      | hd :: tl ->
        if mem already_seen hd ~compare_elt
        then loop tl leftovers already_seen
        else loop tl (hd :: leftovers) (add already_seen hd ~compare_elt)
    in
    loop xs [] empty
  ;;

  open Sexplib

  let t_of_sexp a_of_sexp sexp ~compare_elt =
    match sexp with
    | Type.List lst ->
      let elt_lst = List.map lst ~f:a_of_sexp in
      let set = of_list elt_lst ~compare_elt in
      if length set = List.length lst then
        set
      else
        let compare (_, e) (_, e') = compare_elt e e' in
        begin match List.find_a_dup (List.zip_exn lst elt_lst) ~compare with
        | None -> assert false
        | Some (el_sexp, _) ->
          Conv.of_sexp_error "Set.t_of_sexp: duplicate element in set" el_sexp
        end
    | sexp -> Conv.of_sexp_error "Set.t_of_sexp: list needed" sexp
  ;;

  let sexp_of_t sexp_of_a t =
    Type.List (fold_right t ~init:[] ~f:(fun el acc -> sexp_of_a el :: acc))
  ;;
end

type ('a, 'comparator) t =
  { tree : 'a Tree.t;
    comparator : ('a, 'comparator) Comparator.t;
  }

type ('a, 'comparator) set = ('a, 'comparator) t

type ('a, 'comparator) tree = 'a Tree.t

let like { tree = _; comparator } tree = { tree; comparator }

let compare_elt t = t.comparator.Comparator.compare


module Accessors = struct
  let to_tree t = t.tree
  let length      t = Tree.length      t.tree
  let is_empty    t = Tree.is_empty    t.tree
  let elements    t = Tree.elements    t.tree
  let min_elt     t = Tree.min_elt     t.tree
  let min_elt_exn t = Tree.min_elt_exn t.tree
  let max_elt     t = Tree.max_elt     t.tree
  let max_elt_exn t = Tree.max_elt_exn t.tree
  let choose      t = Tree.choose      t.tree
  let choose_exn  t = Tree.choose_exn  t.tree
  let to_list     t = Tree.to_list     t.tree
  let to_array    t = Tree.to_array    t.tree
  let fold       t ~init ~f = Tree.fold       t.tree ~init ~f
  let fold_until t ~init ~f = Tree.fold_until t.tree ~init ~f
  let fold_right t ~init ~f = Tree.fold_right t.tree ~init ~f
  let iter     t ~f = Tree.iter     t.tree ~f
  let exists   t ~f = Tree.exists   t.tree ~f
  let for_all  t ~f = Tree.for_all  t.tree ~f
  let count    t ~f = Tree.count    t.tree ~f
  let find     t ~f = Tree.find     t.tree ~f
  let find_exn t ~f = Tree.find_exn t.tree ~f
  let find_map t ~f = Tree.find_map t.tree ~f
  let mem t a = Tree.mem t.tree a ~compare_elt:(compare_elt t)
  let filter     t ~f = like t (Tree.filter     t.tree ~f ~compare_elt:(compare_elt t))
  let add    t a = like t (Tree.add    t.tree a ~compare_elt:(compare_elt t))
  let remove t a = like t (Tree.remove t.tree a ~compare_elt:(compare_elt t))
  let union t1 t2 = like t1 (Tree.union t1.tree t2.tree ~compare_elt:(compare_elt t1))
  let inter t1 t2 = like t1 (Tree.inter t1.tree t2.tree ~compare_elt:(compare_elt t1))
  let diff  t1 t2 = like t1 (Tree.diff  t1.tree t2.tree ~compare_elt:(compare_elt t1))
  let compare t1 t2 = Tree.compare t1.tree t2.tree ~compare_elt:(compare_elt t1)
  let equal t1 t2 = Tree.equal t1.tree t2.tree ~compare_elt:(compare_elt t1)
  let subset t1 t2 = Tree.subset  t1.tree t2.tree ~compare_elt:(compare_elt t1)
  let partition_tf t ~f =
    let (tree_t, tree_f) = Tree.partition_tf t.tree ~f ~compare_elt:(compare_elt t) in
    like t tree_t, like t tree_f
  ;;
  let split t a =
    let (tree1, b, tree2) = Tree.split t.tree a ~compare_elt:(compare_elt t) in
    like t tree1, b, like t tree2
  ;;
  let group_by t ~equiv =
    List.map (Tree.group_by t.tree ~equiv ~compare_elt:(compare_elt t)) ~f:(like t)
  ;;
  let find_index t i = Tree.find_index t.tree i
  let remove_index t i = like t (Tree.remove_index t.tree i ~compare_elt:(compare_elt t))
  let sexp_of_t sexp_of_a t = Tree.sexp_of_t sexp_of_a t.tree
end

let of_tree ~comparator tree = { comparator; tree }

let empty ~comparator = { comparator; tree = Tree.empty }

let singleton ~comparator e = { comparator; tree = Tree.singleton e }

let union_list ~comparator l =
  List.fold l ~init:(empty ~comparator) ~f:(fun t1 t2 ->
    like t1 (Tree.union t1.tree t2.tree ~compare_elt:comparator.Comparator.compare))
;;

let of_list ~comparator l =
  { comparator; tree = Tree.of_list l ~compare_elt:comparator.Comparator.compare }
;;

let of_array ~comparator a =
  { comparator; tree = Tree.of_array a ~compare_elt:comparator.Comparator.compare }
;;

let stable_dedup_list ~comparator xs =
  Tree.stable_dedup_list xs ~compare_elt:comparator.Comparator.compare;
;;

let map ~comparator t ~f =
  { comparator; tree = Tree.map t.tree ~f ~compare_elt:comparator.Comparator.compare }
;;

let filter_map ~comparator t ~f =
  { comparator;
    tree = Tree.filter_map t.tree ~f ~compare_elt:comparator.Comparator.compare;
  }
;;

type 'a elt = 'a

include Accessors

module Creators (Elt : Comparator.S1) : sig

  type ('a, 'comparator) t_ = ('a Elt.t, Elt.comparator) set
  type ('a, 'b) tree = 'a Tree.t
  type 'a elt_ = 'a Elt.t

  val t_of_sexp : (Sexp.t -> 'a Elt.t) -> Sexp.t -> ('a, 'comparator) t_

  include Creators
    with type ('a, 'b) set := ('a, 'b) set
    with type ('a, 'b) t := ('a, 'b) t_
    with type ('a, 'b) tree := ('a, 'b) tree
    with type 'a elt := 'a elt_
    with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) create_options_without_comparator

end = struct

  type ('a, 'comparator) t_ = ('a Elt.t, Elt.comparator) set

  type ('a, 'b) tree = 'a Tree.t

  type 'a elt_ = 'a Elt.t

  let comparator = Elt.comparator

  let compare_elt = comparator.Comparator.compare

  let of_tree tree = of_tree ~comparator tree

  let empty = { comparator; tree = Tree.empty }

  let singleton e = singleton ~comparator e

  let union_list l = union_list ~comparator l

  let of_list l = of_list ~comparator l

  let of_array a = of_array ~comparator a

  let stable_dedup_list xs = stable_dedup_list ~comparator xs

  let map t ~f = map ~comparator t ~f

  let filter_map t ~f = filter_map ~comparator t ~f

  let t_of_sexp a_of_sexp sexp = of_tree (Tree.t_of_sexp a_of_sexp sexp ~compare_elt)

end

module Poly = struct
  module Elt = Comparator.Poly

  include Creators (Elt)

  type 'a t = ('a, Elt.comparator) set
  type 'a elt = 'a

  include Accessors

  let sexp_of_t = sexp_of_t

  include Bin_prot.Utils.Make_iterable_binable1 (struct
    type 'a t = ('a, Elt.comparator) set
    type 'a acc = 'a t
    type 'a el = 'a with bin_io

    let module_name = Some "Core.Set"
    let length = length
    let iter t ~f = iter ~f:(fun key -> f key) t
    let init _n = empty

    let insert acc el _i =
      if mem acc el then failwith "Set.bin_read_t_: duplicate element in set"
      else add acc el
    ;;

    let finish t = t
  end)

  TEST_MODULE = struct
    let (=) = Pervasives.(=)
    TEST = stable_dedup_list [] = []
    TEST = stable_dedup_list [5;5;5;5;5] = [5]
    TEST = stable_dedup_list [5;9;3;5;2;2] = [5;9;3;2]
  end
end

module type S         = S         with type ('a, 'b) set := ('a, 'b) t
module type S_binable = S_binable with type ('a, 'b) set := ('a, 'b) t

module Make_using_comparator (Elt : Comparator.S) = struct

  module Elt = Elt

  include Creators (Comparator.S_to_S1 (Elt))

  type t = (Elt.t, Elt.comparator) set

  include Accessors

  let sexp_of_t t = sexp_of_t Elt.sexp_of_t t

  let t_of_sexp sexp = t_of_sexp Elt.t_of_sexp sexp

end

module Make (Elt : Comparator.Pre) = Make_using_comparator (Comparator.Make (Elt))

module Make_binable_using_comparator (Elt' : Comparator.S_binable) = struct

  include (Make_using_comparator (Elt'))

  include Bin_prot.Utils.Make_iterable_binable (struct
    type acc = t
    type t = acc
    type el = Elt'.t with bin_io

    let module_name = Some "Core.Set"
    let length = length
    let iter t ~f = iter ~f:(fun key -> f key) t
    let init _n = empty

    let insert acc el _i =
      if mem acc el then failwith "Set.bin_read_t_: duplicate element in set"
      else add acc el
    ;;

    let finish t = t
  end)

end

module Make_binable (Elt : Comparator.Pre_binable) =
  Make_binable_using_comparator (Comparator.Make_binable (Elt))
