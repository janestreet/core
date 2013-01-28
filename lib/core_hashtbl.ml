open Sexplib
open Sexplib.Conv
open Core_hashtbl_intf
open With_return

let failwiths = Error.failwiths

module Hashable = Core_hashtbl_intf.Hashable

let hash_param = Hashable.hash_param
let hash       = Hashable.hash

(* A few small things copied from other parts of core because they depend on us, so we
   can't use them. *)
module Int = struct
  type t = int

  let max (x : t) y = if x > y then x else y
  let min (x : t) y = if x < y then x else y
end

module List = Core_list
module Array = Core_array

let phys_equal = (==)

type ('k, 'v) t =
  { mutable table : ('k, 'v) Avltree.t array;
    mutable length : int;
    growth_allowed: bool;
    hashable: 'k Hashable.t;
  }

type ('k, 'v) hashtbl = ('k, 'v) t

type 'a key = 'a

module type S         = S         with type ('a, 'b) hashtbl = ('a, 'b) t
module type S_binable = S_binable with type ('a, 'b) hashtbl = ('a, 'b) t

let sexp_of_key t = t.hashable.Hashable.sexp_of_t
let compare_key t = t.hashable.Hashable.compare

let create ?(growth_allowed=true) ?(size = 128) ~hashable () =
  let size = Int.min (Int.max 1 size) Sys.max_array_length in
  { table = Array.create size Avltree.empty;
    length = 0;
    growth_allowed = growth_allowed;
    hashable;
  }
;;

exception Hash_value_must_be_non_negative with sexp

let slot t key =
  let hash = t.hashable.Hashable.hash key in
  if hash < 0 then raise Hash_value_must_be_non_negative;
  hash mod Array.length t.table
;;

let add_worker added replace t ~key ~data =
  let i = slot t key in
  let root = t.table.(i) in
  (* These cases should be quite common, so we manually inline them. *)
  match root with
  | Avltree.Empty ->
    t.table.(i) <- Avltree.Leaf (key, data);
    t.length <- t.length + 1;
    added := true
  | Avltree.Leaf (k, _) ->
    let c = compare_key t k key in
    if c = 0 then begin
      if replace then t.table.(i) <- Avltree.Leaf (key, data);
      added := false
    end else begin
      added := true;
      t.length <- t.length + 1;
      t.table.(i) <-
        if c < 0 then Avltree.Node(root, key, data, 2, Avltree.Empty)
        else Avltree.Node(Avltree.Empty, key, data, 2, root)
    end
  | root ->
    let new_root =
      (* The avl tree might replace the value [replace=true] or do nothing [replace=false]
         to the entry, in that case the table did not get bigger, so we should not
         increment length, we pass in the bool ref t.added so that it can tell us whether
         it added or replaced. We do it this way to avoid extra allocation. Since the bool
         is an immediate it does not go through the write barrier. *)
      Avltree.add ~replace root ~compare:(compare_key t) ~added ~key ~data
    in
    if !added then
      t.length <- t.length + 1;
    (* This little optimization saves a caml_modify when the tree
       hasn't been rebalanced. *)
    if not (phys_equal new_root root) then
      t.table.(i) <- new_root
;;

let maybe_resize_table t =
  let len = Array.length t.table in
  let should_grow = t.length >= len * 2 in
  if should_grow && t.growth_allowed then begin
    let new_array_length = Int.min (len * 2) Sys.max_array_length in
    if new_array_length > len then begin
      let new_table =
        Array.init new_array_length ~f:(fun _ -> Avltree.empty)
      in
      let old_table = t.table in
      let added_or_removed = ref false in
      t.table <- new_table;
      t.length <- 0;
      for i = 0 to Array.length old_table - 1 do
        Avltree.iter old_table.(i) ~f:(fun ~key ~data ->
          add_worker added_or_removed true t ~key ~data)
      done
    end
  end
;;

let set t ~key ~data =
  add_worker (ref false) true t ~key ~data;
  maybe_resize_table t
;;

let replace = set

let add t ~key ~data =
  let added_or_removed = ref false in
  add_worker added_or_removed false t ~key ~data;
  if !added_or_removed then begin
    maybe_resize_table t;
    `Ok
  end else
    `Duplicate
;;

let add_exn (type k) t ~key ~data =
  match add t ~key ~data with
  | `Ok -> ()
  | `Duplicate ->
    let module T = struct
      type key = k
      let sexp_of_key = sexp_of_key t
      exception Add_key_already_present of key with sexp
    end
    in
    raise (T.Add_key_already_present key)
;;

let clear t =
  for i = 0 to Array.length t.table - 1 do
    t.table.(i) <- Avltree.empty;
  done;
  t.length <- 0
;;

let find t key =
  (* with a good hash function these first two cases will be the overwhelming majority,
     and Avltree.find is recursive, so it can't be inlined, so doing this avoids a
     function call in most cases. *)
  match t.table.(slot t key) with
  | Avltree.Empty -> None
  | Avltree.Leaf (k, v) ->
    if compare_key t k key = 0 then Some v
    else None
  | tree -> Avltree.find tree ~compare:(compare_key t) key
;;

let mem t key =
  match t.table.(slot t key) with
  | Avltree.Empty -> false
  | Avltree.Leaf (k, _) -> compare_key t k key = 0
  | tree -> Avltree.mem tree ~compare:(compare_key t) key
;;

let remove t key =
  let i = slot t key in
  let root = t.table.(i) in
  let added_or_removed = ref false in
  let new_root =
    Avltree.remove root
      ~removed:added_or_removed ~compare:(compare_key t) key
  in
  if not (phys_equal root new_root) then
    t.table.(i) <- new_root;
  if !added_or_removed then
    t.length <- t.length - 1
;;

let length t = t.length

let is_empty t = length t = 0

let fold t ~init ~f =
  if length t = 0 then init
  else begin
    let n = Array.length t.table in
    let acc = ref init in
    for i = 0 to n - 1 do
      match Array.unsafe_get t.table i with
      | Avltree.Empty -> ()
      | Avltree.Leaf (key, data) -> acc := f ~key ~data !acc
      | bucket -> acc := Avltree.fold bucket ~init:!acc ~f
    done;
    !acc
  end
;;

let sexp_of_t sexp_of_k sexp_of_d t =
  let coll ~key:k ~data:v acc = Sexp.List [sexp_of_k k; sexp_of_d v] :: acc in
  Sexp.List (fold ~f:coll t ~init:[])
;;

let iter t ~f =
  if t.length = 0 then ()
  else begin
    let n = Array.length t.table in
    for i = 0 to n - 1 do
      match Array.unsafe_get t.table i with
      | Avltree.Empty -> ()
      | Avltree.Leaf (key, data) -> f ~key ~data
      | bucket -> Avltree.iter bucket ~f
    done
  end
;;

let invariant t =
  for i = 0 to Array.length t.table - 1 do
    Avltree.invariant t.table.(i) ~compare:(compare_key t)
  done;
  let real_len = fold t ~init:0 ~f:(fun ~key:_ ~data:_ i -> i + 1) in
  assert (real_len = t.length)
;;

let find_exn t id =
  match find t id with
  | Some x -> x
  | None ->
    raise Not_found
;;

let find_default t key ~default =
  match find t key with
  | None -> default ()
  | Some a -> a

let existsi t ~f =
  with_return (fun r ->
    iter t ~f:(fun ~key ~data -> if f ~key ~data then r.return true);
    false)
;;

let exists t ~f = existsi t ~f:(fun ~key:_ ~data -> f data)

let mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  iter t ~f:(fun ~key ~data -> replace new_t ~key ~data:(f ~key ~data));
  new_t

(* How about this? *)
(*
let mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  let itfun ~key ~data = replace new_t ~key ~data:(f ~key ~data) in
  iter t ~f:itfun;
  new_t
*)

let map t ~f = mapi t ~f:(fun ~key:_ ~data -> f data)

let copy t = map t ~f:Fn.id

let filter_mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  iter t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | Some new_data -> replace new_t ~key ~data:new_data
    | None -> ());
  new_t

(* How about this? *)
(*
let filter_mapi t ~f =
  let new_t =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  let itfun ~key ~data = match f ~key ~data with
    | None -> ()
    | Some d -> replace new_t ~key ~data:d
  in
  iter t ~f:itfun;
  new_t
*)

let filter_map t ~f = filter_mapi t ~f:(fun ~key:_ ~data -> f data)

let filteri t ~f =
  filter_mapi t ~f:(fun ~key ~data -> if f ~key ~data then Some data else None)
;;

let filter t ~f = filteri t ~f:(fun ~key:_ ~data -> f data)

let partition_mapi t ~f =
  let t0 =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  let t1 =
    create ~growth_allowed:t.growth_allowed
      ~hashable:t.hashable ~size:t.length ()
  in
  iter t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | `Fst new_data -> replace t0 ~key ~data:new_data
    | `Snd new_data -> replace t1 ~key ~data:new_data);
  (t0, t1)
;;

let partition_map t ~f = partition_mapi t ~f:(fun ~key:_ ~data -> f data)

let partitioni_tf t ~f =
  partition_mapi t ~f:(fun ~key ~data -> if f ~key ~data then `Fst data else `Snd data)
;;

let partition_tf t ~f = partitioni_tf t ~f:(fun ~key:_ ~data -> f data)

let remove_one t key =
  match find t key with
  | None -> ()
  | Some ([] | [_]) -> remove t key
  | Some (_ :: tl) -> replace t ~key ~data:tl

let find_or_add t id ~default =
  match find t id with
  | Some x -> x
  | None ->
    let default = default () in
    replace t ~key:id ~data:default;
    default

let change t id f =
  match f (find t id) with
  | None -> remove t id
  | Some data -> replace t ~key:id ~data

let incr ?(by = 1) t key =
  change t key
    (function
      | None -> Some 1
      | Some i -> Some (i + by))

let add_multi t ~key ~data =
  match find t key with
  | None -> replace t ~key ~data:[data]
  | Some l -> replace t ~key ~data:(data :: l)

let remove_multi t key =
  match find t key with
  | None -> ()
  | Some [] | Some [_] -> remove t key
  | Some (_ :: tl) -> replace t ~key ~data:tl

let iter_vals t ~f = iter t ~f:(fun ~key:_ ~data -> f data)

let create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data rows =
  let size = match size with Some s -> s | None -> List.length rows in
  let res = create ?growth_allowed ~hashable ~size () in
  let dupes = ref [] in
  List.iter rows ~f:(fun r ->
    let key = get_key r in
    let data = get_data r in
    if mem res key then
      dupes := key :: !dupes
    else
      replace res ~key ~data);
  match !dupes with
  | [] -> `Ok res
  | keys -> `Duplicate_keys (List.dedup ~compare:hashable.Hashable.compare keys)
;;

let create_mapped_exn ?growth_allowed ?size ~hashable ~get_key ~get_data rows =
  let size = match size with Some s -> s | None -> List.length rows in
  let res = create ?growth_allowed ~size ~hashable () in
  List.iter rows ~f:(fun r ->
    let key = get_key r in
    let data = get_data r in
    if mem res key then
      let sexp_of_key = hashable.Hashable.sexp_of_t in
      failwiths "Hashtbl.create_mapped_exn: duplicate key" key <:sexp_of< key >>
    else
      replace res ~key ~data);
  res
;;

let create_mapped_multi ?growth_allowed ?size ~hashable ~get_key ~get_data rows =
  let size = match size with Some s -> s | None -> List.length rows in
  let res = create ?growth_allowed ~size ~hashable () in
  List.iter rows ~f:(fun r ->
    let key = get_key r in
    let data = get_data r in
    add_multi res ~key ~data);
  res
;;

let of_alist ?growth_allowed ?size ~hashable lst =
  match create_mapped ?growth_allowed ?size ~hashable ~get_key:fst ~get_data:snd lst with
  | `Ok t -> `Ok t
  | `Duplicate_keys k -> `Duplicate_key (List.hd_exn k)
;;

let of_alist_report_all_dups ?growth_allowed ?size ~hashable lst =
  create_mapped ?growth_allowed ?size ~hashable ~get_key:fst ~get_data:snd lst
;;

let of_alist_exn ?growth_allowed ?size ~hashable lst =
  match of_alist ?growth_allowed ?size ~hashable lst with
  | `Ok v -> v
  | `Duplicate_key key ->
    let sexp_of_key = hashable.Hashable.sexp_of_t in
    failwiths "Hashtbl.of_alist_exn: duplicate key" key <:sexp_of< key >>
;;

let of_alist_multi ?growth_allowed ?size ~hashable lst =
  create_mapped_multi ?growth_allowed ?size ~hashable ~get_key:fst ~get_data:snd lst
;;

let to_alist t = fold ~f:(fun ~key ~data list -> (key, data)::list) ~init:[] t

let keys t = fold t ~init:[] ~f:(fun ~key ~data:_ acc -> key :: acc)

let data t = fold ~f:(fun ~key:_ ~data list -> data::list) ~init:[] t

let add_to_groups groups ~get_key ~get_data ~combine ~rows =
  List.iter rows ~f:(fun row ->
    let key = get_key row in
    let data = get_data row in
    let data =
      match find groups key with
      | None -> data
      | Some old -> combine old data
    in
    replace groups ~key ~data)
;;

let group ?growth_allowed ?size ~hashable ~get_key ~get_data ~combine rows =
  let res = create ?growth_allowed ?size ~hashable () in
  add_to_groups res ~get_key ~get_data ~combine ~rows;
  res
;;

let create_with_key ?growth_allowed ?size ~hashable ~get_key rows =
  create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data:(fun x -> x) rows
;;

let create_with_key_exn ?growth_allowed ?size ~hashable ~get_key rows =
  match create_with_key ?growth_allowed ?size ~hashable ~get_key rows with
  | `Ok t -> t
  | `Duplicate_keys keys ->
    let sexp_of_key = hashable.Hashable.sexp_of_t in
    failwiths "Hashtbl.create_with_key: duplicate keys" keys <:sexp_of< key list >>
;;

let merge t1 t2 ~f =
  if not (phys_equal t1.hashable t2.hashable)
  then invalid_arg "Hashtbl.merge: different 'hashable' values";
  let create () =
    create
      ~growth_allowed:t1.growth_allowed
      ~hashable:t1.hashable
      ~size:t1.length
      ()
  in
  let t = create () in
  let unique_keys = create () in
  let record_key ~key ~data:_ = replace unique_keys ~key ~data:() in
  iter t1 ~f:record_key;
  iter t2 ~f:record_key;
  iter unique_keys ~f:(fun ~key ~data:_ ->
    let arg =
      match find t1 key, find t2 key with
      | None, None -> assert false
      | None, Some r -> `Right r
      | Some l, None -> `Left l
      | Some l, Some r -> `Both (l, r)
    in
    match f ~key arg with
    | Some data -> replace t ~key ~data
    | None -> ());
  t
;;

let merge_into ~f ~src ~dst =
  iter src ~f:(fun ~key ~data ->
    match f ~key data (find dst key) with
    | Some data -> replace dst ~key ~data
    | None -> ())

let filteri_inplace t ~f =
  let to_remove =
    fold t ~init:[] ~f:(fun ~key ~data ac ->
      if f key data then ac else key :: ac)
  in
  List.iter to_remove ~f:(fun key -> remove t key);
;;

let filter_inplace t ~f =
  filteri_inplace t ~f:(fun _ data -> f data)
;;

let equal t t' equal =
  length t = length t' &&
  with_return (fun r ->
    iter t ~f:(fun ~key ~data ->
      match find t' key with
      | None -> r.return false
      | Some data' -> if not (equal data data') then r.return false);
    true)
;;

module Accessors = struct
  let invariant       = invariant
  let clear           = clear
  let copy            = copy
  let remove          = remove
  let remove_one      = remove_one
  let replace         = replace
  let set             = set
  let add             = add
  let add_exn         = add_exn
  let change          = change
  let add_multi       = add_multi
  let remove_multi    = remove_multi
  let mem             = mem
  let iter            = iter
  let exists          = exists
  let existsi         = existsi
  let fold            = fold
  let length          = length
  let is_empty        = is_empty
  let map             = map
  let mapi            = mapi
  let filter_map      = filter_map
  let filter_mapi     = filter_mapi
  let filter          = filter
  let filteri         = filteri
  let partition_map   = partition_map
  let partition_mapi  = partition_mapi
  let partition_tf    = partition_tf
  let partitioni_tf   = partitioni_tf
  let find_or_add     = find_or_add
  let find            = find
  let find_exn        = find_exn
  let iter_vals       = iter_vals
  let to_alist        = to_alist
  let merge           = merge
  let merge_into      = merge_into
  let keys            = keys
  let data            = data
  let filter_inplace  = filter_inplace
  let filteri_inplace = filteri_inplace
  let equal           = equal
  let add_to_groups   = add_to_groups
  let incr            = incr
  let sexp_of_key     = sexp_of_key
end

module type Key = Key

module Creators (Key : sig
  type 'a t

  val hashable : 'a t Hashable.t
end) : sig

  type ('a, 'b) t_ = ('a Key.t, 'b) t

  val t_of_sexp : (Sexp.t -> 'a Key.t) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t_

  include Creators
    with type ('a, 'b) t := ('a, 'b) t_
    with type 'a key := 'a Key.t
    with type ('key, 'a) create_options := ('key, 'a) create_options_without_hashable

end = struct

  let hashable = Key.hashable

  type ('a, 'b) t_ = ('a Key.t, 'b) t

  let create ?growth_allowed ?size () = create ?growth_allowed ?size ~hashable ()

  let of_alist ?growth_allowed ?size l =
    of_alist ?growth_allowed ~hashable ?size l
  ;;

  let of_alist_report_all_dups ?growth_allowed ?size l =
    of_alist_report_all_dups ?growth_allowed ~hashable ?size l
  ;;

  let of_alist_exn ?growth_allowed ?size l =
    of_alist_exn ?growth_allowed ~hashable ?size l
  ;;

  let t_of_sexp k_of_sexp d_of_sexp sexp =
    let alist = <:of_sexp< (k * d) list >> sexp in
    of_alist_exn alist ~size:(List.length alist)
  ;;

  let of_alist_multi ?growth_allowed ?size l =
    of_alist_multi ?growth_allowed ~hashable ?size l
  ;;

  let create_mapped ?growth_allowed ?size ~get_key ~get_data l =
    create_mapped ?growth_allowed ~hashable ?size ~get_key ~get_data l
  ;;

  let create_with_key ?growth_allowed ?size ~get_key l =
    create_with_key ?growth_allowed ~hashable ?size ~get_key l
  ;;

  let create_with_key_exn ?growth_allowed ?size ~get_key l =
    create_with_key_exn ?growth_allowed ~hashable ?size ~get_key l
  ;;

  let group ?growth_allowed ?size ~get_key ~get_data ~combine l =
    group ?growth_allowed ~hashable ?size ~get_key ~get_data ~combine l
  ;;
end

module Poly = struct

  type ('a, 'b) t = ('a, 'b) hashtbl

  type 'a key = 'a

  let hashable = Hashable.poly

  include Creators (struct
    type 'a t = 'a
    let hashable = hashable
  end)

  include Accessors

  let sexp_of_t = sexp_of_t

  include Bin_prot.Utils.Make_iterable_binable2 (struct
    type ('a, 'b) z = ('a, 'b) t
    type ('a, 'b) t = ('a, 'b) z
    type ('a, 'b) el = 'a * 'b with bin_io
    type ('a, 'b) acc = ('a, 'b) t

    let module_name = Some "Core_hashtbl"
    let length = length
    let iter t ~f = iter t ~f:(fun ~key ~data -> f (key, data))
    let init size = create ~size ()

    let insert t (key, data) _i =
      match find t key with
      | None -> replace t ~key ~data; t
      | Some _ -> failwith "Core_hashtbl.bin_read_t_: duplicate key"
    ;;

    let finish = Fn.id
  end)

end

module Make (Key : Key) = struct

  let hashable =
    { Hashable.
      hash = Key.hash;
      compare = Key.compare;
      sexp_of_t = Key.sexp_of_t;
    }
  ;;

  type key = Key.t with sexp_of
  type ('a, 'b) hashtbl = ('a, 'b) t
  type 'a t = (key, 'a) hashtbl
  type 'a key_ = key

  include Creators (struct
    type 'a t = Key.t
    let hashable = hashable
  end)

  include Accessors

  let sexp_of_t sexp_of_v t = Poly.sexp_of_t Key.sexp_of_t sexp_of_v t

  let t_of_sexp v_of_sexp sexp = t_of_sexp Key.t_of_sexp v_of_sexp sexp

end

module Make_binable (Key : sig
  include Key
  include Binable.S with type t := t
end) = struct
  include Make (Key)

  include Bin_prot.Utils.Make_iterable_binable1 (struct
    type 'a acc = 'a t
    type 'a t = 'a acc
    type 'a el = Key.t * 'a with bin_io

    let module_name = Some "Core_hashtbl"
    let length = length
    let iter t ~f = iter t ~f:(fun ~key ~data -> f (key, data))
    let init size = create ~size ()

    let insert t (key, data) _i =
      match find t key with
      | None -> replace t ~key ~data; t
      | Some _ -> failwiths "Hashtbl.bin_read_t: duplicate key" key <:sexp_of< Key.t >>
    ;;

    let finish = Fn.id
  end)

end
