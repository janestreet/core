open Std_internal

module Array = Core_array

module Entry = struct
  module T = struct
    type ('key, 'data) t =
      { mutable key : 'key;             (* the int is fixed, but the 'key can change *)
        mutable data : 'data;
        (* The index in [defined_entries] where this [Entry.t] is placed. *)
        mutable defined_entries_index : int;
      }
    with fields, sexp_of
  end
  include T
end

open Entry.T

type ('key, 'data) t_detailed =
  { num_keys : int;
    sexp_of_key : ('key -> Sexp.t) option;
    key_to_int : 'key -> int;
    (* The number of entries in the table, not the length of the arrays below. *)
    mutable length : int;
    (* (key, data) is in the table
       iff [entries_by_key.(key_to_index key) = { key; data }] *)
    entries_by_key : ('key, 'data) Entry.t option array;
    (* The first [length] elements of [defined_entries] hold the data in the table.
       This is an optimization for fold, to keep us from wasting iterations when
       the array is sparse. *)
    defined_entries : ('key, 'data) Entry.t option array;
  }
with fields, sexp_of

type ('a, 'b) t = ('a, 'b) t_detailed

type ('a, 'b) table = ('a, 'b) t

let sexp_of_key t =
  match t.sexp_of_key with
  | Some f -> f
  | None -> fun key -> Int.sexp_of_t (t.key_to_int key)
;;

let invariant t =
  try
    let num_keys = t.num_keys in
    assert (num_keys = Array.length t.entries_by_key);
    assert (num_keys = Array.length t.defined_entries);
    assert (0 <= t.length && t.length <= num_keys);
    Array.iteri t.entries_by_key ~f:(fun i -> function
      | None -> ()
      | Some entry ->
        assert (i = t.key_to_int entry.key);
        match t.defined_entries.(entry.defined_entries_index) with
        | None -> assert false
        | Some entry' -> assert (phys_equal entry entry'));
    Array.iteri t.defined_entries ~f:(fun i entry_opt ->
      match i < t.length, entry_opt with
      | false, None -> ()
      | true, Some entry -> assert (i = entry.defined_entries_index)
      | _ -> assert false);
    let get_entries array =
      let a = Array.filter_opt array in
      Array.sort a ~cmp:(fun entry entry' ->
        Int.compare (t.key_to_int entry.key) (t.key_to_int entry'.key));
      a
    in
    let entries = get_entries t.entries_by_key in
    let entries' = get_entries t.defined_entries in
    assert (t.length = Array.length entries);
    assert (Array.equal entries entries' ~equal:phys_equal)
  with exn ->
    let sexp_of_key = sexp_of_key t in
    failwiths "invariant failed" (exn, t) (<:sexp_of< exn * (key, _) t_detailed >>)
;;

let debug = ref false

let check_invariant t = if !debug then invariant t

let is_empty t = length t = 0

let create ?sexp_of_key ~num_keys ~key_to_int () =
  if num_keys < 0 then
    failwiths "num_keys must be nonnegative" num_keys <:sexp_of< int >>;
  let t =
    { num_keys;
      sexp_of_key;
      key_to_int;
      length = 0;
      entries_by_key  = Array.create ~len:num_keys None;
      defined_entries = Array.create ~len:num_keys None;
    }
  in
  check_invariant t;
  t
;;

let create_like { num_keys; sexp_of_key; key_to_int;
                  length = _; entries_by_key = _; defined_entries = _;
                } =
  create ~num_keys ?sexp_of_key ~key_to_int ()
;;

let fold t ~init ~f =
  let rec loop i ac =
    if i = t.length then
      ac
    else begin
      match t.defined_entries.(i) with
      | None -> assert false
      | Some entry -> loop (i + 1) (f ~key:entry.key ~data:entry.data ac)
    end
  in
  loop 0 init
;;

let iter t ~f = fold t ~init:() ~f:(fun ~key ~data () -> f ~key ~data)

let iter_vals t ~f = iter t ~f:(fun ~key:_ ~data -> f data)

let map_entries t ~f = fold t ~init:[] ~f:(fun ~key ~data ac -> f ~key ~data :: ac)

let to_alist t = map_entries t ~f:(fun ~key ~data -> (key, data))

module Serialized = struct
  type ('key, 'data) t =
    { num_keys : int;
      alist : ('key * 'data) list;
    }
  with bin_io, sexp
end

let to_serialized t =
  { Serialized.
    num_keys = t.num_keys;
    alist = to_alist t;
  }
;;

let sexp_of_t sexp_of_key sexp_of_data t =
  Serialized.sexp_of_t sexp_of_key sexp_of_data (to_serialized t)
;;

let keys t = map_entries t ~f:(fun ~key ~data:_ -> key)

let data t = map_entries t ~f:(fun ~key:_ ~data -> data)

let entry_opt t key =
  let index = t.key_to_int key in
  try t.entries_by_key.(index)
  with _ ->
    let sexp_of_key = sexp_of_key t in
    failwiths "key's index out of range"
      (key, index, `Should_be_between_0_and (t.num_keys - 1))
      (<:sexp_of< key * int * [ `Should_be_between_0_and of int ] >>)
;;

let find t key = Option.map (entry_opt t key) ~f:Entry.data

let find_exn t key =
  match entry_opt t key with
  | Some entry -> Entry.data entry
  | None ->
    let sexp_of_key = sexp_of_key t in
    failwiths "Bounded_int_table.find_exn got unknown key" (key, t)
      (<:sexp_of< key * (key, _) t >>)
;;

let mem t key = is_some (entry_opt t key)

let add_assuming_not_there t ~key ~data =
  let defined_entries_index = t.length in
  let entry_opt = Some { Entry. key; data; defined_entries_index } in
  t.entries_by_key.(t.key_to_int key) <- entry_opt;
  t.defined_entries.(defined_entries_index) <- entry_opt;
  t.length <- t.length + 1;
  check_invariant t;
;;

let find_or_add t key ~default =
  match entry_opt t key with
  | Some e -> Entry.data e
  | None ->
    let data = default () in
    add_assuming_not_there t ~key ~data;
    data
;;

let set t ~key ~data =
   match entry_opt t key with
  | None -> add_assuming_not_there t ~key ~data
  | Some entry ->
    entry.key <- key; (* we update the key because we want the latest key in the table *)
    entry.data <- data;
;;

let add t ~key ~data =
  match entry_opt t key with
  | Some entry -> `Duplicate entry.Entry.data
  | None -> add_assuming_not_there t ~key ~data; `Ok
;;

let add_exn t ~key ~data =
  match add t ~key ~data with
  | `Ok -> ()
  | `Duplicate _ ->
    let sexp_of_key = sexp_of_key t in
    failwiths "Bounded_int_table.add_exn of key whose index is already present"
      (key, t.key_to_int key) <:sexp_of< key * int >>
;;

let remove t key =
  begin match entry_opt t key with
  | None -> ()
  | Some entry ->
    t.length <- t.length - 1;
    t.entries_by_key.(t.key_to_int key) <- None;
    let hole = entry.defined_entries_index in
    let last = t.length in
    if hole < last then begin
      match t.defined_entries.(last) with
      | None -> assert false
      | Some entry_to_put_in_hole as entry_to_put_in_hole_opt ->
        t.defined_entries.(hole) <- entry_to_put_in_hole_opt;
        entry_to_put_in_hole.defined_entries_index <- hole;
    end;
    t.defined_entries.(last) <- None;
  end;
  check_invariant t;
;;

let existsi t ~f =
  with_return (fun r ->
    iter t ~f:(fun ~key ~data -> if f ~key ~data then r.return true);
    false)
;;

let exists t ~f = existsi t ~f:(fun ~key:_ ~data -> f data)

let for_alli t ~f = not (existsi t ~f:(fun ~key ~data -> not (f ~key ~data)))

let for_all t ~f = for_alli t ~f:(fun ~key:_ ~data -> f data)

let equal key_equal data_equal t1 t2 =
  length t1 = length t2
  && for_alli t1 ~f:(fun ~key ~data ->
    match entry_opt t2 key with
    | None -> false
    | Some entry ->
      key_equal key entry.Entry.key
      && data_equal data entry.Entry.data)
;;

(* test [exists{,i}], [for_all{,i}] *)
TEST_MODULE = struct
  let of_list keys =
    let t = create ~num_keys:10 ~key_to_int:Fn.id ~sexp_of_key:Int.sexp_of_t () in
    List.iter keys ~f:(fun key -> add_exn t ~key ~data:key);
    t
  ;;

  let test_exists_like_function exists =
    exists (of_list []) ~f:(fun _ -> assert false) = false
    && exists (of_list [1]) ~f:(fun _ -> false) = false
    && exists (of_list [1]) ~f:(fun _ -> true) = true
    && exists (of_list [1]) ~f:(fun data -> data = 1) = true
    && exists (of_list [1; 2; 3]) ~f:(fun _ -> false) = false
    && exists (of_list [1; 2; 3]) ~f:(fun _ -> true) = true
    && exists (of_list [1; 2; 3]) ~f:(fun data -> data = 3) = true
  ;;

  TEST = test_exists_like_function (fun t ~f -> existsi t ~f:(fun ~key:_ ~data -> f data))

  TEST = test_exists_like_function exists

  TEST =
    test_exists_like_function (fun t ~f ->
      not (for_alli t ~f:(fun ~key:_ ~data -> not (f data))))
  ;;

  TEST =
    test_exists_like_function (fun t ~f -> not (for_all t ~f:(fun data -> not (f data))));
  ;;

  let equal_of_list l1 l2 = equal Int.equal Int.equal (of_list l1) (of_list l2)

  TEST = equal_of_list [] [] = true
  TEST = equal_of_list [] [1] = false
  TEST = equal_of_list [1] [] = false
  TEST = equal_of_list [1] [1] = true
  TEST = equal_of_list [1] [1; 2] = false
  TEST = equal_of_list [1; 2] [1; 2] = true
  TEST = equal_of_list [1; 2] [2; 1] = true

  (* test [equal] between tables that have different [to_int] functions. *)
  TEST_UNIT =
    let of_list ~offset keys =
      let t = create ~num_keys:10 ~key_to_int:(fun i -> i + offset) () in
      List.iter keys ~f:(fun key -> add_exn t ~key ~data:key);
      t
    in
    let t0 = of_list [ 1; 2 ] ~offset:0 in
    let t1 = of_list [ 1; 2 ] ~offset:1 in
    let t2 = of_list [ 1; 2 ] ~offset:2 in
    let t3 = of_list [ 2; 3 ] ~offset:0 in
    let equal = equal Int.equal Int.equal in
    assert (equal t0 t1);
    assert (equal t0 t2);
    assert (equal t1 t2);
    assert (not (equal t0 t3));
    assert (not (equal t1 t3));
    assert (not (equal t2 t3));
  ;;
end

module With_key (Key : sig
  type t with bin_io, sexp
  val to_int : t -> int
end) = struct

  type 'data t = (Key.t, 'data) table
  type 'data table = 'data t

  let create ~num_keys =
    create ~sexp_of_key:Key.sexp_of_t ~num_keys ~key_to_int:Key.to_int ()
  ;;

  let of_alist_exn alist =
    let max_key =
      List.fold alist ~init:(-1) ~f:(fun max (key, _) -> Int.max max (Key.to_int key))
    in
    let t = create ~num_keys:(max_key + 1) in
    List.iter alist ~f:(fun (key, data) -> add_exn t ~key ~data);
    t
  ;;

  let of_alist alist = Or_error.try_with (fun () -> of_alist_exn alist)

  let sexp_of_t sexp_of_data = sexp_of_t Key.sexp_of_t sexp_of_data

  let of_serialized { Serialized. num_keys; alist } =
    let t = create ~num_keys in
    List.iter alist ~f:(fun (key, data) -> add_exn t ~key ~data);
    t
  ;;

  let t_of_sexp data_of_sexp sexp =
    of_serialized (Serialized.t_of_sexp Key.t_of_sexp data_of_sexp sexp)
  ;;

  include Bin_prot.Utils.Make_binable1 (struct
    module Binable = struct
      type 'data t = (Key.t, 'data) Serialized.t with bin_io
    end

    type 'data t = 'data table

    let to_binable = to_serialized
    let of_binable = of_serialized
  end)

end

(* test [With_key] *)
TEST_MODULE = struct
  include (With_key (Int))

  TEST = is_empty (create ~num_keys:1)

  TEST = Result.is_ok (of_alist [ ])
  TEST = Result.is_ok (of_alist [ (1, 1) ])
  TEST = Result.is_error (of_alist [ (1, 1); (1, 2) ])

  TEST = is_empty (of_alist_exn [])

  TEST_UNIT =
    let t = of_alist_exn [ (1, 2) ] in
    assert (length t = 1);
    assert (keys t = [1]);
    assert (data t = [2]);
  ;;

  TEST_UNIT =
    let t = of_alist_exn [ (1, 2); (3, 4) ] in
    assert (length t = 2);
    assert (keys t = [1; 3] || keys t = [3; 1]);
    assert (data t = [2; 4] || data t = [4; 2]);
  ;;
end

let filter_mapi t ~f =
  let result = create_like t in
  iter t ~f:(fun ~key ~data ->
    match f ~key ~data with
    | None -> ()
    | Some data -> add_exn result ~key ~data);
  result
;;

let ignore_key f = fun ~key:_ ~data -> f data

let filter_map t ~f = filter_mapi t ~f:(ignore_key f)

let mapi t ~f = filter_mapi t ~f:(fun ~key ~data -> Some (f ~key ~data))

let map t ~f = mapi t ~f:(ignore_key f)


TEST_MODULE = struct
  include (With_key (Int))

  let equal = equal Int.equal Int.equal

  let test_filter_map input ~f expect =
    equal (filter_map (of_alist_exn input) ~f) (of_alist_exn expect)
  ;;

  TEST = test_filter_map [] ~f:(fun _ -> assert false) []
  TEST = test_filter_map [1, 2] ~f:(fun _ -> None) []
  TEST = test_filter_map [1, 2] ~f:(fun x -> Some x) [1, 2]
  TEST = test_filter_map [1, 2] ~f:(fun x -> Some (x + 1)) [1, 3]
  TEST =
    test_filter_map [(1, 2); (3, 4)] ~f:(fun x -> if x = 2 then Some x else None) [1, 2]
  ;;

  let test_map_like map =
    let test input ~f expect =
      equal (map (of_alist_exn input) ~f) (of_alist_exn expect)
    in
    test [] ~f:(fun _ -> assert false) []
    && test [(1, 2)]         ~f:((+) 3)   [(1, 5)]
    && test [(1, 2); (3, 4)] ~f:((+) 5)   [(1, 7); (3, 9)]
  ;;

  TEST = test_map_like (fun t ~f -> mapi t ~f:(fun ~key:_ ~data -> f data))
  TEST = test_map_like map
end


TEST_MODULE = struct
  let () = debug := true

  TEST_UNIT =
    (* Check that [set] replaces the key. *)
    let t = create ~num_keys:1 ~key_to_int:(fun _ -> 0) () in
    set t ~key:13 ~data:();
    set t ~key:14 ~data:();
    assert (keys t = [14]);
  ;;

  let create ~num_keys : (int, _) t = create ~num_keys ~key_to_int:Fn.id ()

  let assert_empty t =
    assert (length t = 0);
    assert (to_alist t = []);
    assert (keys t = []);
    assert (data t = []);
  ;;

  TEST_UNIT =
    begin
      try ignore (create ~num_keys:(-1)); assert false with _ -> ()
    end;

  TEST_UNIT = ignore (create ~num_keys:0)
  TEST_UNIT = ignore (create ~num_keys:1)
  TEST_UNIT = ignore (create ~num_keys:10_000)

  TEST_UNIT =
    let num_keys = 10 in
    let t = create ~num_keys in
    let key_is_valid key = try ignore (find t key); true with _ -> false in
    assert (not (key_is_valid (-1)));
    for key = 0 to num_keys - 1 do
      assert (key_is_valid key);
      assert (is_none (find t key));
    done;
    assert (not (key_is_valid num_keys));
    assert_empty t;
  ;;

  let table_data = data

  TEST_UNIT =
    let num_keys = 10 in
    let t = create ~num_keys in
    let key = 0 in
    let data = "zero" in
    add_exn t ~key ~data;
    assert (length t = 1);
    assert (find t key = Some data);
    for key = 1 to num_keys - 1 do
      assert (find t key = None)
    done;
    assert (to_alist t = [(key, data)]);
    assert (keys t = [key]);
    assert (table_data t = [data]);
    remove t key;
    assert_empty t;
  ;;

  TEST_UNIT =
    let num_keys = 10 in
    let t = create ~num_keys in
    let key = 0 in
    let data = "zero" in
    add_exn t ~key ~data:"bad";
    set t ~key ~data;
    assert (find t key = Some data);
    for key = 1 to num_keys - 1 do
      assert (find t key = None)
    done;
    assert (to_alist t = [(key, data)]);
    assert (keys t = [key]);
    assert (table_data t = [data]);
  ;;

  TEST_UNIT =
    let num_keys = 10 in
    let t = create ~num_keys in
    for key = 1 to 5 do
      add_exn t ~key ~data:(Int.to_string key)
    done;
    assert (length t = 5);
    for key = 1 to 5 do
      remove t key;
    done;
    assert_empty t;
  ;;

  TEST_UNIT =
    let num_keys = 10 in
    let t = create ~num_keys in
    for key = 0 to num_keys - 1 do
      add_exn t ~key ~data:(Int.to_string key)
    done;
    assert (length t = num_keys);
    for key = 0 to num_keys - 1 do
      remove t key;
    done;
    assert_empty t;
  ;;

  (* Additional tests for [with binio], [with sexp], [t_of_sexp], [filter_map{,i}],
     [map{,i}]. *)
  TEST_UNIT =
    let outer_sexp_of_t = sexp_of_t in
    let module M = struct
      module Table = With_key (Int)
      type alist = (int * int) list with sexp_of
      type t = int Table.t with sexp_of
    end in
    let open M in
    let empty = Table.of_alist_exn [] in
    let equal = equal Int.equal Int.equal in
    for n = 0 to 5 do
      let alist = List.init n ~f:(fun i -> (i, i)) in
      let t = Table.of_alist_exn alist in
      assert (equal t t);
      List.iter alist ~f:(fun (key', data') ->
        assert (existsi t ~f:(fun ~key ~data -> key = key' && data = data'));
        assert (exists t ~f:(fun data -> data = data')));
      assert (for_alli t ~f:(fun ~key ~data -> key = data));
      assert (for_all t ~f:(fun data -> 0 <= data && data < n));
      let sort alist = List.sort alist ~cmp:(fun (i, _) (i', _) -> compare i i')  in
      let alist' = sort (to_alist t) in
      if alist <> alist' then
        failwiths "Bounded_int_table alist bug" (t, alist, alist')
          (<:sexp_of< t * alist * alist >>);
      let sexp = sexp_of_t t in
      let sexp' = outer_sexp_of_t Int.sexp_of_t Int.sexp_of_t t in
      if sexp <> sexp' then
        failwiths "Bounded_int_table sexp bug" (t, sexp, sexp')
          (<:sexp_of< t * Sexp.t * Sexp.t >>);
      let ensure_equal message t t' =
        if not (equal t t') then
          failwiths "Bounded_int_table bug" (message, t, t')
            (<:sexp_of< string * t * t >>);
      in
      ensure_equal "t_of_sexp" t (Table.t_of_sexp Int.t_of_sexp sexp);
      ensure_equal "filter_mapi" t (filter_mapi t ~f:(fun ~key ~data:_ -> Some key));
      ensure_equal "filter_map" t (filter_map t ~f:(fun data -> Some data));
      ensure_equal "filter_map None" empty (filter_map t ~f:(fun _ -> None));
      ensure_equal "map" t (map t ~f:Fn.id);
      ensure_equal "mapi" t (mapi t ~f:(fun ~key:_ ~data -> data));
      ensure_equal "map and mapi"
        (map t ~f:(fun x -> x + 1))
        (mapi t ~f:(fun ~key:_ ~data -> data + 1));
      let module T = struct
        type t = int Table.t with bin_io, sexp
      end in
      let binable_m = (module T : Binable.S with type t = T.t) in
      ensure_equal "binio" t (Binable.of_string binable_m (Binable.to_string binable_m t))
    done;
  ;;
end
