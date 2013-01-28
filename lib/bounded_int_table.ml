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

type ('key, 'data) t =
  { num_keys : int;
    sexp_of_key : ('key -> Sexp.t) option;
    key_to_int : 'key -> int;
    (* The number of entries in the table, not the length of the arrays below. *)
    mutable length : int;
    (* (key, data) is in the table
       iff [entries_by_key.(key_to_index key) = { key; data ; _ }] *)
    entries_by_key : ('key, 'data) Entry.t option array;
    (* The first [length] elements of [defined_entries] hold the data in the table.
       This is an optimization for fold, to keep us from wasting iterations when
       the array is sparse. *)
    defined_entries : ('key, 'data) Entry.t option array;
  }
with fields, sexp_of

let sexp_of_key t =
  match t.sexp_of_key with
  | Some f -> f
  | None -> fun key -> Int.sexp_of_t (t.key_to_int key)
;;

let to_sexp_ignore_data t = sexp_of_t (sexp_of_key t) (fun _ -> Sexp.Atom "_") t

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
    failwiths "invariant failed" (exn, to_sexp_ignore_data t) (<:sexp_of< exn * Sexp.t >>)
;;

let debug = ref false

let check_invariant t = if !debug then invariant t

let create ?sexp_of_key ~num_keys ~key_to_int () =
  if num_keys < 0 then
    failwiths "num_keys must be nonnegative" num_keys <:sexp_of< int >>;
  let t =
    { num_keys;
      sexp_of_key;
      key_to_int;
      length = 0;
      entries_by_key  = Array.create num_keys None;
      defined_entries = Array.create num_keys None;
    }
  in
  check_invariant t;
  t
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

type ('key, 'data) repr = ('key * 'data) list with sexp

let sexp_of_t sexp_of_key sexp_of_data t =
  sexp_of_repr sexp_of_key sexp_of_data (to_alist t)
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

let mem t key = is_some (entry_opt t key)

let add_assuming_not_there t ~key ~data =
  let defined_entries_index = t.length in
  let entry_opt = Some { Entry. key; data; defined_entries_index } in
  t.entries_by_key.(t.key_to_int key) <- entry_opt;
  t.defined_entries.(defined_entries_index) <- entry_opt;
  t.length <- t.length + 1;
  check_invariant t;
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
  | Some _ -> `Duplicate
  | None -> add_assuming_not_there t ~key ~data; `Ok
;;

let add_exn t ~key ~data =
  match add t ~key ~data with
  | `Ok -> ()
  | `Duplicate ->
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

TEST_MODULE = struct
  let () = debug := true

  TEST_UNIT =
    (* Check that [set] replaces the key. *)
    let t = create ~num_keys:1 ~key_to_int:(fun _ -> 0) () in
    set t ~key:13 ~data:();
    set t ~key:14 ~data:();
    assert (keys t = [14]);
  ;;

  let create ~num_keys : (_, _) t = create ~num_keys ~key_to_int:Fn.id ()

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
end
