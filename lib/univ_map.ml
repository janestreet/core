open Std_internal

(* INVARIANT:
    for each [(uid, con) : Univ_map.Key.t] and [t : Univ_map.t],
      if [Uid.Map.find t uid = Some univ]
      then [univ] was built using [con].
*)

module Uid = Unique_id.Int63 (struct end) (* uniqueness of ids is required *)

module Key = struct

  type 'a t = Uid.t * 'a Univ.Constr.t with sexp_of

  let create name sexp_of_a = (Uid.create (), Univ.Constr.create name sexp_of_a)

  let name (_, con) = Univ.Constr.name con

end

type t = Univ.t Uid.Map.t

let set t (uid, con) data = Map.add t ~key:uid ~data:(Univ.create con data)

let mem t (uid, _) = Map.mem t uid

let remove t (uid, _) = Map.remove t uid

let empty = Uid.Map.empty

let is_empty = Uid.Map.is_empty

let find t (uid, con) =
  match Map.find t uid with
  | None -> None
  | Some univ ->
    match Univ.match_ univ con with
    | None -> assert false (* see INVARIANT above *)
    | Some _ as result -> result

let find_exn t key =
  match find t key with
  | Some data -> data
  | None -> failwithf "Univ_map.find_exn on unknown key %s" (Key.name key) ()

let add t key data = if mem t key then `Duplicate else `Ok (set t key data)

let add_exn t key data =
  match add t key data with
  | `Ok t -> t
  | `Duplicate -> failwithf "Univ_map.add_exn on existing key %s" (Key.name key) ()

let change_exn t key update =
  match find t key with
  | Some data -> set t key (update data)
  | None -> failwithf "Univ_map.change_exn on unknown key %s" (Key.name key) ()

let change t key update =
  let orig = find t key in
  let next = update orig in
  match next with
  | Some data -> set t key data
  | None -> if Option.is_none orig then t else remove t key

let sexp_of_t t =
  Uid.Map.data t
  |! List.map ~f:(fun u -> (Univ.constr_name u, u))
  |! List.sort ~cmp:(fun (a, _) (b, _) -> String.compare a b)
  |! <:sexp_of< (string * Univ.t) list >>

TEST_MODULE = struct

  let size = Key.create "size" Int.sexp_of_t
  let name = Key.create "name" String.sexp_of_t
  let foo  = Key.create "foo"  Float.sexp_of_t
  let kids = Key.create "kids" (List.sexp_of_t sexp_of_t)

  TEST = is_empty empty

  let test_contains t k v =
    assert (not (is_empty t));
    assert (mem t k);
    begin (* these do not raise *)
      ignore (change_exn t k Fn.id);
      ignore (change t k (function None -> assert false | o -> o));
    end;
    match find t k with
    | None -> assert false
    | Some v' -> assert (phys_equal v v')

  let test_add t k v = test_contains (set t k v) k v

  let test_find t k =
    let f1 = find t k in
    let f2 = Option.try_with (fun () -> find_exn t k) in
    match (f1, f2) with
    | (None,    None)    -> ()
    | (Some v1, Some v2) -> assert (phys_equal v1 v2)
    | (Some _,  None)    -> assert false
    | (None,    Some _)  -> assert false

  let test_change t k v =
    let t_minus = change t k (fun _ -> None) in
    assert (not (mem t_minus k));
    let t_plus = change t k (fun _ -> Some v) in
    test_contains t_plus k v;
    ()

  let test t =
    (* add *)
    test_add t size 12;
    test_add t name "hank";
    test_add t kids [t; empty];
    (* find *)
    test_find t size;
    test_find t name;
    test_find t kids;
    (* change *)
    test_change t size 33;
    test_change t name "frank";
    test_change t kids [];
    ()

  let t0 = empty
  let t1 = set t0 size 9
  let t2 = set t1 foo 13.25
  let t3 = set t2 size 15

  TEST_UNIT = test t0
  TEST_UNIT = test t1
  TEST_UNIT = test t2
  TEST_UNIT = test t3

  TEST = sexp_of_t t3 = Sexp.of_string "((foo 13.25)(size 15))"

end

module With_default = struct

  module Key = struct
    type 'a t = { key : 'a Key.t; default : 'a; }
    let create ~default name sexp_of = { default; key = Key.create name sexp_of }
  end

  let find t {Key.key; default} = Option.value ~default (find t key)

  let set t {Key.key; default=_ } v = set t key v

  let change t k update = set t k (update (find t k))

  TEST_UNIT =
    let key = Key.create ~default:0 "default 0" Int.sexp_of_t in
    assert (find empty key = 0);
    let t = set empty key 1 in
    assert (find t key = 1);
    let t = set empty key 2 in
    assert (find t key = 2);
    let t = change t key (~-) in
    assert (find t key = -2);

  TEST =
    let key = Key.create ~default:1 "default 1" Int.sexp_of_t in
    find (change empty key (~-)) key = -1
end

module With_fold = struct

  module Key = struct
    type ('a, 'b) t = { key : 'b With_default.Key.t; f : 'b -> 'a -> 'b; }
    let create ~init ~f name sexp_of =
      {f; key = With_default.Key.create ~default:init name sexp_of}
  end

  let find t {Key.key; f=_ } = With_default.find t key

  let set t {Key.key; f=_ } v = With_default.set t key v

  let change t {Key.key; f=_ } update = With_default.change t key update

  let add t {Key.key; f} v = With_default.change t key (fun acc -> f acc v)

  TEST_UNIT =
    let key = Key.create ~init:5 ~f:(+) "init 5" Int.sexp_of_t in
    assert (find empty key = 5);
    let t = add empty key 3 in
    assert (find t key = 8);
    let t = change t key (~-) in
    assert (find t key = -8);

  TEST_UNIT =
    let key =
      Key.create ~init:0 ~f:(fun _ -> assert false) "don't fold this" Int.sexp_of_t
    in
    assert (find empty key = 0);
    let t = set empty key 1 in
    assert (find t key = 1);
    let t = change t key (~-) in
    assert (find t key = -1);
end

module Multi = struct
  open With_fold
  module Key = struct
    type 'a t = ('a, 'a list) Key.t
    let create name sexp_of =
      Key.create ~init:[] ~f:(fun xs x -> x :: xs) name (List.sexp_of_t sexp_of)
  end
  let set = set
  let find = find
  let add = add
  let change = change

  TEST_UNIT =
    let key = Key.create "int list" Int.sexp_of_t in
    assert (find empty key = []);
    let t = add empty key 1 in
    assert (find t key = [1]);
    let t = set t key [2;3] in
    assert (find t key = [2;3]);
    let t = change t key (List.map ~f:(~-)) in
    assert (find t key = [-2;-3]);
end

