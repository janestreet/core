open OUnit
open Core

module type S = sig
  val name : string
  module Elt : sig
    type 'a t
    val value : 'a t -> 'a
    val equal : 'a t -> 'a t -> bool
    val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
  end
  type 'a t
  include Container.S1 with type 'a t := 'a t
  include Invariant.S1 with type 'a t := 'a t
  include Sexpable. S1 with type 'a t := 'a t
  val create : unit -> 'a t
  val of_list : 'a list -> 'a t
  val equal : 'a t -> 'a t -> bool
  val is_first : 'a t -> 'a Elt.t -> bool
  val is_last : 'a t -> 'a Elt.t -> bool
  val first_elt : 'a t -> 'a Elt.t option
  val last_elt : 'a t -> 'a Elt.t option
  val first : 'a t -> 'a option
  val last : 'a t -> 'a option
  val next : 'a t -> 'a Elt.t -> 'a Elt.t option
  val prev : 'a t -> 'a Elt.t -> 'a Elt.t option
  val insert_before : 'a t -> 'a Elt.t -> 'a -> 'a Elt.t
  val insert_after : 'a t -> 'a Elt.t -> 'a -> 'a Elt.t
  val insert_first : 'a t -> 'a -> 'a Elt.t
  val insert_last : 'a t -> 'a -> 'a Elt.t
  val remove : 'a t -> 'a Elt.t -> unit
  val remove_first : 'a t -> 'a option
  val remove_last : 'a t -> 'a option
  val find_elt : 'a t -> f:('a -> bool) -> 'a Elt.t option
  val clear : 'a t -> unit
  val copy : 'a t -> 'a t
  val transfer : src:'a t -> dst:'a t -> unit
  val filter_inplace : 'a t -> f:('a -> bool) -> unit
end

module Hero : S = struct
  let name = "hero"
  include Doubly_linked
end

module Foil : S = struct

  let name = "foil"

  type 'a t = {
    mutable elts : 'a elt list;
    mutable num_readers : int;
  }

  and 'a elt = { value : 'a; mutable root : 'a t }

  module Elt = struct
    type 'a t = 'a elt
    let equal (t1 : _ t) t2 = phys_equal t1 t2
    let value t = t.value
    let sexp_of_t sexp_of_a t = sexp_of_a t.value
  end

  let to_list t = List.map ~f:Elt.value t.elts

  let of_list xs =
    let t = { elts = []; num_readers = 0 } in
    t.elts <- List.map xs ~f:(fun x -> {value = x; root = t});
    t

  let length t   = List.length (to_list t)
  let is_empty t = List.is_empty (to_list t)
  let to_array t = List.to_array (to_list t)

  let read_wrap t f    =
    t.num_readers <- t.num_readers + 1;
    Exn.protect ~f ~finally:(fun () ->
      t.num_readers <- t.num_readers - 1)

  let for_all t ~f     = read_wrap t (fun () -> List.for_all (to_list t) ~f)
  let exists t ~f      = read_wrap t (fun () -> List.exists (to_list t) ~f)
  let find t ~f        = read_wrap t (fun () -> List.find (to_list t) ~f)
  let find_map t ~f    = read_wrap t (fun () -> List.find_map (to_list t) ~f)
  let iter t ~f        = read_wrap t (fun () -> List.iter (to_list t) ~f)
  let fold t ~init ~f  = read_wrap t (fun () -> List.fold (to_list t) ~init ~f)
  let count t ~f       = read_wrap t (fun () -> List.count (to_list t) ~f)
  let sum m t ~f       = read_wrap t (fun () -> List.sum m (to_list t) ~f)
  let mem t a ~equal   = read_wrap t (fun () -> List.mem (to_list t) a ~equal)
  let min_elt t ~compare   = read_wrap t (fun () -> List.min_elt ~compare (to_list t))
  let max_elt t ~compare   = read_wrap t (fun () -> List.max_elt ~compare (to_list t))
  let fold_result t ~init ~f  =
    read_wrap t (fun () -> List.fold_result (to_list t) ~init ~f)
  let fold_until t ~init ~f  =
    read_wrap t (fun () -> List.fold_until (to_list t) ~init ~f)

  let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (to_list t)
  let t_of_sexp a_of_sexp s = of_list (List.t_of_sexp a_of_sexp s)

  let invariant _ _ = ()

  let equal t1 t2 = phys_equal t1 t2

  let create () = of_list []

  let assert_no_pending_readers t = assert (t.num_readers = 0)

  let filter_inplace t ~f =
    assert_no_pending_readers t;
    t.elts <- List.filter t.elts ~f:(fun e -> f e.value)

  let copy t = of_list (to_list t)

  let clear t =
    assert_no_pending_readers t;
    let dummy = create () in
    List.iter t.elts ~f:(fun e -> e.root <- dummy);
    t.elts <- []

  let find_elt t ~f =
    List.find t.elts ~f:(fun elt -> f elt.value)

  let first_elt t = List.hd t.elts
  let last_elt t = List.last t.elts

  let is_last t e =
    assert (equal t e.root);
    match last_elt t with
    | None -> assert false
    | Some e' -> Elt.equal e e'

  let is_first t e =
    assert (equal t e.root);
    match first_elt t with
    | None -> assert false
    | Some e' -> Elt.equal e e'

  let first t = Option.map ~f:Elt.value (first_elt t)
  let last t = Option.map ~f:Elt.value (last_elt t)

  type 'a zipper = {
    before : 'a elt list;
    cursor : 'a elt;
    after  : 'a elt list;
  }

  let elts_to_zipper = function
    | [] -> None
    | hd :: tl -> Some { before = []; cursor = hd; after = tl }

  let elts_of_zipper z = List.rev_append z.before (z.cursor :: z.after)

  let search z e =
    let rec loop ({before; cursor = this; after} as z) =
      if Elt.equal e this then Some z else
        match after with
        | [] -> None
        | next :: rest -> loop { before = this :: before; cursor = next; after = rest }
    in
    loop z

  let search_to t e =
    assert (equal t e.root);
    match elts_to_zipper t.elts with
    | None -> failwith "wrong list"
    | Some z ->
      match search z e with
      | None -> failwith "wrong list"
      | Some z -> z

  let neighbor before_or_after t e =
    let z = search_to t e in
    let side =
      match before_or_after with
      | `Before -> z.before
      | `After -> z.after
    in
    List.hd side

  let next t e = neighbor `After t e
  let prev t e = neighbor `Before t e

  let insert_neighbor before_or_after t e x =
    let z = search_to t e in
    let new_elt = { value = x; root = t } in
    let z =
      match before_or_after with
      | `Before -> { z with before = new_elt :: z.before }
      | `After -> { z with after = new_elt :: z.after }
    in
    assert_no_pending_readers t;
    t.elts <- elts_of_zipper z;
    new_elt

  let insert_before t elt x = insert_neighbor `Before t elt x
  let insert_after  t elt x = insert_neighbor `After  t elt x

  let insert_first t x =
    assert_no_pending_readers t;
    let new_elt = { value = x; root = t } in
    t.elts <- new_elt :: t.elts;
    new_elt

  let insert_last t x =
    assert_no_pending_readers t;
    let new_elt = { value = x; root = t } in
    t.elts <- t.elts @ [new_elt];
    new_elt

  let remove t e =
    let z = search_to t e in
    assert_no_pending_readers t;
    e.root <- create ();
    t.elts <- begin
      match z.before with
      | [] -> z.after
      | hd :: tl -> elts_of_zipper {z with before = tl; cursor = hd }
    end

  let remove_first t =
    Option.map (first_elt t) ~f:(fun elt -> remove t elt; Elt.value elt)

  let remove_last t =
    Option.map (last_elt t) ~f:(fun elt -> remove t elt; Elt.value elt)

  let transfer ~src ~dst =
    assert (not (equal src dst));
    List.iter src.elts ~f:(fun e -> e.root <- dst);
    dst.elts <- dst.elts @ src.elts;
    src.elts <- []

end

exception Both_raised

module Both : S = struct

  module M : sig
    type ('a1, 'a2) m
    val ( *@ ) : ('a1 -> 'b1, 'a2 -> 'b2) m -> ('a1, 'a2) m -> ('b1, 'b2) m
    val pure : 'a -> ('a, 'a) m
    val pair : 'a -> 'b -> ('a, 'b) m
    val opt_obs : ('a option, 'b option) m -> ('a, 'b) m option (* observe option *)
    val obs : ('a, 'a) m -> 'a (* observe *)
  end = struct
    type ('a, 'b) m = ('a, exn) Result.t * ('b, exn) Result.t
    let app f x =
      match f with
      | Error e -> Error e
      | Ok f ->
        match x with
        | Error e -> Error e
        | Ok x -> try Ok (f x) with e -> Error e
    let ( *@ ) (f, g) (x, y) = (app f x, app g y)
    let pair x y = (Ok x, Ok y)
    let pure x = pair x x
    let force = function
      | (Ok x,    Ok y   ) -> (x, y)
      | (Error _, Error _) -> raise Both_raised
      | (Error _, Ok _   ) -> failwith "hero failure =/= foil success"
      | (Ok _,    Error _) -> failwith "hero success =/= foil failure"
    let obs t =
      let (x, y) = force t in
      assert (x = y);
      x
    let opt_obs t =
      match force t with
      | (Some x, Some y) -> Some (Ok x, Ok y)
      | (None, None) -> None
      | (Some _, None) -> failwith "hero some =/= foil none"
      | (None, Some _) -> failwith "hero none =/= foil some"
  end
  open M

  let name = "both"

  type 'a t = ('a Hero.t, 'a Foil.t) m

  module Elt = struct

    type 'a t = ('a Hero.Elt.t, 'a Foil.Elt.t) m

    let value t =
      obs (pair Hero.Elt.value Foil.Elt.value *@ t)

    let sexp_of_t sexp_of_a t =
      obs (pair Hero.Elt.sexp_of_t Foil.Elt.sexp_of_t *@ pure sexp_of_a *@ t)

    let equal t1 t2 =
      obs (pair Hero.Elt.equal Foil.Elt.equal *@ t1 *@ t2)
  end


  let sexp_of_t sexp_of_a t =
    obs (pair Hero.sexp_of_t Foil.sexp_of_t *@ pure sexp_of_a *@ t)

  let t_of_sexp a_of_sexp s =
    pair Hero.t_of_sexp Foil.t_of_sexp *@ pure a_of_sexp *@ pure s

  let exists t ~f = obs (pair (Hero.exists ~f) (Foil.exists ~f) *@ t)
  let mem t a ~equal =
    obs (pair (fun h -> Hero.mem h a ~equal) (fun f -> Foil.mem f a ~equal) *@ t)
  ;;
  let find_map t ~f = obs (pair (Hero.find_map ~f) (Foil.find_map ~f) *@ t)
  let find t ~f = obs (pair (Hero.find ~f) (Foil.find ~f) *@ t)
  let for_all t ~f = obs (pair (Hero.for_all ~f) (Foil.for_all ~f) *@ t)
  let is_empty t = obs (pair Hero.is_empty Foil.is_empty *@ t)
  let length t = obs (pair Hero.length Foil.length *@ t)
  let of_list xs = pair Hero.of_list Foil.of_list *@ pure xs
  let to_list t = obs (pair Hero.to_list Foil.to_list *@ t)
  let to_array t = obs (pair Hero.to_array Foil.to_array *@ t)
  let min_elt t ~compare = obs (pair (Hero.min_elt ~compare) (Foil.min_elt ~compare) *@ t)
  let max_elt t ~compare = obs (pair (Hero.max_elt ~compare) (Foil.max_elt ~compare) *@ t)

  (* punt: so as not to duplicate any effects in passed-in functions *)
  let fold _ = failwith "unimplemented"
  let fold_result _ = failwith "unimplemented"
  let fold_until _ = failwith "unimplemented"
  let iter _ = failwith "unimplemented"
  let count _ = failwith "unimplemented"
  let sum _ = failwith "unimplemented"

  let invariant f t = obs (pair (Hero.invariant f) (Foil.invariant f) *@ t)

  let create () = pair Hero.create Foil.create *@ pure ()

  let equal t1 t2 = obs (pair Hero.equal Foil.equal *@ t1 *@ t2)
  let is_first t elt = obs (pair Hero.is_first Foil.is_first *@ t *@ elt)
  let is_last t elt = obs (pair Hero.is_last Foil.is_last *@ t *@ elt)

  let first_elt t = opt_obs (pair Hero.first_elt Foil.first_elt *@ t)
  let last_elt t = opt_obs (pair Hero.last_elt Foil.last_elt *@ t)

  let first t = obs (pair Hero.first Foil.first *@ t)
  let last t = obs (pair Hero.last Foil.last *@ t)

  let next t elt = opt_obs (pair Hero.next Foil.next *@ t *@ elt)
  let prev t elt = opt_obs (pair Hero.prev Foil.prev *@ t *@ elt)

  let insert_before t elt v =
    pair Hero.insert_before Foil.insert_before *@ t *@ elt *@ pure v

  let insert_after t elt v =
    pair Hero.insert_after Foil.insert_after *@ t *@ elt *@ pure v

  let insert_first t v = pair Hero.insert_first Foil.insert_first *@ t *@ pure v
  let insert_last  t v = pair Hero.insert_last  Foil.insert_last  *@ t *@ pure v

  let remove t elt   = obs (pair Hero.remove       Foil.remove       *@ t *@ elt)
  let remove_first t = obs (pair Hero.remove_first Foil.remove_first *@ t)
  let remove_last  t = obs (pair Hero.remove_last  Foil.remove_last  *@ t)

  let clear t = obs (pair Hero.clear Foil.clear *@ t)
  let copy t = pair Hero.copy Foil.copy *@ t
  let find_elt t ~f = opt_obs (pair (Hero.find_elt ~f) (Foil.find_elt ~f) *@ t)
  let filter_inplace t ~f = obs (pair (Hero.filter_inplace ~f) (Foil.filter_inplace ~f) *@ t)

  let transfer ~src ~dst =
    obs
      (pair
         (fun src dst -> Hero.transfer ~src ~dst)
         (fun src dst -> Foil.transfer ~src ~dst)
       *@ src *@ dst)

end

module Make_test (X : S) = struct

  open X

  exception Finished
  let assert_raises f =
    try f (); raise Finished with
    | Finished -> assert false
    | _ -> ()

  module Help = struct
    let of_sexp s = t_of_sexp Int.t_of_sexp (Sexp.of_string s)
    let even n = (n mod 2 = 0)
  end

  let test =
    X.name >:::
    [ "empty" >::
      (fun () ->
         let t = create () in
         assert (length t = 0);
         assert (is_empty t);
         assert (first_elt t = None);
         assert (last_elt t = None);
         assert (first t = None);
         assert (last t = None);
         assert (remove_first t = None);
         assert (remove_last t = None);
         assert (to_list t = [])
      );
      "single" >::
      (fun () ->
         let t = create () in
         let elt = insert_first t 13 in
         assert (length t = 1);
         assert (not (is_empty t));
         assert (first t = Some 13);
         assert (last t = Some 13);
         assert (to_list t = [13]);
         assert (is_first t elt);
         assert (is_last t elt);
      );
      "pair" >::
      (fun () ->
         let t = create () in
         let elt2 = insert_first t 14 in
         let elt1 = insert_first t 13 in
         assert (length t = 2);
         assert (not (is_empty t));
         assert true;
         assert (first t = Some 13);
         assert (last t = Some 14);
         assert (to_list t = [13; 14]);
         assert (is_first t elt1);
         assert (is_last t elt2);
      );
      "container" >::
      (fun () ->
         let module T = Container_test.Test_S1 (X) in
         T.test ();
      );
      "of_list" >::
      (fun () ->
         for i = 0 to 5 do
           let l = List.init i ~f:ident in
           let t = of_list l in
           assert (l = to_list t);
         done
      );
      "clear" >::
      (fun () ->
         for i = 0 to 5 do
           let t = of_list (List.init i ~f:ident) in
           clear t;
           assert (is_empty t)
         done);
      "transfer" >::
      (fun () ->
         for i1 = 0 to 3 do
           let l1 = List.init i1 ~f:ident in
           for i2 = 0 to 3 do
             let l2 = List.init i2 ~f:ident in
             let t1 = of_list l1 in
             let t2 = of_list l2 in
             transfer ~src:t1 ~dst:t2;
             assert (is_empty t1);
             assert (to_list t2 = l2 @ l1);
           done
         done
      );
      "transfer2" >::
      (fun () ->
         let l1 = create () in
         let e = insert_first l1 9 in
         let l2 = create () in
         transfer ~src:l1 ~dst:l2;
         remove l2 e;
         assert (is_empty l1);
         assert (is_empty l2);
      );
      "insert-remove" >::
      (fun () ->
         let t = create () in
         let is_elts elts =
           assert (to_list t = List.map elts ~f:Elt.value);
           let rec loop elt elts =
             match (elt, elts) with
             | (None, []) -> ()
             | (Some elt, elt' :: elts) ->
               assert (Elt.equal elt elt');
               loop (next t elt) elts
             | _ -> assert false
           in
           loop (first_elt t) elts;
           begin match elts with
           | [] -> ()
           | elt :: elts ->
             assert (prev t elt = None);
             assert (is_first t elt);
             assert (Option.equal Elt.equal (first_elt t) (Some elt));
             List.iter elts ~f:(fun elt -> assert (not (is_first t elt)));
             ignore
               (List.fold elts ~init:elt ~f:(fun prev elt ->
                  assert (Option.equal Elt.equal (X.prev t elt)
                            (Some prev));
                  elt));
           end;
           begin match List.rev elts with
           | [] -> ()
           | elt :: elts ->
             assert (next t elt = None);
             assert (is_last t elt);
             assert (Option.equal Elt.equal (last_elt t) (Some elt));
             List.iter elts ~f:(fun elt -> assert (not (is_last t elt)));
             ignore (List.fold elts ~init:elt ~f:(fun next elt ->
               assert (Option.equal Elt.equal (X.next t elt) (Some next));
               elt))
           end
         in
         let elt1 = insert_first t () in
         is_elts [elt1];
         let elt2 = insert_first t () in
         is_elts [elt2; elt1];
         let elt3 = insert_last t () in
         is_elts [elt2; elt1; elt3];
         remove t elt1;
         is_elts [elt2; elt3];
         let elt4 = insert_after t elt2 () in
         is_elts [elt2; elt4; elt3];
         let elt5 = insert_before t elt2 () in
         is_elts [elt5; elt2; elt4; elt3];
         ignore (remove_last t);
         is_elts [elt5; elt2; elt4];
         ignore (remove_first t);
         is_elts [elt2; elt4];
         ignore (remove_first t);
         is_elts [elt4];
         ignore (remove_first t);
         is_elts [];
      );
      "filter-inplace" >::
      (fun () ->
         let t = create () in
         let r1 = ref 0 in
         let r2 = ref 1 in
         let r3 = ref 2 in
         let i x = ignore (insert_first t x) in
         i r1;
         i r2;
         i r3;
         assert (length t = 3);
         filter_inplace t ~f:(fun r -> not (phys_equal r r2));
         assert (length t = 2);
         let len =
           fold t ~init:0 ~f:(fun acc x ->
             assert (not (phys_equal x r2));
             acc + 1)
         in
         assert (len = length t)
      );
      "wrong-list-1" >::
      (fun () ->
         let t1 = create () in
         let t2 = create () in
         let e1 = insert_first t1 0 in
         (try ignore (remove t2 e1); assert false with _ -> ());
      );
      "wrong-list-2" >::
      (fun () ->
         let t4 = create () in
         let t5 = t_of_sexp Int.t_of_sexp (Sexp.of_string "(1 2)") in
         match last_elt t5 with
         | None -> assert false
         | Some e6 ->
           try ignore (prev t4 e6); raise Exit
           with
           | Exit -> assert false
           | _ -> ()
      );
      "transfer-self" >::
      (fun () ->
         let l2 = of_list [] in
         try transfer ~src:l2 ~dst:l2; raise Exit
         with
         | Exit -> assert false
         | _ -> ()
      );
      "write-lock" >::: [
        "remove" >::
        (fun () ->
           let xs = [1; 2; 3] in
           let t = of_list xs in
           let e = Option.value_exn (first_elt t) in
           iter t ~f:(fun _ ->
             assert_raises (fun () -> ignore (remove_first t)));
           assert (to_list t = xs);
           iter t ~f:(fun _ ->
             assert_raises (fun () -> ignore (remove_last t)));
           assert (to_list t = xs);
           iter t ~f:(fun _ ->
             assert_raises (fun () -> ignore (remove t e)));
           assert (to_list t = xs)
        );
        "insert" >::
        (fun () ->
           let xs = [1; 2; 3] in
           let t = of_list xs in
           let e = Option.value_exn (first_elt t) in
           iter t ~f:(fun _ ->
             assert_raises (fun () -> ignore (insert_first t 4)));
           assert (to_list t = xs);
           iter t ~f:(fun _ ->
             assert_raises (fun () -> ignore (insert_last t 5)));
           assert (to_list t = xs);
           iter t ~f:(fun _ ->
             assert_raises (fun () -> ignore (insert_before t e 6)));
           assert (to_list t = xs);
           iter t ~f:(fun _ ->
             assert_raises (fun () -> ignore (insert_after t e 7)));
           assert (to_list t = xs)
        );
      ];
      "transfer2" >::
      (fun () ->
         let open Help in
         let src = of_sexp "(1)" in ignore src;
         let elt = insert_last src 4 in ignore elt;
         let dst = of_sexp "(1 2 3 4)" in ignore dst;
         transfer ~src ~dst;
         ignore (next dst elt);
         ()
      );
      "counterexample1" >::
      (fun () ->
         let open Help in
         let l = of_sexp "(1)" in
         let e = insert_first l 2 in
         invariant ignore l;
         assert (Option.is_some (remove_first l));
         assert_raises (fun () -> ignore (is_last l e));
         ()
      );
      "counterexample2" >::
      (fun () ->
         let l = of_list [1] in
         let e = insert_first l 3 in
         invariant ignore l;
         ignore (remove l e);
         invariant ignore l;
         assert (Option.is_some (first_elt l))
      );
      "counterexample3" >::
      (fun () ->
         let open Help in
         let l1 = of_sexp "(1 2 3)" in
         let l2 = copy l1 in
         transfer ~src:l2 ~dst:l1;
         invariant ignore l1;
      );
      "counterexample4" >::
      (fun () ->
         let open Help in
         let l1 = of_sexp "(1 2 3 4)" in
         assert (length l1 = 4);
         let _ = insert_last l1 4 in
         assert (length l1 = 5);
         let l2 = of_list [1; 2; 3] in
         assert (length l2 = 3);
         transfer ~src:l1 ~dst:l2;
         match (length l1, length l2) with
         | (0, 8) -> ()
         | (len1, len2) ->
           failwithf "%s: len1 = %d =/= 0; len2 = %d =/= 8" X.name len1 len2 ()
      );
    ]

end

module Bisimulation = struct

  module Random = struct
    let prng = Random.State.make
                 [|3; 1; 4; 1; 5; 9; 2; 6; 5; 3; 5; 8; 9; 7; 9; 3; 2; 3; 8; 4;
                   6; 2; 6; 4; 3; 3; 8; 3; 2; 7; 9; 5; 0; 2; 8; 8; 4; 1; 9; 7; 1;
                   6; 9; 3; 9; 9; 3; 7; 5; 1; 0; 5; 8; 2; 0; 9; 7; 4; 9; 4; 4; 5;
                   9; 2; 3; 0; 7; 8; 1; 6; 4; 0; 6; 2; 8; 6; 2; 0; 8; 9; 9; 8; 6;
                   2; 8; 0; 3; 4; 8; 2; 5; 3; 4; 2; 1; 1; 7; 0; 6; 7; 9; 8; 2; 1;
                   4; 8; 0; 8; 6; 5; 1; 3; 2; 8; 2; 3; 0; 6; 6; 4; 7; 0; 9; 3; 8;
                   4; 4; 6; 0; 9; 5; 5; 0; 5; 8; 2; 2; 3; 1; 7; 2; 5; 3; 5; 9; 4;
                   0; 8; 1; 2; 8; 4; 8; 1; 1; 1; 7; 4; 5; 0; 2; 8; 4; 1; 0; 2; 7;
                   0; 1; 9; 3; 8; 5; 2; 1; 1; 0; 5; 5; 5; 9; 6; 4; 4; 6; 2; 2; 9;
                   4; 8; 9; 5; 4; 9; 3; 0; 3; 8; 1; 9; 6; 4; 4; 2; 8; 8; 1; 0; 9;
                   7; 5; 6; 6; 5; 9; 3; 3; 4; 4; 6; 1; 2; 8; 4; 7; 5; 6; 4; 8; 2;
                   3; 3; 7; 8; 6; 7; 8; 3; 1; 6; 5; 2; 7; 1; 2; 0; 1; 9; 0; 9; 1;
                   4; 5; 6; 4; 8; 5; 6; 6; 9; 2; 3; 4; 6; 0; 3; 4; 8; 6; 1; 0; 4;
                   5; 4; 3; 2; 6; 6; 4; 8; 2; 1; 3; 3; 9; 3; 6; 0; 7; 2; 6; 0; 2;
                   4; 9; 1; 4; 1; 2; 7; 3; 7; 2; 4; 5; 8; 7; 0; 0; 6; 6; 0; 6; 3;
                   1; 5; 5; 8; 8; 1; 7; 4; 8; 8; 1; 5; 2; 0; 9; 2; 0; 9; 6; 2; 8;
                   2; 9; 2; 5; 4; 0; 9; 1; 7; 1; 5; 3; 6; 4; 3; 6; 7; 8; 9; 2; 5;
                   9; 0; 3; 6; 0; 0; 1; 1; 3; 3; 0; 5; 3; 0; 5; 4; 8; 8; 2; 0; 4;
                   6; 6; 5; 2; 1; 3; 8; 4; 1; 4; 6; 9; 5; 1; 9; 4; 1; 5; 1; 1; 6;
                   0; 9; 4; 3; 3; 0; 5; 7; 2; 7; 0; 3; 6; 5; 7; 5; 9; 5; 9; 1; 9;
                   5; 3; 0; 9; 2; 1; 8; 6; 1; 1; 7; 3; 8; 1; 9; 3; 2; 6; 1; 1; 7;
                   9; 3; 1; 0; 5; 1; 1; 8; 5; 4; 8; 0; 7; 4; 4; 6; 2; 3; 7; 9; 9;
                   6; 2; 7; 4; 9; 5; 6; 7; 3; 5; 1; 8; 8; 5; 7; 5; 2; 7; 2; 4; 8;
                   9; 1; 2; 2; 7; 9; 3; 8; 1; 8; 3; 0; 1; 1; 9; 4; 9; 1; 2; 9; 8;
                   3; 3; 6; 7; 3; 3; 6; 2; 4; 4; 0; 6; 5; 6; 6; 4; 3; 0; 8; 6; 0;
                 |]
    let int n = Random.State.int prng n
    let bool () = Random.State.bool prng
  end

  module Uid = Unique_id.Int ()

  type v = int [@@deriving sexp]
  type l = Uid.t * v Both.t
  type e = Uid.t * v Both.Elt.t

  let sexp_of_l (id, _) = Sexp.Atom ("l" ^ Uid.to_string id)
  let sexp_of_e (id, _) = Sexp.Atom ("e" ^ Uid.to_string id)

  type p = Even | Odd [@@deriving sexp_of]

  module F = struct
    type t =
      | Clear of l
      | Copy of l
      | Create
      | Elt_equal of e * e
      | Elt_sexp of e
      | Elt_value of e
      | Equal of l * l
      | Exists of l * p
      | Filter_inplace of l * p
      | Find_elt of l * p
      | Find of l * p
      | First_elt of l
      | First of l
      | For_all of l * p
      | Insert_after of l * e * v
      | Insert_before of l * e * v
      | Insert_first of l * v
      | Insert_last of l * v
      | Invariant of l
      | Is_empty of l
      | Is_first of l * e
      | Is_last of l * e
      | Last_elt of l
      | Last of l
      | Length of l
      | Next of l * e
      | Of_list of v list
      | Of_sexp of Sexp.t
      | Prev of l * e
      | Remove_first of l
      | Remove_last of l
      | Remove of l * e
      | To_array of l
      | To_list of l
      | To_sexp of l
      | Transfer of l * l
    [@@deriving sexp_of, variants]
  end
  open F
  type f = F.t [@@deriving sexp_of]

  type env = {
    ls : (Uid.t, l) Hashtbl.t;
    es : (Uid.t, e) Hashtbl.t;
  }

  let values = List.range 1 6
  let lists = List.map (List.range 0 6) ~f:(fun n -> (List.take values n))
  let sexps = List.map lists ~f:(List.sexp_of_t Int.sexp_of_t)

  let values = List.to_array values
  let lists = List.to_array lists
  let sexps = List.to_array sexps

  exception Skip [@@deriving sexp]

  let array_rand arr =
    try
      Array.random_element_exn arr
    with
      _ -> raise Skip (* sometimes we try to select from a not-yet-non-empty array *)

  let hashtbl_rand h =
    let arr = List.to_array (Hashtbl.to_alist h) in
    snd (array_rand arr)

  let rand_p _env = if Random.bool () then Even else Odd
  let rand_v _env = array_rand values
  let rand_vs _env = array_rand lists
  let rand_s _env = array_rand sexps
  let rand_e env = hashtbl_rand env.es
  let rand_l env = hashtbl_rand env.ls

  let rand_f =
    let tbl =
      lazy begin
        let count = ref 0 in
        let h = Hashtbl.Poly.create ~size:50 () in
        let v of_env _ =
          Hashtbl.set h ~key:(incr count; !count) ~data:of_env
        in
        Variants.iter
          ~clear:         (v(fun env -> Clear (rand_l env)))
          ~copy:          (v(fun env -> Copy (rand_l env)))
          ~create:        (v(fun _env -> Create))
          ~elt_equal:     (v(fun env -> Elt_equal (rand_e env, rand_e env)))
          ~elt_sexp:      (v(fun env -> Elt_sexp (rand_e env)))
          ~elt_value:     (v(fun env -> Elt_value (rand_e env)))
          ~equal:         (v(fun env -> Equal (rand_l env, rand_l env)))
          ~exists:        (v(fun env -> Exists (rand_l env, rand_p env)))
          ~filter_inplace:(v(fun env -> Filter_inplace (rand_l env, rand_p env)))
          ~find_elt:      (v(fun env -> Find_elt (rand_l env, rand_p env)))
          ~find:          (v(fun env -> Find (rand_l env, rand_p env)))
          ~first_elt:     (v(fun env -> First_elt (rand_l env)))
          ~first:         (v(fun env -> First (rand_l env)))
          ~for_all:       (v(fun env -> For_all (rand_l env, rand_p env)))
          ~insert_after:  (v(fun env -> Insert_after (rand_l env, rand_e env, rand_v env)))
          ~insert_before: (v(fun env -> Insert_before (rand_l env, rand_e env, rand_v env)))
          ~insert_first:  (v(fun env -> Insert_first (rand_l env, rand_v env)))
          ~insert_last:   (v(fun env -> Insert_last (rand_l env, rand_v env)))
          ~invariant:     (v(fun env -> Invariant (rand_l env)))
          ~is_empty:      (v(fun env -> Is_empty (rand_l env)))
          ~is_first:      (v(fun env -> Is_first (rand_l env, rand_e env)))
          ~is_last:       (v(fun env -> Is_last (rand_l env, rand_e env)))
          ~last_elt:      (v(fun env -> Last_elt (rand_l env)))
          ~last:          (v(fun env -> Last (rand_l env)))
          ~length:        (v(fun env -> Length (rand_l env)))
          ~next:          (v(fun env -> Next (rand_l env, rand_e env)))
          ~of_list:       (v(fun env -> Of_list (rand_vs env)))
          ~of_sexp:       (v(fun env -> Of_sexp (rand_s env)))
          ~prev:          (v(fun env -> Prev (rand_l env, rand_e env)))
          ~remove_first:  (v(fun env -> Remove_first (rand_l env)))
          ~remove_last:   (v(fun env -> Remove_last (rand_l env)))
          ~remove:        (v(fun env -> Remove (rand_l env, rand_e env)))
          ~to_array:      (v(fun env -> To_array (rand_l env)))
          ~to_list:       (v(fun env -> To_list (rand_l env)))
          ~to_sexp:       (v(fun env -> To_sexp (rand_l env)))
          ~transfer:      (v(fun env -> Transfer (rand_l env, rand_l env)));
        h
      end
    in
    fun env ->
      hashtbl_rand (Lazy.force tbl) env

  exception Traced of
      Sexp.t * [ `Operation of f | `New_elt of e | `New_list of l ] list
  [@@deriving sexp]

  let simulate nsteps =
    let env = {
      ls = Hashtbl.Poly.create ~size:50 ();
      es = Hashtbl.Poly.create ~size:50 ();
    }
    in
    let add h v = let id = Uid.create () in Hashtbl.set h ~key:id ~data:(id, v); id in
    let trace = Queue.create () in
    let add_list l = Queue.enqueue trace (`New_list (add env.ls l, l)) in
    let add_elt e = Queue.enqueue trace (`New_elt (add env.es e, e)) in
    let add_elt_opt = function
      | None -> ()
      | Some e -> add_elt e
    in
    let pred = function
      | Even -> fun n -> n mod 0 = 0
      | Odd  -> fun n -> n mod 0 = 1
    in
    try
      for _ = 1 to nsteps do
        try
          let f = rand_f env in
          Queue.enqueue trace (`Operation f);
          match f with
          | Clear l -> Both.clear (snd l)
          | Copy l -> add_list (Both.copy (snd l))
          | Create -> add_list (Both.create ())
          | Elt_equal (e1, e2) -> ignore (Both.Elt.equal (snd e1) (snd e2))
          | Elt_sexp e -> ignore (Both.Elt.sexp_of_t sexp_of_v (snd e))
          | Elt_value e -> ignore (Both.Elt.value (snd e))
          | Equal (t1, t2) -> ignore (Both.equal (snd t1) (snd t2))
          | Exists (t, p) -> ignore (Both.exists (snd t) ~f:(pred p))
          | Filter_inplace (t, p) -> ignore (Both.filter_inplace (snd t) ~f:(pred p))
          | For_all (t, p) -> ignore (Both.for_all (snd t) ~f:(pred p))
          | Find_elt (t, p) -> add_elt_opt (Both.find_elt (snd t) ~f:(pred p))
          | Find (t, p) -> ignore (Both.find (snd t) ~f:(pred p))
          | First_elt t -> add_elt_opt (Both.first_elt (snd t))
          | First t -> ignore (Both.first (snd t))
          | Insert_after (t, e, v) -> add_elt (Both.insert_after (snd t) (snd e) v)
          | Insert_before (t, e, v) -> add_elt (Both.insert_before (snd t) (snd e) v)
          | Insert_first (t, v) -> add_elt (Both.insert_first (snd t) v)
          | Insert_last (t, v) -> add_elt (Both.insert_last (snd t) v)
          | Invariant t -> Both.invariant ignore (snd t)
          | Is_empty t -> ignore (Both.is_empty (snd t))
          | Is_first (t, e) -> ignore (Both.is_first (snd t) (snd e))
          | Is_last (t, e) -> ignore (Both.is_last (snd t) (snd e))
          | Last_elt t -> add_elt_opt (Both.last_elt (snd t))
          | Last t -> ignore (Both.last (snd t))
          | Length t -> ignore (Both.length (snd t))
          | Next (t, e) -> ignore (Both.next (snd t) (snd e))
          | Prev (t, e) -> ignore (Both.prev (snd t) (snd e))
          | Of_list vs -> add_list (Both.of_list vs)
          | Remove_first t -> ignore (Both.remove_first (snd t))
          | Remove_last t -> ignore (Both.remove_last (snd t))
          | Remove (t, e) -> ignore (Both.remove (snd t) (snd e))
          | To_sexp t -> ignore (Both.sexp_of_t sexp_of_v (snd t))
          | To_array t -> ignore (Both.to_array (snd t))
          | Of_sexp s -> add_list (Both.t_of_sexp v_of_sexp s)
          | To_list t -> ignore (Both.to_list (snd t))
          | Transfer (t1, t2) -> ignore (Both.transfer ~src:(snd t1) ~dst:(snd t2))
        with
        | Both_raised | Skip -> ()
      done
    with
      e -> raise (Traced (Exn.sexp_of_t e, Queue.to_list trace))

  let test =
    "bisimulation" >::
    (fun () -> for _ = 1 to 100_000 do simulate 10 done)

end

module Hero_test = Make_test (Hero)
module Foil_test = Make_test (Foil)

let test =
  "doubly_linked" >::: [
    Hero_test.test;
    Foil_test.test;
    Bisimulation.test; (* uncomment this once it passes *)
  ]

