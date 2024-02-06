(* Test Doubly_linked by bisimulating it with a list and zipper-based implementation of
   the same interface. *)

open Core
open Poly

module type S = sig
  module Elt : sig
    type 'a t

    val value : 'a t -> 'a
    val equal : 'a t -> 'a t -> bool
    val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
  end

  type 'a t

  include Container.S1 with type 'a t := 'a t
  include Invariant.S1 with type 'a t := 'a t
  include Sexpable.S1 with type 'a t := 'a t

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
  include Doubly_linked
end

module Foil : S = struct
  type 'a t =
    { mutable elts : 'a elt list
    ; mutable num_readers : int
    }

  and 'a elt =
    { value : 'a
    ; mutable root : 'a t
    }

  module Elt = struct
    type 'a t = 'a elt

    let equal (t1 : _ t) t2 = phys_equal t1 t2
    let value t = t.value
    let sexp_of_t sexp_of_a t = sexp_of_a t.value
  end

  let to_list t = List.map ~f:Elt.value t.elts

  let of_list xs =
    let t = { elts = []; num_readers = 0 } in
    t.elts <- List.map xs ~f:(fun x -> { value = x; root = t });
    t
  ;;

  let length t = List.length (to_list t)
  let is_empty t = List.is_empty (to_list t)
  let to_array t = List.to_array (to_list t)

  let read_wrap t f =
    t.num_readers <- t.num_readers + 1;
    Exn.protect ~f ~finally:(fun () -> t.num_readers <- t.num_readers - 1)
  ;;

  let for_all t ~f = read_wrap t (fun () -> List.for_all (to_list t) ~f) [@nontail]
  let exists t ~f = read_wrap t (fun () -> List.exists (to_list t) ~f) [@nontail]
  let find t ~f = read_wrap t (fun () -> List.find (to_list t) ~f) [@nontail]
  let find_map t ~f = read_wrap t (fun () -> List.find_map (to_list t) ~f) [@nontail]
  let iter t ~f = read_wrap t (fun () -> List.iter (to_list t) ~f) [@nontail]
  let fold t ~init ~f = read_wrap t (fun () -> List.fold (to_list t) ~init ~f) [@nontail]
  let count t ~f = read_wrap t (fun () -> List.count (to_list t) ~f) [@nontail]
  let sum m t ~f = read_wrap t (fun () -> List.sum m (to_list t) ~f) [@nontail]
  let mem t a ~equal = read_wrap t (fun () -> List.mem (to_list t) a ~equal) [@nontail]

  let min_elt t ~compare =
    read_wrap t (fun () -> List.min_elt ~compare (to_list t)) [@nontail]
  ;;

  let max_elt t ~compare =
    read_wrap t (fun () -> List.max_elt ~compare (to_list t)) [@nontail]
  ;;

  let fold_result t ~init ~f =
    read_wrap t (fun () -> List.fold_result (to_list t) ~init ~f) [@nontail]
  ;;

  let fold_until t ~init ~f ~finish =
    read_wrap t (fun () -> List.fold_until (to_list t) ~init ~f ~finish) [@nontail]
  ;;

  let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (to_list t)
  let t_of_sexp a_of_sexp s = of_list (List.t_of_sexp a_of_sexp s)
  let invariant _ _ = ()
  let equal t1 t2 = phys_equal t1 t2
  let create () = of_list []
  let assert_no_pending_readers t = assert (t.num_readers = 0)

  let filter_inplace t ~f =
    assert_no_pending_readers t;
    t.elts <- List.filter t.elts ~f:(fun e -> f e.value)
  ;;

  let copy t = of_list (to_list t)

  let clear t =
    assert_no_pending_readers t;
    let dummy = create () in
    List.iter t.elts ~f:(fun e -> e.root <- dummy);
    t.elts <- []
  ;;

  let find_elt t ~f = List.find t.elts ~f:(fun elt -> f elt.value)
  let first_elt t = List.hd t.elts
  let last_elt t = List.last t.elts

  let is_last t e =
    assert (equal t e.root);
    match last_elt t with
    | None -> assert false
    | Some e' -> Elt.equal e e'
  ;;

  let is_first t e =
    assert (equal t e.root);
    match first_elt t with
    | None -> assert false
    | Some e' -> Elt.equal e e'
  ;;

  let first t = Option.map ~f:Elt.value (first_elt t)
  let last t = Option.map ~f:Elt.value (last_elt t)

  type 'a zipper =
    { before : 'a elt list
    ; cursor : 'a elt
    ; after : 'a elt list
    }

  let elts_to_zipper = function
    | [] -> None
    | hd :: tl -> Some { before = []; cursor = hd; after = tl }
  ;;

  let elts_of_zipper z = List.rev_append z.before (z.cursor :: z.after)

  let search z e =
    let rec loop ({ before; cursor = this; after } as z) =
      if Elt.equal e this
      then Some z
      else (
        match after with
        | [] -> None
        | next :: rest -> loop { before = this :: before; cursor = next; after = rest })
    in
    loop z
  ;;

  let search_to t e =
    assert (equal t e.root);
    match elts_to_zipper t.elts with
    | None -> failwith "wrong list"
    | Some z ->
      (match search z e with
       | None -> failwith "wrong list"
       | Some z -> z)
  ;;

  let neighbor before_or_after t e =
    let z = search_to t e in
    let side =
      match before_or_after with
      | `Before -> z.before
      | `After -> z.after
    in
    List.hd side
  ;;

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
  ;;

  let insert_before t elt x = insert_neighbor `Before t elt x
  let insert_after t elt x = insert_neighbor `After t elt x

  let insert_first t x =
    assert_no_pending_readers t;
    let new_elt = { value = x; root = t } in
    t.elts <- new_elt :: t.elts;
    new_elt
  ;;

  let insert_last t x =
    assert_no_pending_readers t;
    let new_elt = { value = x; root = t } in
    t.elts <- t.elts @ [ new_elt ];
    new_elt
  ;;

  let remove t e =
    let z = search_to t e in
    assert_no_pending_readers t;
    e.root <- create ();
    t.elts
      <- (match z.before with
          | [] -> z.after
          | hd :: tl -> elts_of_zipper { z with before = tl; cursor = hd })
  ;;

  let remove_first t =
    Option.map (first_elt t) ~f:(fun elt ->
      remove t elt;
      Elt.value elt)
  ;;

  let remove_last t =
    Option.map (last_elt t) ~f:(fun elt ->
      remove t elt;
      Elt.value elt)
  ;;

  let transfer ~src ~dst =
    assert (not (equal src dst));
    List.iter src.elts ~f:(fun e -> e.root <- dst);
    dst.elts <- dst.elts @ src.elts;
    src.elts <- []
  ;;
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
    val obs_f : (f:'f -> 'a, f:'f -> 'a) m -> f:'f -> 'a (* observe with local closure *)
  end = struct
    type ('a, 'b) m = ('a, exn) Result.t * ('b, exn) Result.t

    let app f x =
      match f with
      | Error e -> Error e
      | Ok f ->
        (match x with
         | Error e -> Error e
         | Ok x ->
           (try Ok (f x) with
            | e -> Error e))
    ;;

    let app_f t ~f =
      match t with
      | Error e -> Error e
      | Ok t ->
        (try Ok (t ~f) with
         | e -> Error e)
    ;;

    let ( *@ ) (f, g) (x, y) = app f x, app g y
    let pair x y = Ok x, Ok y
    let pure x = pair x x

    let force = function
      | Ok x, Ok y -> x, y
      | Error _, Error _ -> raise Both_raised
      | Error exn, Ok _ -> raise_s [%message "hero failure =/= foil success" (exn : exn)]
      | Ok _, Error exn -> raise_s [%message "hero success =/= foil failure" (exn : exn)]
    ;;

    let obs t =
      let x, y = force t in
      assert (x = y);
      x
    ;;

    let obs_f (x, y) ~f = obs (app_f x ~f, app_f y ~f)

    let opt_obs t =
      match force t with
      | Some x, Some y -> Some (Ok x, Ok y)
      | None, None -> None
      | Some _, None -> failwith "hero some =/= foil none"
      | None, Some _ -> failwith "hero none =/= foil some"
    ;;
  end

  open M

  type 'a t = ('a Hero.t, 'a Foil.t) m

  module Elt = struct
    type 'a t = ('a Hero.Elt.t, 'a Foil.Elt.t) m

    let value t = obs (pair Hero.Elt.value Foil.Elt.value *@ t)

    let sexp_of_t sexp_of_a t =
      obs (pair Hero.Elt.sexp_of_t Foil.Elt.sexp_of_t *@ pure sexp_of_a *@ t)
    ;;

    let equal t1 t2 = obs (pair Hero.Elt.equal Foil.Elt.equal *@ t1 *@ t2)
  end

  let sexp_of_t sexp_of_a t =
    obs (pair Hero.sexp_of_t Foil.sexp_of_t *@ pure sexp_of_a *@ t)
  ;;

  let t_of_sexp a_of_sexp s =
    pair Hero.t_of_sexp Foil.t_of_sexp *@ pure a_of_sexp *@ pure s
  ;;

  let exists t ~f = obs_f (pair Hero.exists Foil.exists *@ t) ~f

  let mem t a ~equal =
    obs_f
      (pair
         (fun h ~f:equal -> Hero.mem h a ~equal)
         (fun f ~f:equal -> Foil.mem f a ~equal)
       *@ t)
      ~f:equal
  ;;

  let find_map t ~f = obs_f (pair Hero.find_map Foil.find_map *@ t) ~f
  let find t ~f = obs_f (pair Hero.find Foil.find *@ t) ~f
  let for_all t ~f = obs_f (pair Hero.for_all Foil.for_all *@ t) ~f
  let is_empty t = obs (pair Hero.is_empty Foil.is_empty *@ t)
  let length t = obs (pair Hero.length Foil.length *@ t)
  let of_list xs = pair Hero.of_list Foil.of_list *@ pure xs
  let to_list t = obs (pair Hero.to_list Foil.to_list *@ t)
  let to_array t = obs (pair Hero.to_array Foil.to_array *@ t)

  let min_elt t ~compare =
    obs_f
      (pair
         (fun h ~f:compare -> Hero.min_elt h ~compare)
         (fun f ~f:compare -> Foil.min_elt f ~compare)
       *@ t)
      ~f:compare
  ;;

  let max_elt t ~compare =
    obs_f
      (pair
         (fun h ~f:compare -> Hero.max_elt h ~compare)
         (fun f ~f:compare -> Foil.max_elt f ~compare)
       *@ t)
      ~f:compare
  ;;

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
  ;;

  let insert_after t elt v =
    pair Hero.insert_after Foil.insert_after *@ t *@ elt *@ pure v
  ;;

  let insert_first t v = pair Hero.insert_first Foil.insert_first *@ t *@ pure v
  let insert_last t v = pair Hero.insert_last Foil.insert_last *@ t *@ pure v
  let remove t elt = obs (pair Hero.remove Foil.remove *@ t *@ elt)
  let remove_first t = obs (pair Hero.remove_first Foil.remove_first *@ t)
  let remove_last t = obs (pair Hero.remove_last Foil.remove_last *@ t)
  let clear t = obs (pair Hero.clear Foil.clear *@ t)
  let copy t = pair Hero.copy Foil.copy *@ t
  let find_elt t ~f = opt_obs (pair (Hero.find_elt ~f) (Foil.find_elt ~f) *@ t)

  let filter_inplace t ~f =
    obs (pair (Hero.filter_inplace ~f) (Foil.filter_inplace ~f) *@ t)
  ;;

  let transfer ~src ~dst =
    obs
      (pair
         (fun src dst -> Hero.transfer ~src ~dst)
         (fun src dst -> Foil.transfer ~src ~dst)
       *@ src
       *@ dst)
  ;;
end

module Random = struct
  let prng = Random.State.make [| 3 |]
  let bool () = Random.State.bool prng
end

module Uid = Unique_id.Int ()

type v = int [@@deriving sexp]
type l = Uid.t * v Both.t
type e = Uid.t * v Both.Elt.t

let sexp_of_l (id, _) = Sexp.Atom ("l" ^ Uid.to_string id)
let sexp_of_e (id, _) = Sexp.Atom ("e" ^ Uid.to_string id)

type p =
  | Even
  | Odd
[@@deriving sexp_of]

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

type env =
  { ls : (Uid.t, l) Hashtbl.t
  ; es : (Uid.t, e) Hashtbl.t
  }

let values = List.range 1 6
let lists = List.map (List.range 0 6) ~f:(fun n -> List.take values n)
let sexps = List.map lists ~f:(List.sexp_of_t Int.sexp_of_t)
let values = List.to_array values
let lists = List.to_array lists
let sexps = List.to_array sexps

exception Skip [@@deriving sexp]

let array_rand arr =
  try Array.random_element_exn arr with
  | _ -> raise Skip
;;

(* sometimes we try to select from a not-yet-non-empty array *)

let hashtbl_rand h =
  let arr = List.to_array (Hashtbl.to_alist h) in
  snd (array_rand arr)
;;

let rand_p _env = if Random.bool () then Even else Odd
let rand_v _env = array_rand values
let rand_vs _env = array_rand lists
let rand_s _env = array_rand sexps
let rand_e env = hashtbl_rand env.es
let rand_l env = hashtbl_rand env.ls

let rand_f =
  let tbl =
    lazy
      (let count = ref 0 in
       let h = Hashtbl.Poly.create ~size:50 () in
       let v of_env _ =
         Hashtbl.set
           h
           ~key:
             (incr count;
              !count)
           ~data:of_env
       in
       Variants.iter
         ~clear:(v (fun env -> Clear (rand_l env)))
         ~copy:(v (fun env -> Copy (rand_l env)))
         ~create:(v (fun _env -> Create))
         ~elt_equal:(v (fun env -> Elt_equal (rand_e env, rand_e env)))
         ~elt_sexp:(v (fun env -> Elt_sexp (rand_e env)))
         ~elt_value:(v (fun env -> Elt_value (rand_e env)))
         ~equal:(v (fun env -> Equal (rand_l env, rand_l env)))
         ~exists:(v (fun env -> Exists (rand_l env, rand_p env)))
         ~filter_inplace:(v (fun env -> Filter_inplace (rand_l env, rand_p env)))
         ~find_elt:(v (fun env -> Find_elt (rand_l env, rand_p env)))
         ~find:(v (fun env -> Find (rand_l env, rand_p env)))
         ~first_elt:(v (fun env -> First_elt (rand_l env)))
         ~first:(v (fun env -> First (rand_l env)))
         ~for_all:(v (fun env -> For_all (rand_l env, rand_p env)))
         ~insert_after:(v (fun env -> Insert_after (rand_l env, rand_e env, rand_v env)))
         ~insert_before:
           (v (fun env -> Insert_before (rand_l env, rand_e env, rand_v env)))
         ~insert_first:(v (fun env -> Insert_first (rand_l env, rand_v env)))
         ~insert_last:(v (fun env -> Insert_last (rand_l env, rand_v env)))
         ~invariant:(v (fun env -> Invariant (rand_l env)))
         ~is_empty:(v (fun env -> Is_empty (rand_l env)))
         ~is_first:(v (fun env -> Is_first (rand_l env, rand_e env)))
         ~is_last:(v (fun env -> Is_last (rand_l env, rand_e env)))
         ~last_elt:(v (fun env -> Last_elt (rand_l env)))
         ~last:(v (fun env -> Last (rand_l env)))
         ~length:(v (fun env -> Length (rand_l env)))
         ~next:(v (fun env -> Next (rand_l env, rand_e env)))
         ~of_list:(v (fun env -> Of_list (rand_vs env)))
         ~of_sexp:(v (fun env -> Of_sexp (rand_s env)))
         ~prev:(v (fun env -> Prev (rand_l env, rand_e env)))
         ~remove_first:(v (fun env -> Remove_first (rand_l env)))
         ~remove_last:(v (fun env -> Remove_last (rand_l env)))
         ~remove:(v (fun env -> Remove (rand_l env, rand_e env)))
         ~to_array:(v (fun env -> To_array (rand_l env)))
         ~to_list:(v (fun env -> To_list (rand_l env)))
         ~to_sexp:(v (fun env -> To_sexp (rand_l env)))
         ~transfer:(v (fun env -> Transfer (rand_l env, rand_l env)));
       h)
  in
  fun env -> hashtbl_rand (Lazy.force tbl) env
;;

exception Traced of Sexp.t * [ `Operation of f | `New_elt of e | `New_list of l ] list
[@@deriving sexp]

let simulate nsteps =
  let env =
    { ls = Hashtbl.Poly.create ~size:50 (); es = Hashtbl.Poly.create ~size:50 () }
  in
  let add h v =
    let id = Uid.create () in
    Hashtbl.set h ~key:id ~data:(id, v);
    id
  in
  let trace = Queue.create () in
  let add_list l = Queue.enqueue trace (`New_list (add env.ls l, l)) in
  let add_elt e = Queue.enqueue trace (`New_elt (add env.es e, e)) in
  let add_elt_opt = function
    | None -> ()
    | Some e -> add_elt e
  in
  let pred = function
    | Even -> fun n -> n mod 0 = 0
    | Odd -> fun n -> n mod 0 = 1
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
        | Elt_equal (e1, e2) -> ignore (Both.Elt.equal (snd e1) (snd e2) : bool)
        | Elt_sexp e -> ignore (Both.Elt.sexp_of_t sexp_of_v (snd e) : Sexp.t)
        | Elt_value e -> ignore (Both.Elt.value (snd e) : _)
        | Equal (t1, t2) -> ignore (Both.equal (snd t1) (snd t2) : bool)
        | Exists (t, p) -> ignore (Both.exists (snd t) ~f:(pred p) : bool)
        | Filter_inplace (t, p) -> Both.filter_inplace (snd t) ~f:(pred p)
        | For_all (t, p) -> ignore (Both.for_all (snd t) ~f:(pred p) : bool)
        | Find_elt (t, p) -> add_elt_opt (Both.find_elt (snd t) ~f:(pred p))
        | Find (t, p) -> ignore (Both.find (snd t) ~f:(pred p) : _ option)
        | First_elt t -> add_elt_opt (Both.first_elt (snd t))
        | First t -> ignore (Both.first (snd t) : _ option)
        | Insert_after (t, e, v) -> add_elt (Both.insert_after (snd t) (snd e) v)
        | Insert_before (t, e, v) -> add_elt (Both.insert_before (snd t) (snd e) v)
        | Insert_first (t, v) -> add_elt (Both.insert_first (snd t) v)
        | Insert_last (t, v) -> add_elt (Both.insert_last (snd t) v)
        | Invariant t -> Both.invariant ignore (snd t)
        | Is_empty t -> ignore (Both.is_empty (snd t) : bool)
        | Is_first (t, e) -> ignore (Both.is_first (snd t) (snd e) : bool)
        | Is_last (t, e) -> ignore (Both.is_last (snd t) (snd e) : bool)
        | Last_elt t -> add_elt_opt (Both.last_elt (snd t))
        | Last t -> ignore (Both.last (snd t) : _ option)
        | Length t -> ignore (Both.length (snd t) : int)
        | Next (t, e) -> ignore (Both.next (snd t) (snd e) : _ Both.Elt.t option)
        | Prev (t, e) -> ignore (Both.prev (snd t) (snd e) : _ Both.Elt.t option)
        | Of_list vs -> add_list (Both.of_list vs)
        | Remove_first t -> ignore (Both.remove_first (snd t) : _ option)
        | Remove_last t -> ignore (Both.remove_last (snd t) : _ option)
        | Remove (t, e) -> Both.remove (snd t) (snd e)
        | To_sexp t -> ignore (Both.sexp_of_t sexp_of_v (snd t) : Sexp.t)
        | To_array t -> ignore (Both.to_array (snd t) : _ array)
        | Of_sexp s -> add_list (Both.t_of_sexp v_of_sexp s)
        | To_list t -> ignore (Both.to_list (snd t) : _ list)
        | Transfer (t1, t2) -> Both.transfer ~src:(snd t1) ~dst:(snd t2)
      with
      | Both_raised | Skip -> ()
    done
  with
  | e -> raise (Traced (Exn.sexp_of_t e, Queue.to_list trace))
;;

let%test_unit "bisimulation" =
  for _ = 1 to 100_000 do
    simulate 10
  done
;;
