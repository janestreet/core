open! Import
open Std_internal

(* The module [T] serves to enforce the invariant that all Blang.t values are in a
   normal form whereby boolean constants True and False only appear as the topmost
   constructor -- in any other position they are simplified away using laws of
   boolean algebra.

   We also enforce that nested [And]s and [Or]s each lean to the right so that [eval]
   doesn't need so much stack space as it would if they leaned to the left.  Thought
   experiment: compare how [eval] works on right-leaning [And (a, And (b, And (c, d)))]
   versus left-leaning [And (And (And (a, b), c), d)].  The former is the best case and is
   enforced.

   Note: this file deviates from the usual pattern of modules with Stable interfaces in
   that the Stable sub-module is not the first thing to be defined in the module.  The
   reason for this deviation is so that one can convince oneself of the aforementioned
   invariant after reading only this small amount of code.  After defining T we then
   immediately define its Stable interface.
*)
module T : sig
  type +'a t = private
    | True
    | False
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Not of 'a t
    | If of 'a t * 'a t * 'a t
    | Base of 'a
  [@@deriving bin_io ~localize, compare, equal, hash, typerep]

  val invariant : 'a t -> unit
  val true_ : 'a t
  val false_ : 'a t
  val not_ : 'a t -> 'a t
  val andalso : 'a t -> 'a t -> 'a t
  val orelse : 'a t -> 'a t -> 'a t
  val if_ : 'a t -> 'a t -> 'a t -> 'a t
  val base : 'a -> 'a t
end = struct
  type +'a t =
    | True
    | False
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Not of 'a t
    | If of 'a t * 'a t * 'a t
    | Base of 'a
  [@@deriving bin_io ~localize, compare, equal, hash, typerep]

  let invariant =
    let subterms = function
      | True | False | Base _ -> []
      | Not t1 -> [ t1 ]
      | And (t1, t2) | Or (t1, t2) -> [ t1; t2 ]
      | If (t1, t2, t3) -> [ t1; t2; t3 ]
    in
    let rec contains_no_constants = function
      | True | False -> assert false
      | t -> List.iter ~f:contains_no_constants (subterms t)
    in
    fun t -> List.iter ~f:contains_no_constants (subterms t)
  ;;

  let true_ = True
  let false_ = False
  let base v = Base v

  let not_ = function
    | True -> False
    | False -> True
    | Not t -> t
    | t -> Not t
  ;;

  let rec andalso t1 t2 =
    match t1, t2 with
    | _, False | False, _ -> False
    | other, True | True, other -> other
    | And (t1a, t1b), _ ->
      (* nested [And]s lean right -- see comment above *)
      And (t1a, andalso t1b t2)
    | _ -> And (t1, t2)
  ;;

  let rec orelse t1 t2 =
    match t1, t2 with
    | _, True | True, _ -> True
    | other, False | False, other -> other
    | Or (t1a, t1b), _ ->
      (* nested [Or]s lean right -- see comment above *)
      Or (t1a, orelse t1b t2)
    | _ -> Or (t1, t2)
  ;;

  let if_ a b c =
    match a with
    | True -> b
    | False -> c
    | _ ->
      (match b, c with
       | True, _ -> orelse a c
       | _, False -> andalso a b
       | _, True -> orelse (not_ a) b
       | False, _ -> andalso (not_ a) c
       | _ -> If (a, b, c))
  ;;
end

module Raw = struct
  type 'a t = 'a T.t = private
    | True
    | False
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Not of 'a t
    | If of 'a t * 'a t * 'a t
    | Base of 'a
  [@@deriving sexp_of]
end

include T

module Stable = struct
  module V1 : sig
    (* THIS TYPE AND ITS SERIALIZATIONS SHOULD NEVER BE CHANGED - PLEASE SPEAK WITH
       ANOTHER DEVELOPER IF YOU NEED MORE DETAIL *)

    type 'a t = 'a T.t = private
      | True
      | False
      | And of 'a t * 'a t
      | Or of 'a t * 'a t
      | Not of 'a t
      | If of 'a t * 'a t * 'a t
      | Base of 'a
    [@@deriving
      bin_io ~localize, stable_witness, compare, equal, hash, sexp, sexp_grammar]

    (* the remainder of this signature consists of functions used in the definitions
       of sexp conversions that are also useful more generally *)

    val and_ : 'a t list -> 'a t
    val or_ : 'a t list -> 'a t
    val gather_conjuncts : 'a t -> 'a t list
    val gather_disjuncts : 'a t -> 'a t list
  end = struct
    type 'a t = 'a T.t = private
      | True
      | False
      | And of 'a t * 'a t
      | Or of 'a t * 'a t
      | Not of 'a t
      | If of 'a t * 'a t * 'a t
      | Base of 'a

    (* This type is assumed to be stable if the 'a is stable (see big comment above). *)
    let stable_witness (_ : 'a Stable_witness.t) : 'a t Stable_witness.t =
      Stable_witness.assert_stable
    ;;

    include (
      T :
        sig
          type 'a t [@@deriving bin_io ~localize, compare, equal, hash]
        end
        with type 'a t := 'a t)

    type sexp = Sexp.t =
      | Atom of string
      | List of sexp list

    (* cheap import *)

    (* flatten out nested and's *)
    let gather_conjuncts t =
      let rec loop acc = function
        | True :: ts -> loop acc ts
        | And (t1, t2) :: ts -> loop acc (t1 :: t2 :: ts)
        | t :: ts -> loop (t :: acc) ts
        | [] -> List.rev acc
      in
      loop [] [ t ]
    ;;

    (* flatten out nested or's *)
    let gather_disjuncts t =
      let rec loop acc = function
        | False :: ts -> loop acc ts
        | Or (t1, t2) :: ts -> loop acc (t1 :: t2 :: ts)
        | t :: ts -> loop (t :: acc) ts
        | [] -> List.rev acc
      in
      loop [] [ t ]
    ;;

    (* [and_] and [or_] use [fold_right] instead of [fold_left] to avoid quadratic
       behavior with [andalso] or [orelse], respectively. *)
    let and_ ts = List.fold_right ts ~init:true_ ~f:andalso
    let or_ ts = List.fold_right ts ~init:false_ ~f:orelse

    let unary name args sexp =
      match args with
      | [ x ] -> x
      | _ ->
        let n = List.length args in
        of_sexp_error (sprintf "%s expects one argument, %d found" name n) sexp
    ;;

    let ternary name args sexp =
      match args with
      | [ x; y; z ] -> x, y, z
      | _ ->
        let n = List.length args in
        of_sexp_error (sprintf "%s expects three arguments, %d found" name n) sexp
    ;;

    let sexp_of_t sexp_of_value t =
      let rec aux t =
        match t with
        | Base x -> sexp_of_value x
        | True -> Atom "true"
        | False -> Atom "false"
        | Not t -> List [ Atom "not"; aux t ]
        | If (t1, t2, t3) -> List [ Atom "if"; aux t1; aux t2; aux t3 ]
        | And _ as t ->
          let ts = gather_conjuncts t in
          List (Atom "and" :: List.map ~f:aux ts)
        | Or _ as t ->
          let ts = gather_disjuncts t in
          List (Atom "or" :: List.map ~f:aux ts)
      in
      aux t
    ;;

    let t_of_sexp base_of_sexp sexp =
      let base sexp = base (base_of_sexp sexp) in
      let rec aux sexp =
        match sexp with
        | Atom kw ->
          (match String.lowercase kw with
           | "true" -> true_
           | "false" -> false_
           | _ -> base sexp)
        | List (Atom kw :: args) ->
          (match String.lowercase kw with
           | "and" -> and_ (List.map ~f:aux args)
           | "or" -> or_ (List.map ~f:aux args)
           | "not" -> not_ (aux (unary "not" args sexp))
           | "if" ->
             let x, y, z = ternary "if" args sexp in
             if_ (aux x) (aux y) (aux z)
           | _ -> base sexp)
        | _ -> base sexp
      in
      aux sexp
    ;;

    let t_sexp_grammar : 'a. 'a Sexplib.Sexp_grammar.t -> 'a t Sexplib.Sexp_grammar.t =
      let defns : Sexplib.Sexp_grammar.defn list =
        let blang : Sexplib.Sexp_grammar.grammar = Recursive ("blang", [ Tyvar "a" ]) in
        [ { tycon = "blang"
          ; tyvars = [ "a" ]
          ; grammar =
              Union
                [ Tyvar "a"
                ; Variant
                    { case_sensitivity = Case_insensitive
                    ; clauses =
                        [ No_tag { name = "true"; clause_kind = Atom_clause }
                        ; No_tag { name = "false"; clause_kind = Atom_clause }
                        ; No_tag
                            { name = "if"
                            ; clause_kind =
                                List_clause
                                  { args = Cons (blang, Cons (blang, Cons (blang, Empty)))
                                  }
                            }
                        ; No_tag
                            { name = "and"
                            ; clause_kind = List_clause { args = Many blang }
                            }
                        ; No_tag
                            { name = "or"
                            ; clause_kind = List_clause { args = Many blang }
                            }
                        ; No_tag
                            { name = "not"
                            ; clause_kind = List_clause { args = Cons (blang, Empty) }
                            }
                        ]
                    }
                ]
          }
        ]
      in
      fun base_grammar -> { untyped = Tycon ("blang", [ base_grammar.untyped ], defns) }
    ;;
  end
end

include (Stable.V1 : module type of Stable.V1 with type 'a t := 'a t)

let constant b = if b then true_ else false_

module type Constructors = sig
  val base : 'a -> 'a t
  val true_ : _ t
  val false_ : _ t
  val constant : bool -> _ t
  val not_ : 'a t -> 'a t
  val and_ : 'a t list -> 'a t
  val or_ : 'a t list -> 'a t
  val if_ : 'a t -> 'a t -> 'a t -> 'a t
end

module O = struct
  include T

  let not = not_
  let and_ = and_
  let or_ = or_
  let constant = constant
  let ( && ) = andalso
  let ( || ) = orelse
  let ( ==> ) a b = (not a) || b
end

let constant_value = function
  | True -> Some true
  | False -> Some false
  | _ -> None
;;

(* [values t] lists the base predicates in [t] from left to right *)
let values t =
  let rec loop acc = function
    | Base v :: ts -> loop (v :: acc) ts
    | True :: ts -> loop acc ts
    | False :: ts -> loop acc ts
    | Not t1 :: ts -> loop acc (t1 :: ts)
    | And (t1, t2) :: ts -> loop acc (t1 :: t2 :: ts)
    | Or (t1, t2) :: ts -> loop acc (t1 :: t2 :: ts)
    | If (t1, t2, t3) :: ts -> loop acc (t1 :: t2 :: t3 :: ts)
    | [] -> List.rev acc
  in
  loop [] [ t ]
;;

module C = Container.Make (struct
  type 'a t = 'a T.t

  let fold t ~init ~f =
    let rec loop acc t pending =
      match t with
      | Base a -> next (f acc a) pending
      | True | False -> next acc pending
      | Not t -> loop acc t pending
      | And (t1, t2) | Or (t1, t2) -> loop acc t1 (t2 :: pending)
      | If (t1, t2, t3) -> loop acc t1 (t2 :: t3 :: pending)
    and next acc = function
      | [] -> acc
      | t :: ts -> loop acc t ts
    in
    loop init t [] [@nontail]
  ;;

  (* Don't allocate *)
  let rec iter t ~f =
    match t with
    | Base a -> f a
    | True | False -> ()
    | Not t -> iter t ~f
    | And (t1, t2) | Or (t1, t2) ->
      iter t1 ~f;
      iter t2 ~f
    | If (t1, t2, t3) ->
      iter t1 ~f;
      iter t2 ~f;
      iter t3 ~f
  ;;

  let iter = `Custom iter
  let length = `Define_using_fold
end)

let count = C.count
let sum = C.sum
let exists = C.exists
let find = C.find
let find_map = C.find_map
let fold = C.fold
let for_all = C.for_all
let is_empty = C.is_empty
let iter = C.iter
let length = C.length
let mem = C.mem
let to_array = C.to_array
let to_list = C.to_list
let min_elt = C.min_elt
let max_elt = C.max_elt
let fold_result = C.fold_result
let fold_until = C.fold_until

let rec bind t ~f:k =
  match t with
  | Base v -> k v
  | True -> true_
  | False -> false_
  | Not t1 -> not_ (bind t1 ~f:k)
  (* Unfortunately we need to duplicate some of the short-circuiting from [andalso] and
     friends here. In principle we could do something involving [Lazy.t] but the
     overhead probably wouldn't be worth it. *)
  | And (t1, t2) ->
    (match bind t1 ~f:k with
     | False -> false_
     | other -> andalso other (bind t2 ~f:k))
  | Or (t1, t2) ->
    (match bind t1 ~f:k with
     | True -> true_
     | other -> orelse other (bind t2 ~f:k))
  | If (t1, t2, t3) ->
    (match bind t1 ~f:k with
     | True -> bind t2 ~f:k
     | False -> bind t3 ~f:k
     | other -> if_ other (bind t2 ~f:k) (bind t3 ~f:k))
;;

(* semantics *)

let rec eval t base_eval =
  match t with
  | True -> true
  | False -> false
  | And (t1, t2) -> eval t1 base_eval && eval t2 base_eval
  | Or (t1, t2) -> eval t1 base_eval || eval t2 base_eval
  | Not t -> not (eval t base_eval)
  | If (t1, t2, t3) -> if eval t1 base_eval then eval t2 base_eval else eval t3 base_eval
  | Base x -> base_eval x
;;

let specialize t f =
  bind t ~f:(fun v ->
    match f v with
    | `Known c -> constant c
    | `Unknown -> base v) [@nontail]
;;

let eval_set ~universe:all set_of_base t =
  let rec aux (b : _ t) =
    match b with
    | True -> force all
    | False -> Set.Using_comparator.empty ~comparator:(Set.comparator (force all))
    | And (a, b) -> Set.inter (aux a) (aux b)
    | Or (a, b) -> Set.union (aux a) (aux b)
    | Not a -> Set.diff (force all) (aux a)
    | Base a -> set_of_base a
    | If (cond, a, b) ->
      let cond = aux cond in
      Set.union (Set.inter cond (aux a)) (Set.inter (Set.diff (force all) cond) (aux b))
  in
  aux t [@nontail]
;;

include Monad.Make (struct
  type 'a t = 'a T.t

  let return = base
  let bind = bind
  let map = `Define_using_bind
end)

module type Monadic = sig
  module M : Monad.S

  val map : 'a t -> f:('a -> 'b M.t) -> 'b t M.t
  val bind : 'a t -> f:('a -> 'b t M.t) -> 'b t M.t
  val eval : 'a t -> f:('a -> bool M.t) -> bool M.t
end

module For_monad (M : Monad.S) : Monadic with module M := M = struct
  open M.Monad_infix

  let rec bind t ~f =
    match t with
    | Base x -> f x
    | True -> M.return true_
    | False -> M.return false_
    | And (a, b) ->
      bind a ~f
      >>= (function
      | False -> M.return false_
      | True -> bind b ~f
      | a -> bind b ~f >>| fun b -> andalso a b)
    | Or (a, b) ->
      bind a ~f
      >>= (function
      | True -> M.return true_
      | False -> bind b ~f
      | a -> bind b ~f >>| fun b -> orelse a b)
    | Not a -> bind a ~f >>| not_
    | If (a, b, c) ->
      bind a ~f
      >>= (function
      | True -> bind b ~f
      | False -> bind c ~f
      | a -> bind b ~f >>= fun b -> bind c ~f >>| fun c -> if_ a b c)
  ;;

  let map t ~f = bind t ~f:(fun x -> f x >>| base)

  let eval t ~f =
    bind t ~f:(fun x ->
      f x
      >>| function
      | true -> true_
      | false -> false_)
    >>| fun t -> eval t Nothing.unreachable_code
  ;;
end

(** We avoid deriving quickcheck to ensure that the invariants described in [T]'s comments
    above are preserved. *)
let quickcheck_generator a_generator =
  Quickcheck.Generator.recursive_union
    [ Quickcheck.Generator.map ~f:base a_generator
    ; Quickcheck.Generator.singleton true_
    ; Quickcheck.Generator.singleton false_
    ]
    ~f:(fun self ->
      [ Quickcheck.Generator.map self ~f:not_
      ; Quickcheck.Generator.map2 self self ~f:O.( || )
      ; Quickcheck.Generator.map2 self self ~f:O.( && )
      ; Quickcheck.Generator.map3 self self self ~f:if_
      ])
;;

let quickcheck_shrinker (type a) (a_shrinker : a Quickcheck.Shrinker.t) =
  Quickcheck.Shrinker.fixed_point (fun self ->
    let binop operator left right =
      Sequence.round_robin
        [ Sequence.singleton left
        ; Sequence.singleton right
        ; Sequence.map (Quickcheck.Shrinker.shrink self left) ~f:(fun left ->
            operator left right)
        ; Sequence.map (Quickcheck.Shrinker.shrink self right) ~f:(fun right ->
            operator left right)
        ]
    in
    Quickcheck.Shrinker.create (fun t ->
      match t with
      | True | False -> Sequence.empty
      | Base a -> Sequence.map ~f:base (Quickcheck.Shrinker.shrink a_shrinker a)
      | Or (left, right) -> binop O.( || ) left right
      | And (left, right) -> binop O.( && ) left right
      | Not t ->
        Sequence.append
          (Sequence.singleton t)
          (Sequence.map ~f:not_ (Quickcheck.Shrinker.shrink self t))
      | If (if_, then_, else_) ->
        Sequence.round_robin
          [ Sequence.singleton if_
          ; Sequence.singleton then_
          ; Sequence.singleton else_
          ; Sequence.map (Quickcheck.Shrinker.shrink self if_) ~f:(fun if_ ->
              O.if_ if_ then_ else_)
          ; Sequence.map (Quickcheck.Shrinker.shrink self then_) ~f:(fun then_ ->
              O.if_ if_ then_ else_)
          ; Sequence.map (Quickcheck.Shrinker.shrink self else_) ~f:(fun else_ ->
              O.if_ if_ then_ else_)
          ]))
;;

let quickcheck_observer (type a) (a_observer : a Quickcheck.Observer.t) =
  Base_quickcheck.Observer.create (fun t ~size ~hash ->
    hash_fold_t
      (fun hash a -> Quickcheck.Observer.observe a_observer a ~size ~hash)
      hash
      t)
;;
