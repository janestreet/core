open Std_internal

module T : sig
  (* this module serves to enforce the invariant that True and False
     may only appear as the topmost constructor.  In any other position
     they are simplified away *)
  type 'a t = private
    | True
    | False
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Not of 'a t
    | If of 'a t * 'a t * 'a t
    | Base of 'a
  with bin_io

  val invariant : 'a t -> unit

  val true_   : 'a t
  val false_  : 'a t
  val not_    : 'a t -> 'a t
  val andalso : 'a t -> 'a t -> 'a t
  val orelse  : 'a t -> 'a t -> 'a t
  val if_     : 'a t -> 'a t -> 'a t -> 'a t
  val base    : 'a -> 'a t

  val is_constant : 'a t -> bool option

  (* for tests *)
  val a : int t
  val b : int t
  val c : int t

end = struct

  type 'a t =
    | True
    | False
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Not of 'a t
    | If of 'a t * 'a t * 'a t
    | Base of 'a
  with bin_io

  let invariant =
    let subterms = function
      | True | False | Base _      -> []
      | Not t1                     -> [t1]
      | And (t1, t2) | Or (t1, t2) -> [t1; t2]
      | If (t1, t2, t3)            -> [t1; t2; t3]
    in
    let rec no_constants = function
      | True | False -> assert false
      | t -> List.iter ~f:no_constants (subterms t)
    in
    fun t ->
      List.iter ~f:no_constants (subterms t)

  let is_constant = function
    | True -> Some true
    | False -> Some false
    | _ -> None

  let true_ = True
  let false_ = False
  let base v = Base v

  let (a, b, c) = (base 1, base 2, base 3) (* for tests *)

  let not_ = function
    | True -> False
    | False -> True
    | t -> Not t

  TEST = not_ True = False
  TEST = not_ False = True

  let andalso t1 t2 =
    match (t1, t2) with
    | (_, False) | (False, _) -> False
    | (other, True) | (True, other) -> other
    | _ -> And (t1, t2)

  TEST = andalso True b = b
  TEST = andalso a True = a
  TEST = andalso False b = False
  TEST = andalso a False = False

  let orelse t1 t2 =
    match (t1, t2) with
    | (_, True) | (True, _) -> True
    | (other, False) | (False, other) -> other
    | _ -> Or (t1, t2)

  TEST = orelse False b = b
  TEST = orelse a False = a
  TEST = orelse True b  = True
  TEST = orelse a True  = True

  let if_ a b c =
    match a with
    | True -> b
    | False -> c
    | _ ->
      match (b, c) with
      | (True, _ ) -> orelse       a  c
      | (_, False) -> andalso      a  b
      | (_, True ) -> orelse  (Not a) b
      | (False, _) -> andalso (Not a) c
      | _ -> If (a, b, c)

  TEST = if_ True b c = b
  TEST = if_ False b c = c
  TEST = if_ a True c = orelse a c
  TEST = if_ a b False = andalso a b
  TEST = if_ a b True = if_ (not_ a) True b  (* b/c (if a b c) = (if (not a) c b) *)
  TEST = if_ a b True = orelse (not_ a) b
  TEST = if_ a False c = if_ (not_ a) c False  (* b/c (if a b c) = (if (not a) c b) *)
  TEST = if_ a False c = andalso (not_ a) c

end
include T

let and_ ts =
  let rec loop acc = function
    | [] -> acc
    | False :: _ -> false_
    | t :: ts -> loop (andalso acc t) ts
  in
  loop true_ ts

TEST = let (a, b, c) = (base 1, base 2, base 3) in
       and_ [a; b; c] = andalso (andalso a b) c

let or_ ts =
  let rec loop acc = function
    | [] -> acc
    | True :: _ -> true_
    | t :: ts -> loop (orelse acc t) ts
  in
  loop false_ ts

TEST = let (a, b, c) = (base 1, base 2, base 3) in
       or_ [a; b; c] = orelse (orelse a b) c

TEST_MODULE "blang-list-and-or" = struct

  let test_and ts = (and_ ts = List.fold ts ~init:true_ ~f:andalso)
  let test_or  ts = (or_  ts = List.fold ts ~init:false_ ~f:orelse)

  TEST = test_or []
  TEST = test_or [a]
  TEST = test_or [true_]
  TEST = test_or [false_]
  TEST = test_or [a; true_; b]
  TEST = test_or [a; false_; b]

  TEST = test_and []
  TEST = test_and [a]
  TEST = test_and [true_]
  TEST = test_and [false_]
  TEST = test_and [a; true_; b]
  TEST = test_and [a; false_; b]

end

let constant b = if b then true_ else false_

(* [values t] lists the base predicates in [t] from left to right *)
let values t =
  let rec loop acc = function
    | Base v          :: ts -> loop (v :: acc) ts
    | True            :: ts -> loop acc ts
    | False           :: ts -> loop acc ts
    | Not t1          :: ts -> loop acc (t1 :: ts)
    | And (t1, t2)    :: ts -> loop acc (t1 :: t2 :: ts)
    | Or (t1, t2)     :: ts -> loop acc (t1 :: t2 :: ts)
    | If (t1, t2, t3) :: ts -> loop acc (t1 :: t2 :: t3 :: ts)
    | []                    -> List.rev acc
  in
  loop [] [t]

TEST = [1; 2; 3; 4; 5; 6; 7] =
  values
    (and_ [
      or_ [base 1; base 2];
      base 3;
      true_;
      if_ (base 4) (base 5) (base 6);
      not_ (base 7);
    ])

(* flatten out nested and's (used in sexp-converter) *)
let gather_conjuncts t =
  let rec loop acc = function
    | True         :: ts -> loop acc ts
    | And (t1, t2) :: ts -> loop acc (t1 :: t2 :: ts)
    | t            :: ts -> loop (t :: acc) ts
    | []                 -> List.rev acc
  in
  loop [] [t]

TEST = gather_conjuncts (base 1) = [base 1]
TEST = gather_conjuncts (and_ []) = []
TEST = gather_conjuncts (and_ [base 1]) = [base 1]
TEST = gather_conjuncts (and_ [base 1; base 2]) = [base 1; base 2]
TEST = gather_conjuncts (and_ [base 1; base 2; base 3]) = [base 1; base 2; base 3]
TEST =
  gather_conjuncts
    (and_ [
      and_ [and_ [base 1; base 2]; base 3];
      and_ [or_ [base 4; base 5]; and_ [base 6; base 7]];
    ])
  =
    [base 1; base 2; base 3; or_ [base 4; base 5]; base 6; base 7]

(* flatten out nested or's (used in sexp-converter) *)
let gather_disjuncts t =
  let rec loop acc = function
    | False       :: ts -> loop acc ts
    | Or (t1, t2) :: ts -> loop acc (t1 :: t2 :: ts)
    | t           :: ts -> loop (t :: acc) ts
    | []                -> List.rev acc
  in
  loop [] [t]

TEST = gather_disjuncts (base 1) = [base 1]
TEST = gather_disjuncts (or_ []) = []
TEST = gather_disjuncts (or_ [base 1]) = [base 1]
TEST = gather_disjuncts (or_ [base 1; base 2]) = [base 1; base 2]
TEST = gather_disjuncts (or_ [base 1; base 2; base 3]) = [base 1; base 2; base 3]
TEST =
  gather_disjuncts
    (or_ [
      or_ [or_ [base 1; base 2]; base 3];
      or_ [and_ [base 4; base 5]; or_ [base 6; base 7]];
    ])
  =
    [base 1; base 2; base 3; and_ [base 4; base 5]; base 6; base 7]

include Container.Make (struct
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
    loop init t []
end)

include (Monad.Make (struct
  type 'a t = 'a T.t
  let return = base
  let rec bind t k =
    match t with
    | Base v -> k v
    | True -> true_
    | False -> false_
    | Not t1 -> not_ (bind t1 k)
    | And (t1, t2) -> andalso (bind t1 k) (bind t2 k)
    | Or (t1, t2) -> orelse (bind t1 k) (bind t2 k)
    | If (t1, t2, t3) -> if_ (bind t1 k) (bind t2 k) (bind t3 k)
end) : Monad.S with type 'a t := 'a t)

(* semantics *)

let eval t base_eval =
  let rec eval = function
    | True -> true
    | False -> false
    | And (t1, t2) -> eval t1 && eval t2
    | Or (t1, t2) -> eval t1 || eval t2
    | Not t -> not (eval t)
    | If (t1, t2, t3) -> if eval t1 then eval t2 else eval t3
    | Base x -> base_eval x
  in
  eval t

let specialize t f =
  bind t (fun v ->
    match f v with
    | `Known c -> constant c
    | `Unknown -> base v)

(* syntax *)

type sexp = Sexp.t = Atom of string | List of sexp list (* cheap import *)

let unary name args sexp =
  match args with
  | [x] -> x
  | _ ->
    let n = List.length args in
    of_sexp_error (sprintf "%s expects one argument, %d found" name n) sexp

let ternary name args sexp =
  match args with
  | [x; y; z] -> (x, y, z)
  | _ ->
    let n = List.length args in
    of_sexp_error (sprintf "%s expects three arguments, %d found" name n) sexp

let sexp_of_t sexp_of_value t =
  let rec aux t =
    match t with
    | Base x          -> sexp_of_value x
    | True            -> Atom "true"
    | False           -> Atom "false"
    | Not t           -> List [Atom "not"; aux t]
    | If (t1, t2, t3) -> List [Atom "if"; aux t1; aux t2; aux t3]
    | And _ as t ->
      let ts = gather_conjuncts t in List (Atom "and" :: List.map ~f:aux ts)
    | Or _ as t ->
      let ts = gather_disjuncts t in List (Atom "or" :: List.map ~f:aux ts)
  in
  aux t

let rec t_of_sexp base_of_sexp sexp =
  let base sexp = base (base_of_sexp sexp) in
  let rec aux sexp =
    match sexp with
    | Atom kw ->
      begin
        match String.lowercase kw with
        | "true"  -> true_
        | "false" -> false_
        | _       -> base sexp
      end
    | List (Atom kw :: args) ->
      begin
        match String.lowercase kw with
        | "and" -> and_ (List.map ~f:aux args)
        | "or"  -> or_  (List.map ~f:aux args)
        | "not" -> not_ (aux (unary "not" args sexp))
        | "if"  ->
          let (x, y, z) = ternary "if" args sexp in
          if_ (aux x) (aux y) (aux z)
        | _ -> base sexp
      end
    | _ -> base sexp
  in
  aux sexp


TEST_MODULE "laws" = struct

  type base = A | B | C with sexp_of

  type 'a base_fun = base -> 'a

  let sexp_of_base_fun sexp_of_a f =
    Sexp.List [
      Sexp.Atom "function";
      Sexp.List [Sexp.Atom "A"; Sexp.Atom "->"; sexp_of_a (f A)];
      Sexp.List [Sexp.Atom "B"; Sexp.Atom "->"; sexp_of_a (f B)];
      Sexp.List [Sexp.Atom "C"; Sexp.Atom "->"; sexp_of_a (f C)];
    ]

  module Gen = struct

    (* we generate all our random stuff based *)
    let prng = Random.State.make
      (String.to_list "31bb128c352e2569228fbacc590e937a29a8bb8f\
                       c4bfe7126504ce3dc400be7f401fa6f5be5dba38"
       |! Array.of_list
       |! Array.map ~f:Char.to_int)

    let bool () = Random.State.bool prng

    let element arr = arr.(Random.State.int prng (Array.length arr))

    let gen_blang gen_base =
      let atomic =
        [| (fun () -> constant (bool ()));
           (fun () -> base (gen_base ())); |]
      in
      let composite =
        [| (fun rand -> not_ (rand ()));
           (fun rand -> andalso (rand ()) (rand ()));
           (fun rand -> orelse (rand ()) (rand ()));
           (fun rand -> if_ (rand ()) (rand ()) (rand ())); |]
      in
      let rec aux ~depth =
        if depth <= 1 then
          element atomic ()
        else
          element composite (fun () -> aux ~depth:(depth - 1))
      in
      aux

    let gen_base =
      let bases = [| A; B; C |] in
      fun () -> element bases

    let gen_base_fun codomain =
      fun () ->
        let a_val = element codomain in
        let b_val = element codomain in
        let c_val = element codomain in
        function
          | A -> a_val
          | B -> b_val
          | C -> c_val

    let t () = gen_blang gen_base ~depth:5

    let f = gen_base_fun [| true; false |]
    let g = gen_base_fun [| `Unknown; `Known true; `Known false |]

    let tf = Quickcheck.pg t f
    let tg = Quickcheck.pg t g
  end

  let law gen sexp_of run =
    match Quickcheck.laws 100 gen run with
    | None -> ()
    | Some v -> failwith (Sexp.to_string_hum (sexp_of v))

  let forall_t  = law Gen.t  <:sexp_of<base t>>
  let forall_tf = law Gen.tf <:sexp_of<base t * bool base_fun>>
  let forall_tg = law Gen.tg <:sexp_of<base t * [`Known of bool | `Unknown] base_fun>>

  TEST_UNIT =
    forall_t (fun t ->
      specialize t (fun _ -> `Unknown) = t)

  TEST_UNIT =
    forall_tf (fun (t, f) ->
      specialize t (fun x -> `Known (f x)) = constant (eval t f))

  TEST_UNIT =
    forall_tg (fun (t, g) ->
      List.for_all (values (specialize t g)) ~f:(fun x -> g x = `Unknown))

  TEST_UNIT =
    forall_tg (fun (t, g) ->
      (* an arbitrary [f] such that [f x = b] whenever [g x = `Known b] *)
      let f =
        let rand_fval x = match g x with `Known b -> b | `Unknown -> Gen.bool () in
        let a_val = rand_fval A in
        let b_val = rand_fval B in
        let c_val = rand_fval C in
        function
          | A -> a_val
          | B -> b_val
          | C -> c_val
      in
      eval t f = eval (specialize t g) f)

end

