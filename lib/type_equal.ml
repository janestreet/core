module Sexp = Sexplib.Sexp
open Sexplib.Std

let phys_equal = (==)

type (_, _) t = T : ('a, 'a) t
type ('a, 'b) equal = ('a, 'b) t

let refl = T

let sym (type a) (type b) (T : (a, b) t) = (T : (b, a) t)

let trans (type a) (type b) (type c) (T : (a, b) t) (T : (b, c) t) = (T : (a, c) t)

let conv (type a) (type b) (T : (a, b) t) (a : a) = (a : b)

module Lift (X : sig type 'a t end) = struct
  let lift (type a) (type b) (T : (a, b) t) = (T : (a X.t, b X.t) t)
end

module Lift2 (X : sig type ('a1, 'a2) t end) = struct
  let lift (type a1) (type b1) (type a2) (type b2) (T : (a1, b1) t) (T : (a2, b2) t) =
    (T : ((a1, a2) X.t, (b1, b2) X.t) t)
  ;;
end

let detuple2 (type a1) (type a2) (type b1) (type b2)
    (T : (a1 * a2, b1 * b2) t) : (a1, b1) t * (a2, b2) t =
  T, T
;;

let tuple2 (type a1) (type a2) (type b1) (type b2)
    (T : (a1, b1) t) (T : (a2, b2) t) : (a1 * a2, b1 * b2) t =
  T
;;

module type Injective = sig
  type 'a t
  val strip : ('a t, 'b t) equal -> ('a, 'b) equal
end

module type Injective2 = sig
  type ('a1, 'a2)  t
  val strip : (('a1, 'a2) t, ('b1, 'b2) t) equal -> ('a1, 'b1) equal * ('a2, 'b2) equal
end

module Composition_preserves_injectivity (M1 : Injective) (M2 : Injective) = struct
  type 'a t = 'a M1.t M2.t
  let strip e = M1.strip (M2.strip e)
end

module Id : sig
  (* The type parameter must be invariant! See the comment over [same_witness] for why.
     This signature is included in the .ml because it is critical to the correctness of
     the module. *)
  type 'a t with sexp_of

  val create : name:string -> _ t

  val hash : _ t -> int
  val name : _ t -> string

  val same : _ t -> _ t -> bool
  val same_witness     : 'a t -> 'b t -> ('a, 'b) equal Or_error.t
  val same_witness_exn : 'a t -> 'b t -> ('a, 'b) equal
end = struct
  module Uid = Unique_id.Int63 (struct end)

  type 'a t =
    { uid : Uid.t;
      name : string;
    }
  with fields, sexp_of

  let create ~name = { uid = Uid.create (); name }

  let hash t = Uid.to_int_exn t.uid

  (* Both arguments are boxed, because records are not immediate values.  [Obj.magic] is
     safe here because [phys_equal] only inspects the addresses of boxed arguments. *)
  let same (type a) (type b) (a : a t) (b : b t) = phys_equal a (Obj.magic b : a t)

  (* The use of [Obj.magic] does not directly create any issues with using the type
     equality proof at runtime since all occurrences of [T] at runtime are the same
     anyway.  That just leaves the question of type safety.  The claim is that if two type
     identities are physically equal then their type parameters are the same.

     Type identities are boxed.  [create] is the only way to create a new type identity
     and always creates a new block.  This ensures that two type identities will be
     physically equal iff they were created by the same call to [create].

     The type parameter for a type identity is invariant, so type identities are subject
     to the value restriction.  Therefore, the result of [create] can't be unified with
     more than one type parameter.

     Because [create] creates type identities that can be instantiated to no more than one
     type parameter and two different invocations of [create] can't create type identities
     that are physically equal, physical equality is sufficient to prove that two type
     identities' type parameters are, in fact, the same.  Therefore, it is safe to create
     a proof of type equality between the two type parameters using [Obj.magic] when the
     type identities are physically equal. *)
  let same_witness (type a) (type b) (a : a t) (b : b t) =
    if same a b
    then Result.Ok (Obj.magic (refl : (a, a) equal) : (a, b) equal)
    else Or_error.error_string "Type_equal.Id.same got different ids"
  ;;

  (* The proof for [same_witness] also applied to [same_witness_exn]. *)
  let same_witness_exn (type a) (type b) (a : a t) (b : b t) =
    if same a b
    then (Obj.magic (refl : (a, a) equal) : (a, b) equal)
    else failwith "Type_equal.Id.same_exn got different ids"
  ;;

  TEST_MODULE = struct
    let t1 = create ~name:"t1"
    let t2 = create ~name:"t2"

    TEST = same t1 t1
    TEST = not (same t1 t2)

    TEST = Result.is_ok (same_witness t1 t1)
    TEST = Result.is_error (same_witness t1 t2)

    TEST_UNIT = ignore (same_witness_exn t1 t1 : (_, _) equal)
    TEST = Result.is_error (Result.try_with (fun () -> same_witness_exn t1 t2))
  end
end

(* This test shows that we need [conv] even though [Type_equal.T] is exposed. *)
TEST_MODULE = struct
  let id = Id.create ~name:"int"

  module A : sig
    type t
    val id : t Id.t
  end = struct
    type t = int
    let id = id
  end

  module B : sig
    type t
    val id : t Id.t
  end = struct
    type t = int
    let id = id
  end

  let _a_to_b (a : A.t) =
    let eq = Id.same_witness_exn A.id B.id in
    (conv eq a : B.t)
  ;;

  (* the following is rejected by the compiler *)
  (* let _a_to_b (a : A.t) =
   *   let T = Id.same_witness_exn A.id B.id in
   *   (a : B.t)
   *)

  module C = struct
    type 'a t
  end

  module Liftc = Lift (C)

  let _ac_to_bc (ac : A.t C.t) =
    let eq = Liftc.lift (Id.same_witness_exn A.id B.id) in
    (conv eq ac : B.t C.t)
  ;;
end
