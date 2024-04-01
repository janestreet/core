(* This module is very much dependent on the runtime representation of values.  Should the
   way the compiler represents various types change, it needs to be reflected in this
   module, otherwise bad things could happen.  Therefore the conversions and
   representations are tested thoroughly in [../test/test_witness.ml] and
   [../test/test_conversions.ml]
*)

open! Import
module List = Base.List
module Hash_set = Base.Hash_set

let sprintf = Printf.sprintf

module Key = struct
  type t = int [@@deriving compare, sexp_of]

  (* The integers here are the values underlying the polymorphic variants, they already
     are hashes of constructor names, and hence are expected to be uniformly
     distributed. *)
  let hash x = x
end

module Allowed_ints = struct
  type t =
    | None
    | All
    | Hash_set of Hash_set.M(Key).t
    | From_zero_to of int

  let _invariant = function
    | None | All | Hash_set _ -> ()
    | From_zero_to n -> assert (n >= 0)
  ;;

  let int_is_value t i =
    match t with
    | None -> false
    | All -> true
    | Hash_set hash_set -> Hash_set.mem hash_set i
    | From_zero_to n -> 0 <= i && i <= n
  ;;
end

module Immediacy = struct
  type t =
    | Always
    | Sometimes
    | Never
    | Unknown
  [@@deriving compare]

  let equal = [%compare.equal: t]

  let to_string = function
    | Always -> "Always"
    | Sometimes -> "Sometimes"
    | Never -> "Never"
    | Unknown -> "Unknown"
  ;;
end

open Immediacy

module T : sig
  type 'a t

  val create : 'a Typename.t -> Immediacy.t -> Allowed_ints.t -> 'a t
  val create_with_name : string -> Immediacy.t -> Allowed_ints.t -> _ t
  val immediacy : _ t -> Immediacy.t
  val allowed_ints : _ t -> Allowed_ints.t
  val typename : _ t -> string

  module Never_values : sig
    val int32 : int32 t
    val int64 : int64 t
    val nativeint : nativeint t
    val float : float t
    val string : string t
    val bytes : bytes t
    val array : _ array t
    val ref_ : _ ref t
    val tuple2 : (_ * _) t
    val tuple3 : (_ * _ * _) t
    val tuple4 : (_ * _ * _ * _) t
    val tuple5 : (_ * _ * _ * _ * _) t
    val function_ : (_ -> _) t
  end

  val never : 'a Typename.t -> 'a t
  val unknown : 'a Typename.t -> 'a t
  val option : _ t
  val list : _ t
  val magic : _ t -> _ t
end = struct
  type t_ =
    { immediacy : Immediacy.t
    ; allowed_ints : Allowed_ints.t
    ; typename : string
    }

  type 'a t = t_

  let create_with_name typename immediacy allowed_ints =
    { immediacy; allowed_ints; typename }
  ;;

  let create typename immediacy allowed_ints =
    create_with_name (Typename.name typename) immediacy allowed_ints
  ;;

  let immediacy t = t.immediacy
  let allowed_ints t = t.allowed_ints
  let typename t = t.typename
  let magic t = t
  let never_with_name name = create_with_name name Never None
  let never typename = create typename Never None
  let unknown typename = create typename Unknown None
  let option = create_with_name "option" Sometimes (Allowed_ints.From_zero_to 0)
  let list = create_with_name "list" Sometimes (Allowed_ints.From_zero_to 0)

  module Never_values = struct
    (* int32 is boxed even on 64b platform at the moment. *)
    let int32 = never typename_of_int32
    let int64 = never typename_of_int64
    let nativeint = never typename_of_nativeint
    let float = never typename_of_float
    let string = never typename_of_string
    let bytes = never typename_of_bytes
    let array = never_with_name "array"
    let ref_ = never_with_name "ref"
    let tuple2 = never_with_name "tuple2"
    let tuple3 = never_with_name "tuple3"
    let tuple4 = never_with_name "tuple4"
    let tuple5 = never_with_name "tuple5"
    let function_ = never_with_name "function"
  end
end

include T

let int = create typename_of_int Always Allowed_ints.All
let unit = create typename_of_unit Always (Allowed_ints.From_zero_to 0)
let bool = create typename_of_bool Always (Allowed_ints.From_zero_to 1)
let char = create typename_of_char Always (Allowed_ints.From_zero_to 255)

module Computation_impl = struct
  type nonrec 'a t = 'a t

  include Type_generic.Variant_and_record_intf.M (struct
    type nonrec 'a t = 'a t
  end)

  include Never_values

  let ref_ _ = ref_
  let array _ = array
  let tuple2 _ _ = tuple2
  let tuple3 _ _ _ = tuple3
  let tuple4 _ _ _ _ = tuple4
  let tuple5 _ _ _ _ _ = tuple5
  let function_ _ _ = function_
  let int = int
  let char = char
  let bool = bool
  let unit = unit
  let option _ = option
  let list _ = list

  (* An [a Lazy.t] might be a boxed closure, so must have immediacy either [Never] or
     [Sometimes].  An [a Lazy.t] value could be immediate if [a] is immediate.  But if [a]
     is never immediate, then [a Lazy.t] cannot be. *)
  let lazy_t t =
    let immediacy =
      match immediacy t with
      | Never -> Never
      | Unknown -> Unknown
      | Sometimes | Always -> Sometimes
    in
    create_with_name "lazy_t" immediacy (allowed_ints t)
  ;;

  let possibly_unboxed typename child_type =
    match immediacy child_type with
    | Never -> never typename
    | Unknown | Always | Sometimes -> unknown typename
  ;;

  let record r =
    if Record.length r > 1
    then never (Record.typename_of_t r)
    else (
      let (Field the_only_field) = Record.field r 0 in
      possibly_unboxed (Record.typename_of_t r) (Field.traverse the_only_field))
  ;;

  (* Variants with all constructors having no arguments are always immediate; variants
     with all constructors having some arguments are never immediate; mixed variants are
     sometimes immediate.

     If a variant has a single constructor, and the constructor has an argument, the
     variant can be unboxed. If unboxed, either explicitly or by default (depending on
     compiler settings), the representation is simply the argument. Otherwise, the rules
     above apply normally. *)
  let variant variant =
    let no_arg_list, one_arg_list, more_arg_list =
      Variant.fold variant ~init:([], [], []) ~f:(fun (no, one, more) (Tag t as tag) ->
        match Tag.arity t with
        | 0 -> tag :: no, one, more
        | 1 -> no, tag :: one, more
        | _ -> no, one, tag :: more)
    in
    match no_arg_list, one_arg_list, more_arg_list with
    | [], [ Tag tag ], [] when not (Variant.is_polymorphic variant) ->
      possibly_unboxed (Variant.typename_of_t variant) (Tag.traverse tag)
    | [], [], [] ->
      (* Type is uninhabited, so it's (vacuously) always immediate yet there are no
         allowed ints *)
      create (Variant.typename_of_t variant) Always Allowed_ints.None
    | [], _ :: _, _ | [], _, _ :: _ -> never (Variant.typename_of_t variant)
    | _ :: _, _, _ ->
      let no_arg_count = List.length no_arg_list in
      let allowed_ints =
        if not (Variant.is_polymorphic variant)
        then Allowed_ints.From_zero_to (no_arg_count - 1)
        else (
          let hash_set = Hash_set.create (module Key) ~size:(no_arg_count * 2) in
          List.iter no_arg_list ~f:(fun (Tag tag) ->
            match Tag.create tag with
            | Tag.Const _ -> Hash_set.add hash_set (Tag.ocaml_repr tag)
            | Tag.Args _ -> assert false);
          Allowed_ints.Hash_set hash_set)
      in
      let immediacy =
        if List.is_empty one_arg_list && List.is_empty more_arg_list
        then Always
        else Sometimes
      in
      create (Variant.typename_of_t variant) immediacy allowed_ints
  ;;

  let name = "is_immediate"
  let required = []

  module Named = struct
    module Context = struct
      type t = unit

      let create () = ()
    end

    type nonrec 'a t = 'a t ref

    (* The default witness - which is created by calling [init] and recovered at any later
       point by calling [get_wip_computation] - can only be used in a recursive type.
       Other types that don't use [get_wip_computation] will just evaluate to the actual
       witness which will replace the initial dummy one. *)
    let init _ name = ref (create name Sometimes Allowed_ints.None)
    let get_wip_computation comp = !comp

    let set_final_computation r t =
      r := t;
      t
    ;;

    let share _ = true
  end
end

module Generic = Type_generic.Make (Computation_impl)

let of_typerep typerep =
  let (`generic t) = Generic.of_typerep typerep in
  t
;;

module For_all_parameters (M : sig
  val immediacy : Immediacy.t
end) =
struct
  let witness typerep1 typerep2 =
    let t1 = of_typerep typerep1 in
    let t2 = of_typerep typerep2 in
    let i1 = immediacy t1 in
    let i2 = immediacy t2 in
    if not (Immediacy.equal i1 i2)
    then
      failwith
        (sprintf
           "type %s is not independent of its arguments"
           (Typename.name (Typerep.typename_of_t typerep1)))
    else if not (Immediacy.equal i1 M.immediacy)
    then
      failwith
        (sprintf
           "type %s does not have desired immediacy: wanted %s but got %s"
           (Typename.name (Typerep.typename_of_t typerep1))
           (Immediacy.to_string M.immediacy)
           (Immediacy.to_string i1))
    else t1
  ;;

  (* always immediate *)
  let ra = Typerep.Int

  (* never immediate *)
  let rn = Typerep.String

  (* Each of the [For_all_parameters_*] functors works by instantiating the n-ary type
     with all [Always] types, and then with all [Never] types.  If those produce the same
     immediacy, then we conclude that the n-ary type is independent of its arguments. *)

  module For_all_parameters_S1 (X : Typerepable.S1) = struct
    let t = witness (X.typerep_of_t ra) (X.typerep_of_t rn)
    let witness () = magic t
  end

  module For_all_parameters_S2 (X : Typerepable.S2) = struct
    let t = witness (X.typerep_of_t ra ra) (X.typerep_of_t rn rn)
    let witness () = magic t
  end

  module For_all_parameters_S3 (X : Typerepable.S3) = struct
    let t = witness (X.typerep_of_t ra ra ra) (X.typerep_of_t rn rn rn)
    let witness () = magic t
  end

  module For_all_parameters_S4 (X : Typerepable.S4) = struct
    let t = witness (X.typerep_of_t ra ra ra ra) (X.typerep_of_t rn rn rn rn)
    let witness () = magic t
  end

  module For_all_parameters_S5 (X : Typerepable.S5) = struct
    let t = witness (X.typerep_of_t ra ra ra ra ra) (X.typerep_of_t rn rn rn rn rn)
    let witness () = magic t
  end
end

let int_is_value t int = Allowed_ints.int_is_value (allowed_ints t) int

let int_as_value (type a) (t : a t) int =
  if int_is_value t int then Some (Obj.magic (int : int) : a) else None
;;

let int_as_value_exn (type a) (t : a t) int =
  if int_is_value t int
  then (Obj.magic (int : int) : a)
  else
    failwith (sprintf "Immediate.int_as_value_exn: typename:%S int:%d" (T.typename t) int)
;;

let value_as_int_exn (type a) (t : a t) a =
  if Obj.is_int (Obj.repr a)
  then (Obj.magic (a : a) : int)
  else failwith (sprintf "Immediate.value_as_int_exn: typename:%S" (T.typename t))
;;

let value_as_int (type a) (_ : a t) a =
  if Obj.is_int (Obj.repr a) then Some (Obj.magic (a : a) : int) else None
;;

let value_is_int (type a) (_ : a t) a = Obj.is_int (Obj.repr a)

module Always = struct
  type nonrec 'a t = 'a t

  include For_all_parameters (struct
    let immediacy = Always
  end)

  let of_typerep typerep =
    let t = of_typerep typerep in
    match immediacy t with
    | Always -> Some t
    | Unknown | Never | Sometimes -> None
  ;;

  let of_typerep_exn here typerep = Option.value_exn ~here (of_typerep typerep)
  let int_as_value = int_as_value
  let int_as_value_exn = int_as_value_exn
  let int_is_value = int_is_value
  let[@inline always] value_as_int (type a) (_ : a t) a = a |> (Obj.magic : a -> int)
  let int = int
  let char = char
  let bool = bool
  let unit = unit
end

module Sometimes = struct
  type nonrec 'a t = 'a t

  include For_all_parameters (struct
    let immediacy = Sometimes
  end)

  let of_typerep typerep =
    let t = of_typerep typerep in
    match immediacy t with
    | Sometimes -> Some t
    | Unknown | Always | Never -> None
  ;;

  let of_typerep_exn here typerep = Option.value_exn ~here (of_typerep typerep)
  let int_as_value = int_as_value
  let int_as_value_exn = int_as_value_exn
  let int_is_value = int_is_value
  let value_as_int = value_as_int
  let value_as_int_exn = value_as_int_exn
  let value_is_int = value_is_int
  let option = option
  let list = list
end

module Never = struct
  type nonrec 'a t = 'a t

  include For_all_parameters (struct
    let immediacy = Never
  end)

  let of_typerep typerep =
    let t = of_typerep typerep in
    match immediacy t with
    | Never -> Some t
    | Unknown | Always | Sometimes -> None
  ;;

  let of_typerep_exn here typerep = Option.value_exn ~here (of_typerep typerep)

  include Never_values
end

type 'a dest =
  | Always of 'a Always.t
  | Sometimes of 'a Sometimes.t
  | Never of 'a Never.t
  | Unknown

let dest t =
  match immediacy t with
  | Always -> Always t
  | Sometimes -> Sometimes t
  | Never -> Never t
  | Unknown -> Unknown
;;
