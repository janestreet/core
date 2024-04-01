(* We don't just include Sexplib.Std because one can only define Hashtbl once in this
   module. *)

open! Import

(** [include]d first so that everything else shadows it *)
include Core_pervasives

include Int.Replace_polymorphic_compare
include Base_quickcheck.Export
include Deprecate_pipe_bang
include Either.Export
include From_sexplib
include Interfaces
include List.Infix
include Never_returns
include Ordering.Export
include Perms.Export
include Result.Export

type -'a return = 'a With_return.return = private { return : 'b. 'a -> 'b } [@@unboxed]

(** Raised if malloc in C bindings fail (errno * size). *)
exception C_malloc_exn of int * int

(* errno, size *)
let () = Callback.register_exception "C_malloc_exn" (C_malloc_exn (0, 0))

exception Finally = Exn.Finally

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let trd3 (_, _, z) = z

(** [phys_same] is like [phys_equal], but with a more general type.  [phys_same] is useful
    when dealing with existential types, when one has a packed value and an unpacked value
    that one wants to check are physically equal.  One can't use [phys_equal] in such a
    situation because the types are different. *)
external phys_same : ('a[@local_opt]) -> ('b[@local_opt]) -> bool = "%eq"

let ( % ) = Int.( % )
let ( /% ) = Int.( /% )
let ( // ) = Int.( // )
let ( ==> ) a b = (not a) || b
let bprintf = Printf.bprintf
let const = Fn.const
let eprintf = Printf.eprintf
let error = Or_error.error
let error_s = Or_error.error_s
let failwithf = Base.Printf.failwithf
let failwiths = Error.failwiths
let force = Base.Lazy.force
let fprintf = Printf.fprintf
let invalid_argf = Base.Printf.invalid_argf
let ifprintf = Printf.ifprintf
let is_none = Option.is_none
let is_some = Option.is_some
let ksprintf = Printf.ksprintf
let ok_exn = Or_error.ok_exn
let phys_equal = Base.phys_equal
let print_s = Stdio.print_s
let eprint_s = Stdio.eprint_s
let printf = Printf.printf
let protect = Exn.protect
let protectx = Exn.protectx
let raise_s = Error.raise_s
let round = Float.round
let ( **. ) = Base.( **. )
let ( %. ) = Base.( %. )
let sprintf = Printf.sprintf
let stage = Staged.stage
let unstage = Staged.unstage
let with_return = With_return.with_return
let with_return_option = With_return.with_return_option

(* With the following aliases, we are just making extra sure that the toplevel sexp
   converters line up with the ones in our modules. *)

include Typerep_lib.Std_internal

include (
  struct
    (* [deriving hash] is missing for [array], [bytes], and [ref] since these types are
     mutable. *)
    type 'a array = 'a Array.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , typerep]

    type bool = Bool.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , hash
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , typerep]

    type char = Char.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , hash
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , typerep]

    type float = Float.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , hash
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , typerep]

    type int = Int.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , hash
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , typerep]

    type int32 = Int32.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , hash
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , typerep]

    type int64 = Int64.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , hash
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , typerep]

    type 'a lazy_t = 'a Lazy.t
    [@@deriving bin_io ~localize, compare ~localize, hash, sexp, sexp_grammar, typerep]

    type 'a list = 'a List.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , hash
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , typerep]

    type nativeint = Nativeint.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , sexp
      , sexp_grammar
      , typerep]

    type 'a option = 'a Option.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , sexp
      , sexp_grammar
      , typerep]

    type ('ok, 'err) result = ('ok, 'err) Result.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , sexp
      , sexp_grammar
      , typerep]

    type string = String.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , sexp
      , sexp_grammar
      , typerep]

    type bytes = Bytes.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , typerep]

    type 'a ref = 'a Ref.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , sexp
      , sexp_grammar
      , typerep]

    type unit = Unit.t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , sexp
      , sexp_grammar
      , typerep]

    (* Bin_prot has optimized functions for float arrays *)
    include struct
      type float_array = float array [@@deriving bin_io ~localize]
    end [@alert "-deprecated"]

    include (
      struct
        type float_array = Float.t array
        [@@deriving compare ~localize, sexp, sexp_grammar, typerep]
      end :
        sig
          type float_array [@@deriving compare ~localize, sexp, sexp_grammar, typerep]
        end
        with type float_array := float_array)
  end :
    sig
      type 'a array
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , sexp
        , sexp_grammar
        , typerep]

      type bool
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]

      type char
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]

      type float
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]

      type int
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]

      type int32
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]

      type int64
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]

      type 'a lazy_t
      [@@deriving bin_io ~localize, compare ~localize, hash, sexp, sexp_grammar, typerep]

      type 'a list
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]

      type nativeint
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]

      type 'a option
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]

      type ('ok, 'err) result
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]

      type string
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]

      type bytes
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , sexp
        , sexp_grammar
        , typerep]

      type 'a ref
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , sexp
        , sexp_grammar
        , typerep]

      type unit
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp
        , sexp_grammar
        , typerep]
    end
    with type 'a array := 'a array
    with type bool := bool
    with type char := char
    with type float := float
    with type int := int
    with type int32 := int32
    with type int64 := int64
    with type 'a list := 'a list
    with type nativeint := nativeint
    with type 'a option := 'a option
    with type ('ok, 'err) result := ('ok, 'err) result
    with type string := string
    with type bytes := bytes
    with type 'a lazy_t := 'a lazy_t
    with type 'a ref := 'a ref
    with type unit := unit)

let sexp_of_exn = Exn.sexp_of_t
