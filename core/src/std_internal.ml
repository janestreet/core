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
include Modes.Export
include Ordering.Export
include Perms.Export
include Result.Export
include Iarray.O

type (-'a : value_or_null) return = 'a With_return.return = private
  { return : ('b : value_or_null). 'a -> 'b }
[@@unboxed]

(** Raised if malloc in C bindings fail (errno * size). *)
exception C_malloc_exn of int * int

(* errno, size *)
let () =
  Basement.Stdlib_shim.Callback.Safe.register_exception
    "C_malloc_exn"
    (C_malloc_exn (0, 0))
;;

exception Finally = Exn.Finally

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let trd3 (_, _, z) = z

(** [phys_same] is like [phys_equal], but with a more general type. [phys_same] is useful
    when dealing with existential types, when one has a packed value and an unpacked value
    that one wants to check are physically equal. One can't use [phys_equal] in such a
    situation because the types are different. *)
external phys_same : ('a[@local_opt]) -> ('b[@local_opt]) -> bool @@ portable = "%eq"

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

[@@@warning "-incompatible-with-upstream"]

[%%template
[@@@kind.default k = (base_or_null, bits32 & bits32)]

let raise_s = (Error.raise_s [@kind k])]

let round = Float.round
let ( **. ) = Base.( **. )
let ( %. ) = Base.( %. )
let sprintf = Printf.sprintf

[%%template
[@@@mode.default p = (portable, nonportable)]

external stage
  :  ('a[@local_opt]) @ p
  -> ('a Staged.t[@local_opt]) @ p
  @@ portable
  = "%identity"

external unstage
  :  ('a Staged.t[@local_opt]) @ p
  -> ('a[@local_opt]) @ p
  @@ portable
  = "%identity"]

let with_return = With_return.with_return
let with_return_option = With_return.with_return_option

(* With the following aliases, we are just making extra sure that the toplevel sexp
   converters line up with the ones in our modules. *)

include Typerep_lib.Std_internal

include%template (
struct
  (* [deriving hash] is missing for [array], [bytes], and [ref] since these types are
     mutable. *)
  type ('a : value_or_null mod separable) array = 'a Array.t
  [@@deriving compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar]

  [%%rederive.portable
    type nonrec 'a array = 'a Array.t [@@deriving bin_io ~localize, typerep]]

  type bool = Bool.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , hash
    , equal ~localize
    , globalize
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type char = Char.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , hash
    , equal ~localize
    , globalize
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type float = Float.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , hash
    , equal ~localize
    , globalize
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type ('a : value_or_null mod separable) iarray = 'a Iarray.t
  [@@deriving compare ~localize, equal ~localize, sexp ~stackify, sexp_grammar]

  [%%rederive.portable
    type nonrec 'a iarray = 'a Iarray.t
    [@@deriving bin_io ~localize, hash, globalize, typerep]]

  type int = Int.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , hash
    , equal ~localize
    , globalize
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type int32 = Int32.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , hash
    , equal ~localize
    , globalize
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type int64 = Int64.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , hash
    , equal ~localize
    , globalize
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type 'a lazy_t = 'a Lazy.t
  [@@deriving
    bin_io ~localize, compare ~localize, hash, sexp ~stackify, sexp_grammar, typerep]

  type ('a : value_or_null) list = 'a List.t
  [@@deriving
    compare ~localize
    , hash
    , equal ~localize
    , globalize
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  [%%rederive type nonrec 'a list = 'a List.t [@@deriving bin_io ~localize]]

  type nativeint = Nativeint.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type ('a : value_or_null) option = 'a Option.t
  [@@deriving
    compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  [%%rederive type 'a option = 'a Option.t [@@deriving bin_io ~localize]]

  type%template nonrec ('a : k) option = ('a Option.t[@kind k])
  [@@deriving bin_io ~localize, compare ~localize, equal ~localize, sexp ~stackify]
  [@@kind k = base_non_value]

  type ('ok : value_or_null, 'err : value_or_null) result = ('ok, 'err) Result.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type%template nonrec ('ok : k, 'err) result = (('ok, 'err) Result.t[@kind k])
  [@@deriving bin_io ~localize, compare ~localize, equal ~localize, sexp ~stackify]
  [@@kind k = base_non_value]

  (* Referring to [String.t] inserts [string.ml] in the middle of a ton of module
     dependencies in this library. Instead we just nonrec with [string]. *)
  type nonrec string = string
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type bytes = Bytes.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type ('a : value_or_null) ref = 'a Ref.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type unit = Unit.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type 'a or_null = 'a Base.Or_null.t
  [@@deriving compare ~localize, equal ~localize, globalize, hash, sexp ~stackify]
end :
sig
@@ portable
  type ('a : value_or_null mod separable) array
  [@@deriving compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar]

  [%%rederive: type nonrec 'a array = 'a Array.t [@@deriving bin_io ~localize, typerep]]

  type bool
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type char
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type float
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type ('a : value_or_null mod separable) iarray = 'a Iarray.t
  [@@deriving compare ~localize, equal ~localize, sexp ~stackify, sexp_grammar]

  [%%rederive:
    type nonrec 'a iarray = 'a iarray
    [@@deriving bin_io ~localize, hash, globalize, typerep]]

  type int
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type int32
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type int64
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type 'a lazy_t
  [@@deriving
    bin_io ~localize, compare ~localize, hash, sexp ~stackify, sexp_grammar, typerep]

  type ('a : value_or_null) list
  [@@deriving
    compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  [%%rederive: type nonrec 'a list = 'a List.t [@@deriving bin_io ~localize]]

  type nativeint
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type ('a : value_or_null) option
  [@@deriving
    compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  [%%rederive: type nonrec 'a option = 'a option [@@deriving bin_io ~localize]]

  type%template ('a : k) option
  [@@deriving bin_io ~localize, compare ~localize, equal ~localize, sexp ~stackify]
  [@@kind k = base_non_value]

  type ('ok : value_or_null, 'err : value_or_null) result
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type%template ('ok : k, 'err) result
  [@@deriving bin_io ~localize, compare ~localize, equal ~localize, sexp ~stackify]
  [@@kind k = base_non_value]

  type string
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type bytes
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type ('a : value_or_null) ref
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type unit
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp ~stackify
    , sexp_grammar
    , typerep]

  type nonrec 'a or_null : value_or_null mod everything with 'a = 'a or_null
  [@@or_null_reexport]
  [@@deriving compare ~localize, equal ~localize, globalize, hash, sexp ~stackify]
end
[@with:
  type ('a : value_or_null mod separable) array := 'a array
  type bool := bool
  type char := char
  type float := float
  type ('a : value_or_null mod separable) iarray := 'a iarray
  type int := int
  type int32 := int32
  type int64 := int64
  type ('a : value_or_null) list := 'a list
  type nativeint := nativeint

  (* use a destructive substitution for [value] only to re-expose the constructors;
     otherwise, in certain cases this produces arcane errors about the type being abstract
     and ruins the output of mdx tests *)

  type ('a : any) option := 'a option
  type ('a : any) option = ('a option[@kind k]) [@@kind k = base_non_value]
  type ('ok : any, 'err : any) result := ('ok, 'err) result

  type ('ok : any, 'err : any) result = (('ok, 'err) result[@kind k])
  [@@kind k = base_non_value]

  type string := string
  type bytes := bytes
  type 'a lazy_t := 'a lazy_t
  type ('a : value_or_null) ref := 'a ref
  type unit := unit
  type 'a or_null := 'a or_null])

(* When running with versions of the compiler that don't support the [iarray] extension,
   re-export it from [Base], which defines it as an abstract type in such cases.

   We avoid re-exporting it internally as this constrains the kinding of the type
   parameter as compared to the compiler built-in [iarray]. *)

(* Export ['a or_null] with constructors [Null] and [This] whenever Core is opened, so
   uses of those identifiers work in both upstream OCaml and OxCaml. *)

let sexp_of_exn = Exn.sexp_of_t
