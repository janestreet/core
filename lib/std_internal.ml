(* bgrundmann:
   I can't just include Sexplib.Std because
   I can only define Hashtbl once in this module
*)
module Big_int = Sexplib.Std.Big_int
module Nat     = Sexplib.Std.Nat
module Ratio   = Sexplib.Std.Ratio
module Num     = Sexplib.Std.Num
module Set = Core_set
module Map = Core_map
module Array = Core_array
include Array.Infix
module Hashtbl = Core_hashtbl
module String = Core_string
module List = struct
  include Core_list
  (** [stable_dedup] Same as [dedup] but maintains the order of the list and doesn't allow
      compare function to be specified (otherwise, the implementation in terms of Set.t
      would hide a heavyweight functor instantiation at each call). *)
  let stable_dedup = Set.Poly.stable_dedup_list

  (* This function is staged to indicate that real work (the functor application) takes
     place after a partial application. *)
  let stable_dedup_staged (type a) ~(compare : a -> a -> int)
      : (a list -> a list) Staged.t =
    let module Set =
      Set.Make (struct
        type t = a
        let compare = compare
        (* [stable_dedup_list] never calls these *)
        let t_of_sexp _ = assert false
        let sexp_of_t _ = assert false
      end)
    in
    Staged.stage Set.stable_dedup_list
  ;;

end
include List.Infix

module Queue = Core_queue
module Random = Core_random
module Stack = Core_stack
module Sys = Core_sys
module Char = Core_char

module Ordering = Ordering

module Bool = Bool
module Int = Core_int
include Int.Infix
module Int32 = Core_int32
module Int64 = Core_int64
module Nativeint = Core_nativeint

module Lazy = Core_lazy

module Field = Core_field

module Ref = Ref

(* handy shortcuts *)
include Common

include (Float : Interfaces.Robustly_comparable with type t := float)
include String.Infix
let round = Float.round
include Interfaces

module Sexp = Core_sexp

include (Sexplib.Conv : sig

  type bigstring = Sexplib.Conv.bigstring with sexp
  type mat = Sexplib.Conv.mat with sexp
  type vec = Sexplib.Conv.vec with sexp

  (* [sexp_of_opaque] and [opaque_of_sexp] are used by the code generated from
     [with sexp], [<:sexp_of< >>], and [<:of_sexp< >>].  The type [_ sexp_opaque] expands
     to uses of [sexp_of_opaque] and [opaque_of_sexp]. *)
  val sexp_of_opaque : _ -> Sexp.t
  val opaque_of_sexp : Sexp.t -> _

  val sexp_of_pair : ('a -> Sexp.t) -> ('b -> Sexp.t) -> 'a * 'b -> Sexp.t
  val pair_of_sexp:  (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> 'a * 'b

  exception Of_sexp_error of exn * Sexp.t
  val of_sexp_error : string -> Sexp.t -> _
  val of_sexp_error_exn : exn -> Sexp.t -> _

end)

let printf   = Printf.printf
let bprintf  = Printf.bprintf
let eprintf  = Printf.eprintf
let fprintf  = Printf.fprintf
let sprintf  = Printf.sprintf
let ksprintf = Printf.ksprintf

include Result.Export

(* With the following aliases, we are just making extra sure that the toplevel sexp
   converters line up with the ones in our modules. *)





include (struct
  type 'a array  = 'a Array.    t with bin_io,          sexp
  type bool      = Bool.        t with bin_io, compare, sexp
  type char      = Char.        t with bin_io, compare, sexp
  type float     = Float.       t with bin_io, compare, sexp
  type int       = Int.         t with bin_io, compare, sexp
  type int32     = Int32.       t with bin_io, compare, sexp
  type int64     = Int64.       t with bin_io, compare, sexp
  type 'a lazy_t = 'a Core_lazy.t with bin_io, compare, sexp
  type 'a list   = 'a List.     t with bin_io,          sexp
  type nativeint = Nativeint.   t with bin_io, compare, sexp
  type 'a option = 'a Option.   t with bin_io,          sexp
  type string    = String.      t with bin_io, compare, sexp
  type 'a ref    = 'a Ref.      t with bin_io,          sexp
  type unit      = Unit.        t with bin_io, compare, sexp
end : sig
  type 'a array  with bin_io,          sexp
  type bool      with bin_io, compare, sexp
  type char      with bin_io, compare, sexp
  type float     with bin_io, compare, sexp
  type int       with bin_io, compare, sexp
  type int32     with bin_io, compare, sexp
  type int64     with bin_io, compare, sexp
  type 'a lazy_t with bin_io,          sexp
  type 'a list   with bin_io,          sexp
  type nativeint with bin_io, compare, sexp
  type 'a option with bin_io,          sexp
  type string    with bin_io, compare, sexp
  type 'a ref    with bin_io,          sexp
  type unit      with bin_io, compare, sexp
end
  with type 'a array  := 'a array
  with type bool      := bool
  with type char      := char
  with type float     := float
  with type int       := int
  with type int32     := int32
  with type int64     := int64
  with type 'a list   := 'a list
  with type nativeint := nativeint
  with type 'a option := 'a option
  with type string    := string
  with type 'a lazy_t := 'a lazy_t
  with type 'a ref    := 'a ref
  with type unit      := unit
)

let sexp_of_exn = Exn.sexp_of_t


(* The below declarations define converters for the special types recognized by pa-sexp.
   E.g. this allows the following to work:

   type t = { foo : int sexp_option } with bin_io, sexp, compare

   [sexp_array], [sexp_bool], [sexp_list], and [sexp_option] allow a record field to be
   absent when converting from a sexp, and if absent, the field will take a default value
   of the appropriate type:

   {v
     sexp_array   [||]
     sexp_bool    false
     sexp_list    []
     sexp_option  None
   v}

   [sexp_opaque] causes the conversion to sexp to produce the atom {v <opaque> v}.

   For more documentation, see sexplib/README.md.
*)
type 'a sexp_array  = 'a array  with bin_io, compare
type sexp_bool      = bool      with bin_io, compare
type 'a sexp_list   = 'a list   with bin_io, compare
type 'a sexp_option = 'a option with bin_io, compare
type 'a sexp_opaque = 'a        with bin_io, compare

include Ordering.Export

(* The code below checks that the signatures in core_map.mli and core_set.mli are
   consistent with the generic map and set signatures defined in core_map_intf.ml
   and core_set_intf.ml. *)

let module M : sig
  open Core_set_intf

  module Tree : sig
    type ('a, 'b) t

    include Creators_and_accessors2_with_comparator
      with type ('a, 'b) set  := ('a, 'b) t
      with type ('a, 'b) t    := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) t
  end

  type ('a, 'b) t

  include Accessors2
    with type ('a, 'b) t    := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) Tree.t

  include Creators2_with_comparator
    with type ('a, 'b) set  := ('a, 'b) t
    with type ('a, 'b) t    := ('a, 'b) t
    with type ('a, 'b) tree := ('a, 'b) Tree.t
end = Set in ()

let module M : sig
  open Core_map_intf

  module Tree : sig
    type ('a, 'b, 'c) t

    include Creators_and_accessors3_with_comparator
      with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) t
  end

  type ('a, 'b, 'c) t

  include Accessors3
    with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t

  include Creators3_with_comparator
    with type ('a, 'b, 'c) t    := ('a, 'b, 'c) t
    with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
end = Map in ()
