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

  let stable_dedup_involving_an_application_of_the_set_functor (type _t) ~compare =
    let module Set =
          Set.Make (struct
            type t = _t
            let compare = compare
            (* [stable_dedup_list] never calls these *)
            let t_of_sexp _ = assert false
            let sexp_of_t _ = assert false
          end)
    in
    Set.stable_dedup_list
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

(* handy shortcuts *)
include Common

include (Float : Interfaces.Robustly_comparable with type t := float)
include String.Infix
let round = Float.round
include Interfaces
module Sexp = Core_sexp
include (Sexplib.Conv : module type of Sexplib.Conv
  (* hide the special sexp type constructors so we can define them below
     with bin_io and friends. *)
  with type 'a sexp_option := 'a option
  with type 'a sexp_list   := 'a list
  with type 'a sexp_array  := 'a array
  with type 'a sexp_opaque := 'a Sexplib.Conv.sexp_opaque)
include Printf
include Scanf
include Bin_prot.Std

(* The below declarations define converters for the special type constructors recognized
   by pa-sexp.  E.g. this allows the following to work:

   type t = { foo : int sexp_option } with bin_io, sexp, compare
*)
type 'a sexp_option = 'a option with bin_io, compare
type 'a sexp_list   = 'a list   with bin_io, compare
type 'a sexp_array  = 'a array  with bin_io, compare
type 'a sexp_opaque = 'a        with bin_io, compare

include Result.Export

(* With the following aliases, we are just making extra sure that the toplevel sexp
   converters line up with the ones in our modules. *)

let sexp_of_array = Array.sexp_of_t
let array_of_sexp = Array.t_of_sexp
let sexp_of_bool = Bool.sexp_of_t
let bool_of_sexp = Bool.t_of_sexp
let sexp_of_char = Char.sexp_of_t
let char_of_sexp = Char.t_of_sexp
let sexp_of_exn = Exn.sexp_of_t
let sexp_of_float = Float.sexp_of_t
let float_of_sexp = Float.t_of_sexp
let sexp_of_int = Int.sexp_of_t
let int_of_sexp = Int.t_of_sexp
let sexp_of_int32 = Int32.sexp_of_t
let int32_of_sexp = Int32.t_of_sexp
let sexp_of_int64 = Int64.sexp_of_t
let int64_of_sexp = Int64.t_of_sexp
let sexp_of_list = List.sexp_of_t
let list_of_sexp = List.t_of_sexp
let sexp_of_nativeint = Nativeint.sexp_of_t
let nativeint_of_sexp = Nativeint.t_of_sexp
let sexp_of_option = Option.sexp_of_t
let option_of_sexp = Option.t_of_sexp
let sexp_of_string = String.sexp_of_t
let string_of_sexp = String.t_of_sexp
let lazy_t_of_sexp = Lazy.t_of_sexp
let sexp_of_lazy_t = Lazy.sexp_of_t


include Ordering.Export
