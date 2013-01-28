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

module Bool = Bool
module Int = Core_int
include Int.Infix
module Int32 = Core_int32
module Int64 = Core_int64
module Nativeint = Core_nativeint

(* handy shortcuts *)
include Common

include (Float : Interfaces.Robustly_comparable with type t := float)
include String.Infix
let int_of_float = Float.to_int
(* Float.ceil and floor are excluded because we haven't changed them from the default *)
let round = Float.round
include Interfaces
module Sexp = Core_sexp
include Core_sexp.Sexp_option
include Core_sexp.Sexp_list
include Core_sexp.Sexp_array
include Core_sexp.Sexp_opaque
include Sexplib.Conv
include Printf
include Scanf
include Bin_prot.Std

include Result.Export

(* These are no shorter than the Sweeksified versions, but are required if we want to use
   the type [list] instead of [List.t] and still have [with sexp] work. *)

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

let (!!) = Default.override
