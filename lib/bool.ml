open Sexplib.Std
open Bin_prot.Std

let invalid_argf = Core_printf.invalid_argf

module T = struct
  type t = bool with bin_io, sexp
  let compare (t : t) t' = compare t t'

  (* we use physical equality here because for bools it is the same *)
  let equal (t : t) t' = t == t'

  let hash x = if x then 1 else 0
end

include T

let of_string = function
  | "true" -> true
  | "false" -> false
  | s -> invalid_argf "Bool.of_string: expected true or false but got %s" s ()
;;

let to_string = string_of_bool

module Replace_polymorphic_compare = struct
  let min (x : t) y = if x < y then x else y
  let max (x : t) y = if x > y then x else y
  let compare = compare
  let ascending = compare
  let descending x y = compare y x
  let ( >= ) (x : t) y = x >= y
  let ( <= ) (x : t) y = x <= y
  let ( = ) = equal
  let equal = equal
  let ( > ) (x : t) y = x > y
  let ( < ) (x : t) y = x < y
  let ( <> ) (x : t) y = x != y
end

include Replace_polymorphic_compare

(* Making bool hashable may seem frivolous, but consider an aggregate type with
   a bool in it that needs a custom hash function. *)
include Hashable.Make (T)

include Comparable.Map_and_set_binable (T)

module True_  = (val Default.create true  : Default.S with type real = t)
module False_ = (val Default.create false : Default.S with type real = t)
