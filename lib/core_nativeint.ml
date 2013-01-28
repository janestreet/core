open Sexplib.Std
open Bin_prot.Std

open Nativeint

module T = struct
  type t = nativeint with sexp, bin_io
  let compare (x : t) y = compare x y
  let equal (x : t) y = x = y
  let hash (x : t) = Hashtbl.hash x

  let to_string = to_string
  let of_string = of_string
end

include T

let num_bits = Word_size.num_bits Word_size.word_size

let shift_right_logical = shift_right_logical
let shift_right = shift_right
let shift_left = shift_left
let bit_not = lognot
let bit_xor = logxor
let bit_or = logor
let bit_and = logand
let min_value = min_int
let max_value = max_int
let abs = abs
let pred = pred
let succ = succ
let rem = rem
let neg = neg
let minus_one = minus_one
let one = one
let zero = zero
let to_float = to_float
let of_float = of_float

module Replace_polymorphic_compare = struct
  let equal = equal
  let compare = compare
  let ascending = compare
  let descending x y = compare y x
  let min (x : t) y = if x < y then x else y
  let max (x : t) y = if x > y then x else y
  let ( >= ) (x : t) y = x >= y
  let ( <= ) (x : t) y = x <= y
  let ( = ) (x : t) y = x = y
  let ( > ) (x : t) y = x > y
  let ( < ) (x : t) y = x < y
  let ( <> ) (x : t) y = x <> y
end

include Replace_polymorphic_compare

include Hashable.Make_binable (T)
include Comparable.Map_and_set_binable (T)

let ( / ) = div
let ( * ) = mul
let ( - ) = sub
let ( + ) = add

let incr r = r := !r + one
let decr r = r := !r - one

let of_nativeint t = t
let of_nativeint_exn = of_nativeint
let to_nativeint t = t
let to_nativeint_exn = to_nativeint

module Conv = Int_conversions
let of_int = Conv.int_to_nativeint
let of_int_exn = of_int
let to_int = Conv.nativeint_to_int
let to_int_exn = Conv.nativeint_to_int_exn
let of_int32 = Conv.int32_to_nativeint
let of_int32_exn = of_int32
let to_int32 = Conv.nativeint_to_int32
let to_int32_exn = Conv.nativeint_to_int32_exn
let of_int64 = Conv.int64_to_nativeint
let of_int64_exn = Conv.int64_to_nativeint_exn
let to_int64 = Conv.nativeint_to_int64

include Conv.Make (T)

