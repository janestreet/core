open Sexplib.Std
open Bin_prot.Std
open Int64

module T = struct
  type t = int64 with sexp, bin_io

  let compare (x : t) y = compare x y
  let equal (x : t) y = x = y
  let hash (x : t) = Hashtbl.hash x

  let to_string = to_string
  let of_string = of_string
end

include T

let num_bits = 64

let float_of_bits = float_of_bits
let bits_of_float = bits_of_float
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
let compare = compare
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

let of_int64 t = t
let of_int64_exn = of_int64
let to_int64 t = t

module Conv = Int_conversions
let of_int = Conv.int_to_int64
let of_int_exn = of_int
let to_int = Conv.int64_to_int
let to_int_exn = Conv.int64_to_int_exn
let of_int32 = Conv.int32_to_int64
let of_int32_exn = of_int32
let to_int32 = Conv.int64_to_int32
let to_int32_exn = Conv.int64_to_int32_exn
let of_nativeint = Conv.nativeint_to_int64
let of_nativeint_exn = of_nativeint
let to_nativeint = Conv.int64_to_nativeint
let to_nativeint_exn = Conv.int64_to_nativeint_exn

include Conv.Make (T)
