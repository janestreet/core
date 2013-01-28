open Sexplib.Std
open Bin_prot.Std
module Sexp = Sexplib.Sexp
module String = Core_string
open Core_printf

module T = struct
  type t = float with sexp, bin_io
  let compare (x : t) y = compare x y
  let equal (x : t) y = x = y
  let hash (x : t) = Hashtbl.hash_param 1 1 x
end

include T
type outer = t with bin_io,sexp (* alias for use by sub-modules *)

let to_float x = x
let of_float x = x

let of_string s =
  try float_of_string s with
  | _ -> invalid_argf "Float.of_string %s" s ()
;;

let to_string = string_of_float

let max_value = infinity
let min_value = neg_infinity
let max_finite_value = Pervasives.max_float
let min_positive_value = Pervasives.min_float
let zero = 0.

let is_nan x = (x : t) <> x
include
  (Float_robust_compare.Make(struct let epsilon = 1E-7 end) : Float_robust_compare.S)

include Hashable.Make_binable (T)

let of_int = Pervasives.float_of_int

let to_int f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> int_of_float f
  | FP_infinite | FP_nan -> invalid_arg "Float.to_int on nan or inf"

let of_int64 i = Int64.to_float i

let to_int64 f =
  match classify_float f with
  | FP_normal | FP_subnormal | FP_zero -> Int64.of_float f
  | FP_infinite | FP_nan -> invalid_arg "Float.to_int64 on nan or inf"

(* max_int/min_int are architecture dependent, e.g. +/- 2^30, +/- 2^62 if 32-bit, 64-bit
   (respectively) while float is IEEE standard for double (52 significant bits).  We may
   lose precision (e.g. beyond the 52 bits) as we round from float to int but, by capping
   with float_round_lb and float_round_ub, we shouldn't run afoul of flipping the sign bit
   on our integers.  With strict inequalities used on float_round_lb and float_round_ub we
   could actually define them without the +. or -. 257.; this is a bit of extra precaution
   in case someone uses them with <= or >=.
*)
let float_round_lb = max (float_of_int min_int) (-1.0 *. 2.0 ** 62.0 +. 257.)
let float_round_ub = min (float_of_int max_int) (2.0 ** 62.0 -. 257.)
let int_round_lb = int_of_float float_round_lb
let int_round_ub = int_of_float float_round_ub

let iround_towards_zero_exn x =
  if is_nan x then
    invalid_arg "Float.iround_towards_zero_exn: Unable to handle NaN"
  else
    begin
      if float_round_lb < x && x < float_round_ub then truncate x
      else invalid_argf "Float.iround_towards_zero_exn: argument out of bounds (%f)" x ()
    end

let iround_towards_zero x =
  try Some (iround_towards_zero_exn x)
  with _ -> None

let round x = floor (x +. 0.5)

let iround_down_exn x =
  if is_nan x then
    invalid_arg "Float.iround_down_exn: Unable to handle NaN"
  else
    begin
      if float_round_lb < x && x < float_round_ub then int_of_float (floor x)
      else invalid_argf "Float.iround_down_exn: argument out of bounds (%f)" x ()
    end

let iround_down x =
  try Some (iround_down_exn x)
  with _ -> None

let iround_up_exn x =
  if is_nan x then
    invalid_arg "Float.iround_up_exn: Unable to handle NaN"
  else
    begin
      if float_round_lb < x && x < float_round_ub then int_of_float (ceil x)
      else invalid_argf "Float.iround_up_exn: argument out of bounds (%f)" x ()
    end

let iround_up x =
  try Some (iround_up_exn x)
  with _ -> None

let iround_nearest x =
  if float_round_lb < x && x < float_round_ub then
    Some (int_of_float (round x))
  else None (* float too big to round reliably to int *)

TEST = iround_nearest 3.4 = Some 3
TEST = iround_nearest 3.6 = Some 4
TEST = iround_nearest (-3.4) = Some (-3)
TEST = iround_nearest (-3.6) = Some (-4)

let iround_nearest_exn x =
  match iround_nearest x with
  | None -> invalid_argf "Float.iround_nearest_exn: argument out of bounds (%f)" x ()
  | Some n -> n

TEST = iround_nearest_exn 3.4 = 3
TEST = iround_nearest_exn 3.6 = 4
TEST = iround_nearest_exn (-3.4) = (-3)
TEST = iround_nearest_exn (-3.6) = (-4)

let is_inf x = (classify_float x = FP_infinite);;

let min_inan (x : t) y =
  if is_nan y then x
  else if is_nan x then y
  else if x < y then x else y

let max_inan (x : t) y =
  if is_nan y then x
  else if is_nan x then y
  else if x > y then x else y

let add = (+.)
let sub = (-.)
let neg = (~-.)
let abs = abs_float
let scale = ( *. )

let min (x : t) y =
  if is_nan x || is_nan y then nan
  else if x < y then x else y

let max (x : t) y =
  if is_nan x || is_nan y then nan
  else if x > y then x else y

module Parts : sig
  type t

  val fractional : t -> float
  val integral : t -> float
  val modf : float -> t
end = struct
  type t = float * float

  let fractional t = fst t
  let integral t = snd t
  let modf = modf
end
let modf = Parts.modf

let round_down = floor
TEST = round_down 3.6 = 3. && round_down (-3.6) = -4.

let round_up = ceil
TEST = round_up 3.6 = 4. && round_up (-3.6) = -3.

let round_towards_zero t = if t >= 0. then round_down t else round_up t
TEST = round_towards_zero 3.6 = 3. && round_towards_zero (-3.6) = -3.

let round_nearest t = round t
TEST = round_nearest 3.6 = 4. && round_nearest (-3.6) = -4.

let mod_float = mod_float

module Class = struct
  type t =
  | Infinite
  | Nan
  | Normal
  | Subnormal
  | Zero
  with sexp, bin_io

  let to_string t = Sexp.to_string (sexp_of_t t)
  let of_string s = t_of_sexp (Sexp.Atom s)
end

let classify t =
  let module C = Class in
  match Pervasives.classify_float t with
  | FP_normal -> C.Normal
  | FP_subnormal -> C.Subnormal
  | FP_zero -> C.Zero
  | FP_infinite -> C.Infinite
  | FP_nan -> C.Nan
;;

module Replace_polymorphic_compare = struct
  let equal = equal
  let compare (x : t) y = compare x y
  let ascending = compare
  let descending x y = compare y x
  let min = min
  let max = max
  let ( >= ) (x : t) y = x >= y
  let ( <= ) (x : t) y = x <= y
  let ( = ) (x : t) y = x = y
  let ( > ) (x : t) y = x > y
  let ( < ) (x : t) y = x < y
  let ( <> ) (x : t) y = x <> y
end

include Replace_polymorphic_compare

let (+) t t' = t +. t'
let (-) t t' = t -. t'
let ( * ) t t' = t *. t'
let (/) t t' = t /. t'

include Comparable.Map_and_set_binable (T)

module Sign = struct
  type t = Neg | Zero | Pos with sexp
end

let sign t =
  if t >. 0. then Sign.Pos
  else if t <. 0. then Sign.Neg
  else Sign.Zero

module Terse = struct
  type t = outer with bin_io
  let t_of_sexp = t_of_sexp

  let to_string x = Core_printf.sprintf "%.8G" x
  let sexp_of_t x = Sexp.Atom (to_string x)
  let of_string x = of_string x
end
