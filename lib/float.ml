open Sexplib.Std
open Bin_prot.Std
module Sexp = Sexplib.Sexp
module String = Core_string
open Core_printf

let failwiths = Error.failwiths

module T = struct
  type t = float with sexp, bin_io
  let compare (x : t) y = compare x y
  let equal (x : t) y = x = y
  let hash (x : t) = Hashtbl.hash_param 1 1 x
end

include T
type outer = t with sexp, bin_io (* alias for use by sub-modules *)

let to_float x = x
let of_float x = x

let of_string s =
  try Pervasives.float_of_string s with
  | _ -> invalid_argf "Float.of_string %s" s ()
;;

let to_string = Pervasives.string_of_float

let nan = Pervasives.nan

let infinity = Pervasives.infinity
let neg_infinity = Pervasives.neg_infinity

let max_value = infinity
let min_value = neg_infinity

let max_finite_value = Pervasives.max_float
let min_positive_value = Pervasives.min_float
let zero = 0.

let frexp = Pervasives.frexp
let ldexp = Pervasives.ldexp

let is_nan x = (x : t) <> x
include
  (Float_robust_compare.Make(struct let epsilon = 1E-7 end) : Float_robust_compare.S)

let epsilon_float = Pervasives.epsilon_float

include Hashable.Make_binable (T)

let of_int = Pervasives.float_of_int

let to_int f =
  let module P = Pervasives in
  match P.classify_float f with
  | P.FP_normal | P.FP_subnormal | P.FP_zero -> int_of_float f
  | P.FP_infinite | P.FP_nan -> invalid_arg "Float.to_int on nan or inf"

let of_int64 i = Int64.to_float i

let to_int64 f =
  let module P = Pervasives in
  match P.classify_float f with
  | P.FP_normal | P.FP_subnormal | P.FP_zero -> Int64.of_float f
  | P.FP_infinite | P.FP_nan -> invalid_arg "Float.to_int64 on nan or inf"

(* max_int/min_int are architecture dependent, e.g. +/- 2^30, +/- 2^62 if 32-bit, 64-bit
   (respectively) while float is IEEE standard for double (52 significant bits).  We may
   lose precision (e.g. beyond the 52 bits) as we round from float to int but, by capping
   with float_round_lb and float_round_ub, we shouldn't run afoul of flipping the sign bit
   on our integers.  With strict inequalities used on float_round_lb and float_round_ub we
   could actually define them without the +. or -. 257.; this is a bit of extra precaution
   in case someone uses them with <= or >=.
*)
let float_round_lb = max (of_int min_int) (-1.0 *. 2.0 ** 62.0 +. 257.)
let float_round_ub = min (of_int max_int) (        2.0 ** 62.0 -. 257.)

let iround_exn ?(dir=`Nearest) t =
  if is_nan t
  then invalid_arg "Float.iround_exn: Unable to handle NaN"
  else if t <= float_round_lb || t >= float_round_ub
  then invalid_argf "Float.iround_exn: argument out of bounds (%f)" t ()
  else match dir with
  | `Zero    -> truncate t
  | `Nearest -> to_int (floor (t +. 0.5))
  | `Up      -> to_int (ceil  t)
  | `Down    -> to_int (floor t)

let iround ?(dir=`Nearest) t =
  try Some (iround_exn ~dir t)
  with _ -> None

TEST = iround_exn ~dir:`Nearest 3.4 = 3
TEST = iround_exn ~dir:`Nearest 3.6 = 4
TEST = iround_exn ~dir:`Nearest (-3.4) = (-3)
TEST = iround_exn ~dir:`Nearest (-3.6) = (-4)

TEST = iround ~dir:`Nearest 3.4 = Some 3
TEST = iround ~dir:`Nearest 3.6 = Some 4
TEST = iround ~dir:`Nearest (-3.4) = Some (-3)
TEST = iround ~dir:`Nearest (-3.6) = Some (-4)

let is_inf x = (Pervasives.classify_float x = Pervasives.FP_infinite);;

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
let abs = Pervasives.abs_float
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
TEST =
  round_down      3.6  =  3.
  && round_down (-3.6) = -4.

let round_up = ceil
TEST =
  round_up      3.6  =  4.
  && round_up (-3.6) = -3.

let round_towards_zero t =
  if t >= 0.
  then round_down t
  else round_up   t
TEST =
  round_towards_zero      3.6  =  3.
  && round_towards_zero (-3.6) = -3.

let round_nearest t = floor (t +. 0.5)
TEST =
  round_nearest      3.6  =  4.
  && round_nearest (-3.6) = -4.

let round ?(dir=`Nearest) t =
  match dir with
  | `Nearest -> round_nearest      t
  | `Down    -> round_down         t
  | `Up      -> round_up           t
  | `Zero    -> round_towards_zero t

let mod_float = Pervasives.mod_float

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
  let module P = Pervasives in
  match P.classify_float t with
  | P.FP_normal    -> C.Normal
  | P.FP_subnormal -> C.Subnormal
  | P.FP_zero      -> C.Zero
  | P.FP_infinite  -> C.Infinite
  | P.FP_nan       -> C.Nan
;;

let is_finite t =
  let module C = Class in
  match classify t with
  | C.Normal | C.Subnormal | C.Zero -> true
  | C.Infinite | C.Nan -> false

let to_string_hum ?(delimiter='_') ?(decimals=3) ?(strip_zero=false) f =
  if decimals < 0 then
    invalid_argf "to_string_hum: invalid argument ~decimals=%d" decimals ();
  match classify f with
  | Class.Infinite -> if f >. 0. then "inf" else "-inf"
  | Class.Nan -> "nan"
  | Class.Normal
  | Class.Subnormal
  | Class.Zero ->
    let sprintf_result = sprintf "%.*f" decimals f in
    match String.lsplit2 sprintf_result ~on:'.' with
    | None ->
      assert (decimals = 0);
      Int_conversions.insert_delimiter sprintf_result ~delimiter
    | Some (left, right) ->
      let left = Int_conversions.insert_delimiter left ~delimiter in
      let right =
        if strip_zero
        then String.rstrip right ~drop:(fun c -> c = '0')
        else right
      in
      match right with
      | "" -> left
      | _ -> left ^ "." ^ right
;;

TEST_MODULE = struct
  let test ?delimiter ~decimals f s s_strip_zero =
    let s' = to_string_hum ?delimiter ~decimals ~strip_zero:false f in
    if s' <> s then
      failwiths "to_string_hum ~strip_zero:false"
        (`input f, `decimals decimals, `got s', `expected s)
        (<:sexp_of< ([ `input of float ]
                     * [ `decimals of int ]
                     * [ `got of string ]
                     * [ `expected of string ]) >>);
    let s_strip_zero' = to_string_hum ?delimiter ~decimals ~strip_zero:true f in
    if s_strip_zero' <> s_strip_zero then
      failwiths "to_string_hum ~strip_zero:true"
        (`input f, `decimals decimals, `got s_strip_zero, `expected s_strip_zero')
        (<:sexp_of< ([ `input of float ]
                     * [ `decimals of int ]
                     * [ `got of string ]
                     * [ `expected of string ]) >>);
  ;;

  TEST_UNIT = test ~decimals:3 0.99999 "1.000" "1"
  TEST_UNIT = test ~decimals:3 0.00001 "0.000" "0"
  TEST_UNIT = test ~decimals:3 ~-.12345.1 "-12_345.100" "-12_345.1"
  TEST_UNIT = test ~delimiter:',' ~decimals:3 ~-.12345.1 "-12,345.100" "-12,345.1"
  TEST_UNIT = test ~decimals:0 0.99999 "1" "1"
  TEST_UNIT = test ~decimals:0 0.00001 "0" "0"
  TEST_UNIT = test ~decimals:0 ~-.12345.1 "-12_345" "-12_345"
  TEST_UNIT = test ~decimals:0 (5.0 /. 0.0) "inf" "inf"
  TEST_UNIT = test ~decimals:0 (-5.0 /. 0.0) "-inf" "-inf"
  TEST_UNIT = test ~decimals:0 (0.0 /. 0.0) "nan" "nan"
  TEST_UNIT = test ~decimals:2 (5.0 /. 0.0) "inf" "inf"
  TEST_UNIT = test ~decimals:2 (-5.0 /. 0.0) "-inf" "-inf"
  TEST_UNIT = test ~decimals:2 (0.0 /. 0.0) "nan" "nan"
  TEST_UNIT = test ~decimals:5 (10_000.0 /. 3.0) "3_333.33333" "3_333.33333"
  TEST_UNIT = test ~decimals:2 ~-.0.00001 "-0.00" "-0"

  let rand_test n =
    let go () =
      let f = Random.float 1_000_000.0 -. 500_000.0 in
      let repeatable to_str =
        let s = to_str f in
        if (String.split s ~on:',' |! String.concat |! of_string |! to_str) <> s
        then failwithf "failed on testing %f" f ()
      in
      repeatable (to_string_hum ~decimals:3 ~strip_zero:false);
    in
    try
      for _i = 0 to n - 1 do go () done;
      true
    with e ->
      Printf.eprintf "%s\n%!" (Exn.to_string e);
      false
  ;;

  TEST = rand_test 10_000
  ;;
end
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
  let between t ~low ~high = low <= t && t <= high
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
