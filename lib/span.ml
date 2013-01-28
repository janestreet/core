open Std_internal

module Parts = struct
  type t = {
      sign : Float.Sign.t;
      hr   : int;
      min  : int;
      sec  : int;
      ms   : int;
      us   : int;
    }
    with sexp
end

module T : sig
  include Constrained_float.S
  val (+)     : t -> t -> t
  val (-)     : t -> t -> t
  val zero    : t
  val epsilon : t
  val abs     : t -> t
  val neg     : t -> t
  val scale   : t -> float -> t

  module Constant : sig
    val nanosecond : t
    val microsecond : t
    val millisecond : t
    val second : t
    val minute : t
    val hour : t
    val day : t
  end

  val to_parts : t -> Parts.t
end = struct
  include (Float : sig
    include Constrained_float.S
    val (+)     : t -> t -> t
    val (-)     : t -> t -> t
    val zero    : t
    val epsilon : t
    val abs     : t -> t
    val neg     : t -> t
    val scale   : t -> float -> t
  end)

  (* this prevents any worry about having these very common names redefined below and
    makes their usage within this module safer.  Constant is included at the very bottom
    to re-export these constants in a more convenient way *)
  module Constant = struct
    (* spans are stored as a float in seconds *)
    let nanosecond  = of_float 1E-9
    let microsecond = of_float 1E-6
    let millisecond = of_float 1E-3
    let second      = of_float 1.
    let minute      = of_float 60.
    let hour        = of_float (60. *. 60.)
    let day         = of_float (24. *. 60. *. 60.)
  end

  let to_parts_64 t =
    let t = to_float t *. 1.E6 in
    let sign = Float.sign t in
    let t =
      match sign with
      | Float.Sign.Neg -> Float.neg t
      | Float.Sign.Pos | Float.Sign.Zero -> t
    in
    let t   = Float.iround_nearest_exn t in
    let sec = t / 1_000_000 in
    let min = sec / 60 in
    let sec = sec mod 60 in
    let hr  = min / 60 in
    let min = min mod 60 in
    let us  = t mod 1_000_000 in
    let ms  = us / 1_000 in
    let us  = us mod 1_000 in
    {Parts.
     sign = sign;
     hr   = hr;
     min  = min;
     sec  = sec;
     ms   = ms;
     us   = us;
    }

  let to_parts_32 t =
    let t      = Float.round (to_float t *. 1E6) /. 1E6 in
    let sign   = Float.sign t in
    let t =
      match sign with
      | Float.Sign.Neg -> Float.neg t
      | Float.Sign.Pos | Float.Sign.Zero -> t
    in
    let parts  = Float.modf t in
    let intval = Float.iround_down_exn (Float.Parts.integral parts) in
    let min    = intval / 60 in
    let sec    = intval mod 60 in
    let hr     = min / 60 in
    let min    = min mod 60 in
    let us     = Float.iround_nearest_exn (Float.Parts.fractional parts *. 1.E6) in
    let ms     = us / 1_000 in
    let us     = us mod 1_000 in
    {Parts.
      sign = sign;
      hr   = hr;
      min  = min;
      sec  = sec;
      ms   = ms;
      us   = us;
    }

  let to_parts = if Int.(=) Core_sys.word_size 64 then to_parts_64 else to_parts_32
end

let format_decimal n tenths units =
  assert (tenths >= 0 && tenths < 10);
  if n < 10 && tenths <> 0
  then sprintf "%d.%d%s" n tenths units
  else sprintf "%d%s" n units

let to_short_string span =
  let open Parts in
  let parts = T.to_parts span in
  let s =
    if parts.hr > 24 then
      format_decimal
        (parts.hr / 24) (Int.of_float (Float.of_int (parts.hr % 24) /. 2.4)) "d"
    else if parts.hr > 0 then format_decimal parts.hr (parts.min / 6) "h"
    else if parts.min > 0 then format_decimal parts.min (parts.sec / 6) "m"
    else if parts.sec > 0 then format_decimal parts.sec (parts.ms / 100) "s"
    else sprintf "%ims" parts.ms
  in
  if parts.sign = Float.Sign.Neg then "-" ^ s else s

(* due to precision limitations in float we can't expect better than microsecond
   precision *)
include Float_robust_compare.Make(struct let epsilon = 1E-6 end)

let (/) t f = T.of_float ((t : T.t :> float) /. f)
let (//) (f:T.t) (t:T.t) = (f :> float) /. (t :> float)

let to_ns x        = x // T.Constant.nanosecond
let to_us x        = x // T.Constant.microsecond
let to_ms x        = x // T.Constant.millisecond
let to_us x        = x // T.Constant.microsecond
let to_sec (x:T.t) = (x :> float)
let to_min x       = x // T.Constant.minute
let to_hr x        = x // T.Constant.hour
let to_day x       = x // T.Constant.day

let ( ** ) f (t:T.t) = T.of_float (f *. (t :> float))
let of_ns x        = x ** T.Constant.nanosecond
let of_us x        = x ** T.Constant.microsecond
let of_ms x        = x ** T.Constant.millisecond
let of_us x        = x ** T.Constant.microsecond
let of_sec x       = T.of_float x
let of_int_sec x   = T.of_float (Float.of_int x)
let of_min x       = x ** T.Constant.minute
let of_hr x        = x ** T.Constant.hour
let of_day x       = x ** T.Constant.day

let randomize (t:T.t) ~percent =
  if (percent < 0. || percent > 1.0) then
    invalid_argf "percent must be between 0 and 1, %f given" percent ();
  let t = to_sec t in
  let distance = Random.float (t *. percent) in
  of_sec (if Random.bool () then t +. distance else t -. distance)
;;

let create
    ?(sign=Float.Sign.Pos)
    ?(day = 0)
    ?(hr = 0)
    ?(min = 0)
    ?(sec = 0)
    ?(ms = 0)
    ?(us = 0)
    () =
  let (+) = T.(+) in
  let t =
    of_day    (Float.of_int day)
    + of_hr  (Float.of_int hr)
    + of_min (Float.of_int min)
    + of_sec (Float.of_int sec)
    + of_ms  (Float.of_int ms)
    + of_us  (Float.of_int us)
  in
  match sign with
  | Float.Sign.Neg -> T.(-) T.zero t
  | Float.Sign.Pos | Float.Sign.Zero -> t

include T
include Constant

let of_string (s:string) =
  try
    begin match s with
    | "" -> failwith "empty string"
    | _  ->
      let float n =
        match (String.drop_suffix s n) with
        | "" -> failwith "no number given"
        | s  -> Float.of_string s
      in
      let len = String.length s in
      match s.[Int.(-) len 1] with
      | 's' ->
        if Int.(>=) len 2 && Char.(=) s.[Int.(-) len 2] 'm' then of_ms (float 2)
        else T.of_float (float 1)
      | 'm' -> of_min (float 1)
      | 'h' -> of_hr (float 1)
      | 'd' -> of_day (float 1)
      | _ -> failwith "Time spans must end in ms, s, m, h, or d."
    end
  with exn ->
    invalid_argf "Span.of_string could not parse '%s': %s" s (Exn.to_string exn) ()

let of_sexp_error_exn exn sexp =
  of_sexp_error (Exn.to_string exn) sexp

exception T_of_sexp of Sexp.t * exn with sexp
exception T_of_sexp_expected_atom_but_got of Sexp.t with sexp

let t_of_sexp sexp =
  match sexp with
  | Sexp.Atom x ->
    begin
      try of_string x
      with exn -> of_sexp_error_exn (T_of_sexp (sexp, exn)) sexp
    end
  | Sexp.List _ ->
    of_sexp_error_exn (T_of_sexp_expected_atom_but_got sexp) sexp

(* I'd like it to be the case that you could never construct an infinite span, but I
   can't think of a good way to enforce it.  So this to_string function can produce
   strings that will raise an exception when they are fed to of_string *)
let to_string (t:T.t) =
  (* this is a sad broken abstraction... *)
  match classify_float (t :> float) with
  | FP_subnormal | FP_zero -> "0s"
  | FP_infinite -> if T.(>) t T.zero then "inf" else "-inf"
  | FP_nan -> "nan"
  | FP_normal ->
    let (<) = T.(<) in
    let abs_t = T.of_float (abs_float (t :> float)) in
    if abs_t < T.Constant.second then sprintf "%gms" (to_ms t)
    else if abs_t < T.Constant.minute then sprintf "%gs" (to_sec t)
    else if abs_t < T.Constant.hour then sprintf "%gm" (to_min t)
    else if abs_t < T.Constant.day then sprintf "%gh" (to_hr t)
    else sprintf "%gd" (to_day t)

let sexp_of_t t = Sexp.Atom (to_string t)

let pp ppf t = Format.fprintf ppf "%s" (to_string t)
let () = Pretty_printer.register "Core.Span.pp"
