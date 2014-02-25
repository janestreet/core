open Core_kernel.Std

module Stable = struct
  module V1 = struct
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

    module type Like_a_float = sig
      type t = private float with bin_io
      include Comparable.S_common  with type t := t
      include Comparable.With_zero with type t := t
      include Hashable_binable     with type t := t
      include Robustly_comparable  with type t := t
      include Stringable           with type t := t
      include Floatable            with type t := t
      val (+)     : t -> t -> t
      val (-)     : t -> t -> t
      val zero    : t
      val epsilon : t
      val abs     : t -> t
      val neg     : t -> t
      val scale   : t -> float -> t
    end

    module T : sig
      include Like_a_float

      module Constant : sig
        val nanoseconds_per_second : float
        val microseconds_per_second : float
        val milliseconds_per_second : float
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
      (* IF THIS REPRESENTATION EVER CHANGES, ENSURE THAT EITHER
         (1) all values serialize the same way in both representations, or
         (2) you add a new Time.Span version to stable.ml *)
      include (Float : Like_a_float)

      (* this prevents any worry about having these very common names redefined below and
         makes their usage within this module safer.  Constant is included at the very
         bottom to re-export these constants in a more convenient way *)
      module Constant = struct
        let nanoseconds_per_second = 1E9
        let microseconds_per_second = 1E6
        let milliseconds_per_second = 1E3
        (* spans are stored as a float in seconds *)
        let nanosecond  = of_float (1. /. nanoseconds_per_second)
        let microsecond = of_float (1. /. microseconds_per_second)
        let millisecond = of_float (1. /. milliseconds_per_second)
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
        let t   = Float.iround_exn ~dir:`Nearest t in
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
        let intval = Float.iround_exn ~dir:`Down (Float.Parts.integral parts) in
        let min    = intval / 60 in
        let sec    = intval mod 60 in
        let hr     = min / 60 in
        let min    = min mod 60 in
        let us     = Float.iround_exn ~dir:`Nearest (Float.Parts.fractional parts *. 1.E6) in
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
        else if parts.ms  > 0 then format_decimal parts.ms  (parts.us / 100) "ms"
        else sprintf "%ius" parts.us
      in
      if parts.sign = Float.Sign.Neg then "-" ^ s else s

    (* due to precision limitations in float we can't expect better than microsecond
       precision *)
    include Core_kernel.Float_robust_compare.Make(struct let epsilon = 1E-6 end)

    let (/) t f = T.of_float ((t : T.t :> float) /. f)
    let (//) (f:T.t) (t:T.t) = (f :> float) /. (t :> float)

    (* Multiplying by 1E3 is more accurate than division by 1E-3 *)
    let to_ns (x:T.t)  = (x :> float) *. T.Constant.nanoseconds_per_second
    let to_us (x:T.t)  = (x :> float) *. T.Constant.microseconds_per_second
    let to_ms (x:T.t)  = (x :> float) *. T.Constant.milliseconds_per_second
    let to_sec (x:T.t) = (x :> float)
    let to_min x       = x // T.Constant.minute
    let to_hr x        = x // T.Constant.hour
    let to_day x       = x // T.Constant.day

    let ( ** ) f (t:T.t) = T.of_float (f *. (t :> float))
    (* Division by 1E3 is more accurate than multiplying by 1E-3 *)
    let of_ns x        = T.of_float (x /. T.Constant.nanoseconds_per_second)
    let of_us x        = T.of_float (x /. T.Constant.microseconds_per_second)
    let of_ms x        = T.of_float (x /. T.Constant.milliseconds_per_second)
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

    (* WARNING: if you are going to change this function in any material way, make sure
       you update Stable appropriately. *)
    let of_string (s:string) =
      try
        begin match s with
        | "" -> failwith "empty string"
        | _  ->
          let float n =
            match (String.drop_suffix s n) with
            | "" -> failwith "no number given"
            | s  ->
              let v = Float.of_string s in
              Validate.maybe_raise (Float.validate_ordinary v);
              v
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

    (* WARNING: if you are going to change this function in any material way, make sure
       you update Stable appropriately. *)
    (* I'd like it to be the case that you could never construct an infinite span, but I
       can't think of a good way to enforce it.  So this to_string function can produce
       strings that will raise an exception when they are fed to of_string *)
    let to_string (t:T.t) =
      (* this is a sad broken abstraction... *)
      let module C = Float.Class in
      match Float.classify (t :> float) with
      | C.Subnormal
      | C.Zero -> "0s"
      | C.Infinite -> if T.(>) t T.zero then "inf" else "-inf"
      | C.Nan -> "nan"
      | C.Normal ->
        let (<) = T.(<) in
        let abs_t = T.of_float (Float.abs (t :> float)) in
        if abs_t < T.Constant.second then sprintf "%gms" (to_ms t)
        else if abs_t < T.Constant.minute then sprintf "%gs" (to_sec t)
        else if abs_t < T.Constant.hour then sprintf "%gm" (to_min t)
        else if abs_t < T.Constant.day then sprintf "%gh" (to_hr t)
        else sprintf "%gd" (to_day t)

    let sexp_of_t t = Sexp.Atom (to_string t)
  end

  TEST_MODULE "Span.V1" = Core_kernel.Stable_unit_test.Make (struct
    include V1

    let equal t1 t2 = Int.(=) 0 (compare t1 t2)

    let tests =
      let span = of_sec in
      [ span 1234.56,    "20.576m",  "\010\215\163\112\061\074\147\064";
        span 0.000001,   "0.001ms",  "\141\237\181\160\247\198\176\062";
        span 80000006.4, "925.926d", "\154\153\153\025\208\018\147\065";
      ]
  end)
end
include Stable.V1

include Pretty_printer.Register (struct
  type nonrec t = t
  let to_string = to_string
  let module_name = "Core.Std.Time.Span"
end)

module C = struct
  type t = T.t with bin_io

  type comparator_witness = T.comparator_witness

  let comparator = T.comparator

  (* In 108.06a and earlier, spans in sexps of Maps and Sets were raw floats.  From 108.07
     through 109.13, the output format remained raw as before, but both the raw and pretty
     format were accepted as input.  From 109.14 on, the output format was changed from
     raw to pretty, while continuing to accept both formats.  Once we believe most
     programs are beyond 109.14, we will switch the input format to no longer accept
     raw. *)
  let sexp_of_t = sexp_of_t

  let t_of_sexp sexp =
    match Option.try_with (fun () -> T.of_float (Float.t_of_sexp sexp)) with
    | Some t -> t
    | None -> t_of_sexp sexp
  ;;
end

module Map = Map.Make_binable_using_comparator (C)
module Set = Set.Make_binable_using_comparator (C)

TEST =
  Set.equal (Set.of_list [hour])
    (Set.t_of_sexp (Sexp.List [Float.sexp_of_t (to_float hour)]))
;;
