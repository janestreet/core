open! Import
open Std_internal
open! Int.Replace_polymorphic_compare

module Stable = struct
  module V1 = struct
    module Parts = struct
      type t =
        { sign : Sign.t
        ; hr : int
        ; min : int
        ; sec : int
        ; ms : int
        ; us : int
        ; ns : int
        }
      [@@deriving compare, sexp, sexp_grammar]
    end

    module type Like_a_float = sig
      type t [@@deriving bin_io, hash, quickcheck, typerep]

      include Comparable.S_common with type t := t
      include Comparable.With_zero with type t := t
      include Floatable with type t := t

      val ( + ) : t -> t -> t
      val ( - ) : t -> t -> t
      val zero : t
      val robust_comparison_tolerance : t
      val abs : t -> t
      val neg : t -> t
      val scale : t -> float -> t
    end

    module T : sig
      type underlying = float [@@deriving hash]
      type t = private underlying [@@deriving bin_io, hash, stable_witness]

      include Like_a_float with type t := t
      include Robustly_comparable with type t := t

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
      val next : t -> t
      val prev : t -> t
    end = struct
      type underlying = float [@@deriving hash, stable_witness]
      type t = underlying [@@deriving hash, stable_witness]

      let next t = Float.one_ulp `Up t
      let prev t = Float.one_ulp `Down t

      (* IF THIS REPRESENTATION EVER CHANGES, ENSURE THAT EITHER
         (1) all values serialize the same way in both representations, or
         (2) you add a new Time.Span version to stable.ml *)
      include (
        struct
          include Float

          let sign = sign_exn
        end :
          Like_a_float with type t := t)

      (* due to precision limitations in float we can't expect better than microsecond
         precision *)
      include Float.Robust_compare.Make (struct
        let robust_comparison_tolerance = 1E-6
      end)

      (* this prevents any worry about having these very common names redefined below and
         makes their usage within this module safer.  Constant is included at the very
         bottom to re-export these constants in a more convenient way *)
      module Constant = struct
        let nanoseconds_per_second = 1E9
        let microseconds_per_second = 1E6
        let milliseconds_per_second = 1E3

        (* spans are stored as a float in seconds *)
        let nanosecond = of_float (1. /. nanoseconds_per_second)
        let microsecond = of_float (1. /. microseconds_per_second)
        let millisecond = of_float (1. /. milliseconds_per_second)
        let second = of_float 1.
        let minute = of_float 60.
        let hour = of_float (60. *. 60.)
        let day = of_float (24. *. 60. *. 60.)
      end

      let to_parts t : Parts.t =
        let sign = Float.sign_exn t in
        let t = abs t in
        let integral = Float.round_down t in
        let fractional = t -. integral in
        let seconds = Float.iround_down_exn integral in
        let nanoseconds = Float.iround_nearest_exn (fractional *. 1E9) in
        let seconds, nanoseconds =
          if Int.equal nanoseconds 1_000_000_000
          then Int.succ seconds, 0
          else seconds, nanoseconds
        in
        let sec = seconds mod 60 in
        let minutes = seconds / 60 in
        let min = minutes mod 60 in
        let hr = minutes / 60 in
        let ns = nanoseconds mod 1000 in
        let microseconds = nanoseconds / 1000 in
        let us = microseconds mod 1000 in
        let milliseconds = microseconds / 1000 in
        let ms = milliseconds in
        { sign; hr; min; sec; ms; us; ns }
      ;;
    end

    let ( / ) t f = T.of_float ((t : T.t :> float) /. f)
    let ( // ) (f : T.t) (t : T.t) = (f :> float) /. (t :> float)

    (* Multiplying by 1E3 is more accurate than division by 1E-3 *)
    let to_ns (x : T.t) = (x :> float) *. T.Constant.nanoseconds_per_second
    let to_us (x : T.t) = (x :> float) *. T.Constant.microseconds_per_second
    let to_ms (x : T.t) = (x :> float) *. T.Constant.milliseconds_per_second
    let to_sec (x : T.t) = (x :> float)
    let to_min x = x // T.Constant.minute
    let to_hr x = x // T.Constant.hour
    let to_day x = x // T.Constant.day
    let to_int63_seconds_round_down_exn x = Float.int63_round_down_exn (to_sec x)
    let ( ** ) f (t : T.t) = T.of_float (f *. (t :> float))

    (* Division by 1E3 is more accurate than multiplying by 1E-3 *)
    let of_ns x = T.of_float (x /. T.Constant.nanoseconds_per_second)
    let of_us x = T.of_float (x /. T.Constant.microseconds_per_second)
    let of_ms x = T.of_float (x /. T.Constant.milliseconds_per_second)
    let of_sec x = T.of_float x
    let of_int32_seconds sec = of_sec (Int32.to_float sec)

    (* Note that [Int63.to_float] can lose precision, but only on inputs large enough that
       [of_sec] in either the Time_ns or Time_float case would lose precision (or just be
       plain out of bounds) anyway. *)
    let of_int63_seconds sec = of_sec (Int63.to_float sec)
    let of_min x = x ** T.Constant.minute
    let of_hr x = x ** T.Constant.hour
    let of_day x = x ** T.Constant.day
    let of_int_ns x = of_ns (Float.of_int x)
    let of_int_us x = of_us (Float.of_int x)
    let of_int_ms x = of_ms (Float.of_int x)
    let of_int_sec x = of_sec (Float.of_int x)
    let of_int_min x = of_min (Float.of_int x)
    let of_int_hr x = of_hr (Float.of_int x)
    let of_int_day x = of_day (Float.of_int x)

    let divide_by_unit_of_time t unit_of_time =
      match (unit_of_time : Unit_of_time.t) with
      | Nanosecond -> to_ns t
      | Microsecond -> to_us t
      | Millisecond -> to_ms t
      | Second -> to_sec t
      | Minute -> to_min t
      | Hour -> to_hr t
      | Day -> to_day t
    ;;

    let scale_by_unit_of_time float unit_of_time =
      match (unit_of_time : Unit_of_time.t) with
      | Nanosecond -> of_ns float
      | Microsecond -> of_us float
      | Millisecond -> of_ms float
      | Second -> of_sec float
      | Minute -> of_min float
      | Hour -> of_hr float
      | Day -> of_day float
    ;;

    let create
      ?(sign = Sign.Pos)
      ?(day = 0)
      ?(hr = 0)
      ?(min = 0)
      ?(sec = 0)
      ?(ms = 0)
      ?(us = 0)
      ?(ns = 0)
      ()
      =
      let ( + ) = T.( + ) in
      let t =
        of_day (Float.of_int day)
        + of_hr (Float.of_int hr)
        + of_min (Float.of_int min)
        + of_sec (Float.of_int sec)
        + of_ms (Float.of_int ms)
        + of_us (Float.of_int us)
        + of_ns (Float.of_int ns)
      in
      match sign with
      | Neg -> T.( - ) T.zero t
      | Pos | Zero -> t
    ;;

    include T
    include Constant

    let randomize ?(state = Random.State.default) t ~percent =
      Span_helpers.randomize t state ~percent ~scale
    ;;

    let to_short_string t =
      let ({ sign; hr; min; sec; ms; us; ns } : Parts.t) = to_parts t in
      Span_helpers.short_string ~sign ~hr ~min ~sec ~ms ~us ~ns
    ;;

    (* WARNING: if you are going to change this function in any material way, make sure
       you update Stable appropriately. *)
    let of_string_v1_v2 (s : string) ~is_v2 =
      try
        match s with
        | "" -> failwith "empty string"
        | _ ->
          let float n =
            match String.drop_suffix s n with
            | "" -> failwith "no number given"
            | s ->
              let v = Float.of_string s in
              Validate.maybe_raise (Float.validate_ordinary v);
              v
          in
          let len = String.length s in
          (match s.[Int.( - ) len 1] with
           | 's' ->
             if Int.( >= ) len 2 && Char.( = ) s.[Int.( - ) len 2] 'm'
             then of_ms (float 2)
             else if is_v2 && Int.( >= ) len 2 && Char.( = ) s.[Int.( - ) len 2] 'u'
             then of_us (float 2)
             else if is_v2 && Int.( >= ) len 2 && Char.( = ) s.[Int.( - ) len 2] 'n'
             then of_ns (float 2)
             else T.of_float (float 1)
           | 'm' -> of_min (float 1)
           | 'h' -> of_hr (float 1)
           | 'd' -> of_day (float 1)
           | _ ->
             if is_v2
             then failwith "Time spans must end in ns, us, ms, s, m, h, or d."
             else failwith "Time spans must end in ms, s, m, h, or d.")
      with
      | exn ->
        invalid_argf "Span.of_string could not parse '%s': %s" s (Exn.to_string exn) ()
    ;;

    let of_sexp_error_exn exn sexp = of_sexp_error (Exn.to_string exn) sexp

    exception T_of_sexp of Sexp.t * exn [@@deriving sexp]
    exception T_of_sexp_expected_atom_but_got of Sexp.t [@@deriving sexp]

    let t_of_sexp_v1_v2 sexp ~is_v2 =
      match sexp with
      | Sexp.Atom x ->
        (try of_string_v1_v2 x ~is_v2 with
         | exn -> of_sexp_error_exn (T_of_sexp (sexp, exn)) sexp)
      | Sexp.List _ -> of_sexp_error_exn (T_of_sexp_expected_atom_but_got sexp) sexp
    ;;

    let string ~is_v2 suffix float =
      if is_v2
         (* This is the same float-to-string conversion used in [Float.sexp_of_t].  It's like
         [Float.to_string], but may leave off trailing period. *)
      then !Sexplib.Conv.default_string_of_float float ^ suffix
      else sprintf "%g%s" float suffix
    ;;

    (* WARNING: if you are going to change this function in any material way, make sure
       you update Stable appropriately. *)
    (* I'd like it to be the case that you could never construct an infinite span, but I
       can't think of a good way to enforce it.  So this to_string function can produce
       strings that will raise an exception when they are fed to of_string *)
    let to_string_v1_v2 (t : T.t) ~is_v2 =
      (* this is a sad broken abstraction... *)
      let module C = Float.Class in
      match Float.classify (t :> float) with
      | C.Subnormal | C.Zero -> "0s"
      | C.Infinite -> if T.( > ) t T.zero then "inf" else "-inf"
      | C.Nan -> "nan"
      | C.Normal ->
        let ( < ) = T.( < ) in
        let abs_t = T.of_float (Float.abs (t :> float)) in
        if is_v2 && abs_t < T.Constant.microsecond
        then string ~is_v2 "ns" (to_ns t)
        else if is_v2 && abs_t < T.Constant.millisecond
        then string ~is_v2 "us" (to_us t)
        else if abs_t < T.Constant.second
        then string ~is_v2 "ms" (to_ms t)
        else if abs_t < T.Constant.minute
        then string ~is_v2 "s" (to_sec t)
        else if abs_t < T.Constant.hour
        then string ~is_v2 "m" (to_min t)
        else if abs_t < T.Constant.day
        then string ~is_v2 "h" (to_hr t)
        else string ~is_v2 "d" (to_day t)
    ;;

    let sexp_of_t_v1_v2 t ~is_v2 = Sexp.Atom (to_string_v1_v2 t ~is_v2)
    let t_of_sexp sexp = t_of_sexp_v1_v2 sexp ~is_v2:false
    let sexp_of_t t = sexp_of_t_v1_v2 t ~is_v2:false
    let t_sexp_grammar = Sexplib.Sexp_grammar.coerce String.t_sexp_grammar

    include Diffable.Atomic.Make (struct
      type nonrec t = t [@@deriving bin_io, equal, sexp]
    end)
  end

  module V2 = struct
    include V1

    let t_of_sexp sexp = t_of_sexp_v1_v2 sexp ~is_v2:true
    let sexp_of_t t = sexp_of_t_v1_v2 t ~is_v2:true

    include Diffable.Atomic.Make (struct
      type nonrec t = t [@@deriving bin_io, equal, sexp]
    end)
  end

  module V3 = struct
    include V1

    let to_unit_of_time t : Unit_of_time.t =
      let open T in
      let open Constant in
      let abs_t = T.abs t in
      if abs_t >= day
      then Day
      else if abs_t >= hour
      then Hour
      else if abs_t >= minute
      then Minute
      else if abs_t >= second
      then Second
      else if abs_t >= millisecond
      then Millisecond
      else if abs_t >= microsecond
      then Microsecond
      else Nanosecond
    ;;

    let of_unit_of_time : Unit_of_time.t -> T.t =
      let open T.Constant in
      function
      | Nanosecond -> nanosecond
      | Microsecond -> microsecond
      | Millisecond -> millisecond
      | Second -> second
      | Minute -> minute
      | Hour -> hour
      | Day -> day
    ;;

    let suffix_of_unit_of_time unit_of_time =
      match (unit_of_time : Unit_of_time.t) with
      | Nanosecond -> "ns"
      | Microsecond -> "us"
      | Millisecond -> "ms"
      | Second -> "s"
      | Minute -> "m"
      | Hour -> "h"
      | Day -> "d"
    ;;

    module Of_string = struct
      let invalid_string string ~reason =
        let message = "Time.Span.of_string: " ^ reason in
        raise_s [%message message string]
      ;;

      let rec find_unit_of_time_by_suffix string ~index unit_of_time_list =
        match unit_of_time_list with
        | [] -> invalid_string string ~reason:"invalid span part suffix"
        | unit_of_time :: rest ->
          let suffix = suffix_of_unit_of_time unit_of_time in
          if String.is_substring_at string ~pos:index ~substring:suffix
          then unit_of_time
          else find_unit_of_time_by_suffix string ~index rest
      ;;

      let parse_suffix string ~index =
        (* We rely on the fact that "ms" comes before "m" in [Unit_of_time.all] to get a
           correct match on millisecond timestamps. This assumption is demonstrated in the
           expect test below. *)
        find_unit_of_time_by_suffix string ~index Unit_of_time.all
      ;;

      (* We validate magnitude strings so that we know where the unit-of-time suffix
         begins, and so that only sensible strings are allowed. We do not want to be as
         permissive as [Float.of_string]; for example, hexadecimal span magnitudes are not
         allowed. After validation, we still use [Float.of_string] to produce the actual
         value. *)
      module Float_parser = struct
        (* [In_decimal_have_digit] includes having a digit before the decimal point. *)
        type state =
          | In_integer_need_digit
          | In_integer_have_digit
          | In_decimal_need_digit
          | In_decimal_have_digit
          | In_exponent_need_digit_or_sign
          | In_exponent_need_digit
          | In_exponent_have_digit

        type token =
          | Digit
          | Point
          | Under
          | Sign
          | Expt

        let state_is_final = function
          | In_integer_have_digit | In_decimal_have_digit | In_exponent_have_digit -> true
          | In_integer_need_digit
          | In_decimal_need_digit
          | In_exponent_need_digit_or_sign
          | In_exponent_need_digit -> false
        ;;

        let token_of_char = function
          | '0' .. '9' -> Some Digit
          | '.' -> Some Point
          | '_' -> Some Under
          | '-' | '+' -> Some Sign
          | 'E' | 'e' -> Some Expt
          | _ -> None
        ;;

        let invalid_string string =
          invalid_string string ~reason:"invalid span part magnitude"
        ;;

        let rec find_index_after_float_in_state string ~index ~len ~state =
          let open Int.O in
          if index = len
          then if state_is_final state then index else invalid_string string
          else (
            match token_of_char string.[index] with
            | None -> if state_is_final state then index else invalid_string string
            | Some token ->
              let state =
                match state, token with
                | In_integer_need_digit, Digit -> In_integer_have_digit
                | In_integer_need_digit, Point -> In_decimal_need_digit
                | In_integer_need_digit, Under
                | In_integer_need_digit, Sign
                | In_integer_need_digit, Expt -> invalid_string string
                | In_integer_have_digit, Digit | In_integer_have_digit, Under ->
                  In_integer_have_digit
                | In_integer_have_digit, Point -> In_decimal_have_digit
                | In_integer_have_digit, Expt -> In_exponent_need_digit_or_sign
                | In_integer_have_digit, Sign -> invalid_string string
                | In_decimal_need_digit, Digit -> In_decimal_have_digit
                | In_decimal_need_digit, Point
                | In_decimal_need_digit, Under
                | In_decimal_need_digit, Expt
                | In_decimal_need_digit, Sign -> invalid_string string
                | In_decimal_have_digit, Digit | In_decimal_have_digit, Under ->
                  In_decimal_have_digit
                | In_decimal_have_digit, Expt -> In_exponent_need_digit_or_sign
                | In_decimal_have_digit, Point | In_decimal_have_digit, Sign ->
                  invalid_string string
                | In_exponent_need_digit_or_sign, Digit -> In_exponent_have_digit
                | In_exponent_need_digit_or_sign, Sign -> In_exponent_need_digit
                | In_exponent_need_digit_or_sign, Point
                | In_exponent_need_digit_or_sign, Under
                | In_exponent_need_digit_or_sign, Expt -> invalid_string string
                | In_exponent_need_digit, Digit -> In_exponent_have_digit
                | In_exponent_need_digit, Point
                | In_exponent_need_digit, Under
                | In_exponent_need_digit, Expt
                | In_exponent_need_digit, Sign -> invalid_string string
                | In_exponent_have_digit, Digit | In_exponent_have_digit, Under ->
                  In_exponent_have_digit
                | In_exponent_have_digit, Point
                | In_exponent_have_digit, Expt
                | In_exponent_have_digit, Sign -> invalid_string string
              in
              find_index_after_float_in_state string ~index:(index + 1) ~len ~state)
        ;;

        let find_index_after_float string ~index ~len =
          find_index_after_float_in_state string ~index ~len ~state:In_integer_need_digit
        ;;
      end

      let rec accumulate_magnitude string ~magnitude ~index ~len =
        if Int.equal index len
        then magnitude
        else (
          let suffix_index = Float_parser.find_index_after_float string ~index ~len in
          let unit_of_time = parse_suffix string ~index:suffix_index in
          let until_index =
            Int.( + ) suffix_index (String.length (suffix_of_unit_of_time unit_of_time))
          in
          let float_string =
            String.sub string ~pos:index ~len:(Int.( - ) suffix_index index)
          in
          let float = Float.of_string float_string in
          let magnitude = magnitude + scale_by_unit_of_time float unit_of_time in
          accumulate_magnitude string ~magnitude ~index:until_index ~len)
      ;;

      let parse_magnitude string ~index ~len =
        accumulate_magnitude string ~magnitude:T.zero ~index ~len
      ;;

      let of_string string =
        let open Int.O in
        match string with
        | "NANs" -> of_sec Float.nan
        | "-INFs" -> of_sec Float.neg_infinity
        | "INFs" -> of_sec Float.infinity
        | _ ->
          let len = String.length string in
          if len = 0 then invalid_string string ~reason:"empty input";
          let negative, index =
            match string.[0] with
            | '-' -> true, 1
            | '+' -> false, 1
            | _ -> false, 0
          in
          if index >= len then invalid_string string ~reason:"empty input";
          let magnitude = parse_magnitude string ~index ~len in
          if negative then T.neg magnitude else magnitude
      ;;
    end

    let of_string = Of_string.of_string

    module To_string = struct
      let string_of_float_without_trailing_decimal float =
        let string = Float.to_string float in
        let suffix = "." in
        if String.is_suffix string ~suffix
        then String.chop_suffix_exn string ~suffix
        else string
      ;;

      (* As we build up a string, we keep a running sum of the value that will be read
         back in, so that we can compute the remainder that needs to be generated. *)
      let sum ~sum_t ~unit_of_time ~magnitude =
        sum_t + scale_by_unit_of_time magnitude unit_of_time
      ;;

      (* For some units (very large numbers of days, or seconds and smaller) we just
         render a float directly, with a fix for roundoff error. *)
      let to_float_string ~abs_t ~unit_of_time ~fixup_unit_of_time =
        let magnitude = divide_by_unit_of_time abs_t unit_of_time in
        let sum_t = sum ~sum_t:zero ~unit_of_time ~magnitude in
        if sum_t = abs_t
        then
          string_of_float_without_trailing_decimal magnitude
          ^ suffix_of_unit_of_time unit_of_time
        else (
          let magnitude =
            if sum_t < abs_t
            then magnitude
            else divide_by_unit_of_time (prev abs_t) unit_of_time
          in
          let sum_t = sum ~sum_t:zero ~unit_of_time ~magnitude in
          let rem_t = abs_t - sum_t in
          let fixup_magnitude = divide_by_unit_of_time rem_t fixup_unit_of_time in
          string_of_float_without_trailing_decimal magnitude
          ^ suffix_of_unit_of_time unit_of_time
          (* [rem_t] is at ULP size of [abs_t], it needs just one bit of precision *)
          ^ sprintf "%.1g" fixup_magnitude
          ^ suffix_of_unit_of_time fixup_unit_of_time)
      ;;

      (* For non-decimal units (minutes and greater), we render an integer magnitude, and
         return that with the running sum so the remainder can be rendered at a smaller
         unit. *)
      let to_int_string_and_sum unit_of_time ~abs_t ~sum_t =
        let unit_span = of_unit_of_time unit_of_time in
        let rem_t = abs_t - sum_t in
        (* We calculate the approximate multiple of [unit_of_time] that needs to be
           added to [sum_t]. Due to rounding, this can be off by one (we've never seen a
           case off by two or more), so we re-compute the remainder and correct if
           necessary. *)
        let magnitude = Float.round_down (rem_t // unit_span) in
        let new_sum_t = sum ~sum_t ~unit_of_time ~magnitude in
        let new_rem_t = abs_t - new_sum_t in
        let magnitude =
          if new_rem_t = zero
          then magnitude
          else if new_rem_t < zero
          then magnitude -. 1.
          else (
            let next_magnitude = magnitude +. 1. in
            let next_sum_t = sum ~sum_t ~unit_of_time ~magnitude:next_magnitude in
            let next_rem_t = abs_t - next_sum_t in
            if next_rem_t < zero then magnitude else next_magnitude)
        in
        if Float.( <= ) magnitude 0.
        then "", sum_t
        else (
          let new_sum_t = sum ~sum_t ~unit_of_time ~magnitude in
          let string =
            Int63.to_string (Int63.of_float magnitude)
            ^ suffix_of_unit_of_time unit_of_time
          in
          string, new_sum_t)
      ;;

      let decimal_order_of_magnitude t = Float.log10 (to_sec t)

      (* The final seconds-or-smaller unit needs to be printed with enough digits to
         round-trip the whole span (which is minutes or greater); this can be
         significantly fewer digits than would be needed for the seconds-or-smaller
         remainder itself. *)
      let to_float_string_after_int_strings ~sum_t ~abs_t =
        if sum_t >= abs_t
        then ""
        else (
          let rem_t = abs_t - sum_t in
          let unit_of_time = to_unit_of_time rem_t in
          let unit_span = of_unit_of_time unit_of_time in
          let magnitude = rem_t // unit_span in
          let new_sum_t = sum ~sum_t ~unit_of_time ~magnitude in
          let new_rem_t = abs_t - new_sum_t in
          if abs rem_t <= abs new_rem_t
          then ""
          else (
            let order_of_magnitude_of_first_digit =
              Float.iround_down_exn (decimal_order_of_magnitude rem_t)
            in
            let half_ulp = (abs_t - prev abs_t) / 2. in
            let order_of_magnitude_of_final_digit =
              (* This works out to rounding down, except in the case of exact integers,
                 which are decremented. This makes sure we always stop at a digit with
                 strictly more precision than half the ULP. *)
              Int.pred (Float.iround_up_exn (decimal_order_of_magnitude half_ulp))
            in
            let number_of_digits =
              let open Int.O in
              1 + order_of_magnitude_of_first_digit - order_of_magnitude_of_final_digit
            in
            let suffix = suffix_of_unit_of_time unit_of_time in
            sprintf "%.*g" number_of_digits magnitude ^ suffix))
      ;;

      (* This helper avoids unnecessary allocation, because for our use below, it is
         common to have either or both arguments be empty. Currently (2018-02), the
         built-in [^] allocates even when appending to an empty string. *)
      let ( ^? ) x y =
        if String.is_empty x then y else if String.is_empty y then x else x ^ y
      ;;

      let to_string t =
        let float = to_float t in
        if not (Float.is_finite float)
        then
          if (* We print specific special strings for non-finite floats *)
             Float.is_nan float
          then "NANs"
          else if Float.is_negative float
          then "-INFs"
          else "INFs"
        else if t = zero
        then "0s"
        else (
          let unit_of_time = to_unit_of_time t in
          let abs_t = abs t in
          let sign = if t < zero then "-" else "" in
          let magnitude_string =
            match unit_of_time with
            (* We can use normal float notation for seconds and sub-second units, they are
               readable with a decimal point. *)
            | Nanosecond | Microsecond | Millisecond | Second ->
              to_float_string ~abs_t ~unit_of_time ~fixup_unit_of_time:Nanosecond
            (* For large enough values that the ULP is a day or more, we can use float
               notation because we are expressing a single, very large integer. *)
            | Day when next abs_t - abs_t >= day ->
              to_float_string ~abs_t ~unit_of_time ~fixup_unit_of_time:Day
            (* For everything in between, we need to use integer units of days, hours,
               and/or minutes, because those units are not readable as decimals, and we
               tack on a decimal remainder of a seconds-or-smaller unit if necessary. *)
            | Minute | Hour | Day ->
              let sum_t = zero in
              let day_string, sum_t = to_int_string_and_sum ~abs_t ~sum_t Day in
              let hour_string, sum_t = to_int_string_and_sum ~abs_t ~sum_t Hour in
              let minute_string, sum_t = to_int_string_and_sum ~abs_t ~sum_t Minute in
              let float_string = to_float_string_after_int_strings ~abs_t ~sum_t in
              day_string ^? hour_string ^? minute_string ^? float_string
          in
          sign ^? magnitude_string)
      ;;
    end

    let to_string = To_string.to_string
    let sexp_of_t t = Sexp.Atom (to_string t)

    let t_of_sexp s =
      match s with
      | Sexp.Atom x ->
        (try of_string x with
         | exn -> of_sexp_error (Exn.to_string exn) s)
      | Sexp.List _ ->
        of_sexp_error "Time.Span.Stable.V3.t_of_sexp: sexp must be an Atom" s
    ;;

    let t_sexp_grammar = Sexplib.Sexp_grammar.coerce String.t_sexp_grammar

    include Diffable.Atomic.Make (struct
      type nonrec t = t [@@deriving bin_io, equal, sexp]
    end)
  end
end

include Stable.V3

let to_proportional_float = to_float

let to_string_hum
  ?(delimiter = '_')
  ?(decimals = 3)
  ?(align_decimal = false)
  ?unit_of_time
  t
  =
  let float, suffix =
    match Option.value unit_of_time ~default:(to_unit_of_time t) with
    | Day -> to_day t, "d"
    | Hour -> to_hr t, "h"
    | Minute -> to_min t, "m"
    | Second -> to_sec t, "s"
    | Millisecond -> to_ms t, "ms"
    | Microsecond -> to_us t, "us"
    | Nanosecond -> to_ns t, "ns"
  in
  let prefix =
    Float.to_string_hum float ~delimiter ~decimals ~strip_zero:(not align_decimal)
  in
  let suffix =
    if align_decimal && Int.( = ) (String.length suffix) 1 then suffix ^ " " else suffix
  in
  prefix ^ suffix
;;

let gen_incl lo hi =
  Float.gen_incl (to_sec lo) (to_sec hi) |> Quickcheck.Generator.map ~f:of_sec
;;

let gen_uniform_incl lo hi =
  (* Technically exclusive rather than inclusive, but otherwise satisfies the contract to
     within 1ulp of the given bounds. *)
  Float.gen_uniform_excl (to_sec lo) (to_sec hi) |> Quickcheck.Generator.map ~f:of_sec
;;

let quickcheck_generator =
  (* We generate spans up to (slightly more than) a millennium, positive or negative. This
     is based on the Gregorian calendar, in which years average 365.2425 days when
     accounting for leap days. Covering a two-millennium span is more than enough for most
     practical purposes, certainly more than enough to cover the representable range of
     [Span_ns], and results in finite spans and times that can be serialized.

     We generate by filtering the default generator so that spans are still skewed toward
     small values, even though the bounds are large. *)
  let millennium = of_day (Float.round_up (365.2425 *. 1000.)) in
  Quickcheck.Generator.filter quickcheck_generator ~f:(fun t ->
    neg millennium <= t && t <= millennium)
;;

include Pretty_printer.Register (struct
  type nonrec t = t

  let to_string = to_string
  let module_name = "Core.Time.Span"
end)

include Hashable.Make_binable (struct
  type nonrec t = t [@@deriving bin_io, compare, hash, sexp_of]

  (* Previous versions rendered hash-based containers using float serialization rather
       than time serialization, so when reading hash-based containers in we accept either
       serialization. *)
  let t_of_sexp sexp =
    match Float.t_of_sexp sexp with
    | float -> of_float float
    | exception _ -> t_of_sexp sexp
  ;;
end)

module C = struct
  type t = T.t [@@deriving bin_io]
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

include Comparable.With_zero (struct
  type nonrec t = t [@@deriving compare, sexp_of]

  let zero = zero
end)

module Private = struct
  let suffix_of_unit_of_time = suffix_of_unit_of_time
  let parse_suffix = Stable.V3.Of_string.parse_suffix
end
