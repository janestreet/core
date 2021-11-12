(* This file contains benchmarks for nearly every function in [Core.Time_ns]. The code is
   carefully laid out using patterns that make it easier to read and maintain such a
   tedious, hand-written set of benchmarks.

   To make sure we do not forget any functions, this module exports [module type of
   Time_ns]. Internally, we avoid shorthands such as [include] so that we are forced to
   define each function individually. We can then check that every [let] of a function is
   followed by one or more inline benchmarks.

   When functions are exported in more than one place, such as [Time_ns.compare] and
   [Time_ns.Replace_polymorphic_compare.compare], we benchmark both separately. Artifacts
   such as functor shadowing can affect inlining and cause different exports of the same
   function to perform differently.

   Within each submodule, we maintain [open]s so that names from the original module are
   in scope. This shortens code, and also avoids accidentally referring to the wrong
   version of a function.

   In order to keep benchmarks meaningful, every benchmark is an application of the
   function defined above to constants and/or variables. Every variable is a constant
   defined above and wrapped with [Sys.opaque_identity] (shorthand as [opaque]) so that
   the benchmark cannot be specialized.

   When we define helpers, such as constant values, we put them in a separate submodule
   and [open], but not [include], the module. That keeps helper definitions from ever
   clashing with new bindings that might be added to [Time_ns].

   There are some definitions we do not bother to benchmark in this file. Obviously,
   constants and types cannot be benchmarked. For now we ignore benchmarking any [bin_io]
   performance, although we may want to add benchmarks for that some day.  We also skip
   benchmarking functor-produced modules like [*.Map] and [*.Table]. *)
open! Core
module Time = Time_unix
module Time_ns = Time_ns_unix

module Constants = struct
  let opaque = Sys.opaque_identity
  let hash_state = opaque (Hash.create ())
  let sign_neg = opaque Sign.Neg
  let float_ten = opaque 10.
  let int_ten = opaque 10
  let int32_ten = opaque 10l
  let int63_ten = opaque (Int63.of_int 10)
  let percent_half = opaque (Percent.of_mult 0.5)
  let non_ns_day = opaque Time.Span.day
  let zone = opaque (Time.Zone.find_exn "America/New_York")
  let nyc = zone
  let date = opaque (Date.create_exn ~y:2013 ~m:Oct ~d:07)
end

open Constants
open Time_ns

module Span = struct
  open Span
  module Map = Map
  module Set = Set
  module Table = Table
  module Hash_set = Hash_set
  module Hash_queue = Hash_queue

  (* Nothing interesting to benchmark in [Parts]. *)
  module Parts = Parts

  (* In [Core], there's no particularly good reason to use [Alternate_sexp]. *)
  module Alternate_sexp = Alternate_sexp [@warning "-3"]

  type nonrec t = t [@@deriving bin_io, quickcheck, sexp_grammar, typerep]
  type nonrec underlying = underlying
  type nonrec comparator_witness = comparator_witness

  let arg_type = arg_type
  let comparator = comparator
  let hashable = hashable
  let pp = pp
  let gen_incl = gen_incl
  let gen_uniform_incl = gen_uniform_incl
  let zero = opaque zero
  let min_value_representable = opaque min_value_representable
  let max_value_representable = opaque max_value_representable
  let min_value_for_1us_rounding = opaque min_value_for_1us_rounding
  let max_value_for_1us_rounding = opaque max_value_for_1us_rounding
  let min_value = opaque min_value_for_1us_rounding
  let max_value = opaque max_value_for_1us_rounding
  let nanosecond = opaque nanosecond
  let microsecond = opaque microsecond
  let millisecond = opaque millisecond
  let second = opaque second
  let minute = opaque minute
  let hour = opaque hour
  let day = opaque day
  let robust_comparison_tolerance = opaque robust_comparison_tolerance

  module Span_constants = struct
    let kiloday = opaque (create ~day:1000 ())
    let sexp_of_second = opaque (sexp_of_t second)
    let string_of_nanosecond = opaque (to_string nanosecond)
    let string_of_microsecond = opaque (to_string microsecond)
    let string_of_millisecond = opaque (to_string millisecond)
    let string_of_second = opaque (to_string second)
    let string_of_minute = opaque (to_string minute)
    let string_of_hour = opaque (to_string hour)
    let string_of_day = opaque (to_string day)
    let string_of_kiloday = opaque (to_string kiloday)
    let pi_nanoseconds = opaque (of_ns Float.pi)
    let pi_microseconds = opaque (of_us Float.pi)
    let pi_milliseconds = opaque (of_ms Float.pi)
    let pi_seconds = opaque (of_sec Float.pi)
    let pi_minutes = opaque (of_min Float.pi)
    let pi_hours = opaque (of_hr Float.pi)
    let pi_days = opaque (of_day Float.pi)
    let pi_kilodays = opaque (of_day (Float.pi *. 1000.))
    let sexp_of_pi_nanoseconds = opaque (sexp_of_t pi_nanoseconds)
    let sexp_of_pi_microseconds = opaque (sexp_of_t pi_microseconds)
    let sexp_of_pi_milliseconds = opaque (sexp_of_t pi_milliseconds)
    let sexp_of_pi_seconds = opaque (sexp_of_t pi_seconds)
    let sexp_of_pi_minutes = opaque (sexp_of_t pi_minutes)
    let sexp_of_pi_hours = opaque (sexp_of_t pi_hours)
    let sexp_of_pi_days = opaque (sexp_of_t pi_days)
    let sexp_of_pi_kilodays = opaque (sexp_of_t pi_kilodays)
    let v1_sexp_of_second = opaque (Stable.V1.sexp_of_t second)
    let v1_sexp_of_pi_nanoseconds = opaque (Stable.V1.sexp_of_t pi_nanoseconds)
    let v1_sexp_of_pi_microseconds = opaque (Stable.V1.sexp_of_t pi_microseconds)
    let v1_sexp_of_pi_milliseconds = opaque (Stable.V1.sexp_of_t pi_milliseconds)
    let v1_sexp_of_pi_seconds = opaque (Stable.V1.sexp_of_t pi_seconds)
    let v1_sexp_of_pi_minutes = opaque (Stable.V1.sexp_of_t pi_minutes)
    let v1_sexp_of_pi_hours = opaque (Stable.V1.sexp_of_t pi_hours)
    let v1_sexp_of_pi_days = opaque (Stable.V1.sexp_of_t pi_days)
    let v1_sexp_of_pi_kilodays = opaque (Stable.V1.sexp_of_t pi_kilodays)
    let string_of_pi_nanoseconds = opaque (to_string pi_nanoseconds)
    let string_of_pi_microseconds = opaque (to_string pi_microseconds)
    let string_of_pi_milliseconds = opaque (to_string pi_milliseconds)
    let string_of_pi_seconds = opaque (to_string pi_seconds)
    let string_of_pi_minutes = opaque (to_string pi_minutes)
    let string_of_pi_hours = opaque (to_string pi_hours)
    let string_of_pi_days = opaque (to_string pi_days)
    let string_of_pi_kilodays = opaque (to_string pi_kilodays)
    let string_of_decimal_pi_days = opaque (String.( ^ ) (Float.to_string Float.pi) "d")
    let sexp_of_decimal_pi_days = opaque (Sexp.Atom string_of_decimal_pi_days)
  end

  open Span_constants

  let hash = hash

  let%bench "hash" = hash zero

  let hash_fold_t = hash_fold_t

  let%bench "hash_fold_t" = hash_fold_t hash_state zero

  module Replace_polymorphic_compare = struct
    open Replace_polymorphic_compare

    let compare = compare

    let%bench "compare" = compare min_value_for_1us_rounding max_value_for_1us_rounding

    let equal = equal

    let%bench "equal" = equal min_value_for_1us_rounding max_value_for_1us_rounding

    let min = min

    let%bench "min" = min min_value_for_1us_rounding max_value_for_1us_rounding

    let max = max

    let%bench "max" = max min_value_for_1us_rounding max_value_for_1us_rounding

    let ( = ) = ( = )

    let%bench "(=)" = min_value_for_1us_rounding = max_value_for_1us_rounding

    let ( < ) = ( < )

    let%bench "(<)" = min_value_for_1us_rounding < max_value_for_1us_rounding

    let ( > ) = ( > )

    let%bench "(>)" = min_value_for_1us_rounding > max_value_for_1us_rounding

    let ( <> ) = ( <> )

    let%bench "(<>)" = min_value_for_1us_rounding <> max_value_for_1us_rounding

    let ( <= ) = ( <= )

    let%bench "(<=)" = min_value_for_1us_rounding <= max_value_for_1us_rounding

    let ( >= ) = ( >= )

    let%bench "(>=)" = min_value_for_1us_rounding >= max_value_for_1us_rounding
  end

  let compare = compare

  let%bench "compare" = compare min_value_for_1us_rounding max_value_for_1us_rounding

  let ascending = ascending

  let%bench "ascending" = ascending min_value_for_1us_rounding max_value_for_1us_rounding

  let descending = descending

  let%bench "descending" =
    descending min_value_for_1us_rounding max_value_for_1us_rounding
  ;;

  let equal = equal

  let%bench "equal" = equal min_value_for_1us_rounding max_value_for_1us_rounding

  let min = min

  let%bench "min" = min min_value_for_1us_rounding max_value_for_1us_rounding

  let max = max

  let%bench "max" = max min_value_for_1us_rounding max_value_for_1us_rounding

  let ( = ) = ( = )

  let%bench "(=)" = min_value_for_1us_rounding = max_value_for_1us_rounding

  let ( < ) = ( < )

  let%bench "(<)" = min_value_for_1us_rounding < max_value_for_1us_rounding

  let ( > ) = ( > )

  let%bench "(>)" = min_value_for_1us_rounding > max_value_for_1us_rounding

  let ( <> ) = ( <> )

  let%bench "(<>)" = min_value_for_1us_rounding <> max_value_for_1us_rounding

  let ( <= ) = ( <= )

  let%bench "(<=)" = min_value_for_1us_rounding <= max_value_for_1us_rounding

  let ( >= ) = ( >= )

  let%bench "(>=)" = min_value_for_1us_rounding >= max_value_for_1us_rounding

  let between = between

  let%bench "between" =
    between ~low:min_value_for_1us_rounding ~high:max_value_for_1us_rounding zero
  ;;

  let clamp_exn = clamp_exn

  let%bench "clamp_exn" =
    clamp_exn ~min:min_value_for_1us_rounding ~max:max_value_for_1us_rounding zero
  ;;

  let clamp = clamp

  let%bench "clamp" =
    clamp ~min:min_value_for_1us_rounding ~max:max_value_for_1us_rounding zero
  ;;

  let validate_ubound = validate_ubound

  let%bench "validate_ubound (success)" =
    (validate_ubound ~max:(Incl zero) zero : Validate.t)
  ;;

  let%bench "validate_ubound (failure)" =
    (validate_ubound ~max:(Excl zero) zero : Validate.t)
  ;;

  let validate_lbound = validate_lbound

  let%bench "validate_lbound (success)" =
    (validate_lbound ~min:(Incl zero) zero : Validate.t)
  ;;

  let%bench "validate_lbound (failure)" =
    (validate_lbound ~min:(Excl zero) zero : Validate.t)
  ;;

  let validate_bound = validate_bound

  let%bench "validate_bound (success)" =
    (validate_bound
       ~min:(Excl min_value_for_1us_rounding)
       ~max:(Excl max_value_for_1us_rounding)
       zero
     : Validate.t)
  ;;

  let%bench "validate_bound (failure)" =
    (validate_bound
       ~min:(Excl min_value_for_1us_rounding)
       ~max:(Excl max_value_for_1us_rounding)
       min_value_for_1us_rounding
     : Validate.t)
  ;;

  let validate_positive = validate_positive

  let%bench "validate_positive (success)" =
    (validate_positive max_value_for_1us_rounding : Validate.t)
  ;;

  let%bench "validate_positive (failure)" =
    (validate_positive min_value_for_1us_rounding : Validate.t)
  ;;

  let validate_negative = validate_negative

  let%bench "validate_negative (success)" =
    (validate_negative min_value_for_1us_rounding : Validate.t)
  ;;

  let%bench "validate_negative (failure)" =
    (validate_negative max_value_for_1us_rounding : Validate.t)
  ;;

  let validate_non_positive = validate_non_positive

  let%bench "validate_non_positive (success)" =
    (validate_non_positive min_value_for_1us_rounding : Validate.t)
  ;;

  let%bench "validate_non_positive (failure)" =
    (validate_non_positive max_value_for_1us_rounding : Validate.t)
  ;;

  let validate_non_negative = validate_non_negative

  let%bench "validate_non_negative (success)" =
    (validate_non_negative max_value_for_1us_rounding : Validate.t)
  ;;

  let%bench "validate_non_negative (failure)" =
    (validate_non_negative min_value_for_1us_rounding : Validate.t)
  ;;

  let is_positive = is_positive

  let%bench "is_positive" = is_positive zero

  let is_negative = is_negative

  let%bench "is_negative" = is_negative zero

  let is_non_positive = is_non_positive

  let%bench "is_non_positive" = is_non_positive zero

  let is_non_negative = is_non_negative

  let%bench "is_non_negative" = is_non_negative zero

  let sign = sign

  let%bench "sign" = sign day

  let robustly_compare = robustly_compare

  let%bench "robustly_compare" =
    robustly_compare min_value_for_1us_rounding max_value_for_1us_rounding
  ;;

  let ( =. ) = ( =. )

  let%bench "(=.)" = min_value_for_1us_rounding =. max_value_for_1us_rounding

  let ( <. ) = ( <. )

  let%bench "(<.)" = min_value_for_1us_rounding <. max_value_for_1us_rounding

  let ( >. ) = ( >. )

  let%bench "(>.)" = min_value_for_1us_rounding >. max_value_for_1us_rounding

  let ( <>. ) = ( <>. )

  let%bench "(<>.)" = min_value_for_1us_rounding <>. max_value_for_1us_rounding

  let ( <=. ) = ( <=. )

  let%bench "(<=.)" = min_value_for_1us_rounding <=. max_value_for_1us_rounding

  let ( >=. ) = ( >=. )

  let%bench "(>=.)" = min_value_for_1us_rounding >=. max_value_for_1us_rounding

  let to_span_float_round_nearest = to_span_float_round_nearest

  let%bench "to_span_float_round_nearest" = to_span_float_round_nearest day

  let of_span_float_round_nearest = of_span_float_round_nearest

  let%bench "of_span_float_round_nearest" = of_span_float_round_nearest non_ns_day

  let to_span = to_span_float_round_nearest_microsecond
  let to_span_float_round_nearest_microsecond = to_span_float_round_nearest_microsecond

  let%bench "to_span_float_round_nearest_microsecond" =
    to_span_float_round_nearest_microsecond day
  ;;

  let of_span = of_span_float_round_nearest_microsecond
  let of_span_float_round_nearest_microsecond = of_span_float_round_nearest_microsecond

  let%bench "of_span_float_round_nearest_microsecond" =
    of_span_float_round_nearest_microsecond non_ns_day
  ;;

  let of_ns = of_ns

  let%bench "of_ns" = of_ns float_ten

  let of_us = of_us

  let%bench "of_us" = of_us float_ten

  let of_ms = of_ms

  let%bench "of_ms" = of_ms float_ten

  let of_sec = of_sec

  let%bench "of_sec" = of_sec float_ten

  let of_min = of_min

  let%bench "of_min" = of_min float_ten

  let of_hr = of_hr

  let%bench "of_hr" = of_hr float_ten

  let of_day = of_day

  let%bench "of_day" = of_day float_ten

  let to_ns = to_ns

  let%bench "to_ns" = to_ns zero

  let to_us = to_us

  let%bench "to_us" = to_us zero

  let to_ms = to_ms

  let%bench "to_ms" = to_ms zero

  let to_sec = to_sec

  let%bench "to_sec" = to_sec zero

  let to_min = to_min

  let%bench "to_min" = to_min zero

  let to_hr = to_hr

  let%bench "to_hr" = to_hr zero

  let to_day = to_day

  let%bench "to_day" = to_day zero

  let of_int_ns = of_int_ns

  let%bench "of_int_ns" = of_int_ns int_ten

  let of_int_us = of_int_us

  let%bench "of_int_us" = of_int_us int_ten

  let of_int_ms = of_int_ms

  let%bench "of_int_ms" = of_int_ms int_ten

  let of_int_sec = of_int_sec

  let%bench "of_int_sec" = of_int_sec int_ten

  let to_int_ns = to_int_ns

  let%bench "to_int_ns" = to_int_ns second

  let to_int_us = to_int_us

  let%bench "to_int_us" = to_int_us second

  let to_int_ms = to_int_ms

  let%bench "to_int_ms" = to_int_ms second

  let to_int_sec = to_int_sec

  let%bench "to_int_sec" = to_int_sec second

  let to_int63_ns = to_int63_ns

  let%bench "to_int63_ns" = to_int63_ns second

  let of_int63_ns = of_int63_ns

  let%bench "of_int63_ns" = of_int63_ns int63_ten

  let ( + ) = ( + )

  let%bench "(+)" = day + hour

  let ( - ) = ( - )

  let%bench "(-)" = day - hour

  let abs = abs

  let%bench "abs" = abs min_value_for_1us_rounding

  let neg = neg

  let%bench "neg" = neg min_value_for_1us_rounding

  let scale = scale

  let%bench "scale" = scale day float_ten

  let scale_int = scale_int

  let%bench "scale_int" = scale_int day int_ten

  let div = div

  let%bench "div" = div day hour

  let ( // ) = ( // )

  let%bench "(//)" = day // hour

  let ( / ) = ( / )

  let%bench "(/)" = day / float_ten

  let to_proportional_float = to_proportional_float

  let%bench "to_proportional_float" = to_proportional_float day

  let create = create

  let%bench "create" =
    create
      ~sign:sign_neg
      ~day:int_ten
      ~hr:int_ten
      ~min:int_ten
      ~sec:int_ten
      ~ms:int_ten
      ~us:int_ten
      ~ns:int_ten
      ()
  ;;

  let to_parts = to_parts

  let%bench "to_parts" = to_parts day

  let to_unit_of_time = to_unit_of_time

  let%bench "to_unit_of_time (ns)" = to_unit_of_time pi_nanoseconds
  let%bench "to_unit_of_time (us)" = to_unit_of_time pi_microseconds
  let%bench "to_unit_of_time (ms)" = to_unit_of_time pi_milliseconds
  let%bench "to_unit_of_time (s)" = to_unit_of_time pi_seconds
  let%bench "to_unit_of_time (m)" = to_unit_of_time pi_minutes
  let%bench "to_unit_of_time (h)" = to_unit_of_time pi_hours
  let%bench "to_unit_of_time (d)" = to_unit_of_time pi_days

  let of_unit_of_time = of_unit_of_time

  let%bench "of_unit_of_time (ns)" = of_unit_of_time Nanosecond
  let%bench "of_unit_of_time (us)" = of_unit_of_time Microsecond
  let%bench "of_unit_of_time (ms)" = of_unit_of_time Millisecond
  let%bench "of_unit_of_time (s)" = of_unit_of_time Second
  let%bench "of_unit_of_time (m)" = of_unit_of_time Minute
  let%bench "of_unit_of_time (h)" = of_unit_of_time Hour
  let%bench "of_unit_of_time (d)" = of_unit_of_time Day

  let randomize = randomize

  let%bench "randomize" = randomize pi_microseconds ~percent:percent_half

  let random = random

  let%bench "random" = random ()

  let to_string_hum = to_string_hum

  let%bench "to_string_hum (ns)" = to_string_hum pi_nanoseconds
  let%bench "to_string_hum (us)" = to_string_hum pi_microseconds
  let%bench "to_string_hum (ms)" = to_string_hum pi_milliseconds
  let%bench "to_string_hum (s)" = to_string_hum pi_seconds
  let%bench "to_string_hum (m)" = to_string_hum pi_minutes
  let%bench "to_string_hum (h)" = to_string_hum pi_hours
  let%bench "to_string_hum (d)" = to_string_hum pi_days
  let%bench "to_string_hum (ns, .##)" = to_string_hum pi_nanoseconds ~decimals:2
  let%bench "to_string_hum (us, .##)" = to_string_hum pi_microseconds ~decimals:2
  let%bench "to_string_hum (ms, .##)" = to_string_hum pi_milliseconds ~decimals:2
  let%bench "to_string_hum (s, .##)" = to_string_hum pi_seconds ~decimals:2
  let%bench "to_string_hum (m, .##)" = to_string_hum pi_minutes ~decimals:2
  let%bench "to_string_hum (h, .##)" = to_string_hum pi_hours ~decimals:2
  let%bench "to_string_hum (d, .##)" = to_string_hum pi_days ~decimals:2
  let%bench "to_string_hum (ns, just)" = to_string_hum pi_nanoseconds ~align_decimal:true
  let%bench "to_string_hum (us, just)" = to_string_hum pi_microseconds ~align_decimal:true
  let%bench "to_string_hum (ms, just)" = to_string_hum pi_milliseconds ~align_decimal:true
  let%bench "to_string_hum (s, just)" = to_string_hum pi_seconds ~align_decimal:true
  let%bench "to_string_hum (m, just)" = to_string_hum pi_minutes ~align_decimal:true
  let%bench "to_string_hum (h, just)" = to_string_hum pi_hours ~align_decimal:true
  let%bench "to_string_hum (d, just)" = to_string_hum pi_days ~align_decimal:true

  let to_short_string = to_short_string

  let%bench "to_short_string (ns)" = to_short_string nanosecond
  let%bench "to_short_string (us)" = to_short_string microsecond
  let%bench "to_short_string (ms)" = to_short_string millisecond
  let%bench "to_short_string (s)" = to_short_string second
  let%bench "to_short_string (m)" = to_short_string minute
  let%bench "to_short_string (h)" = to_short_string hour
  let%bench "to_short_string (d)" = to_short_string day
  let%bench "to_short_string (kd)" = to_short_string kiloday
  let%bench "to_short_string (ns+)" = to_short_string pi_nanoseconds
  let%bench "to_short_string (us+)" = to_short_string pi_microseconds
  let%bench "to_short_string (ms+)" = to_short_string pi_milliseconds
  let%bench "to_short_string (s+)" = to_short_string pi_seconds
  let%bench "to_short_string (m+)" = to_short_string pi_minutes
  let%bench "to_short_string (h+)" = to_short_string pi_hours
  let%bench "to_short_string (d+)" = to_short_string pi_days
  let%bench "to_short_string (kd+)" = to_short_string pi_kilodays

  let to_string = to_string

  let%bench "to_string (ns)" = to_string nanosecond
  let%bench "to_string (us)" = to_string microsecond
  let%bench "to_string (ms)" = to_string millisecond
  let%bench "to_string (s)" = to_string second
  let%bench "to_string (m)" = to_string minute
  let%bench "to_string (h)" = to_string hour
  let%bench "to_string (d)" = to_string day
  let%bench "to_string (kd)" = to_string kiloday
  let%bench "to_string (ns+)" = to_string pi_nanoseconds
  let%bench "to_string (us+)" = to_string pi_microseconds
  let%bench "to_string (ms+)" = to_string pi_milliseconds
  let%bench "to_string (s+)" = to_string pi_seconds
  let%bench "to_string (m+)" = to_string pi_minutes
  let%bench "to_string (h+)" = to_string pi_hours
  let%bench "to_string (d+)" = to_string pi_days
  let%bench "to_string (kd+)" = to_string pi_kilodays

  let of_string = of_string

  let%bench "of_string (ns)" = of_string string_of_nanosecond
  let%bench "of_string (us)" = of_string string_of_microsecond
  let%bench "of_string (ms)" = of_string string_of_millisecond
  let%bench "of_string (s)" = of_string string_of_second
  let%bench "of_string (m)" = of_string string_of_minute
  let%bench "of_string (h)" = of_string string_of_hour
  let%bench "of_string (d)" = of_string string_of_day
  let%bench "of_string (kd)" = of_string string_of_kiloday
  let%bench "of_string (ns+)" = of_string string_of_pi_nanoseconds
  let%bench "of_string (us+)" = of_string string_of_pi_microseconds
  let%bench "of_string (ms+)" = of_string string_of_pi_milliseconds
  let%bench "of_string (s+)" = of_string string_of_pi_seconds
  let%bench "of_string (m+)" = of_string string_of_pi_minutes
  let%bench "of_string (h+)" = of_string string_of_pi_hours
  let%bench "of_string (d+)" = of_string string_of_pi_days
  let%bench "of_string (kd+)" = of_string string_of_pi_kilodays
  let%bench "of_string (d.)" = of_string string_of_decimal_pi_days

  let sexp_of_t = sexp_of_t

  let%bench "sexp_of_t (s)" = sexp_of_t second
  let%bench "sexp_of_t (ns+)" = sexp_of_t pi_nanoseconds
  let%bench "sexp_of_t (us+)" = sexp_of_t pi_microseconds
  let%bench "sexp_of_t (ms+)" = sexp_of_t pi_milliseconds
  let%bench "sexp_of_t (s+)" = sexp_of_t pi_seconds
  let%bench "sexp_of_t (m+)" = sexp_of_t pi_minutes
  let%bench "sexp_of_t (h+)" = sexp_of_t pi_hours
  let%bench "sexp_of_t (d+)" = sexp_of_t pi_days
  let%bench "sexp_of_t (kd+)" = sexp_of_t pi_kilodays

  let t_of_sexp = t_of_sexp

  let%bench "t_of_sexp (s)" = t_of_sexp sexp_of_second
  let%bench "t_of_sexp (ns+)" = t_of_sexp sexp_of_pi_nanoseconds
  let%bench "t_of_sexp (us+)" = t_of_sexp sexp_of_pi_microseconds
  let%bench "t_of_sexp (ms+)" = t_of_sexp sexp_of_pi_milliseconds
  let%bench "t_of_sexp (s+)" = t_of_sexp sexp_of_pi_seconds
  let%bench "t_of_sexp (m+)" = t_of_sexp sexp_of_pi_minutes
  let%bench "t_of_sexp (h+)" = t_of_sexp sexp_of_pi_hours
  let%bench "t_of_sexp (d+)" = t_of_sexp sexp_of_pi_days
  let%bench "t_of_sexp (kd+)" = t_of_sexp sexp_of_pi_kilodays
  let%bench "t_of_sexp (d.)" = t_of_sexp sexp_of_decimal_pi_days

  let since_unix_epoch = since_unix_epoch

  let%bench "since_unix_epoch" = since_unix_epoch ()

  let prev = prev

  let%bench "prev" = prev zero

  let next = next

  let%bench "next" = next zero

  let scale_int63 = scale_int63

  let%bench "scale_int63" = scale_int63 minute int63_ten

  let of_sec_with_microsecond_precision = of_sec_with_microsecond_precision

  let%bench "of_sec_with_microsecond_precision" =
    of_sec_with_microsecond_precision float_ten
  ;;

  let to_int63_seconds_round_down_exn = to_int63_seconds_round_down_exn

  let%bench "to_int63_seconds_round_down_exn" = to_int63_seconds_round_down_exn minute

  let of_int63_seconds = of_int63_seconds

  let%bench "of_int63_seconds" = of_int63_seconds int63_ten

  let of_int32_seconds = of_int32_seconds

  let%bench "of_int32_seconds" = of_int32_seconds int32_ten

  module Stable = struct
    open Stable

    module V1 = struct
      open V1

      type nonrec t = t [@@deriving bin_io, hash]
      type nonrec comparator_witness = comparator_witness

      let comparator = comparator
      let compare = compare

      let%bench "compare" = compare min_value_for_1us_rounding max_value_for_1us_rounding

      let equal = equal

      let%bench "equal" = equal zero zero

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t (s)" = sexp_of_t second
      let%bench "sexp_of_t (ns+)" = sexp_of_t pi_nanoseconds
      let%bench "sexp_of_t (us+)" = sexp_of_t pi_microseconds
      let%bench "sexp_of_t (ms+)" = sexp_of_t pi_milliseconds
      let%bench "sexp_of_t (s+)" = sexp_of_t pi_seconds
      let%bench "sexp_of_t (m+)" = sexp_of_t pi_minutes
      let%bench "sexp_of_t (h+)" = sexp_of_t pi_hours
      let%bench "sexp_of_t (d+)" = sexp_of_t pi_days
      let%bench "sexp_of_t (kd+)" = sexp_of_t pi_kilodays

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp (s)" = t_of_sexp v1_sexp_of_second
      let%bench "t_of_sexp (ns+)" = t_of_sexp v1_sexp_of_pi_nanoseconds
      let%bench "t_of_sexp (us+)" = t_of_sexp v1_sexp_of_pi_microseconds
      let%bench "t_of_sexp (ms+)" = t_of_sexp v1_sexp_of_pi_milliseconds
      let%bench "t_of_sexp (s+)" = t_of_sexp v1_sexp_of_pi_seconds
      let%bench "t_of_sexp (m+)" = t_of_sexp v1_sexp_of_pi_minutes
      let%bench "t_of_sexp (h+)" = t_of_sexp v1_sexp_of_pi_hours
      let%bench "t_of_sexp (d+)" = t_of_sexp v1_sexp_of_pi_days
      let%bench "t_of_sexp (kd+)" = t_of_sexp v1_sexp_of_pi_kilodays
      let%bench "t_of_sexp (d.)" = t_of_sexp sexp_of_decimal_pi_days

      let to_int63 = to_int63

      let%bench "to_int63" = to_int63 day

      let of_int63_exn = of_int63_exn

      let%bench "of_int63_exn" = of_int63_exn int63_ten
    end

    module V2 = struct
      open V2
      module Map = Map
      module Set = Set

      type nonrec t = t [@@deriving bin_io, hash]
      type nonrec comparator_witness = comparator_witness

      let comparator = comparator
      let compare = compare

      let%bench "compare" = compare min_value_for_1us_rounding max_value_for_1us_rounding

      let equal = equal

      let%bench "equal" = equal zero zero

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t (s)" = sexp_of_t second
      let%bench "sexp_of_t (kd+)" = sexp_of_t pi_kilodays

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp (s)" = t_of_sexp sexp_of_second
      let%bench "t_of_sexp (kd+)" = t_of_sexp sexp_of_pi_kilodays
      let%bench "t_of_sexp (d.)" = t_of_sexp sexp_of_decimal_pi_days

      let to_string = to_string

      let%bench "to_string (s)" = to_string second
      let%bench "to_string (kd+)" = to_string pi_kilodays

      let of_string = of_string

      let%bench "of_string (s)" = of_string string_of_second
      let%bench "of_string (kd+)" = of_string string_of_pi_kilodays
      let%bench "of_string (d.)" = of_string string_of_decimal_pi_days

      let to_int63 = to_int63

      let%bench "to_int63" = to_int63 day

      let of_int63_exn = of_int63_exn

      let%bench "of_int63_exn" = of_int63_exn int63_ten
    end
  end

  module Option = struct
    open Option
    module Map = Map
    module Set = Set
    module Table = Table
    module Hash_set = Hash_set
    module Hash_queue = Hash_queue

    type nonrec t = t [@@deriving bin_io, quickcheck, typerep]
    type nonrec comparator_witness = comparator_witness

    let comparator = comparator
    let hashable = hashable
    let pp = pp
    let none = opaque none

    module Span_option_constants = struct
      let some_hour = opaque (some hour)
      let some_day = opaque (some day)
      let some_day_option = opaque (Some day)
      let string_of_none = opaque (to_string none)
      let string_of_some_day = opaque (to_string some_day)
      let sexp_of_none = opaque (sexp_of_t none)
      let sexp_of_some_day = opaque (sexp_of_t some_day)
      let v1_sexp_of_none = opaque (Stable.V1.sexp_of_t none)
      let v1_sexp_of_some_day = opaque (Stable.V1.sexp_of_t some_day)
      let int63_of_none = opaque (Stable.V1.to_int63 none)
      let int63_of_some_day = opaque (Stable.V1.to_int63 some_day)
    end

    open Span_option_constants

    let hash = hash

    let%bench "hash" = hash none

    let hash_fold_t = hash_fold_t

    let%bench "hash_fold_t" = hash_fold_t hash_state none

    module Replace_polymorphic_compare = struct
      open Replace_polymorphic_compare

      let compare = compare

      let%bench "compare" = compare none some_day

      let equal = equal

      let%bench "equal" = equal none some_day

      let min = min

      let%bench "min" = min none some_day

      let max = max

      let%bench "max" = max none some_day

      let ( = ) = ( = )

      let%bench "(=)" = none = some_day

      let ( < ) = ( < )

      let%bench "(<)" = none < some_day

      let ( > ) = ( > )

      let%bench "(>)" = none > some_day

      let ( <> ) = ( <> )

      let%bench "(<>)" = none <> some_day

      let ( <= ) = ( <= )

      let%bench "(<=)" = none <= some_day

      let ( >= ) = ( >= )

      let%bench "(>=)" = none >= some_day
    end

    let compare = compare

    let%bench "compare" = compare none some_day

    let ascending = ascending

    let%bench "ascending" = ascending none some_day

    let descending = descending

    let%bench "descending" = descending none some_day

    let equal = equal

    let%bench "equal" = equal none some_day

    let min = min

    let%bench "min" = min none some_day

    let max = max

    let%bench "max" = max none some_day

    let ( = ) = ( = )

    let%bench "(=)" = none = some_day

    let ( < ) = ( < )

    let%bench "(<)" = none < some_day

    let ( > ) = ( > )

    let%bench "(>)" = none > some_day

    let ( <> ) = ( <> )

    let%bench "(<>)" = none <> some_day

    let ( <= ) = ( <= )

    let%bench "(<=)" = none <= some_day

    let ( >= ) = ( >= )

    let%bench "(>=)" = none >= some_day

    let between = between

    let%bench "between" = between ~low:none ~high:some_day some_hour

    let clamp_exn = clamp_exn

    let%bench "clamp_exn" = clamp_exn ~min:none ~max:some_day some_hour

    let clamp = clamp

    let%bench "clamp" = clamp ~min:none ~max:some_day some_hour

    let validate_ubound = validate_ubound

    let%bench "validate_ubound (success)" =
      (validate_ubound ~max:(Incl some_hour) some_hour : Validate.t)
    ;;

    let%bench "validate_ubound (failure)" =
      (validate_ubound ~max:(Excl some_hour) some_hour : Validate.t)
    ;;

    let validate_lbound = validate_lbound

    let%bench "validate_lbound (success)" =
      (validate_lbound ~min:(Incl some_hour) some_hour : Validate.t)
    ;;

    let%bench "validate_lbound (failure)" =
      (validate_lbound ~min:(Excl some_hour) some_hour : Validate.t)
    ;;

    let validate_bound = validate_bound

    let%bench "validate_bound (success)" =
      (validate_bound ~min:(Excl none) ~max:(Excl some_day) some_hour : Validate.t)
    ;;

    let%bench "validate_bound (failure)" =
      (validate_bound ~min:(Excl none) ~max:(Excl some_day) none : Validate.t)
    ;;

    let some = some

    let%bench "some" = some day

    let is_none = is_none

    let%bench "is_none" = is_none some_day

    let is_some = is_some

    let%bench "is_some" = is_some some_day

    let value_exn = value_exn

    let%bench "value_exn" = value_exn some_day

    let unchecked_value = unchecked_value

    let%bench "unchecked_value" = unchecked_value some_day

    let value = value

    let%bench "value (none)" = value none ~default:hour
    let%bench "value (some)" = value some_day ~default:hour

    let some_is_representable = some_is_representable

    let%bench "some_is_representable" = some_is_representable day

    let of_option = of_option

    let%bench "of_option" = of_option some_day_option

    let to_option = to_option

    let%bench "to_option" = to_option some_day

    let of_string = of_string

    let%bench "of_string (none)" = of_string string_of_none
    let%bench "of_string (some)" = of_string string_of_some_day

    let to_string = to_string

    let%bench "to_string (none)" = to_string none
    let%bench "to_string (some)" = to_string some_day

    let t_of_sexp = t_of_sexp

    let%bench "t_of_sexp (none)" = t_of_sexp sexp_of_none
    let%bench "t_of_sexp (some)" = t_of_sexp sexp_of_some_day

    let sexp_of_t = sexp_of_t

    let%bench "sexp_of_t (none)" = sexp_of_t none
    let%bench "sexp_of_t (some)" = sexp_of_t some_day

    module Optional_syntax = struct
      open Optional_syntax

      module Optional_syntax = struct
        open Optional_syntax

        let is_none = is_none

        let%bench "is_none" = is_none some_day

        let unsafe_value = unsafe_value

        let%bench "unsafe_value" = unsafe_value some_day
      end
    end

    module Stable = struct
      open Stable

      module V1 = struct
        open V1

        type nonrec t = t [@@deriving bin_io]
        type nonrec comparator_witness = comparator_witness

        let comparator = comparator
        let compare = compare

        let%bench "compare" = compare none some_day

        let t_of_sexp = t_of_sexp

        let%bench "t_of_sexp (none)" = t_of_sexp v1_sexp_of_none
        let%bench "t_of_sexp (some)" = t_of_sexp v1_sexp_of_some_day

        let sexp_of_t = sexp_of_t

        let%bench "sexp_of_t (none)" = sexp_of_t none
        let%bench "sexp_of_t (some)" = sexp_of_t some_day

        let to_int63 = to_int63

        let%bench "to_int63 (none)" = to_int63 none
        let%bench "to_int63 (some)" = to_int63 some_day

        let of_int63_exn = of_int63_exn

        let%bench "of_int63_exn (none)" = of_int63_exn int63_of_none
        let%bench "of_int63_exn (some)" = of_int63_exn int63_of_some_day
      end

      module V2 = struct
        open V2

        type nonrec t = t [@@deriving bin_io]
        type nonrec comparator_witness = comparator_witness

        let comparator = comparator
        let compare = compare

        let%bench "compare" = compare none some_day

        let t_of_sexp = t_of_sexp

        let%bench "t_of_sexp (none)" = t_of_sexp sexp_of_none
        let%bench "t_of_sexp (some)" = t_of_sexp sexp_of_some_day

        let sexp_of_t = sexp_of_t

        let%bench "sexp_of_t (none)" = sexp_of_t none
        let%bench "sexp_of_t (some)" = sexp_of_t some_day

        let to_int63 = to_int63

        let%bench "to_int63 (none)" = to_int63 none
        let%bench "to_int63 (some)" = to_int63 some_day

        let of_int63_exn = of_int63_exn

        let%bench "of_int63_exn (none)" = of_int63_exn int63_of_none
        let%bench "of_int63_exn (some)" = of_int63_exn int63_of_some_day
      end
    end
  end
end

module Ofday = struct
  open Ofday
  module Map = Map
  module Set = Set
  module Table = Table
  module Hash_set = Hash_set
  module Hash_queue = Hash_queue

  type nonrec underlying = underlying
  type nonrec t = t [@@deriving bin_io, quickcheck, sexp_grammar, typerep]
  type nonrec comparator_witness = comparator_witness

  let arg_type = arg_type
  let comparator = comparator
  let hashable = hashable
  let pp = pp
  let gen_incl = gen_incl
  let gen_uniform_incl = gen_uniform_incl
  let start_of_day = opaque start_of_day
  let approximate_end_of_day = opaque approximate_end_of_day
  let start_of_next_day = opaque start_of_next_day

  module Ofday_constants = struct
    let example = create ~hr:13 ~min:29 ~sec:59 ~ms:654 ~us:321 () |> opaque

    let morning =
      opaque
        (of_span_since_start_of_day_exn
           (Span.create ~hr:09 ~min:30 ~sec:14 ~ms:123 ~us:456 ~ns:789 ()))
    ;;

    let noon = opaque (Time_ns.Ofday.create ~hr:12 ())

    let evening =
      opaque
        (of_span_since_start_of_day_exn
           (Span.create ~hr:18 ~min:29 ~sec:59 ~ms:987 ~us:654 ~ns:321 ()))
    ;;

    let dst_time_ns =
      opaque (Time_ns.of_date_ofday ~zone (Date.of_string "2013-10-07") morning)
    ;;

    let no_dst_time_ns =
      opaque (Time_ns.of_date_ofday ~zone (Date.of_string "2013-12-11") evening)
    ;;

    let start_of_day_ofday = opaque (to_ofday_float_round_nearest start_of_day)
    let morning_ofday = opaque (to_ofday_float_round_nearest morning)
    let evening_ofday = opaque (to_ofday_float_round_nearest evening)
    let start_of_day_span = opaque (to_span_since_start_of_day start_of_day)
    let morning_span = opaque (to_span_since_start_of_day morning)
    let evening_span = opaque (to_span_since_start_of_day evening)
    let sexp_of_start_of_day = opaque (sexp_of_t start_of_day)
    let sexp_of_morning = opaque (sexp_of_t morning)
    let sexp_of_evening = opaque (sexp_of_t evening)
    let string_of_start_of_day = opaque (to_string start_of_day)
    let string_of_morning = opaque (to_string morning)
    let string_of_evening = opaque (to_string evening)
    let int63_of_start_of_day = opaque (Stable.Ofday.V1.to_int63 start_of_day)
  end

  open Ofday_constants

  let hash = hash

  let%bench "hash" = hash start_of_day

  let hash_fold_t = hash_fold_t

  let%bench "hash_fold_t" = hash_fold_t hash_state start_of_day

  module Replace_polymorphic_compare = struct
    open Replace_polymorphic_compare

    let compare = compare

    let%bench "compare" = compare start_of_day evening

    let equal = equal

    let%bench "equal" = equal start_of_day evening

    let min = min

    let%bench "min" = min start_of_day evening

    let max = max

    let%bench "max" = max start_of_day evening

    let ( = ) = ( = )

    let%bench "(=)" = start_of_day = evening

    let ( < ) = ( < )

    let%bench "(<)" = start_of_day < evening

    let ( > ) = ( > )

    let%bench "(>)" = start_of_day > evening

    let ( <> ) = ( <> )

    let%bench "(<>)" = start_of_day <> evening

    let ( <= ) = ( <= )

    let%bench "(<=)" = start_of_day <= evening

    let ( >= ) = ( >= )

    let%bench "(>=)" = start_of_day >= evening
  end

  let compare = compare

  let%bench "compare" = compare start_of_day evening

  let ascending = ascending

  let%bench "ascending" = ascending start_of_day evening

  let descending = descending

  let%bench "descending" = descending start_of_day evening

  let equal = equal

  let%bench "equal" = equal start_of_day evening

  let min = min

  let%bench "min" = min start_of_day evening

  let max = max

  let%bench "max" = max start_of_day evening

  let ( = ) = ( = )

  let%bench "(=)" = start_of_day = evening

  let ( < ) = ( < )

  let%bench "(<)" = start_of_day < evening

  let ( > ) = ( > )

  let%bench "(>)" = start_of_day > evening

  let ( <> ) = ( <> )

  let%bench "(<>)" = start_of_day <> evening

  let ( <= ) = ( <= )

  let%bench "(<=)" = start_of_day <= evening

  let ( >= ) = ( >= )

  let%bench "(>=)" = start_of_day >= evening

  let between = between

  let%bench "between" = between ~low:start_of_day ~high:evening morning

  let clamp_exn = clamp_exn

  let%bench "clamp_exn" = clamp_exn ~min:start_of_day ~max:evening morning

  let clamp = clamp

  let%bench "clamp" = clamp ~min:start_of_day ~max:evening morning

  let validate_ubound = validate_ubound

  let%bench "validate_ubound (success)" =
    (validate_ubound ~max:(Incl morning) morning : Validate.t)
  ;;

  let%bench "validate_ubound (failure)" =
    (validate_ubound ~max:(Excl morning) morning : Validate.t)
  ;;

  let validate_lbound = validate_lbound

  let%bench "validate_lbound (success)" =
    (validate_lbound ~min:(Incl morning) morning : Validate.t)
  ;;

  let%bench "validate_lbound (failure)" =
    (validate_lbound ~min:(Excl morning) morning : Validate.t)
  ;;

  let validate_bound = validate_bound

  let%bench "validate_bound (success)" =
    (validate_bound ~min:(Excl start_of_day) ~max:(Excl evening) morning : Validate.t)
  ;;

  let%bench "validate_bound (failure)" =
    (validate_bound ~min:(Excl start_of_day) ~max:(Excl evening) start_of_day
     : Validate.t)
  ;;

  let add_exn = add_exn

  let%bench "add_exn" = add_exn morning Span.hour

  let sub_exn = sub_exn

  let%bench "sub_exn" = sub_exn evening Span.hour

  let add = add

  let%bench "add" = add example Span.hour

  let sub = sub

  let%bench "sub" = sub example Span.hour

  let next = next

  let%bench "next" = next example

  let prev = prev

  let%bench "prev" = prev example

  let diff = diff

  let%bench "diff" = diff example start_of_day

  let small_diff = small_diff

  let%bench "small_diff" = small_diff example start_of_day

  let robustly_compare = robustly_compare

  let%bench "robustly_compare (<)" = robustly_compare start_of_day example
  let%bench "robustly_compare (>)" = robustly_compare example start_of_day
  let%bench "robustly_compare (=)" = robustly_compare start_of_day start_of_day

  let ( =. ) = ( =. )

  let%bench "(=.)" = start_of_day =. example

  let ( <. ) = ( <. )

  let%bench "(<.)" = start_of_day <. example

  let ( >. ) = ( >. )

  let%bench "(>.)" = start_of_day >. example

  let ( <=. ) = ( <=. )

  let%bench "(<=.)" = start_of_day <=. example

  let ( >=. ) = ( >=. )

  let%bench "(>=.)" = start_of_day >=. example

  let ( <>. ) = ( <>. )

  let%bench "(<>.)" = start_of_day <>. example

  let create = create

  let%bench "create" = create ~hr:12 ~min:34 ~sec:56 ~ms:123 ~us:456 ()

  let to_parts = to_parts

  let%bench "to_parts" = to_parts example

  let every = every

  let%bench "every" = every Span.hour ~start:start_of_day ~stop:example

  let now = now

  let%bench "now (utc)" = now ~zone:Zone.utc
  let%bench "now (local)" = now ~zone

  let of_ofday_float_round_nearest = of_ofday_float_round_nearest

  let%bench "of_ofday_float_round_nearest (midnight)" =
    of_ofday_float_round_nearest start_of_day_ofday
  ;;

  let%bench "of_ofday_float_round_nearest (morning)" =
    of_ofday_float_round_nearest morning_ofday
  ;;

  let%bench "of_ofday_float_round_nearest (evening)" =
    of_ofday_float_round_nearest evening_ofday
  ;;

  let to_ofday_float_round_nearest = to_ofday_float_round_nearest

  let%bench "to_ofday_float_round_nearest (midnight)" =
    to_ofday_float_round_nearest start_of_day
  ;;

  let%bench "to_ofday_float_round_nearest (morning)" =
    to_ofday_float_round_nearest morning
  ;;

  let%bench "to_ofday_float_round_nearest (evening)" =
    to_ofday_float_round_nearest evening
  ;;

  let of_ofday = of_ofday_float_round_nearest_microsecond
  let of_ofday_float_round_nearest_microsecond = of_ofday_float_round_nearest_microsecond

  let%bench "of_ofday_float_round_nearest_microsecond (midnight)" =
    of_ofday_float_round_nearest_microsecond start_of_day_ofday
  ;;

  let%bench "of_ofday_float_round_nearest_microsecond (morning)" =
    of_ofday_float_round_nearest_microsecond morning_ofday
  ;;

  let%bench "of_ofday_float_round_nearest_microsecond (evening)" =
    of_ofday_float_round_nearest_microsecond evening_ofday
  ;;

  let to_ofday = to_ofday_float_round_nearest_microsecond
  let to_ofday_float_round_nearest_microsecond = to_ofday_float_round_nearest_microsecond

  let%bench "to_ofday_float_round_nearest_microsecond (midnight)" =
    to_ofday_float_round_nearest_microsecond start_of_day
  ;;

  let%bench "to_ofday_float_round_nearest_microsecond (morning)" =
    to_ofday_float_round_nearest_microsecond morning
  ;;

  let%bench "to_ofday_float_round_nearest_microsecond (evening)" =
    to_ofday_float_round_nearest_microsecond evening
  ;;

  let of_span_since_start_of_day_exn = of_span_since_start_of_day_exn
  let of_span_since_start_of_day = of_span_since_start_of_day_exn

  let%bench "of_span_since_start_of_day_exn (midnight)" =
    of_span_since_start_of_day_exn start_of_day_span
  ;;

  let%bench "of_span_since_start_of_day_exn (morning)" =
    of_span_since_start_of_day_exn morning_span
  ;;

  let%bench "of_span_since_start_of_day_exn (evening)" =
    of_span_since_start_of_day_exn evening_span
  ;;

  let of_span_since_start_of_day_unchecked = of_span_since_start_of_day_unchecked

  let%bench "of_span_since_start_of_day_unchecked" =
    of_span_since_start_of_day_unchecked start_of_day_span
  ;;

  let span_since_start_of_day_is_valid = span_since_start_of_day_is_valid

  let%bench "span_since_start_of_day_is_valid" =
    span_since_start_of_day_is_valid start_of_day_span
  ;;

  let to_span_since_start_of_day = to_span_since_start_of_day

  let%bench "to_span_since_start_of_day (midnight)" =
    to_span_since_start_of_day start_of_day
  ;;

  let%bench "to_span_since_start_of_day (morning)" = to_span_since_start_of_day morning
  let%bench "to_span_since_start_of_day (evening)" = to_span_since_start_of_day evening

  let sexp_of_t = sexp_of_t

  let%bench "sexp_of_t (midnight)" = sexp_of_t start_of_day
  let%bench "sexp_of_t (morning)" = sexp_of_t morning
  let%bench "sexp_of_t (evening)" = sexp_of_t evening

  let t_of_sexp = t_of_sexp

  let%bench "t_of_sexp (midnight)" = t_of_sexp sexp_of_start_of_day
  let%bench "t_of_sexp (morning)" = t_of_sexp sexp_of_morning
  let%bench "t_of_sexp (evening)" = t_of_sexp sexp_of_evening

  let to_string = to_string

  let%bench "to_string (midnight)" = to_string start_of_day
  let%bench "to_string (morning)" = to_string morning
  let%bench "to_string (evening)" = to_string evening

  let of_string = of_string

  let%bench "of_string (midnight)" = of_string string_of_start_of_day
  let%bench "of_string (morning)" = of_string string_of_morning
  let%bench "of_string (evening)" = of_string string_of_evening

  let to_string_trimmed = to_string_trimmed

  let%bench "to_string_trimmed (midnight)" = to_string_trimmed start_of_day
  let%bench "to_string_trimmed (morning)" = to_string_trimmed morning
  let%bench "to_string_trimmed (evening)" = to_string_trimmed evening

  let to_sec_string = to_sec_string

  let%bench "to_sec_string (midnight)" = to_sec_string start_of_day
  let%bench "to_sec_string (morning)" = to_sec_string morning
  let%bench "to_sec_string (evening)" = to_sec_string evening

  let to_millisecond_string = to_millisecond_string
  let to_millisec_string = to_millisecond_string

  let%bench "to_millisecond_string (midnight)" = to_millisecond_string start_of_day
  let%bench "to_millisecond_string (morning)" = to_millisecond_string morning
  let%bench "to_millisecond_string (evening)" = to_millisecond_string evening

  let to_microsecond_string = to_microsecond_string

  let%bench "to_microsecond_string (midnight)" = to_microsecond_string start_of_day
  let%bench "to_microsecond_string (morning)" = to_microsecond_string morning
  let%bench "to_microsecond_string (evening)" = to_microsecond_string evening

  let of_string_iso8601_extended = of_string_iso8601_extended

  let%bench "of_string_iso8601_extended (midnight)" =
    of_string_iso8601_extended string_of_start_of_day
  ;;

  let%bench "of_string_iso8601_extended (morning)" =
    of_string_iso8601_extended string_of_morning
  ;;

  let%bench "of_string_iso8601_extended (evening)" =
    of_string_iso8601_extended string_of_evening
  ;;

  module Zoned = struct
    open Zoned

    type nonrec t = t [@@deriving bin_io]

    let arg_type = arg_type
    let pp = pp

    module Ofday_zoned_constants = struct
      let opaque = Sys.opaque_identity
      let utc = opaque Time.Zone.utc
      let date = opaque (Date.create_exn ~y:2013 ~m:Oct ~d:07)
      let zero_utc = create start_of_day utc |> opaque

      let example =
        create Ofday_constants.example (Zone.find_exn "America/New_York") |> opaque
      ;;

      let example_sexp = sexp_of_t example |> opaque
      let example_string = to_string example |> opaque
    end

    include Ofday_zoned_constants

    module With_nonchronological_compare = struct
      open With_nonchronological_compare

      type nonrec t = t [@@deriving bin_io]

      let compare = compare

      let%bench "compare (<)" = compare zero_utc example
      let%bench "compare (>)" = compare example zero_utc
      let%bench "compare (=)" = compare zero_utc zero_utc

      let equal = equal

      let%bench "equal (=)" = equal example example
      let%bench "equal (<>)" = equal zero_utc example

      let hash = hash

      let%bench "hash" = hash zero_utc

      let hash_fold_t = hash_fold_t

      let%bench "hash_fold_t" = hash_fold_t hash_state zero_utc

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t" = sexp_of_t example

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp" = t_of_sexp example_sexp
    end

    let hash = hash

    let%bench "hash" = hash zero_utc

    let hash_fold_t = hash_fold_t

    let%bench "hash_fold_t" = hash_fold_t hash_state zero_utc

    let create = create

    let%bench "create" = create Ofday_constants.example utc

    let create_local = create_local

    let%bench "create_local" = create_local Ofday_constants.example

    let ofday = ofday

    let%bench "ofday" = ofday example

    let zone = zone

    let%bench "zone" = zone example

    let to_time_ns = to_time_ns

    let%bench "to_time_ns" = to_time_ns example date

    let sexp_of_t = sexp_of_t

    let%bench "sexp_of_t" = sexp_of_t example

    let t_of_sexp = t_of_sexp

    let%bench "t_of_sexp" = t_of_sexp example_sexp

    let to_string = to_string

    let%bench "to_string" = to_string example

    let of_string = of_string

    let%bench "of_string" = of_string example_string
  end

  module Option = struct
    open Option
    module Map = Map
    module Set = Set
    module Table = Table
    module Hash_set = Hash_set
    module Hash_queue = Hash_queue

    type nonrec t = t [@@deriving bin_io, quickcheck, typerep]
    type nonrec comparator_witness = comparator_witness

    let comparator = comparator
    let hashable = hashable
    let pp = pp
    let none = opaque none

    module Ofday_option_constants = struct
      let some_start_of_day = opaque (some start_of_day)
      let some_morning = opaque (some morning)
      let some_evening = opaque (some evening)
      let some_start_of_day_option = opaque (Some start_of_day)
      let sexp_of_none = opaque (sexp_of_t none)
      let sexp_of_some_start_of_day = opaque (sexp_of_t some_start_of_day)
      let string_of_none = opaque (to_string none)
      let string_of_some_start_of_day = opaque (to_string some_start_of_day)
      let int63_of_none = opaque (Stable.V1.to_int63 none)
      let int63_of_some_start_of_day = opaque (Stable.V1.to_int63 some_start_of_day)
    end

    open Ofday_option_constants

    let hash = hash

    let%bench "hash" = hash some_start_of_day

    let hash_fold_t = hash_fold_t

    let%bench "hash_fold_t" = hash_fold_t hash_state some_start_of_day

    module Replace_polymorphic_compare = struct
      open Replace_polymorphic_compare

      let compare = compare

      let%bench "compare" = compare none some_start_of_day

      let equal = equal

      let%bench "equal" = equal none some_start_of_day

      let min = min

      let%bench "min" = min none some_start_of_day

      let max = max

      let%bench "max" = max none some_start_of_day

      let ( = ) = ( = )

      let%bench "(=)" = none = some_start_of_day

      let ( < ) = ( < )

      let%bench "(<)" = none < some_start_of_day

      let ( > ) = ( > )

      let%bench "(>)" = none > some_start_of_day

      let ( <> ) = ( <> )

      let%bench "(<>)" = none <> some_start_of_day

      let ( <= ) = ( <= )

      let%bench "(<=)" = none <= some_start_of_day

      let ( >= ) = ( >= )

      let%bench "(>=)" = none >= some_start_of_day
    end

    let compare = compare

    let%bench "compare" = compare none some_start_of_day

    let ascending = ascending

    let%bench "ascending" = ascending none some_start_of_day

    let descending = descending

    let%bench "descending" = descending none some_start_of_day

    let equal = equal

    let%bench "equal" = equal none some_start_of_day

    let min = min

    let%bench "min" = min none some_start_of_day

    let max = max

    let%bench "max" = max none some_start_of_day

    let ( = ) = ( = )

    let%bench "(=)" = none = some_start_of_day

    let ( < ) = ( < )

    let%bench "(<)" = none < some_start_of_day

    let ( > ) = ( > )

    let%bench "(>)" = none > some_start_of_day

    let ( <> ) = ( <> )

    let%bench "(<>)" = none <> some_start_of_day

    let ( <= ) = ( <= )

    let%bench "(<=)" = none <= some_start_of_day

    let ( >= ) = ( >= )

    let%bench "(>=)" = none >= some_start_of_day

    let between = between

    let%bench "between" = between ~low:none ~high:some_morning some_evening

    let clamp_exn = clamp_exn

    let%bench "clamp_exn" = clamp_exn ~min:none ~max:some_morning some_evening

    let clamp = clamp

    let%bench "clamp" = clamp ~min:none ~max:some_morning some_evening

    let validate_ubound = validate_ubound

    let%bench "validate_ubound (success)" =
      (validate_ubound ~max:(Incl some_morning) some_morning : Validate.t)
    ;;

    let%bench "validate_ubound (failure)" =
      (validate_ubound ~max:(Excl some_morning) some_morning : Validate.t)
    ;;

    let validate_lbound = validate_lbound

    let%bench "validate_lbound (success)" =
      (validate_lbound ~min:(Incl some_morning) some_morning : Validate.t)
    ;;

    let%bench "validate_lbound (failure)" =
      (validate_lbound ~min:(Excl some_morning) some_morning : Validate.t)
    ;;

    let validate_bound = validate_bound

    let%bench "validate_bound (success)" =
      (validate_bound ~min:(Excl none) ~max:(Excl some_evening) some_morning : Validate.t)
    ;;

    let%bench "validate_bound (failure)" =
      (validate_bound ~min:(Excl none) ~max:(Excl some_evening) none : Validate.t)
    ;;

    let some = some

    let%bench "some" = some start_of_day

    let is_none = is_none

    let%bench "is_none" = is_none some_start_of_day

    let is_some = is_some

    let%bench "is_some" = is_some some_start_of_day

    let value_exn = value_exn

    let%bench "value_exn" = value_exn some_start_of_day

    let unchecked_value = unchecked_value

    let%bench "unchecked_value" = unchecked_value some_start_of_day

    let value = value

    let%bench "value (none)" = value none ~default:start_of_day
    let%bench "value (some)" = value some_start_of_day ~default:start_of_day

    let some_is_representable = some_is_representable

    let%bench "some_is_representable" = some_is_representable start_of_day

    let of_option = of_option

    let%bench "of_option" = of_option some_start_of_day_option

    let to_option = to_option

    let%bench "to_option" = to_option some_start_of_day

    let sexp_of_t = sexp_of_t

    let%bench "sexp_of_t (none)" = sexp_of_t none
    let%bench "sexp_of_t (some)" = sexp_of_t some_start_of_day

    let t_of_sexp = t_of_sexp

    let%bench "t_of_sexp (none)" = t_of_sexp sexp_of_none
    let%bench "t_of_sexp (some)" = t_of_sexp sexp_of_some_start_of_day

    let to_string = to_string

    let%bench "to_string (none)" = to_string none
    let%bench "to_string (some)" = to_string some_start_of_day

    let of_string = of_string

    let%bench "of_string (none)" = of_string string_of_none
    let%bench "of_string (some)" = of_string string_of_some_start_of_day

    let of_span_since_start_of_day = of_span_since_start_of_day

    let%bench "of_span_since_start_of_day (midnight)" =
      of_span_since_start_of_day start_of_day_span
    ;;

    let%bench "of_span_since_start_of_day (morning)" =
      of_span_since_start_of_day morning_span
    ;;

    let%bench "of_span_since_start_of_day (evening)" =
      of_span_since_start_of_day evening_span
    ;;

    let%bench "of_span_since_start_of_day (max_value)" =
      of_span_since_start_of_day Span.max_value
    ;;

    let%bench "of_span_since_start_of_day (min_value)" =
      of_span_since_start_of_day Span.min_value_for_1us_rounding
    ;;

    module Optional_syntax = struct
      open Optional_syntax

      module Optional_syntax = struct
        open Optional_syntax

        let is_none = is_none

        let%bench "is_none" = is_none some_start_of_day

        let unsafe_value = unsafe_value

        let%bench "unsafe_value" = unsafe_value some_start_of_day
      end
    end

    module Stable = struct
      open Stable

      module V1 = struct
        open V1

        type nonrec t = t [@@deriving bin_io]
        type nonrec comparator_witness = comparator_witness

        let comparator = comparator
        let compare = compare

        let%bench "compare" = compare none some_start_of_day

        let sexp_of_t = sexp_of_t

        let%bench "sexp_of_t (none)" = sexp_of_t none
        let%bench "sexp_of_t (some)" = sexp_of_t some_start_of_day

        let t_of_sexp = t_of_sexp

        let%bench "t_of_sexp (none)" = t_of_sexp sexp_of_none
        let%bench "t_of_sexp (some)" = t_of_sexp sexp_of_some_start_of_day

        let to_int63 = to_int63

        let%bench "to_int63 (none)" = to_int63 none
        let%bench "to_int63 (some)" = to_int63 some_start_of_day

        let of_int63_exn = of_int63_exn

        let%bench "of_int63_exn (none)" = of_int63_exn int63_of_none
        let%bench "of_int63_exn (some)" = of_int63_exn int63_of_some_start_of_day
      end
    end
  end
end

type nonrec t = t [@@deriving bin_io, quickcheck, sexp_grammar, typerep]
type nonrec comparator_witness = comparator_witness

let arg_type = arg_type
let comparator = comparator
let hashable = hashable
let pp = pp
let pause = pause
let pause_forever = pause_forever
let interruptible_pause = interruptible_pause
let gen_incl = gen_incl
let gen_uniform_incl = gen_uniform_incl

module Map = Map
module Set = Set
module Table = Table
module Hash_set = Hash_set
module Hash_queue = Hash_queue
module Zone = Time.Zone

let epoch = opaque epoch
let min_value_representable = opaque min_value_representable
let max_value_representable = opaque max_value_representable
let min_value_for_1us_rounding = opaque min_value_for_1us_rounding
let max_value_for_1us_rounding = opaque max_value_for_1us_rounding
let min_value = opaque min_value_for_1us_rounding
let max_value = opaque max_value_for_1us_rounding

module Time_constants = struct
  let example = of_date_ofday ~zone:Zone.utc date Ofday.Ofday_constants.example |> opaque
  let zone_for_sexp = get_sexp_zone () |> opaque
  let example_sexp = sexp_of_t example |> opaque
  let example_string = to_string example |> opaque
  let example_localized_string = opaque (to_sec_string example ~zone:Zone.utc)
  let dst_t = Ofday.Ofday_constants.dst_time_ns
  let no_dst_t = Ofday.Ofday_constants.no_dst_time_ns
  let epoch_time = opaque (to_time_float_round_nearest epoch)
  let sexp_of_epoch = opaque (sexp_of_t epoch)
  let sexp_of_dst_t = opaque (sexp_of_t dst_t)
  let sexp_of_no_dst_t = opaque (sexp_of_t no_dst_t)
  let string_of_epoch = opaque (to_string epoch)
  let string_of_dst_t = opaque (to_string dst_t)
  let string_of_no_dst_t = opaque (to_string no_dst_t)
  let fix_utc_string_of_epoch = opaque (to_string_fix_proto `Utc epoch)
  let fix_local_string_of_dst_t = opaque (to_string_fix_proto `Local dst_t)
  let fix_local_string_of_no_dst_t = opaque (to_string_fix_proto `Local no_dst_t)
  let abs_string_of_epoch = opaque (to_string_abs epoch ~zone:Zone.utc)
  let abs_string_of_dst_t = opaque (to_string_abs dst_t ~zone)
  let abs_string_of_no_dst_t = opaque (to_string_abs no_dst_t ~zone)
  let filename_string_of_epoch = opaque (to_filename_string epoch ~zone:Zone.utc)
  let filename_string_of_dst_t = opaque (to_filename_string dst_t ~zone)
  let filename_string_of_no_dst_t = opaque (to_filename_string no_dst_t ~zone)
  let int63_of_epoch = opaque (Stable.V1.to_int63 epoch)

  (* We define some dates and times with different daylight savings properties for
     purposes of benchmarking time zone logic. *)

  let nyc_winter_date = opaque (Date.create_exn ~y:2013 ~m:Jan ~d:03)
  let nyc_summer_date = opaque (Date.create_exn ~y:2014 ~m:Jul ~d:04)
  let nyc_transition_date = opaque (Date.create_exn ~y:2015 ~m:Nov ~d:01)
  let nyc_transition_ofday = opaque (Time_ns.Ofday.create ~hr:01 ~min:30 ())
  let nyc_skip_date = opaque (Date.create_exn ~y:2015 ~m:Mar ~d:08)
  let nyc_skip_ofday = opaque (Time_ns.Ofday.create ~hr:02 ~min:30 ())

  let nyc_winter_time =
    opaque (Time_ns.of_date_ofday ~zone:nyc nyc_winter_date Ofday.Ofday_constants.noon)
  ;;

  let nyc_summer_time =
    opaque (Time_ns.of_date_ofday ~zone:nyc nyc_summer_date Ofday.Ofday_constants.noon)
  ;;

  let nyc_transition_time =
    opaque (Time_ns.of_date_ofday ~zone:nyc nyc_transition_date nyc_transition_ofday)
  ;;

  let nyc_skip_time =
    opaque (Time_ns.of_date_ofday ~zone:nyc nyc_skip_date nyc_skip_ofday)
  ;;

  let%test_module "daylight savings" =
    (module struct
      type precise =
        Date.t
        * Time_ns.Ofday.t
        * [ `Also_at of Time_ns.t | `Also_skipped of Date.t * Time_ns.Ofday.t | `Only ]
      [@@deriving sexp_of]

      let test_time time =
        printf !"%{sexp: Time_ns.t}\n" time;
        printf !"%{sexp: precise}\n" (Time_ns.to_date_ofday_precise ~zone:nyc time)
      ;;

      let%expect_test "winter" =
        test_time nyc_winter_time;
        [%expect
          {|
          (2013-01-03 12:00:00.000000000-05:00)
          (2013-01-03 12:00:00.000000000 Only) |}]
      ;;

      let%expect_test "summer" =
        test_time nyc_summer_time;
        [%expect
          {|
          (2014-07-04 12:00:00.000000000-04:00)
          (2014-07-04 12:00:00.000000000 Only) |}]
      ;;

      let%expect_test "transition" =
        test_time nyc_transition_time;
        [%expect
          {|
          (2015-11-01 01:30:00.000000000-05:00)
          (2015-11-01 01:30:00.000000000
           (Also_at (2015-11-01 01:30:00.000000000-04:00))) |}]
      ;;

      let%expect_test "skip" =
        test_time nyc_skip_time;
        [%expect
          {|
          (2015-03-08 03:30:00.000000000-04:00)
          (2015-03-08 03:30:00.000000000
           (Also_skipped (2015-03-08 02:30:00.000000000))) |}]
      ;;
    end)
  ;;
end

open Time_constants

let hash = hash

let%bench "hash" = hash epoch

let hash_fold_t = hash_fold_t

let%bench "hash_fold_t" = hash_fold_t hash_state epoch

module Replace_polymorphic_compare = struct
  open Replace_polymorphic_compare

  let compare = compare

  let%bench "compare" = compare min_value_for_1us_rounding max_value_for_1us_rounding

  let equal = equal

  let%bench "equal" = equal min_value_for_1us_rounding max_value_for_1us_rounding

  let min = min

  let%bench "min" = min min_value_for_1us_rounding max_value_for_1us_rounding

  let max = max

  let%bench "max" = max min_value_for_1us_rounding max_value_for_1us_rounding

  let ( = ) = ( = )

  let%bench "(=)" = min_value_for_1us_rounding = max_value_for_1us_rounding

  let ( < ) = ( < )

  let%bench "(<)" = min_value_for_1us_rounding < max_value_for_1us_rounding

  let ( > ) = ( > )

  let%bench "(>)" = min_value_for_1us_rounding > max_value_for_1us_rounding

  let ( <> ) = ( <> )

  let%bench "(<>)" = min_value_for_1us_rounding <> max_value_for_1us_rounding

  let ( <= ) = ( <= )

  let%bench "(<=)" = min_value_for_1us_rounding <= max_value_for_1us_rounding

  let ( >= ) = ( >= )

  let%bench "(>=)" = min_value_for_1us_rounding >= max_value_for_1us_rounding
end

let compare = compare

let%bench "compare" = compare min_value_for_1us_rounding max_value_for_1us_rounding

let ascending = ascending

let%bench "ascending" = ascending min_value_for_1us_rounding max_value_for_1us_rounding

let descending = descending

let%bench "descending" = descending min_value_for_1us_rounding max_value_for_1us_rounding

let equal = equal

let%bench "equal" = equal min_value_for_1us_rounding max_value_for_1us_rounding

let min = min

let%bench "min" = min min_value_for_1us_rounding max_value_for_1us_rounding

let max = max

let%bench "max" = max min_value_for_1us_rounding max_value_for_1us_rounding

let ( = ) = ( = )

let%bench "(=)" = min_value_for_1us_rounding = max_value_for_1us_rounding

let ( < ) = ( < )

let%bench "(<)" = min_value_for_1us_rounding < max_value_for_1us_rounding

let ( > ) = ( > )

let%bench "(>)" = min_value_for_1us_rounding > max_value_for_1us_rounding

let ( <> ) = ( <> )

let%bench "(<>)" = min_value_for_1us_rounding <> max_value_for_1us_rounding

let ( <= ) = ( <= )

let%bench "(<=)" = min_value_for_1us_rounding <= max_value_for_1us_rounding

let ( >= ) = ( >= )

let%bench "(>=)" = min_value_for_1us_rounding >= max_value_for_1us_rounding

let is_earlier = is_earlier

let%bench "is_earlier" =
  is_earlier min_value_for_1us_rounding ~than:max_value_for_1us_rounding
;;

let is_later = is_later

let%bench "is_later" =
  is_later min_value_for_1us_rounding ~than:max_value_for_1us_rounding
;;

let between = between

let%bench "between" =
  between ~low:min_value_for_1us_rounding ~high:max_value_for_1us_rounding epoch
;;

let clamp_exn = clamp_exn

let%bench "clamp_exn" =
  clamp_exn ~min:min_value_for_1us_rounding ~max:max_value_for_1us_rounding epoch
;;

let clamp = clamp

let%bench "clamp" =
  clamp ~min:min_value_for_1us_rounding ~max:max_value_for_1us_rounding epoch
;;

let validate_ubound = validate_ubound

let%bench "validate_ubound (success)" =
  (validate_ubound ~max:(Incl epoch) epoch : Validate.t)
;;

let%bench "validate_ubound (failure)" =
  (validate_ubound ~max:(Excl epoch) epoch : Validate.t)
;;

let validate_lbound = validate_lbound

let%bench "validate_lbound (success)" =
  (validate_lbound ~min:(Incl epoch) epoch : Validate.t)
;;

let%bench "validate_lbound (failure)" =
  (validate_lbound ~min:(Excl epoch) epoch : Validate.t)
;;

let validate_bound = validate_bound

let%bench "validate_bound (success)" =
  (validate_bound
     ~min:(Excl min_value_for_1us_rounding)
     ~max:(Excl max_value_for_1us_rounding)
     epoch
   : Validate.t)
;;

let%bench "validate_bound (failure)" =
  (validate_bound
     ~min:(Excl min_value_for_1us_rounding)
     ~max:(Excl max_value_for_1us_rounding)
     min_value_for_1us_rounding
   : Validate.t)
;;

let to_time_float_round_nearest = to_time_float_round_nearest

let%bench "to_time_float_round_nearest" = to_time_float_round_nearest epoch

let of_time_float_round_nearest = of_time_float_round_nearest

let%bench "of_time_float_round_nearest" = of_time_float_round_nearest epoch_time

let to_time = to_time_float_round_nearest_microsecond
let to_time_float_round_nearest_microsecond = to_time_float_round_nearest_microsecond

let%bench "to_time_float_round_nearest_microsecond" =
  to_time_float_round_nearest_microsecond epoch
;;

let of_time = of_time_float_round_nearest_microsecond
let of_time_float_round_nearest_microsecond = of_time_float_round_nearest_microsecond

let%bench "of_time_float_round_nearest_microsecond" =
  of_time_float_round_nearest_microsecond epoch_time
;;

let now = now

let%bench "now" = now ()

let add = add

let%bench "add" = add epoch Span.day

let sub = sub

let%bench "sub" = sub epoch Span.day

let add_saturating = add_saturating

let%bench "add_saturating (ok)" = add epoch Span.day
let%bench "add_saturating (saturates)" = add max_value_representable Span.day

let sub_saturating = sub_saturating

let%bench "sub_saturating (ok)" = sub epoch Span.day
let%bench "sub_saturating (saturates)" = sub min_value_representable Span.day

let next = next

let%bench "next" = next example

let prev = prev

let%bench "prev" = prev example

let diff = diff

let%bench "diff" = diff dst_t no_dst_t

let abs_diff = abs_diff

let%bench "abs_diff" = abs_diff dst_t no_dst_t

let to_span_since_epoch = to_span_since_epoch

let%bench "to_span_since_epoch" = to_span_since_epoch max_value_for_1us_rounding

let of_span_since_epoch = of_span_since_epoch

let%bench "of_span_since_epoch" = of_span_since_epoch Span.day

let to_int63_ns_since_epoch = to_int63_ns_since_epoch

let%bench "to_int63_ns_since_epoch" = to_int63_ns_since_epoch max_value_for_1us_rounding

let of_int63_ns_since_epoch = of_int63_ns_since_epoch

let%bench "of_int63_ns_since_epoch" = of_int63_ns_since_epoch int63_ten

let to_int_ns_since_epoch = to_int_ns_since_epoch

let%bench "to_int_ns_since_epoch" = to_int_ns_since_epoch max_value_for_1us_rounding

let of_int_ns_since_epoch = of_int_ns_since_epoch

let%bench "of_int_ns_since_epoch" = of_int_ns_since_epoch int_ten

(* Basically any function that uses zones (not just these) uses a zone cache too, so we're
   benchmarking the "hot-cache" cases here. See bench_zone.ml for benchmarks that always
   reset the cache first. *)
let of_date_ofday = of_date_ofday

let%bench "of_date_ofday (est)" =
  of_date_ofday ~zone:nyc nyc_winter_date Ofday.Ofday_constants.noon
;;

let%bench "of_date_ofday (edt)" =
  of_date_ofday ~zone:nyc nyc_summer_date Ofday.Ofday_constants.noon
;;

let%bench "of_date_ofday (e?t)" =
  of_date_ofday ~zone:nyc nyc_transition_date nyc_transition_ofday
;;

let%bench "of_date_ofday (n/a)" = of_date_ofday ~zone:nyc nyc_skip_date nyc_skip_ofday

let of_date_ofday_precise = of_date_ofday_precise

let%bench "of_date_ofday_precise (est)" =
  of_date_ofday_precise ~zone:nyc nyc_winter_date Ofday.Ofday_constants.noon
;;

let%bench "of_date_ofday_precise (edt)" =
  of_date_ofday_precise ~zone:nyc nyc_summer_date Ofday.Ofday_constants.noon
;;

let%bench "of_date_ofday_precise (e?t)" =
  of_date_ofday_precise ~zone:nyc nyc_transition_date nyc_transition_ofday
;;

let%bench "of_date_ofday_precise (n/a)" =
  of_date_ofday_precise ~zone:nyc nyc_skip_date nyc_skip_ofday
;;

(* These benchmarks use the date cache (see [reset_date_cache]). There are benchmarks
   that bypass the cache in bench_zone.ml. *)
let to_date_ofday = to_date_ofday

let%bench "to_date_ofday (est)" = to_date_ofday ~zone:nyc nyc_winter_time
let%bench "to_date_ofday (edt)" = to_date_ofday ~zone:nyc nyc_summer_time
let%bench "to_date_ofday (e?t)" = to_date_ofday ~zone:nyc nyc_transition_time

(* The following three functions build on [to_date_ofday] so have the same caching
   caveats. *)
let to_date_ofday_precise = to_date_ofday_precise

let%bench "to_date_ofday_precise (est)" = to_date_ofday_precise ~zone:nyc nyc_winter_time
let%bench "to_date_ofday_precise (edt)" = to_date_ofday_precise ~zone:nyc nyc_summer_time

let%bench "to_date_ofday_precise (e?t)" =
  to_date_ofday_precise ~zone:nyc nyc_transition_time
;;

let to_date = to_date

let%bench "to_date (utc)" = to_date epoch ~zone:Zone.utc
let%bench "to_date (w/ dst)" = to_date dst_t ~zone
let%bench "to_date (w/o dst)" = to_date no_dst_t ~zone

let to_ofday = to_ofday

let%bench "to_ofday (utc)" = to_ofday epoch ~zone:Zone.utc
let%bench "to_ofday (w/ dst)" = to_ofday dst_t ~zone
let%bench "to_ofday (w/o dst)" = to_ofday no_dst_t ~zone

let of_date_ofday_zoned = of_date_ofday_zoned

let%bench "of_date_ofday_zoned" =
  of_date_ofday_zoned date Ofday.Zoned.Ofday_zoned_constants.example
;;

let to_date_ofday_zoned = to_date_ofday_zoned

let%bench "to_date_ofday_zoned (est)" = to_date_ofday_zoned nyc_winter_time ~zone:nyc

let to_ofday_zoned = to_ofday_zoned

let%bench "to_ofday_zoned (est)" = to_ofday_zoned nyc_winter_time ~zone:nyc

(* This function is only meant for use in benchmarks: slowing it down doesn't matter in
   itself but seems important to know about for the interpretation of other benchmarks. *)
let reset_date_cache = reset_date_cache

let%bench "reset_date_cache" = reset_date_cache ()

let convert = convert

let%bench "convert" =
  convert ~from_tz:Zone.utc ~to_tz:Zone.utc date Ofday.Ofday_constants.example
;;

let utc_offset = utc_offset

let%bench "utc_offset" = utc_offset example ~zone:Zone.utc

let next_multiple = next_multiple

let%bench "next_multiple" =
  next_multiple ~base:min_value_for_1us_rounding ~after:epoch ~interval:Span.minute ()
;;

let prev_multiple = prev_multiple

let%bench "prev_multiple" =
  prev_multiple ~base:min_value_for_1us_rounding ~before:epoch ~interval:Span.minute ()
;;

let occurrence = occurrence

let%bench "occurrence" =
  occurrence `First_after_or_at dst_t ~ofday:Ofday.start_of_day ~zone
;;

let set_sexp_zone = set_sexp_zone

let%bench "set_sexp_zone" = set_sexp_zone zone_for_sexp

let get_sexp_zone = get_sexp_zone

let%bench "get_sexp_zone" = get_sexp_zone ()

let random = random

let%bench "random" = random ()

let sexp_of_t = sexp_of_t

let%bench "sexp_of_t (epoch)" = sexp_of_t epoch
let%bench "sexp_of_t (w/ dst)" = sexp_of_t dst_t
let%bench "sexp_of_t (w/o dst)" = sexp_of_t no_dst_t

let t_of_sexp = t_of_sexp

let%bench "t_of_sexp (epoch)" = t_of_sexp sexp_of_epoch
let%bench "t_of_sexp (w/ dst)" = t_of_sexp sexp_of_dst_t
let%bench "t_of_sexp (w/o dst)" = t_of_sexp sexp_of_no_dst_t

let sexp_of_t_abs = sexp_of_t_abs

let%bench "sexp_of_t_abs" = sexp_of_t_abs ~zone:Zone.utc example

let t_of_sexp_abs = t_of_sexp_abs

let%bench "t_of_sexp_abs" = t_of_sexp_abs example_sexp

let to_string = to_string

let%bench "to_string (epoch)" = to_string epoch
let%bench "to_string (w/ dst)" = to_string dst_t
let%bench "to_string (w/o dst)" = to_string no_dst_t

let to_string_utc = to_string_utc

let%bench "to_string_utc (epoch)" = to_string_utc epoch
let%bench "to_string_utc (w/ dst)" = to_string_utc dst_t
let%bench "to_string_utc (w/o dst)" = to_string_utc no_dst_t

let of_string = of_string

let%bench "of_string (epoch)" = of_string string_of_epoch
let%bench "of_string (w/ dst)" = of_string string_of_dst_t
let%bench "of_string (w/o dst)" = of_string string_of_no_dst_t

let of_string_with_utc_offset = of_string_with_utc_offset

let%bench "of_string_with_utc_offset (epoch)" = of_string_with_utc_offset string_of_epoch
let%bench "of_string_with_utc_offset (w/ dst)" = of_string_with_utc_offset string_of_dst_t

let%bench "of_string_with_utc_offset (w/o dst)" =
  of_string_with_utc_offset string_of_no_dst_t
;;

let to_sec_string = to_sec_string

let%bench "to_sec_string (utc)" = to_sec_string ~zone:Zone.utc epoch
let%bench "to_sec_string (w/ dst)" = to_sec_string ~zone dst_t
let%bench "to_sec_string (w/o dst)" = to_sec_string ~zone no_dst_t

let to_sec_string_with_zone = to_sec_string_with_zone

let%bench "to_sec_string_with_zone (utc)" = to_sec_string_with_zone ~zone:Zone.utc epoch
let%bench "to_sec_string_with_zone (w/ dst)" = to_sec_string_with_zone ~zone dst_t
let%bench "to_sec_string_with_zone (w/o dst)" = to_sec_string_with_zone ~zone no_dst_t

let of_localized_string = of_localized_string

let%bench "of_localized_string" =
  of_localized_string ~zone:Zone.utc example_localized_string
;;

let of_string_gen = of_string_gen

let%bench "of_string_gen" = of_string_gen ~if_no_timezone:`Local example_string

let to_filename_string = to_filename_string

let%bench "to_filename_string (utc)" = to_filename_string ~zone:Zone.utc epoch
let%bench "to_filename_string (w/ dst)" = to_filename_string ~zone dst_t
let%bench "to_filename_string (w/o dst)" = to_filename_string ~zone no_dst_t

let of_filename_string = of_filename_string

let%bench "of_filename_string (utc)" =
  of_filename_string ~zone:Zone.utc filename_string_of_epoch
;;

let%bench "of_filename_string (w/ dst)" =
  of_filename_string ~zone filename_string_of_dst_t
;;

let%bench "of_filename_string (w/o dst)" =
  of_filename_string ~zone filename_string_of_no_dst_t
;;

let to_string_trimmed = to_string_trimmed

let%bench "to_string_trimmed" = to_string_trimmed ~zone:Zone.utc example

let to_string_abs = to_string_abs

let%bench "to_string_abs (utc)" = to_string_abs ~zone:Zone.utc epoch
let%bench "to_string_abs (w/ dst)" = to_string_abs ~zone dst_t
let%bench "to_string_abs (w/o dst)" = to_string_abs ~zone no_dst_t

let of_string_abs = of_string_abs

let%bench "of_string_abs (utc)" = of_string_abs abs_string_of_epoch
let%bench "of_string_abs (w/ dst)" = of_string_abs abs_string_of_dst_t
let%bench "of_string_abs (w/o dst)" = of_string_abs abs_string_of_no_dst_t

let to_string_abs_trimmed = to_string_abs_trimmed

let%bench "to_string_abs_trimmed" = to_string_abs_trimmed ~zone:Zone.utc example

let to_string_abs_parts = to_string_abs_parts

let%bench "to_string_abs_parts" = to_string_abs_parts ~zone:Zone.utc example

let to_string_iso8601_basic = to_string_iso8601_basic

let%bench "to_string_iso8601_basic" = to_string_iso8601_basic ~zone:Zone.utc example

let to_string_fix_proto = to_string_fix_proto

let%bench "to_string_fix_proto (utc)" = to_string_fix_proto `Utc epoch
let%bench "to_string_fix_proto (w/ dst)" = to_string_fix_proto `Local dst_t
let%bench "to_string_fix_proto (w/o dst)" = to_string_fix_proto `Local no_dst_t

let of_string_fix_proto = of_string_fix_proto

let%bench "of_string_fix_proto (utc)" = of_string_fix_proto `Utc fix_utc_string_of_epoch

let%bench "of_string_fix_proto (w/ dst)" =
  of_string_fix_proto `Local fix_local_string_of_dst_t
;;

let%bench "of_string_fix_proto (w/o dst)" =
  of_string_fix_proto `Local fix_local_string_of_no_dst_t
;;

module Utc = struct
  open Utc

  let to_date_and_span_since_start_of_day = to_date_and_span_since_start_of_day

  let%bench "to_date_and_span_since_start_of_day" =
    to_date_and_span_since_start_of_day epoch
  ;;

  let of_date_and_span_since_start_of_day = of_date_and_span_since_start_of_day

  let%bench "of_date_and_span_since_start_of_day" =
    of_date_and_span_since_start_of_day date Span.Span_constants.pi_hours
  ;;
end

module Alternate_sexp = struct
  open Alternate_sexp
  module Set = Set
  module Map = Map

  type nonrec t = t [@@deriving sexp_grammar]
  type nonrec comparator_witness = comparator_witness

  let comparator = comparator
  let sexp_of_t = sexp_of_t

  let%bench "sexp_of_t" = sexp_of_t epoch

  let t_of_sexp = t_of_sexp

  let%bench "t_of_sexp" = t_of_sexp sexp_of_epoch

  let hash = hash

  let%bench "hash" = hash epoch

  let hash_fold_t = hash_fold_t

  let%bench "hash_fold_t" = hash_fold_t hash_state epoch

  let compare = compare

  let%bench "compare" = compare min_value_for_1us_rounding max_value_for_1us_rounding

  let ascending = ascending

  let%bench "ascending" = ascending min_value_for_1us_rounding max_value_for_1us_rounding

  let descending = descending

  let%bench "descending" =
    descending min_value_for_1us_rounding max_value_for_1us_rounding
  ;;

  let equal = equal

  let%bench "equal" = equal min_value_for_1us_rounding max_value_for_1us_rounding

  let min = min

  let%bench "min" = min min_value_for_1us_rounding max_value_for_1us_rounding

  let max = max

  let%bench "max" = max min_value_for_1us_rounding max_value_for_1us_rounding

  let ( = ) = ( = )

  let%bench "(=)" = min_value_for_1us_rounding = max_value_for_1us_rounding

  let ( < ) = ( < )

  let%bench "(<)" = min_value_for_1us_rounding < max_value_for_1us_rounding

  let ( > ) = ( > )

  let%bench "(>)" = min_value_for_1us_rounding > max_value_for_1us_rounding

  let ( <> ) = ( <> )

  let%bench "(<>)" = min_value_for_1us_rounding <> max_value_for_1us_rounding

  let ( <= ) = ( <= )

  let%bench "(<=)" = min_value_for_1us_rounding <= max_value_for_1us_rounding

  let ( >= ) = ( >= )

  let%bench "(>=)" = min_value_for_1us_rounding >= max_value_for_1us_rounding

  let between = between

  let%bench "between" =
    between ~low:min_value_for_1us_rounding ~high:max_value_for_1us_rounding epoch
  ;;

  let clamp_exn = clamp_exn

  let%bench "clamp_exn" =
    clamp_exn ~min:min_value_for_1us_rounding ~max:max_value_for_1us_rounding epoch
  ;;

  let clamp = clamp

  let%bench "clamp" =
    clamp ~min:min_value_for_1us_rounding ~max:max_value_for_1us_rounding epoch
  ;;

  let validate_ubound = validate_ubound

  let%bench "validate_ubound (success)" =
    (validate_ubound ~max:(Incl epoch) epoch : Validate.t)
  ;;

  let%bench "validate_ubound (failure)" =
    (validate_ubound ~max:(Excl epoch) epoch : Validate.t)
  ;;

  let validate_lbound = validate_lbound

  let%bench "validate_lbound (success)" =
    (validate_lbound ~min:(Incl epoch) epoch : Validate.t)
  ;;

  let%bench "validate_lbound (failure)" =
    (validate_lbound ~min:(Excl epoch) epoch : Validate.t)
  ;;

  let validate_bound = validate_bound

  let%bench "validate_bound (success)" =
    (validate_bound
       ~min:(Excl min_value_for_1us_rounding)
       ~max:(Excl max_value_for_1us_rounding)
       epoch
     : Validate.t)
  ;;

  let%bench "validate_bound (failure)" =
    (validate_bound
       ~min:(Excl min_value_for_1us_rounding)
       ~max:(Excl max_value_for_1us_rounding)
       min_value_for_1us_rounding
     : Validate.t)
  ;;

  module Replace_polymorphic_compare = struct
    open Replace_polymorphic_compare

    let compare = compare

    let%bench "compare" = compare min_value_for_1us_rounding max_value_for_1us_rounding

    let equal = equal

    let%bench "equal" = equal min_value_for_1us_rounding max_value_for_1us_rounding

    let min = min

    let%bench "min" = min min_value_for_1us_rounding max_value_for_1us_rounding

    let max = max

    let%bench "max" = max min_value_for_1us_rounding max_value_for_1us_rounding

    let ( = ) = ( = )

    let%bench "(=)" = min_value_for_1us_rounding = max_value_for_1us_rounding

    let ( < ) = ( < )

    let%bench "(<)" = min_value_for_1us_rounding < max_value_for_1us_rounding

    let ( > ) = ( > )

    let%bench "(>)" = min_value_for_1us_rounding > max_value_for_1us_rounding

    let ( <> ) = ( <> )

    let%bench "(<>)" = min_value_for_1us_rounding <> max_value_for_1us_rounding

    let ( <= ) = ( <= )

    let%bench "(<=)" = min_value_for_1us_rounding <= max_value_for_1us_rounding

    let ( >= ) = ( >= )

    let%bench "(>=)" = min_value_for_1us_rounding >= max_value_for_1us_rounding
  end
end

module Option = struct
  open Option
  module Map = Map
  module Set = Set
  module Table = Table
  module Hash_set = Hash_set
  module Hash_queue = Hash_queue

  type nonrec t = t [@@deriving bin_io, quickcheck, typerep]
  type nonrec comparator_witness = comparator_witness

  let comparator = comparator
  let hashable = hashable
  let pp = pp
  let none = opaque none

  module Time_option_constants = struct
    let some_epoch = opaque (some epoch)
    let some_dst_t = opaque (some dst_t)
    let some_no_dst_t = opaque (some no_dst_t)
    let some_epoch_option = opaque (Some epoch)
    let sexp_of_none = opaque (sexp_of_t none)
    let sexp_of_some_dst_t = opaque (sexp_of_t some_dst_t)
    let sexp_of_some_no_dst_t = opaque (sexp_of_t some_no_dst_t)
    let string_of_none = opaque (to_string none)
    let string_of_some_dst_t = opaque (to_string some_dst_t)
    let string_of_some_no_dst_t = opaque (to_string some_no_dst_t)
    let int63_of_none = opaque (Stable.V1.to_int63 none)
    let int63_of_some_epoch = opaque (Stable.V1.to_int63 some_epoch)
  end

  open Time_option_constants

  let hash = hash

  let%bench "hash" = hash some_epoch

  let hash_fold_t = hash_fold_t

  let%bench "hash_fold_t" = hash_fold_t hash_state some_epoch

  module Replace_polymorphic_compare = struct
    open Replace_polymorphic_compare

    let compare = compare

    let%bench "compare" = compare none some_epoch

    let equal = equal

    let%bench "equal" = equal none some_epoch

    let min = min

    let%bench "min" = min none some_epoch

    let max = max

    let%bench "max" = max none some_epoch

    let ( = ) = ( = )

    let%bench "(=)" = none = some_epoch

    let ( < ) = ( < )

    let%bench "(<)" = none < some_epoch

    let ( > ) = ( > )

    let%bench "(>)" = none > some_epoch

    let ( <> ) = ( <> )

    let%bench "(<>)" = none <> some_epoch

    let ( <= ) = ( <= )

    let%bench "(<=)" = none <= some_epoch

    let ( >= ) = ( >= )

    let%bench "(>=)" = none >= some_epoch
  end

  let compare = compare

  let%bench "compare" = compare none some_epoch

  let ascending = ascending

  let%bench "ascending" = ascending none some_epoch

  let descending = descending

  let%bench "descending" = descending none some_epoch

  let equal = equal

  let%bench "equal" = equal none some_epoch

  let min = min

  let%bench "min" = min none some_epoch

  let max = max

  let%bench "max" = max none some_epoch

  let ( = ) = ( = )

  let%bench "(=)" = none = some_epoch

  let ( < ) = ( < )

  let%bench "(<)" = none < some_epoch

  let ( > ) = ( > )

  let%bench "(>)" = none > some_epoch

  let ( <> ) = ( <> )

  let%bench "(<>)" = none <> some_epoch

  let ( <= ) = ( <= )

  let%bench "(<=)" = none <= some_epoch

  let ( >= ) = ( >= )

  let%bench "(>=)" = none >= some_epoch

  let between = between

  let%bench "between" = between ~low:none ~high:some_epoch some_dst_t

  let clamp_exn = clamp_exn

  let%bench "clamp_exn" = clamp_exn ~min:none ~max:some_epoch some_dst_t

  let clamp = clamp

  let%bench "clamp" = clamp ~min:none ~max:some_epoch some_dst_t

  let validate_ubound = validate_ubound

  let%bench "validate_ubound (success)" =
    (validate_ubound ~max:(Incl some_epoch) some_epoch : Validate.t)
  ;;

  let%bench "validate_ubound (failure)" =
    (validate_ubound ~max:(Excl some_epoch) some_epoch : Validate.t)
  ;;

  let validate_lbound = validate_lbound

  let%bench "validate_lbound (success)" =
    (validate_lbound ~min:(Incl some_epoch) some_epoch : Validate.t)
  ;;

  let%bench "validate_lbound (failure)" =
    (validate_lbound ~min:(Excl some_epoch) some_epoch : Validate.t)
  ;;

  let validate_bound = validate_bound

  let%bench "validate_bound (success)" =
    (validate_bound ~min:(Excl none) ~max:(Excl some_dst_t) some_epoch : Validate.t)
  ;;

  let%bench "validate_bound (failure)" =
    (validate_bound ~min:(Excl none) ~max:(Excl some_dst_t) none : Validate.t)
  ;;

  let some = some

  let%bench "some" = some epoch

  let is_none = is_none

  let%bench "is_none" = is_none some_epoch

  let is_some = is_some

  let%bench "is_some" = is_some some_epoch

  let value_exn = value_exn

  let%bench "value_exn" = value_exn some_epoch

  let unchecked_value = unchecked_value

  let%bench "unchecked_value" = unchecked_value some_epoch

  let value = value

  let%bench "value (none)" = value none ~default:min_value_for_1us_rounding
  let%bench "value (some)" = value some_epoch ~default:min_value_for_1us_rounding

  let some_is_representable = some_is_representable

  let%bench "some_is_representable" = some_is_representable epoch

  let of_option = of_option

  let%bench "of_option" = of_option some_epoch_option

  let to_option = to_option

  let%bench "to_option" = to_option some_epoch

  let sexp_of_t = sexp_of_t

  let%bench "sexp_of_t (none)" = sexp_of_t none
  let%bench "sexp_of_t (w/ dst)" = sexp_of_t some_dst_t
  let%bench "sexp_of_t (w/o dst)" = sexp_of_t some_no_dst_t

  let t_of_sexp = t_of_sexp

  let%bench "t_of_sexp (none)" = t_of_sexp sexp_of_none
  let%bench "t_of_sexp (w/ dst)" = t_of_sexp sexp_of_some_dst_t
  let%bench "t_of_sexp (w/o dst)" = t_of_sexp sexp_of_some_no_dst_t

  let to_string = to_string

  let%bench "to_string (none)" = to_string none
  let%bench "to_string (w/ dst)" = to_string some_dst_t
  let%bench "to_string (w/o dst)" = to_string some_no_dst_t

  let of_string = of_string

  let%bench "of_string (none)" = of_string string_of_none
  let%bench "of_string (w/ dst)" = of_string string_of_some_dst_t
  let%bench "of_string (w/o dst)" = of_string string_of_some_no_dst_t

  module Optional_syntax = struct
    open Optional_syntax

    module Optional_syntax = struct
      open Optional_syntax

      let is_none = is_none

      let%bench "is_none" = is_none some_epoch

      let unsafe_value = unsafe_value

      let%bench "unsafe_value" = unsafe_value some_epoch
    end
  end

  module Stable = struct
    open Stable

    module V1 = struct
      open V1

      type nonrec t = t [@@deriving bin_io]
      type nonrec comparator_witness = comparator_witness

      let comparator = comparator
      let compare = compare

      let%bench "compare" = compare none some_epoch

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t (none)" = sexp_of_t none
      let%bench "sexp_of_t (w/ dst)" = sexp_of_t some_dst_t
      let%bench "sexp_of_t (w/o dst)" = sexp_of_t some_no_dst_t

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp (none)" = t_of_sexp sexp_of_none
      let%bench "t_of_sexp (w/ dst)" = t_of_sexp sexp_of_some_dst_t
      let%bench "t_of_sexp (w/o dst)" = t_of_sexp sexp_of_some_no_dst_t

      let to_int63 = to_int63

      let%bench "to_int63 (none)" = to_int63 none
      let%bench "to_int63 (some)" = to_int63 some_epoch

      let of_int63_exn = of_int63_exn

      let%bench "of_int63_exn (none)" = of_int63_exn int63_of_none
      let%bench "of_int63_exn (some)" = of_int63_exn int63_of_some_epoch
    end
  end
end

module Stable = struct
  module V1 = struct
    open Stable.V1
    module Map = Map
    module Set = Set

    type nonrec t = t [@@deriving bin_io]
    type nonrec comparator_witness = comparator_witness

    let comparator = comparator
    let compare = compare

    let%bench "compare" = compare min_value_for_1us_rounding max_value_for_1us_rounding

    let sexp_of_t = sexp_of_t

    let%bench "sexp_of_t (w/ dst)" = sexp_of_t dst_t
    let%bench "sexp_of_t (w/o dst)" = sexp_of_t no_dst_t

    let t_of_sexp = t_of_sexp

    let%bench "t_of_sexp (w/ dst)" = t_of_sexp sexp_of_dst_t
    let%bench "t_of_sexp (w/o dst)" = t_of_sexp sexp_of_no_dst_t

    let to_int63 = to_int63

    let%bench "to_int63" = to_int63 epoch

    let of_int63_exn = of_int63_exn

    let%bench "of_int63_exn" = of_int63_exn int63_of_epoch
  end

  (* There's no particularly good reason to use [Alternate_sexp] from [Core.Time_ns]. *)
  module Alternate_sexp = Time_ns.Stable.Alternate_sexp

  module Option = struct
    module V1 = struct
      open Option
      open Time_ns.Stable.Option.V1
      open Time_option_constants

      type nonrec t = t [@@deriving bin_io]
      type nonrec comparator_witness = comparator_witness

      let comparator = comparator
      let compare = compare

      let%bench "compare" = compare none some_epoch

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t (none)" = sexp_of_t none
      let%bench "sexp_of_t (w/ dst)" = sexp_of_t some_dst_t
      let%bench "sexp_of_t (w/o dst)" = sexp_of_t some_no_dst_t

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp (none)" = t_of_sexp sexp_of_none
      let%bench "t_of_sexp (w/ dst)" = t_of_sexp sexp_of_some_dst_t
      let%bench "t_of_sexp (w/o dst)" = t_of_sexp sexp_of_some_no_dst_t

      let to_int63 = to_int63

      let%bench "to_int63 (none)" = to_int63 none
      let%bench "to_int63 (some)" = to_int63 some_epoch

      let of_int63_exn = of_int63_exn

      let%bench "of_int63_exn (none)" = of_int63_exn int63_of_none
      let%bench "of_int63_exn (some)" = of_int63_exn int63_of_some_epoch
    end
  end

  module Span = struct
    module V1 = struct
      open Span
      open Time_ns.Stable.Span.V1
      open Span_constants

      type nonrec t = t [@@deriving bin_io, hash]
      type nonrec comparator_witness = comparator_witness

      let comparator = comparator
      let compare = compare

      let%bench "compare" = compare min_value_for_1us_rounding max_value_for_1us_rounding

      let equal = equal

      let%bench "equal" = equal zero zero

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t (s)" = sexp_of_t second
      let%bench "sexp_of_t (ns+)" = sexp_of_t pi_nanoseconds
      let%bench "sexp_of_t (us+)" = sexp_of_t pi_microseconds
      let%bench "sexp_of_t (ms+)" = sexp_of_t pi_milliseconds
      let%bench "sexp_of_t (s+)" = sexp_of_t pi_seconds
      let%bench "sexp_of_t (m+)" = sexp_of_t pi_minutes
      let%bench "sexp_of_t (h+)" = sexp_of_t pi_hours
      let%bench "sexp_of_t (d+)" = sexp_of_t pi_days
      let%bench "sexp_of_t (kd+)" = sexp_of_t pi_kilodays

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp (s)" = t_of_sexp v1_sexp_of_second
      let%bench "t_of_sexp (ns+)" = t_of_sexp v1_sexp_of_pi_nanoseconds
      let%bench "t_of_sexp (us+)" = t_of_sexp v1_sexp_of_pi_microseconds
      let%bench "t_of_sexp (ms+)" = t_of_sexp v1_sexp_of_pi_milliseconds
      let%bench "t_of_sexp (s+)" = t_of_sexp v1_sexp_of_pi_seconds
      let%bench "t_of_sexp (m+)" = t_of_sexp v1_sexp_of_pi_minutes
      let%bench "t_of_sexp (h+)" = t_of_sexp v1_sexp_of_pi_hours
      let%bench "t_of_sexp (d+)" = t_of_sexp v1_sexp_of_pi_days
      let%bench "t_of_sexp (kd+)" = t_of_sexp v1_sexp_of_pi_kilodays
      let%bench "t_of_sexp (d.)" = t_of_sexp sexp_of_decimal_pi_days

      let to_int63 = to_int63

      let%bench "to_int63" = to_int63 day

      let of_int63_exn = of_int63_exn

      let%bench "of_int63_exn" = of_int63_exn int63_ten
    end

    module V2 = struct
      open Span
      open Time_ns.Stable.Span.V2
      open Span_constants
      module Map = Map
      module Set = Set

      type nonrec t = t [@@deriving bin_io, hash]
      type nonrec comparator_witness = comparator_witness

      let comparator = comparator
      let compare = compare

      let%bench "compare" = compare min_value_for_1us_rounding max_value_for_1us_rounding

      let equal = equal

      let%bench "equal" = compare zero zero

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t (s)" = sexp_of_t second
      let%bench "sexp_of_t (kd+)" = sexp_of_t pi_kilodays

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp (s)" = t_of_sexp sexp_of_second
      let%bench "t_of_sexp (kd+)" = t_of_sexp sexp_of_pi_kilodays
      let%bench "t_of_sexp (d.)" = t_of_sexp sexp_of_decimal_pi_days

      let to_string = to_string

      let%bench "to_string (s)" = to_string second
      let%bench "to_string (kd+)" = to_string pi_kilodays

      let of_string = of_string

      let%bench "of_string (s)" = of_string string_of_second
      let%bench "of_string (kd+)" = of_string string_of_pi_kilodays
      let%bench "of_string (d.)" = of_string string_of_decimal_pi_days

      let to_int63 = to_int63

      let%bench "to_int63" = to_int63 day

      let of_int63_exn = of_int63_exn

      let%bench "of_int63_exn" = of_int63_exn int63_ten
    end

    module Option = struct
      module V1 = struct
        open Span.Option
        open Time_ns.Stable.Span.Option.V1
        open Span_option_constants

        type nonrec t = t [@@deriving bin_io]
        type nonrec comparator_witness = comparator_witness

        let comparator = comparator
        let compare = compare

        let%bench "compare" = compare none some_day

        let t_of_sexp = t_of_sexp

        let%bench "t_of_sexp (none)" = t_of_sexp v1_sexp_of_none
        let%bench "t_of_sexp (some)" = t_of_sexp v1_sexp_of_some_day

        let sexp_of_t = sexp_of_t

        let%bench "sexp_of_t (none)" = sexp_of_t none
        let%bench "sexp_of_t (some)" = sexp_of_t some_day

        let to_int63 = to_int63

        let%bench "to_int63 (none)" = to_int63 none
        let%bench "to_int63 (some)" = to_int63 some_day

        let of_int63_exn = of_int63_exn

        let%bench "of_int63_exn (none)" = of_int63_exn int63_of_none
        let%bench "of_int63_exn (some)" = of_int63_exn int63_of_some_day
      end

      module V2 = struct
        open Span.Option
        open Time_ns.Stable.Span.Option.V2
        open Span_option_constants

        type nonrec t = t [@@deriving bin_io]
        type nonrec comparator_witness = comparator_witness

        let comparator = comparator
        let compare = compare

        let%bench "compare" = compare none some_day

        let t_of_sexp = t_of_sexp

        let%bench "t_of_sexp (none)" = t_of_sexp sexp_of_none
        let%bench "t_of_sexp (some)" = t_of_sexp sexp_of_some_day

        let sexp_of_t = sexp_of_t

        let%bench "sexp_of_t (none)" = sexp_of_t none
        let%bench "sexp_of_t (some)" = sexp_of_t some_day

        let to_int63 = to_int63

        let%bench "to_int63 (none)" = to_int63 none
        let%bench "to_int63 (some)" = to_int63 some_day

        let of_int63_exn = of_int63_exn

        let%bench "of_int63_exn (none)" = of_int63_exn int63_of_none
        let%bench "of_int63_exn (some)" = of_int63_exn int63_of_some_day
      end
    end
  end

  module Ofday = struct
    module V1 = struct
      open Ofday
      open Time_ns.Stable.Ofday.V1
      open Ofday_constants

      type nonrec t = t [@@deriving bin_io]
      type nonrec comparator_witness = comparator_witness

      let comparator = comparator
      let compare = compare

      let%bench "compare" = compare start_of_day evening

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t (midnight)" = sexp_of_t start_of_day
      let%bench "sexp_of_t (morning)" = sexp_of_t morning
      let%bench "sexp_of_t (evening)" = sexp_of_t evening

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp (midnight)" = t_of_sexp sexp_of_start_of_day
      let%bench "t_of_sexp (morning)" = t_of_sexp sexp_of_morning
      let%bench "t_of_sexp (evening)" = t_of_sexp sexp_of_evening

      let to_int63 = to_int63

      let%bench "to_int63" = to_int63 start_of_day

      let of_int63_exn = of_int63_exn

      let%bench "of_int63_exn" = of_int63_exn int63_of_start_of_day
    end

    module Zoned = struct
      module V1 = struct
        open Ofday.Zoned
        open Time_ns.Stable.Ofday.Zoned.V1
        open Ofday_zoned_constants

        type nonrec t = t [@@deriving bin_io]

        let compare = compare

        let%bench "compare (<)" = compare zero_utc example
        let%bench "compare (>)" = compare example zero_utc
        let%bench "compare (=)" = compare zero_utc zero_utc

        let hash = hash

        let%bench "hash" = hash zero_utc

        let hash_fold_t = hash_fold_t

        let%bench "hash_fold_t" = hash_fold_t hash_state zero_utc

        let sexp_of_t = sexp_of_t

        let%bench "sexp_of_t" = sexp_of_t example

        let t_of_sexp = t_of_sexp

        let%bench "t_of_sexp" = t_of_sexp example_sexp
      end
    end

    module Option = struct
      module V1 = struct
        open Ofday.Option
        open Time_ns.Stable.Ofday.Option.V1
        open Ofday_option_constants

        type nonrec t = t [@@deriving bin_io]
        type nonrec comparator_witness = comparator_witness

        let comparator = comparator
        let compare = compare

        let%bench "compare" = compare none some_start_of_day

        let sexp_of_t = sexp_of_t

        let%bench "sexp_of_t (none)" = sexp_of_t none
        let%bench "sexp_of_t (some)" = sexp_of_t some_start_of_day

        let t_of_sexp = t_of_sexp

        let%bench "t_of_sexp (none)" = t_of_sexp sexp_of_none
        let%bench "t_of_sexp (some)" = t_of_sexp sexp_of_some_start_of_day

        let to_int63 = to_int63

        let%bench "to_int63 (none)" = to_int63 none
        let%bench "to_int63 (some)" = to_int63 some_start_of_day

        let of_int63_exn = of_int63_exn

        let%bench "of_int63_exn (none)" = of_int63_exn int63_of_none
        let%bench "of_int63_exn (some)" = of_int63_exn int63_of_some_start_of_day
      end
    end
  end
end
