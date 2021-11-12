open! Core
module Unix = Core_unix
module Time = Time_unix

module Constants = struct
  (* We wrap all constants with [Sys.opaque_identity] so that callsites cannot specialize
     code to their values. *)
  let opaque = Sys.opaque_identity
  let int_zero = opaque 0
  let int32_zero = opaque 0l
  let int63_zero = opaque Int63.zero
  let float_pi = opaque Float.pi
  let percent_half = opaque (Percent.of_mult 0.5)
  let date = opaque (Date.create_exn ~y:2013 ~m:Oct ~d:07)
  let hash_state = opaque (Hash.create ())
  let utc = opaque Time.Zone.utc
  let nyc = opaque (Time.Zone.find_exn "America/New_York")

  (* We define some dates and times with different daylight savings properties for
     purposes of benchmarking time zone logic. *)

  let nyc_winter_date = opaque (Date.create_exn ~y:2013 ~m:Jan ~d:03)
  let nyc_summer_date = opaque (Date.create_exn ~y:2014 ~m:Jul ~d:04)
  let noon = opaque (Time.Ofday.create ~hr:12 ())
  let nyc_transition_date = opaque (Date.create_exn ~y:2015 ~m:Nov ~d:01)
  let nyc_transition_ofday = opaque (Time.Ofday.create ~hr:01 ~min:30 ())
  let nyc_skip_date = opaque (Date.create_exn ~y:2015 ~m:Mar ~d:08)
  let nyc_skip_ofday = opaque (Time.Ofday.create ~hr:02 ~min:30 ())
  let nyc_winter_time = opaque (Time.of_date_ofday ~zone:nyc nyc_winter_date noon)
  let nyc_summer_time = opaque (Time.of_date_ofday ~zone:nyc nyc_summer_date noon)

  let nyc_transition_time =
    opaque (Time.of_date_ofday ~zone:nyc nyc_transition_date nyc_transition_ofday)
  ;;

  let nyc_skip_time = opaque (Time.of_date_ofday ~zone:nyc nyc_skip_date nyc_skip_ofday)

  let%test_module "daylight savings" =
    (module struct
      type precise =
        Date.t
        * Time.Ofday.t
        * [ `Also_at of Time.t | `Also_skipped of Date.t * Time.Ofday.t | `Only ]
      [@@deriving sexp_of]

      let test_time time =
        printf !"%{sexp: Time.t}\n" time;
        printf !"%{sexp: precise}\n" (Time.to_date_ofday_precise ~zone:nyc time)
      ;;

      let%expect_test "winter" =
        test_time nyc_winter_time;
        [%expect
          {|
          (2013-01-03 12:00:00.000000-05:00)
          (2013-01-03 12:00:00.000000 Only) |}]
      ;;

      let%expect_test "summer" =
        test_time nyc_summer_time;
        [%expect
          {|
          (2014-07-04 12:00:00.000000-04:00)
          (2014-07-04 12:00:00.000000 Only) |}]
      ;;

      let%expect_test "transition" =
        test_time nyc_transition_time;
        [%expect
          {|
          (2015-11-01 01:30:00.000000-05:00)
          (2015-11-01 01:30:00.000000 (Also_at (2015-11-01 01:30:00.000000-04:00))) |}]
      ;;

      let%expect_test "skip" =
        test_time nyc_skip_time;
        [%expect
          {|
          (2015-03-08 03:30:00.000000-04:00)
          (2015-03-08 03:30:00.000000 (Also_skipped (2015-03-08 02:30:00.000000))) |}]
      ;;
    end)
  ;;
end

open Constants

(* Below this point we [open] the modules in [Time], alias their definitions, and
   benchmark each function after its alias. This ensures we benchmark (or explicitly
   ignore) every function in the public API. We make a few exceptions like submodules from
   the [Identifiable] functor whose code is not in [Time] itself, but for individual
   functions it's often as easy to write a benchmark as it is to write a comment
   justifying the lack of benchmark, so we don't insist that every benchmark is obviously
   valuable in order for it to be included. *)
(* Many of these benchmarks duplicate the corresponding benchmarks in Core -- this is
   intentional, because we want to ensure that whatever mechanism we use to share code
   between them doesn't differentially slow one of them down. *)
open Time

module Span = struct
  open Span
  module Parts = Parts
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
  let zero = opaque zero
  let nanosecond = opaque nanosecond
  let microsecond = opaque microsecond
  let millisecond = opaque millisecond
  let second = opaque second
  let minute = opaque minute
  let hour = opaque hour
  let day = opaque day
  let robust_comparison_tolerance = opaque robust_comparison_tolerance

  module Span_constants = struct
    let day_sexp = sexp_of_t day |> opaque
    let day_string = to_string day |> opaque
  end

  open Span_constants

  module Replace_polymorphic_compare = struct
    open Replace_polymorphic_compare

    let compare = compare

    let%bench "compare (<)" = compare zero day
    let%bench "compare (>)" = compare day zero
    let%bench "compare (=)" = compare zero zero

    let equal = equal

    let%bench "equal" = equal zero day

    let min = min

    let%bench "min" = min zero day

    let max = max

    let%bench "max" = max zero day

    let ( = ) = ( = )

    let%bench "(=)" = zero = day

    let ( < ) = ( < )

    let%bench "(<)" = zero < day

    let ( > ) = ( > )

    let%bench "(>)" = zero > day

    let ( <= ) = ( <= )

    let%bench "(<=)" = zero <= day

    let ( >= ) = ( >= )

    let%bench "(>=)" = zero >= day

    let ( <> ) = ( <> )

    let%bench "(<>)" = zero <> day
  end

  let compare = compare

  let%bench "compare (<)" = compare zero day
  let%bench "compare (>)" = compare day zero
  let%bench "compare (=)" = compare zero zero

  let ascending = ascending

  let%bench "ascending" = ascending zero day

  let descending = descending

  let%bench "descending" = descending zero day

  let equal = equal

  let%bench "equal" = equal zero day

  let min = min

  let%bench "min" = min zero day

  let max = max

  let%bench "max" = max zero day

  let ( = ) = ( = )

  let%bench "(=)" = zero = day

  let ( < ) = ( < )

  let%bench "(<)" = zero < day

  let ( > ) = ( > )

  let%bench "(>)" = zero > day

  let ( <= ) = ( <= )

  let%bench "(<=)" = zero <= day

  let ( >= ) = ( >= )

  let%bench "(>=)" = zero >= day

  let ( <> ) = ( <> )

  let%bench "(<>)" = zero <> day

  let between = between

  let%bench "between" = between zero ~low:zero ~high:day

  let clamp = clamp

  let%bench "clamp" = clamp zero ~min:zero ~max:day

  let clamp_exn = clamp_exn

  let%bench "clamp_exn" = clamp_exn zero ~min:zero ~max:day

  let validate_bound = validate_bound

  let%bench "validate_bound" = validate_bound ~min:(Incl zero) ~max:(Incl day) zero

  let validate_lbound = validate_lbound

  let%bench "validate_lbound" = validate_lbound ~min:(Incl zero) zero

  let validate_ubound = validate_ubound

  let%bench "validate_ubound" = validate_ubound ~max:(Incl day) zero

  let validate_positive = validate_positive

  let%bench "validate_positive" = validate_positive zero

  let validate_negative = validate_negative

  let%bench "validate_negative" = validate_negative zero

  let validate_non_positive = validate_non_positive

  let%bench "validate_non_positive" = validate_non_positive zero

  let validate_non_negative = validate_non_negative

  let%bench "validate_non_negative" = validate_non_negative zero

  let is_positive = is_positive

  let%bench "is_positive" = is_positive zero

  let is_negative = is_negative

  let%bench "is_negative" = is_negative zero

  let is_non_positive = is_non_positive

  let%bench "is_non_positive" = is_non_positive zero

  let is_non_negative = is_non_negative

  let%bench "is_non_negative" = is_non_negative zero

  let sign = sign

  let%bench "sign" = sign zero

  let hash = hash

  let%bench "hash" = hash zero

  let hash_fold_t = hash_fold_t

  let%bench "hash_fold_t" = hash_fold_t hash_state zero

  let robustly_compare = robustly_compare

  let%bench "robustly_compare (<)" = robustly_compare zero day
  let%bench "robustly_compare (>)" = robustly_compare day zero
  let%bench "robustly_compare (=)" = robustly_compare zero zero

  let ( =. ) = ( =. )

  let%bench "(=.)" = zero =. day

  let ( <. ) = ( <. )

  let%bench "(<.)" = zero <. day

  let ( >. ) = ( >. )

  let%bench "(>.)" = zero >. day

  let ( <=. ) = ( <=. )

  let%bench "(<=.)" = zero <=. day

  let ( >=. ) = ( >=. )

  let%bench "(>=.)" = zero >=. day

  let ( <>. ) = ( <>. )

  let%bench "(<>.)" = zero <>. day

  let create = create

  let%bench "create" = create ~sign:Pos ~day:1 ~hr:1 ~min:1 ~sec:1 ~ms:1 ~us:1 ()

  let to_parts = to_parts

  let%bench "to_parts" = to_parts day

  let of_ns = of_ns

  let%bench "of_ns" = of_ns float_pi

  let of_us = of_us

  let%bench "of_us" = of_us float_pi

  let of_ms = of_ms

  let%bench "of_ms" = of_ms float_pi

  let of_sec = of_sec

  let%bench "of_sec" = of_sec float_pi

  let of_min = of_min

  let%bench "of_min" = of_min float_pi

  let of_hr = of_hr

  let%bench "of_hr" = of_hr float_pi

  let of_day = of_day

  let%bench "of_day" = of_day float_pi

  let of_int_sec = of_int_sec

  let%bench "of_int_sec" = of_int_sec int_zero

  let of_int32_seconds = of_int32_seconds

  let%bench "of_int32_seconds" = of_int32_seconds int32_zero

  let of_int63_seconds = of_int63_seconds

  let%bench "of_int63_seconds" = of_int63_seconds int63_zero

  let to_ns = to_ns

  let%bench "to_ns" = to_ns day

  let to_us = to_us

  let%bench "to_us" = to_us day

  let to_ms = to_ms

  let%bench "to_ms" = to_ms day

  let to_sec = to_sec

  let%bench "to_sec" = to_sec day

  let to_min = to_min

  let%bench "to_min" = to_min day

  let to_hr = to_hr

  let%bench "to_hr" = to_hr day

  let to_day = to_day

  let%bench "to_day" = to_day day

  let to_int63_seconds_round_down_exn = to_int63_seconds_round_down_exn

  let%bench "to_int63_seconds_round_down_exn" = to_int63_seconds_round_down_exn day

  let to_proportional_float = to_proportional_float

  let%bench "to_proportional_float" = to_proportional_float day

  let ( + ) = ( + )

  let%bench "(+)" = day + hour

  let ( - ) = ( - )

  let%bench "(-)" = day - hour

  let abs = abs

  let%bench "abs" = abs day

  let neg = neg

  let%bench "neg" = neg day

  let scale = scale

  let%bench "scale" = scale day float_pi

  let ( / ) = ( / )

  let%bench "(/)" = day / float_pi

  let ( // ) = ( // )

  let%bench "(//)" = day // hour

  let next = next

  let%bench "next" = next minute

  let prev = prev

  let%bench "prev" = prev minute

  let to_unit_of_time = to_unit_of_time

  let%bench "to_unit_of_time" = to_unit_of_time day

  let of_unit_of_time = of_unit_of_time

  let%bench "of_unit_of_time" = of_unit_of_time Day

  let randomize = randomize

  let%bench "randomize" = randomize day ~percent:percent_half

  let sexp_of_t = sexp_of_t

  let%bench "sexp_of_t" = sexp_of_t day

  let t_of_sexp = t_of_sexp

  let%bench "t_of_sexp" = t_of_sexp day_sexp

  let to_string = to_string

  let%bench "to_string" = to_string day

  let of_string = of_string

  let%bench "of_string" = of_string day_string

  let to_short_string = to_short_string

  let%bench "to_short_string" = to_short_string day

  let to_string_hum = to_string_hum

  let%bench "to_string_hum" = to_string_hum day
end

module Ofday = struct
  open Ofday
  module Map = Map
  module Set = Set
  module Table = Table
  module Hash_set = Hash_set
  module Hash_queue = Hash_queue

  type nonrec t = t [@@deriving bin_io, quickcheck, sexp_grammar, typerep]
  type nonrec comparator_witness = comparator_witness

  let arg_type = arg_type
  let comparator = comparator
  let hashable = hashable
  let pp = pp
  let gen_incl = gen_incl
  let gen_uniform_incl = gen_uniform_incl
  let start_of_day = opaque start_of_day
  let start_of_next_day = opaque start_of_next_day
  let approximate_end_of_day = opaque approximate_end_of_day

  module Ofday_constants = struct
    let example = create ~hr:13 ~min:29 ~sec:59 ~ms:654 ~us:321 () |> opaque
    let example_sexp = sexp_of_t example |> opaque
    let example_string = to_string example |> opaque
  end

  open Ofday_constants

  module Replace_polymorphic_compare = struct
    open Replace_polymorphic_compare

    let compare = compare

    let%bench "compare (<)" = compare start_of_day example
    let%bench "compare (>)" = compare example start_of_day
    let%bench "compare (=)" = compare start_of_day start_of_day

    let equal = equal

    let%bench "equal" = equal start_of_day example

    let min = min

    let%bench "min" = min start_of_day example

    let max = max

    let%bench "max" = max start_of_day example

    let ( = ) = ( = )

    let%bench "(=)" = start_of_day = example

    let ( < ) = ( < )

    let%bench "(<)" = start_of_day < example

    let ( > ) = ( > )

    let%bench "(>)" = start_of_day > example

    let ( <= ) = ( <= )

    let%bench "(<=)" = start_of_day <= example

    let ( >= ) = ( >= )

    let%bench "(>=)" = start_of_day >= example

    let ( <> ) = ( <> )

    let%bench "(<>)" = start_of_day <> example
  end

  let compare = compare

  let%bench "compare (<)" = compare start_of_day example
  let%bench "compare (>)" = compare example start_of_day
  let%bench "compare (=)" = compare start_of_day start_of_day

  let ascending = ascending

  let%bench "ascending" = ascending start_of_day example

  let descending = descending

  let%bench "descending" = descending start_of_day example

  let equal = equal

  let%bench "equal" = equal start_of_day example

  let min = min

  let%bench "min" = min start_of_day example

  let max = max

  let%bench "max" = max start_of_day example

  let ( = ) = ( = )

  let%bench "(=)" = start_of_day = example

  let ( < ) = ( < )

  let%bench "(<)" = start_of_day < example

  let ( > ) = ( > )

  let%bench "(>)" = start_of_day > example

  let ( <= ) = ( <= )

  let%bench "(<=)" = start_of_day <= example

  let ( >= ) = ( >= )

  let%bench "(>=)" = start_of_day >= example

  let ( <> ) = ( <> )

  let%bench "(<>)" = start_of_day <> example

  let between = between

  let%bench "between" = between start_of_day ~low:start_of_day ~high:example

  let clamp = clamp

  let%bench "clamp" = clamp start_of_day ~min:start_of_day ~max:example

  let clamp_exn = clamp_exn

  let%bench "clamp_exn" = clamp_exn start_of_day ~min:start_of_day ~max:example

  let validate_bound = validate_bound

  let%bench "validate_bound" =
    validate_bound ~min:(Incl start_of_day) ~max:(Incl example) start_of_day
  ;;

  let validate_lbound = validate_lbound

  let%bench "validate_lbound" = validate_lbound ~min:(Incl start_of_day) start_of_day

  let validate_ubound = validate_ubound

  let%bench "validate_ubound" = validate_ubound ~max:(Incl example) start_of_day

  let hash = hash

  let%bench "hash" = hash start_of_day

  let hash_fold_t = hash_fold_t

  let%bench "hash_fold_t" = hash_fold_t hash_state start_of_day

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

  let%bench "create" = create ~hr:13 ~min:29 ~sec:59 ~ms:654 ~us:321 ()

  let to_parts = to_parts

  let%bench "to_parts" = to_parts example

  let to_span_since_start_of_day = to_span_since_start_of_day

  let%bench "to_span_since_start_of_day" = to_span_since_start_of_day example

  let of_span_since_start_of_day_exn = of_span_since_start_of_day_exn
  let of_span_since_start_of_day = of_span_since_start_of_day_exn

  let%bench "of_span_since_start_of_day_exn" = of_span_since_start_of_day_exn Span.hour

  let of_span_since_start_of_day_unchecked = of_span_since_start_of_day_unchecked

  let%bench "of_span_since_start_of_day_unchecked" =
    of_span_since_start_of_day_unchecked Span.hour
  ;;

  let span_since_start_of_day_is_valid = span_since_start_of_day_is_valid

  let%bench "span_since_start_of_day_is_valid" =
    span_since_start_of_day_is_valid Span.hour
  ;;

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

  let sexp_of_t = sexp_of_t

  let%bench "sexp_of_t" = sexp_of_t example

  let t_of_sexp = t_of_sexp

  let%bench "t_of_sexp" = t_of_sexp example_sexp

  let to_string = to_string

  let%bench "to_string" = to_string example

  let of_string = of_string

  let%bench "of_string" = of_string example_string

  let to_string_trimmed = to_string_trimmed

  let%bench "to_string_trimmed" = to_string_trimmed example

  let to_sec_string = to_sec_string

  let%bench "to_sec_string" = to_sec_string example

  let to_millisecond_string = to_millisecond_string
  let to_millisec_string = to_millisecond_string

  let%bench "to_millisecond_string" = to_millisecond_string example

  let of_string_iso8601_extended = of_string_iso8601_extended

  let%bench "of_string_iso8601_extended" = of_string_iso8601_extended example_string

  let now = now

  let%bench "now" = now ~zone:utc

  module Zoned = struct
    open Zoned

    type nonrec t = t [@@deriving bin_io]

    let arg_type = arg_type
    let pp = pp

    module Ofday_zoned_constants = struct
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

      let%bench "compare (=)" = equal example example
      let%bench "compare (<>)" = equal zero_utc example

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

    let to_time = to_time

    let%bench "to_time" = to_time example date

    let sexp_of_t = sexp_of_t

    let%bench "sexp_of_t" = sexp_of_t example

    let t_of_sexp = t_of_sexp

    let%bench "t_of_sexp" = t_of_sexp example_sexp

    let to_string = to_string

    let%bench "to_string" = to_string example

    let of_string = of_string

    let%bench "of_string" = of_string example_string
  end
end

module Map = Map
module Set = Set
module Table = Table
module Hash_set = Hash_set
module Hash_queue = Hash_queue
module Exposed_for_tests = Exposed_for_tests

type nonrec underlying = underlying
type nonrec t = t [@@deriving bin_io, quickcheck, typerep]
type nonrec comparator_witness = comparator_witness

let arg_type = arg_type
let comparator = comparator
let hashable = hashable
let pp = pp
let gen_incl = gen_incl
let gen_uniform_incl = gen_uniform_incl
let epoch = opaque epoch

module Time_constants = struct
  let example = of_date_ofday ~zone:utc date Ofday.Ofday_constants.example |> opaque
  let example_sexp = sexp_of_t example |> opaque
  let example_string = to_string example |> opaque
  let fix_example_string = to_string_fix_proto `Utc example |> opaque
  let example_filename_string = to_filename_string ~zone:utc example |> opaque
  let example_localized_string = format example "%F %T" ~zone:utc |> opaque
  let example_formatted = format example "%F %T%z" ~zone:utc |> opaque
  let zone_for_sexp = get_sexp_zone () |> opaque
  let epoch_tm = Unix.gmtime 0. |> opaque
  let index_example = Zone.index nyc example |> opaque
end

open Time_constants

(* don't benchmark pauses *)
let pause = pause
let interruptible_pause = interruptible_pause
let pause_forever = pause_forever
let hash = hash

let%bench "hash" = hash epoch

let hash_fold_t = hash_fold_t

let%bench "hash_fold_t" = hash_fold_t hash_state epoch

module Replace_polymorphic_compare = struct
  open Replace_polymorphic_compare

  let compare = compare

  let%bench "compare (<)" = compare epoch example
  let%bench "compare (>)" = compare example epoch
  let%bench "compare (=)" = compare epoch epoch

  let equal = equal

  let%bench "equal" = equal epoch example

  let min = min

  let%bench "min" = min epoch example

  let max = max

  let%bench "max" = max epoch example

  let ( = ) = ( = )

  let%bench "(=)" = epoch = example

  let ( < ) = ( < )

  let%bench "(<)" = epoch < example

  let ( > ) = ( > )

  let%bench "(>)" = epoch > example

  let ( <= ) = ( <= )

  let%bench "(<=)" = epoch <= example

  let ( >= ) = ( >= )

  let%bench "(>=)" = epoch >= example

  let ( <> ) = ( <> )

  let%bench "(<>)" = epoch <> example
end

let compare = compare

let%bench "compare (<)" = compare epoch example
let%bench "compare (>)" = compare example epoch
let%bench "compare (=)" = compare epoch epoch

let ascending = ascending

let%bench "ascending" = ascending epoch example

let descending = descending

let%bench "descending" = descending epoch example

let equal = equal

let%bench "equal" = equal epoch example

let min = min

let%bench "min" = min epoch example

let max = max

let%bench "max" = max epoch example

let ( = ) = ( = )

let%bench "(=)" = epoch = example

let ( < ) = ( < )

let%bench "(<)" = epoch < example

let ( > ) = ( > )

let%bench "(>)" = epoch > example

let ( <= ) = ( <= )

let%bench "(<=)" = epoch <= example

let ( >= ) = ( >= )

let%bench "(>=)" = epoch >= example

let ( <> ) = ( <> )

let%bench "(<>)" = epoch <> example

let between = between

let%bench "between" = between epoch ~low:epoch ~high:example

let clamp = clamp

let%bench "clamp" = clamp epoch ~min:epoch ~max:example

let clamp_exn = clamp_exn

let%bench "clamp_exn" = clamp_exn epoch ~min:epoch ~max:example

let validate_bound = validate_bound

let%bench "validate_bound" = validate_bound ~min:(Incl epoch) ~max:(Incl example) epoch

let validate_lbound = validate_lbound

let%bench "validate_lbound" = validate_lbound ~min:(Incl epoch) epoch

let validate_ubound = validate_ubound

let%bench "validate_ubound" = validate_ubound ~max:(Incl example) epoch

let robustly_compare = robustly_compare

let%bench "robustly_compare (<)" = robustly_compare epoch example
let%bench "robustly_compare (>)" = robustly_compare example epoch
let%bench "robustly_compare (=)" = robustly_compare epoch epoch

let ( =. ) = ( =. )

let%bench "(=.)" = epoch =. example

let ( <. ) = ( <. )

let%bench "(<.)" = epoch <. example

let ( >. ) = ( >. )

let%bench "(>.)" = epoch >. example

let ( <=. ) = ( <=. )

let%bench "(<=.)" = epoch <=. example

let ( >=. ) = ( >=. )

let%bench "(>=.)" = epoch >=. example

let ( <>. ) = ( <>. )

let%bench "(<>.)" = epoch <>. example

let next = next

let%bench "next" = next epoch

let prev = prev

let%bench "prev" = prev epoch

let to_span_since_epoch = to_span_since_epoch

let%bench "to_span_since_epoch" = to_span_since_epoch epoch

let of_span_since_epoch = of_span_since_epoch

let%bench "of_span_since_epoch" = of_span_since_epoch Span.zero

let now = now

let%bench "now" = now ()

let diff = diff

let%bench "diff" = diff epoch example

let abs_diff = abs_diff

let%bench "abs_diff" = abs_diff epoch example

let sub = sub

let%bench "sub" = sub example Span.day

let add = add

let%bench "add" = add example Span.day

let is_earlier = is_earlier

let%bench "is_earlier" = is_earlier epoch ~than:example

let is_later = is_later

let%bench "is_later" = is_later epoch ~than:example

(* Basically any function that uses zones (not just these) uses a zone cache too, so we're
   benchmarking the "hot-cache" cases here. See bench_zone.ml for benchmarks that always
   reset the cache first. *)
let of_date_ofday = of_date_ofday

let%bench "of_date_ofday (est)" = of_date_ofday ~zone:nyc nyc_winter_date noon
let%bench "of_date_ofday (edt)" = of_date_ofday ~zone:nyc nyc_summer_date noon

let%bench "of_date_ofday (e?t)" =
  of_date_ofday ~zone:nyc nyc_transition_date nyc_transition_ofday
;;

let%bench "of_date_ofday (n/a)" = of_date_ofday ~zone:nyc nyc_skip_date nyc_skip_ofday

let of_date_ofday_precise = of_date_ofday_precise

let%bench "of_date_ofday_precise (est)" =
  of_date_ofday_precise ~zone:nyc nyc_winter_date noon
;;

let%bench "of_date_ofday_precise (edt)" =
  of_date_ofday_precise ~zone:nyc nyc_summer_date noon
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

let%bench "to_date (est)" = to_date ~zone:nyc nyc_winter_time
let%bench "to_date (edt)" = to_date ~zone:nyc nyc_summer_time
let%bench "to_date (e?t)" = to_date ~zone:nyc nyc_transition_time

let to_ofday = to_ofday

let%bench "to_ofday (est)" = to_ofday ~zone:nyc nyc_winter_time
let%bench "to_ofday (edt)" = to_ofday ~zone:nyc nyc_summer_time
let%bench "to_ofday (e?t)" = to_ofday ~zone:nyc nyc_transition_time

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

let%bench "convert" = convert ~from_tz:utc ~to_tz:utc date Ofday.Ofday_constants.example

let utc_offset = utc_offset

let%bench "utc_offset" = utc_offset example ~zone:utc

let occurrence = occurrence

let%bench "occurrence" =
  occurrence `First_after_or_at example ~ofday:Ofday.start_of_day ~zone:utc
;;

let next_multiple = next_multiple

let%bench "next_multiple" =
  next_multiple ~base:epoch ~after:example ~interval:Span.hour ()
;;

let prev_multiple = prev_multiple

let%bench "prev_multiple" =
  prev_multiple ~base:epoch ~before:example ~interval:Span.hour ()
;;

let of_tm = of_tm

let%bench "of_tm" = of_tm epoch_tm ~zone:utc

let format = format

let%bench "format" = format example "%F %T%z" ~zone:utc

let parse = parse

let%bench "parse" = parse example_formatted ~fmt:"%F %T%z" ~zone:utc

let set_sexp_zone = set_sexp_zone

let%bench "set_sexp_zone" = set_sexp_zone zone_for_sexp

let get_sexp_zone = get_sexp_zone

let%bench "get_sexp_zone" = get_sexp_zone ()

let to_string = to_string

let%bench "to_string" = to_string example

let to_string_utc = to_string_utc

let%bench "to_string_utc" = to_string_utc example

let of_string = of_string

let%bench "of_string" = of_string example_string

let of_string_with_utc_offset = of_string_with_utc_offset

let%bench "of_string_with_utc_offset" = of_string_with_utc_offset example_string

let to_filename_string = to_filename_string

let%bench "to_filename_string" = to_filename_string ~zone:utc example

let of_filename_string = of_filename_string

let%bench "of_filename_string" = of_filename_string ~zone:utc example_filename_string

let to_string_trimmed = to_string_trimmed

let%bench "to_string_trimmed" = to_string_trimmed ~zone:utc example

let to_sec_string = to_sec_string

let%bench "to_sec_string" = to_sec_string ~zone:utc example

let to_sec_string_with_zone = to_sec_string_with_zone

let%bench "to_sec_string_with_zone" = to_sec_string_with_zone ~zone:utc example

let of_localized_string = of_localized_string

let%bench "of_localized_string" = of_localized_string ~zone:utc example_localized_string

let of_string_gen = of_string_gen

let%bench "of_string_gen" = of_string_gen ~if_no_timezone:`Local example_string

let to_string_fix_proto = to_string_fix_proto

let%bench "to_string_fix_proto" = to_string_fix_proto `Local example

let of_string_fix_proto = of_string_fix_proto

let%bench "of_string_fix_proto" = of_string_fix_proto `Local fix_example_string

let to_string_abs = to_string_abs

let%bench "to_string_abs" = to_string_abs ~zone:utc example

let of_string_abs = of_string_abs

let%bench "of_string_abs" = of_string_abs example_string

let to_string_abs_trimmed = to_string_abs_trimmed

let%bench "to_string_abs_trimmed" = to_string_abs_trimmed ~zone:utc example

let to_string_abs_parts = to_string_abs_parts

let%bench "to_string_abs_parts" = to_string_abs_parts ~zone:utc example

let to_string_iso8601_basic = to_string_iso8601_basic

let%bench "to_string_iso8601_basic" = to_string_iso8601_basic ~zone:utc example

let sexp_of_t = sexp_of_t

let%bench "sexp_of_t" = sexp_of_t example

let t_of_sexp = t_of_sexp

let%bench "t_of_sexp" = t_of_sexp example_sexp

let sexp_of_t_abs = sexp_of_t_abs

let%bench "sexp_of_t_abs" = sexp_of_t_abs ~zone:utc example

let t_of_sexp_abs = t_of_sexp_abs

let%bench "t_of_sexp_abs" = t_of_sexp_abs example_sexp

module Date_and_ofday = struct
  open Date_and_ofday

  type nonrec t = t

  module Date_and_ofday_constants = struct
    let rel_epoch = of_absolute epoch ~offset_from_utc:Span.zero |> opaque

    let rel_example =
      of_absolute Time_constants.example ~offset_from_utc:Span.zero |> opaque
    ;;

    let nyc_rel_winter_time =
      of_absolute nyc_winter_time ~offset_from_utc:Span.zero |> opaque
    ;;

    let nyc_rel_summer_time =
      of_absolute nyc_summer_time ~offset_from_utc:Span.zero |> opaque
    ;;

    let nyc_rel_transition_time =
      of_absolute nyc_transition_time ~offset_from_utc:Span.zero |> opaque
    ;;
  end

  open Date_and_ofday_constants

  let to_synthetic_span_since_epoch = to_synthetic_span_since_epoch

  let%bench "to_synthetic_span_since_epoch" = to_synthetic_span_since_epoch rel_epoch

  let of_synthetic_span_since_epoch = of_synthetic_span_since_epoch

  let%bench "of_synthetic_span_since_epoch" = of_synthetic_span_since_epoch Span.zero

  let of_date_ofday = of_date_ofday

  let%bench "of_date_ofday" = of_date_ofday date Ofday.Ofday_constants.example

  let of_absolute = of_absolute

  let%bench "of_absolute" = of_absolute epoch ~offset_from_utc:Span.zero

  let to_absolute = to_absolute

  let%bench "to_absolute" = to_absolute rel_epoch ~offset_from_utc:Span.zero

  let to_date_ofday = to_date_ofday

  let%bench "to_date_ofday" = to_date_ofday rel_example

  let to_date = to_date

  let%bench "to_date" = to_date rel_example

  let to_ofday = to_ofday

  let%bench "to_ofday" = to_ofday rel_example
end

module Zone = struct
  open Zone
  module Map = Map
  module Set = Set
  module Table = Table
  module Hash_set = Hash_set
  module Hash_queue = Hash_queue

  type nonrec t = t [@@deriving bin_io]
  type nonrec comparator_witness = comparator_witness

  let arg_type = arg_type
  let comparator = comparator
  let hashable = hashable
  let pp = pp

  (* skip benchmark for file I/O functions *)
  let init = init
  let input_tz_file = input_tz_file
  let utc = opaque utc
  let local = opaque local
  let likely_machine_zones = opaque likely_machine_zones

  module Zone_constants = struct
    let example = find_exn "America/New_York" |> opaque
    let utc_sexp = sexp_of_t utc |> opaque
    let utc_string = to_string utc |> opaque

    let full_data_sexp_of_utc = Time.Stable.Zone.Full_data.V1.sexp_of_t utc |> opaque
  end

  open Zone_constants

  module Replace_polymorphic_compare = struct
    open Replace_polymorphic_compare

    let compare = compare

    let%bench "compare (<)" = compare utc example
    let%bench "compare (>)" = compare example utc
    let%bench "compare (=)" = compare utc utc

    let equal = equal

    let%bench "equal" = equal utc example

    let min = min

    let%bench "min" = min utc example

    let max = max

    let%bench "max" = max utc example

    let ( = ) = ( = )

    let%bench "(=)" = utc = example

    let ( < ) = ( < )

    let%bench "(<)" = utc < example

    let ( > ) = ( > )

    let%bench "(>)" = utc > example

    let ( <= ) = ( <= )

    let%bench "(<=)" = utc <= example

    let ( >= ) = ( >= )

    let%bench "(>=)" = utc >= example

    let ( <> ) = ( <> )

    let%bench "(<>)" = utc <> example
  end

  let compare = compare

  let%bench "compare (<)" = compare utc example
  let%bench "compare (>)" = compare example utc
  let%bench "compare (=)" = compare utc utc

  let ascending = ascending

  let%bench "ascending" = ascending utc example

  let descending = descending

  let%bench "descending" = descending utc example

  let equal = equal

  let%bench "equal" = equal utc example

  let min = min

  let%bench "min" = min utc example

  let max = max

  let%bench "max" = max utc example

  let ( = ) = ( = )

  let%bench "(=)" = utc = example

  let ( < ) = ( < )

  let%bench "(<)" = utc < example

  let ( > ) = ( > )

  let%bench "(>)" = utc > example

  let ( <= ) = ( <= )

  let%bench "(<=)" = utc <= example

  let ( >= ) = ( >= )

  let%bench "(>=)" = utc >= example

  let ( <> ) = ( <> )

  let%bench "(<>)" = utc <> example

  let between = between

  let%bench "between" = between utc ~low:utc ~high:example

  let clamp = clamp

  let%bench "clamp" = clamp utc ~min:utc ~max:example

  let clamp_exn = clamp_exn

  let%bench "clamp_exn" = clamp_exn utc ~min:utc ~max:utc

  let validate_bound = validate_bound

  let%bench "validate_bound" = validate_bound ~min:(Incl utc) ~max:(Incl utc) utc

  let validate_lbound = validate_lbound

  let%bench "validate_lbound" = validate_lbound ~min:(Incl utc) utc

  let validate_ubound = validate_ubound

  let%bench "validate_ubound" = validate_ubound ~max:(Incl example) utc

  let hash = hash

  let%bench "hash" = hash utc

  let hash_fold_t = hash_fold_t

  let%bench "hash_fold_t" = hash_fold_t hash_state utc

  let name = name

  let%bench "name" = name utc

  let original_filename = original_filename

  let%bench "original_filename" = original_filename utc

  let of_utc_offset = of_utc_offset

  let%bench "of_utc_offset" = of_utc_offset ~hours:1

  let of_utc_offset_explicit_name = of_utc_offset_explicit_name

  let%bench "of_utc_offset_explicit_name" =
    of_utc_offset_explicit_name ~name:"UTC+1" ~hours:1
  ;;

  let digest = digest

  let%bench "digest" = digest utc

  let find = find

  let%bench "find" = find "America/New_York"

  let find_exn = find_exn

  let%bench "find_exn" = find_exn "America/New_York"

  let initialized_zones = initialized_zones

  let%bench "initialized_zones" = initialized_zones ()

  let abbreviation = abbreviation

  let%bench "abbreviation (est)" = abbreviation nyc nyc_winter_time
  let%bench "abbreviation (edt)" = abbreviation nyc nyc_summer_time
  let%bench "abbreviation (e?t)" = abbreviation nyc nyc_transition_time

  let date_and_ofday_of_absolute_time = date_and_ofday_of_absolute_time

  let%bench "date_and_ofday_of_absolute_time est" =
    date_and_ofday_of_absolute_time nyc nyc_winter_time
  ;;

  let%bench "date_and_ofday_of_absolute_time edt" =
    date_and_ofday_of_absolute_time nyc nyc_summer_time
  ;;

  let%bench "date_and_ofday_of_absolute_time e?t" =
    date_and_ofday_of_absolute_time nyc nyc_transition_time
  ;;

  let absolute_time_of_date_and_ofday = absolute_time_of_date_and_ofday

  let%bench "absolute_time_of_date_and_ofday est" =
    absolute_time_of_date_and_ofday
      nyc
      Date_and_ofday.Date_and_ofday_constants.nyc_rel_winter_time
  ;;

  let%bench "absolute_time_of_date_and_ofday edt" =
    absolute_time_of_date_and_ofday
      nyc
      Date_and_ofday.Date_and_ofday_constants.nyc_rel_summer_time
  ;;

  let%bench "absolute_time_of_date_and_ofday e?t" =
    absolute_time_of_date_and_ofday
      nyc
      Date_and_ofday.Date_and_ofday_constants.nyc_rel_transition_time
  ;;

  let prev_clock_shift = prev_clock_shift

  let%bench "prev_clock_shift (est)" = prev_clock_shift nyc ~at_or_before:nyc_winter_time
  let%bench "prev_clock_shift (edt)" = prev_clock_shift nyc ~at_or_before:nyc_summer_time

  let%bench "prev_clock_shift (e?t)" =
    prev_clock_shift nyc ~at_or_before:nyc_transition_time
  ;;

  let next_clock_shift = next_clock_shift

  let%bench "next_clock_shift (est)" =
    next_clock_shift nyc ~strictly_after:nyc_winter_time
  ;;

  let%bench "next_clock_shift (edt)" =
    next_clock_shift nyc ~strictly_after:nyc_summer_time
  ;;

  let%bench "next_clock_shift (e?t)" =
    next_clock_shift nyc ~strictly_after:nyc_transition_time
  ;;

  (* This function is only meant for use in benchmarks: slowing it down doesn't matter in
     itself but seems important to know about for the interpretation of other benchmarks.
  *)
  let reset_transition_cache = reset_transition_cache

  let%bench "reset_transition_cache" = reset_transition_cache nyc

  let sexp_of_t = sexp_of_t

  let%bench "sexp_of_t" = sexp_of_t utc

  let t_of_sexp = t_of_sexp

  let%bench "t_of_sexp" = t_of_sexp utc_sexp

  let to_string = to_string

  let%bench "to_string" = to_string utc

  let of_string = of_string

  let%bench "of_string" = of_string utc_string

  module Index = Index

  let index = index

  let%bench "index" = index nyc Time_constants.example

  let index_of_date_and_ofday = index_of_date_and_ofday

  let%bench "index_of_date_and_ofday" =
    index_of_date_and_ofday nyc Date_and_ofday.Date_and_ofday_constants.rel_example
  ;;

  let index_abbreviation_exn = index_abbreviation_exn

  let%bench "index_abbreviation_exn" = index_abbreviation_exn nyc index_example

  let index_offset_from_utc_exn = index_offset_from_utc_exn

  let%bench "index_offset_from_utc_exn" = index_offset_from_utc_exn nyc index_example

  let index_has_prev_clock_shift = index_has_prev_clock_shift

  let%bench "index_has_prev_clock_shift" = index_has_prev_clock_shift nyc index_example

  let index_prev_clock_shift_time_exn = index_prev_clock_shift_time_exn

  let%bench "index_prev_clock_shift_time_exn" =
    index_prev_clock_shift_time_exn nyc index_example
  ;;

  let index_prev_clock_shift_amount_exn = index_prev_clock_shift_amount_exn

  let%bench "index_prev_clock_shift_amount_exn" =
    index_prev_clock_shift_amount_exn nyc index_example
  ;;

  let index_has_next_clock_shift = index_has_next_clock_shift

  let%bench "index_has_next_clock_shift" = index_has_next_clock_shift nyc index_example

  let index_next_clock_shift_time_exn = index_next_clock_shift_time_exn

  let%bench "index_next_clock_shift_time_exn" =
    index_next_clock_shift_time_exn nyc index_example
  ;;

  let index_next_clock_shift_amount_exn = index_next_clock_shift_amount_exn

  let%bench "index_next_clock_shift_amount_exn" =
    index_next_clock_shift_amount_exn nyc index_example
  ;;
end

module Stable = struct
  module V1 = struct
    open Stable.V1
    module Map = Map
    module Set = Set

    type nonrec t = t [@@deriving bin_io, hash, typerep]
    type nonrec comparator_witness = comparator_witness

    let comparator = comparator
    let compare = compare

    let%bench "compare (<)" = compare epoch example
    let%bench "compare (>)" = compare example epoch
    let%bench "compare (=)" = compare epoch epoch

    let sexp_of_t = sexp_of_t

    let%bench "sexp_of_t" = sexp_of_t example

    let t_of_sexp = t_of_sexp

    let%bench "t_of_sexp" = t_of_sexp example_sexp
  end

  module With_utc_sexp = struct
    module V1 = struct
      open Stable.With_utc_sexp.V1
      module Map = Map
      module Set = Set

      type nonrec t = t [@@deriving bin_io]
      type nonrec comparator_witness = comparator_witness

      let comparator = comparator
      let compare = compare

      let%bench "compare (<)" = compare epoch example
      let%bench "compare (>)" = compare example epoch
      let%bench "compare (=)" = compare epoch epoch

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t" = sexp_of_t example

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp" = t_of_sexp example_sexp
    end

    module V2 = struct
      open Stable.With_utc_sexp.V2
      module Map = Map
      module Set = Set

      type nonrec t = t [@@deriving bin_io, hash]
      type nonrec comparator_witness = comparator_witness

      let comparator = comparator
      let compare = compare

      let%bench "compare (<)" = compare epoch example
      let%bench "compare (>)" = compare example epoch
      let%bench "compare (=)" = compare epoch epoch

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t" = sexp_of_t example

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp" = t_of_sexp example_sexp
    end
  end

  module With_t_of_sexp_abs = struct
    module V1 = struct
      open Stable.With_t_of_sexp_abs.V1

      type nonrec t = t [@@deriving bin_io]
      type nonrec comparator_witness = comparator_witness

      let comparator = comparator
      let compare = compare

      let%bench "compare (<)" = compare epoch example
      let%bench "compare (>)" = compare example epoch
      let%bench "compare (=)" = compare epoch epoch

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t" = sexp_of_t example

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp" = t_of_sexp example_sexp
    end
  end

  module Span = struct
    module V1 = struct
      open Span
      open Time.Stable.Span.V1
      open Span_constants

      type nonrec t = t [@@deriving bin_io]

      let compare = compare

      let%bench "compare (<)" = compare zero day
      let%bench "compare (>)" = compare day zero
      let%bench "compare (=)" = compare zero zero

      let equal = equal

      let%bench "equal" = equal zero zero

      let hash = hash

      let%bench "hash" = hash zero

      let hash_fold_t = hash_fold_t

      let%bench "hash_fold_t" = hash_fold_t hash_state zero

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t" = sexp_of_t day

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp" = t_of_sexp day_sexp
    end

    module V2 = struct
      open Span
      open Time.Stable.Span.V2
      open Span_constants

      type nonrec t = t [@@deriving bin_io]

      let compare = compare

      let%bench "compare (<)" = compare zero day
      let%bench "compare (>)" = compare day zero
      let%bench "compare (=)" = compare zero zero

      let equal = equal

      let%bench "equal" = equal zero zero

      let hash = hash

      let%bench "hash" = hash zero

      let hash_fold_t = hash_fold_t

      let%bench "hash_fold_t" = hash_fold_t hash_state zero

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t" = sexp_of_t day

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp" = t_of_sexp day_sexp
    end

    module V3 = struct
      open Span
      open Time.Stable.Span.V3
      open Span_constants

      type nonrec t = t [@@deriving bin_io, typerep]

      let compare = compare

      let%bench "compare (<)" = compare zero day
      let%bench "compare (>)" = compare day zero
      let%bench "compare (=)" = compare zero zero

      let equal = equal

      let%bench "equal" = equal zero zero

      let hash = hash

      let%bench "hash" = hash zero

      let hash_fold_t = hash_fold_t

      let%bench "hash_fold_t" = hash_fold_t hash_state zero

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t" = sexp_of_t day

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp" = t_of_sexp day_sexp
    end
  end

  module Ofday = struct
    module V1 = struct
      open Ofday
      open Time.Stable.Ofday.V1
      open Ofday_constants

      type nonrec t = t [@@deriving bin_io]

      let compare = compare

      let%bench "compare (<)" = compare start_of_day example
      let%bench "compare (>)" = compare example start_of_day
      let%bench "compare (=)" = compare start_of_day start_of_day

      let hash = hash

      let%bench "hash" = hash start_of_day

      let hash_fold_t = hash_fold_t

      let%bench "hash_fold_t" = hash_fold_t hash_state start_of_day

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t" = sexp_of_t example

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp" = t_of_sexp example_sexp
    end

    module Zoned = struct
      module V1 = struct
        open Ofday.Zoned
        open Time.Stable.Ofday.Zoned.V1
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
  end

  module Zone = struct
    module V1 = struct
      open Zone
      open Time.Stable.Zone.V1
      open Zone_constants

      type nonrec t = t [@@deriving bin_io]

      let compare = compare

      let%bench "compare" = compare utc utc

      let hash = hash

      let%bench "hash" = hash utc

      let hash_fold_t = hash_fold_t

      let%bench "hash_fold_t" = hash_fold_t hash_state utc

      let sexp_of_t = sexp_of_t

      let%bench "sexp_of_t" = sexp_of_t utc

      let t_of_sexp = t_of_sexp

      let%bench "t_of_sexp" = t_of_sexp utc_sexp
    end

    module Full_data = struct
      module V1 = struct
        open Zone
        open Time.Stable.Zone.Full_data.V1
        open Zone_constants

        type nonrec t = t [@@deriving bin_io]

        let compare = compare

        let%bench "compare" = compare utc utc

        let sexp_of_t = sexp_of_t

        let%bench "sexp_of_t" = sexp_of_t utc

        let t_of_sexp = t_of_sexp

        let%bench "t_of_sexp" = t_of_sexp full_data_sexp_of_utc
      end
    end
  end
end
