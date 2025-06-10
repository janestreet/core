open! Import
open! Std_internal

module Rounding_direction = struct
  type t =
    | Down
    | Nearest
    | Up
    | Zero
  [@@deriving equal ~localize, enumerate, sexp_of]
end

module type Span = sig @@ portable
  (** [t] is immediate on 64bit boxes and so plays nicely with the GC write barrier. *)
  type t = private Int63.t [@@deriving hash, typerep]

  include Span_intf.S with type underlying = Int63.t and type t := t

  include%template Quickcheck.S_int [@mode portable] with type t := t

  val of_sec_with_microsecond_precision : float -> t
  val of_int_us : int -> t [@@zero_alloc strict]
  val of_int_ms : int -> t [@@zero_alloc strict]
  val to_int_us : t -> int [@@zero_alloc strict]
  val to_int_ms : t -> int [@@zero_alloc strict]
  val to_int_sec : t -> int [@@zero_alloc strict]

  (** Approximations of float conversions using multiplication instead of division. *)

  val to_us_approx : t -> float
  val to_ms_approx : t -> float
  val to_sec_approx : t -> float
  val to_min_approx : t -> float
  val to_hr_approx : t -> float
  val to_day_approx : t -> float

  (** The minimum representable time span. *)
  val min_value_representable : t

  (** The maximum representable time span. *)
  val max_value_representable : t

  (** The minimum span that rounds to a [Time.Span.t] with microsecond precision. *)
  val min_value_for_1us_rounding : t

  (** The maximum span that rounds to a [Time.Span.t] with microsecond precision. *)
  val max_value_for_1us_rounding : t

  (** An alias for [min_value_for_1us_rounding]. *)
  val min_value : t
  [@@deprecated
    "[since 2019-02] use [min_value_representable] or [min_value_for_1us_rounding] \
     instead"]

  (** An alias for [max_value_for_1us_rounding]. *)
  val max_value : t
  [@@deprecated
    "[since 2019-02] use [max_value_representable] or [max_value_for_1us_rounding] \
     instead"]

  (** overflows silently *)
  val scale_int : t -> int -> t [@@zero_alloc strict]

  (** overflows silently *)
  val scale_int63 : t -> Int63.t -> t

  (** Rounds down, and raises unless denominator is positive. *)
  val div : t -> t -> Int63.t

  (** Fast, implemented as the identity function. *)
  val to_int63_ns : t -> Int63.t

  (** Fast, implemented as the identity function. *)
  val of_int63_ns : Int63.t -> t

  (** Will raise on 32-bit platforms. Consider [to_int63_ns] instead. *)
  val to_int_ns : t -> int
  [@@zero_alloc]

  val of_int_ns : int -> t
  val since_unix_epoch : unit -> t [@@zero_alloc]
  val random : ?state:Random.State.t -> unit -> t

  (** WARNING!!! [to_span] and [of_span] both round to the nearest 1us.

      Around 135y magnitudes [to_span] and [of_span] raise. *)
  val to_span : t -> Span_float.t
  [@@deprecated
    "[since 2019-01] use [to_span_float_round_nearest] or \
     [to_span_float_round_nearest_microsecond]"]

  val of_span : Span_float.t -> t
  [@@deprecated
    "[since 2019-01] use [of_span_float_round_nearest] or \
     [of_span_float_round_nearest_microsecond]"]

  (** [*_round_nearest] vs [*_round_nearest_microsecond]: If you don't know that you need
      microsecond precision, use the [*_round_nearest] version.
      [*_round_nearest_microsecond] is for historical purposes. *)

  val to_span_float_round_nearest : t -> Span_float.t
  val to_span_float_round_nearest_microsecond : t -> Span_float.t
  val of_span_float_round_nearest : Span_float.t -> t
  val of_span_float_round_nearest_microsecond : Span_float.t -> t

  module Rounding_direction = Rounding_direction

  (** These operations round the span to the nearest whole-number of the given factor. *)

  val round_up : t -> to_multiple_of:t -> t
  val round_down : t -> to_multiple_of:t -> t
  val round_nearest : t -> to_multiple_of:t -> t
  val round_towards_zero : t -> to_multiple_of:t -> t
  val round : t -> dir:Rounding_direction.t -> to_multiple_of:t -> t

  (** Note that we expose a sexp format that is not the one exposed in [Core]. *)
  module Alternate_sexp : sig
    type nonrec t = t [@@deriving sexp, sexp_grammar]
  end
  [@@deprecated "[since 2018-04] use [Span.sexp_of_t] and [Span.t_of_sexp] instead"]

  val arg_type : t Command.Arg_type.t

  module O : sig
    val ( / ) : t -> float -> t [@@zero_alloc]
    val ( // ) : t -> t -> float
    val ( + ) : t -> t -> t [@@zero_alloc strict]
    val ( - ) : t -> t -> t [@@zero_alloc strict]

    (** alias for [neg] *)
    val ( ~- ) : t -> t [@@zero_alloc strict]

    (** alias for [scale] *)
    val ( *. ) : t -> float -> t [@@zero_alloc]

    (** alias for [scale_int] *)
    val ( * ) : t -> int -> t [@@zero_alloc strict]

    include Comparisons.Infix_with_zero_alloc with type t := t
  end

  (** [Span.Option.t] is like [Span.t option], except that the value is immediate on
      architectures where [Int63.t] is immediate. This module should mainly be used to
      avoid allocations. *)
  module Option : sig
    type value := t

    type t : immediate64
    [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

    include Immediate_option.S_int63_zero_alloc with type value := value and type t := t

    include%template Identifiable.S [@mode local] with type t := t

    include Diffable.S_atomic with type t := t
    include Quickcheck.S with type t := t

    module Stable : sig
      module V1 : sig
        type nonrec t = t
        [@@deriving
          bin_io ~localize, compare ~localize, equal ~localize, globalize, typerep]

        include%template
          Stable_int63able.With_stable_witness.S [@mode local] with type t := t

        include Diffable.S_atomic with type t := t
      end

      module V2 : sig
        type nonrec t = t
        [@@deriving
          bin_io ~localize, compare ~localize, equal ~localize, globalize, typerep]

        include%template
          Stable_int63able.With_stable_witness.S [@mode local] with type t := t

        include Diffable.S_atomic with type t := t
      end
    end
  end

  module Stable : sig
    module V1 : sig
      type nonrec t = t
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp_grammar]

      include%template
        Stable_int63able.With_stable_witness.S [@mode local] with type t := t

      include Diffable.S_atomic with type t := t
    end

    module V2 : sig
      type nonrec t = t
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp_grammar
        , typerep]

      type nonrec comparator_witness = comparator_witness

      include%template
        Stable_int63able.With_stable_witness.S
        [@mode local]
        with type t := t
        with type comparator_witness := comparator_witness

      include
        Comparable.Stable.V1.With_stable_witness.S
        with type comparable := t
        with type comparator_witness := comparator_witness

      include Stringable.S with type t := t
      include Diffable.S_atomic with type t := t
      include Quickcheck.S with type t := t
    end
  end

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    val of_parts : Parts.t -> t
    val to_parts : t -> Parts.t
  end
end

module type Ofday = sig
  module Span : Span

  (** [t] is immediate on 64bit boxes and so plays nicely with the GC write barrier. *)
  type t = private Int63.t

  (** String and sexp output takes the form 'HH:MM:SS.sssssssss'; see {!Core.Ofday_intf}
      for accepted input. If input includes more than 9 decimal places in seconds, rounds
      to the nearest nanosecond, with the midpoint rounded up. Allows 60[.sss...] seconds
      for leap seconds but treats it as exactly 60s regardless of fractional part. *)
  include
    Ofday_intf.S with type underlying = Int63.t and type t := t and module Span := Span

  (** The largest representable value below [start_of_next_day], i.e. one nanosecond
      before midnight. *)
  val approximate_end_of_day : t

  (*_ This is already exported from [Ofday_intf.S], but we re-declare it to add
    documentation. *)

  (** [add_exn t span] shifts the time of day [t] by [span]. It raises if the result is
      not in the same 24-hour day. Daylight savings shifts are not accounted for. *)
  val add_exn : t -> Span.t -> t
  [@@zero_alloc]

  (** [sub_exn t span] shifts the time of day [t] back by [span]. It raises if the result
      is not in the same 24-hour day. Daylight savings shifts are not accounted for. *)
  val sub_exn : t -> Span.t -> t
  [@@zero_alloc]

  (** [every span ~start ~stop] returns a sorted list of all [t]s that can be expressed as
      [start + (i * span)] without overflow, and satisfying [t >= start && t <= stop].

      If [span <= Span.zero || start > stop], returns an Error.

      The result never crosses the midnight boundary. Constructing a list crossing
      midnight, e.g. every hour from 10pm to 2am, requires multiple calls to [every]. *)
  val every : Span.t -> start:t -> stop:t -> t list Or_error.t

  val to_microsecond_string : t -> string
  val to_nanosecond_string : t -> string

  module Stable : sig
    module V1 : sig
      type nonrec t = t
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp_grammar]

      include%template
        Stable_int63able.With_stable_witness.S
        [@mode local]
        with type t := t
         and type comparator_witness = comparator_witness

      include Diffable.S_atomic with type t := t
    end
  end
end

module type Time_ns = sig @@ portable
  (** An absolute point in time, more efficient and precise than the [float]-based
      {!Time_float}, but representing a narrower range of times.

      This module represents absolute times with nanosecond precision, approximately
      between the years 1823 and 2116 CE.

      Some reasons you might prefer [Time_ns.t] over float-based [Time_float.t]:

      - It has superior performance.

      - It uses [int]s rather than [float]s internally, which makes certain things easier
        to reason about, since [int]s respect a bunch of arithmetic identities that
        [float]s don't, e.g., [x + (y + z) = (x + y) + z].

      Some reasons you might prefer to use float-based [Time_float] instead of this
      module:

      - Some libraries use [Time_float.t] values, often for historical reasons, so it may
        be necessary to use [Time_float.t] with them.

      - [Time_ns] silently ignores overflow.

      Neither {!Time_ns_unix} nor {!Time_float_unix} are available in JavaScript, but both
      {!Core.Time_ns} and {!Core.Time_float} are. *)

  type t = private Int63.t
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , sexp
    , sexp_grammar
    , typerep]

  type time_ns := t

  module Span : Span

  module Ofday : sig
    include Ofday with module Span := Span

    val arg_type : t Command.Arg_type.t
    val now : zone:Timezone.t -> t

    val to_ofday : t -> Time_float.Ofday.t
    [@@deprecated
      "[since 2019-01] use [to_ofday_float_round_nearest] or \
       [to_ofday_float_round_nearest_microsecond]"]

    val of_ofday : Time_float.Ofday.t -> t
    [@@deprecated
      "[since 2019-01] use [of_ofday_float_round_nearest] or \
       [of_ofday_float_round_nearest_microsecond]"]

    val to_ofday_float_round_nearest : t -> Time_float.Ofday.t
    val to_ofday_float_round_nearest_microsecond : t -> Time_float.Ofday.t
    val of_ofday_float_round_nearest : Time_float.Ofday.t -> t [@@zero_alloc]
    val of_ofday_float_round_nearest_microsecond : Time_float.Ofday.t -> t [@@zero_alloc]

    module Zoned : sig
      (** Sexps look like "(12:01 nyc)"

          Two [t]'s may or may not correspond to the same times depending on which date
          they're evaluated. *)
      type ofday := t

      type t [@@deriving bin_io, sexp, sexp_grammar, hash]

      include Pretty_printer.S with type t := t

      (** Strings look like "12:01 nyc" *)
      include Stringable.S with type t := t

      val arg_type : t Command.Arg_type.t
      val create : ofday -> Time_float.Zone.t -> t
      val create_local : ofday -> t
      val ofday : t -> ofday
      val zone : t -> Time_float.Zone.t
      val to_time_ns : t -> Date.t -> time_ns

      module With_nonchronological_compare : sig
        (** It is possible to consistently compare [t]'s, but due to the complexities of
            time zones and daylight savings, the resulting ordering is not chronological.
            That is, [compare t1 t2 > 0] does not imply [t2] occurs after [t1] every day,
            or any day. *)
        type nonrec t = t
        [@@deriving bin_io, sexp, sexp_grammar, compare ~localize, equal ~localize, hash]
      end

      module Stable : sig
        module V1 : sig
          type nonrec t = t [@@deriving equal ~localize, hash, sexp_grammar]

          include%template
            Stable_without_comparator_with_witness [@mode local] with type t := t
        end
      end
    end

    module Option : sig
      type value := t

      type t : immediate64
      [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

      include Immediate_option.S_int63_zero_alloc with type value := value and type t := t

      include%template Identifiable.S [@mode local] with type t := t

      include Quickcheck.S with type t := t
      include Diffable.S_atomic with type t := t

      module Stable : sig
        module V1 : sig
          type nonrec t = t
          [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

          include%template Stable_int63able_with_witness [@mode local] with type t := t

          include Diffable.S_atomic with type t := t
        end
      end

      (** Returns [some] if the given span is a valid time since start of day, and [none]
          otherwise. *)
      val of_span_since_start_of_day : Span.t -> t
    end
  end

  include%template Comparisons.S_with_zero_alloc [@mode local] with type t := t

  include Diffable.S_atomic with type t := t
  module Zone : module type of Time_float.Zone with type t = Time_float.Zone.t

  (** Note that we expose a sexp format that is not the one exposed in [Time_ns_unix]. The
      sexp is a single atom rendered as with [to_string_utc], except that all trailing
      zeros are trimmed, rather than trimming in groups of three. *)
  module Alternate_sexp : sig
    type nonrec t = t
    [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, sexp_grammar]

    include%template Comparable.S [@mode local] with type t := t

    include Diffable.S_atomic with type t := t
  end

  (** [Option.t] is like [t option], except that the value is immediate. This module
      should mainly be used to avoid allocations. *)
  module Option : sig
    type value := t

    type t = private Span.Option.t
    [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

    include Immediate_option.S_int63_zero_alloc with type value := value with type t := t

    include%template Identifiable.S [@mode local] with type t := t

    include Quickcheck.S with type t := t
    include Diffable.S_atomic with type t := t

    module Stable : sig
      module V1 : sig
        type nonrec t = t
        [@@deriving
          bin_io ~localize, compare ~localize, equal ~localize, globalize, typerep]

        include%template Stable_int63able_with_witness [@mode local] with type t := t

        include Diffable.S_atomic with type t := t
      end
    end

    include Comparisons.S_with_zero_alloc with type t := t

    (** Just like [Time_ns.Alternate_sexp], this is different from the sexp format exposed
        in [Time_ns_unix]. See the comment above. *)
    module Alternate_sexp : sig
      type nonrec t = t
      [@@deriving bin_io, compare ~localize, equal ~localize, hash, sexp, sexp_grammar]

      include%template Comparable.S [@mode local] with type t := t

      include Diffable.S_atomic with type t := t
    end
  end

  include
    Time_intf.Shared with type t := t with module Span := Span with module Ofday := Ofday

  (** These functions are identical to those in [Time] and get/set the same variable. *)

  val get_sexp_zone : unit -> Zone.t
  val set_sexp_zone : Zone.t -> unit

  (** [t_of_sexp_abs sexp] as [t_of_sexp], but demands that [sexp] indicate the timezone
      the time is expressed in. *)
  val t_of_sexp_abs : Sexp.t -> t

  val sexp_of_t_abs : t -> zone:Zone.t -> Sexp.t

  (** Conversion functions that involved Ofday.Zoned.t, exactly analogous to the
      conversion functions that involve Ofday.t *)
  val of_date_ofday_zoned : Date.t -> Ofday.Zoned.t -> t

  val to_date_ofday_zoned : t -> zone:Zone.t -> Date.t * Ofday.Zoned.t
  val to_ofday_zoned : t -> zone:Zone.t -> Ofday.Zoned.t
  val to_string_fix_proto : [ `Utc | `Local ] -> t -> string
  val of_string_fix_proto : [ `Utc | `Local ] -> string -> t

  (** This is like [of_string] except that if the string doesn't specify the zone then it
      raises rather than assume the local timezone. *)
  val of_string_abs : string -> t

  (** [of_string_gen ~if_no_timezone ?find_zone s] attempts to parse [s] to a [t]. If [s]
      doesn't supply a time zone [if_no_timezone] is consulted. [find_zone] is used to
      look up time zones by name. *)
  val of_string_gen
    :  if_no_timezone:
         [ `Fail
         | `Local
         | `Use_this_one of Zone.t
         | `Use_this_one_lazy of Zone.t Lazy.t
         ]
    -> ?find_zone:(string -> Zone.t) (** default: [Timezone.find_exn] *)
    -> string
    -> t

  (** [of_string_with_utc_offset] requires its input to have an explicit UTC offset, e.g.
      [2000-01-01 12:34:56.789012-23], or use the UTC zone, "Z", e.g.
      [2000-01-01 12:34:56.789012Z]. *)
  val of_string_with_utc_offset : string -> t

  (** [to_string_utc] generates a time string with the UTC zone, "Z", e.g.
      [2000-01-01 12:34:56.789012Z]. *)
  val to_string_utc : t -> string

  (** Returns the ISO 8601 extended format for a time (including the date) in the given
      time zone. The number of decimal places is determined by [precision], default [`ns].

      For example:

      {[
        to_string_iso8601_extended epoch ~zone:Zone.utc ~precision:`us
        = "1970-01-01T00:00:00.000000Z"
      ]} *)
  val to_string_iso8601_extended
    :  ?precision:
         [ `sec (** second precision: no decimal *)
         | `ms (** millisecond precision: three decimal places *)
         | `us (** microsecond precision: six decimal places *)
         | `ns (** nanosecond precision: nine decimal places *)
         ]
    -> zone:Zone.t
    -> t
    -> string

  (** Unix epoch (1970-01-01 00:00:00 UTC) *)
  val epoch : t

  (** The minimum representable time. *)
  val min_value_representable : t

  (** The maximum representable time. *)
  val max_value_representable : t

  (** The minimum time that rounds to a [Time.t] with microsecond precision. *)
  val min_value_for_1us_rounding : t

  (** The maximum time that rounds to a [Time.t] with microsecond precision. *)
  val max_value_for_1us_rounding : t

  (** An alias for [min_value_for_1us_rounding]. *)
  val min_value : t
  [@@deprecated
    "[since 2019-02] use [min_value_representable] or [min_value_for_1us_rounding] \
     instead"]

  (** An alias for [max_value_for_1us_rounding]. *)
  val max_value : t
  [@@deprecated
    "[since 2019-02] use [max_value_representable] or [max_value_for_1us_rounding] \
     instead"]

  (** The current time. *)
  val now : unit -> t [@@zero_alloc]

  (** overflows silently *)
  val add : t -> Span.t -> t [@@zero_alloc strict]

  (** As [add]; rather than over/underflowing, clamps the result to the closed interval
      between [min_value_representable] and [max_value_representable]. *)
  val add_saturating : t -> Span.t -> t
  [@@zero_alloc strict]

  (** As [sub]; rather than over/underflowing, clamps the result to the closed interval
      between [min_value_representable] and [max_value_representable]. *)
  val sub_saturating : t -> Span.t -> t
  [@@zero_alloc strict]

  (** overflows silently *)
  val sub : t -> Span.t -> t [@@zero_alloc strict]

  (** overflows silently *)
  val next : t -> t [@@zero_alloc strict]

  (** overflows silently *)
  val prev : t -> t [@@zero_alloc strict]

  (** overflows silently *)
  val diff : t -> t -> Span.t [@@zero_alloc strict]

  (** overflows silently *)
  val abs_diff : t -> t -> Span.t [@@zero_alloc strict]

  val to_span_since_epoch : t -> Span.t [@@zero_alloc strict]
  val of_span_since_epoch : Span.t -> t [@@zero_alloc strict]
  val to_int63_ns_since_epoch : t -> Int63.t
  val of_int63_ns_since_epoch : Int63.t -> t

  (** Will raise on 32-bit platforms. Consider [to_int63_ns_since_epoch] instead. *)
  val to_int_ns_since_epoch : t -> int
  [@@zero_alloc]

  val of_int_ns_since_epoch : int -> t [@@zero_alloc]

  (** [next_multiple ~base ~after ~interval] returns the smallest [time] of the form:

      {[
        time = base + (k * interval)
      ]}

      where [k >= 0] and [time > after]. It is an error if [interval <= 0].

      Supplying [~can_equal_after:true] allows the result to satisfy [time >= after].

      This function is useful for finding linear time intervals, like every 30 minutes or
      every 24 hours. This is different from rounding to apparent clock-face internvals,
      like "every hour at :00 and :30" or "every day at noon", because time zone and
      daylight savings transitions may cause linear intervals and apparent clock-face
      intervals to differ.

      Time zone offsets (in the tzdata time zone database, at least) are expressed in
      seconds, so rounding to units of seconds or smaller is not affected by time zones.
      The [round*] functions below provide some straightforward cases. For other small
      units that evenly divide into a second, call with [base = epoch], [after] as the
      time to round, and [interval] as the unit span you are rounding to. *)
  val next_multiple
    :  ?can_equal_after:bool (** default is [false] *)
    -> base:t
    -> after:t
    -> interval:Span.t
    -> unit
    -> t

  (** [prev_multiple ~base ~before ~interval] returns the largest [time] of the form:

      {[
        time = base + (k * interval)
      ]}

      where [k >= 0] and [time < before]. It is an error if [interval <= 0].

      Supplying [~can_equal_before:true] allows the result to satisfy [time <= before].

      This function is useful for finding linear time intervals, like every 30 minutes or
      every 24 hours. This is different from rounding to apparent clock-face internvals,
      like "every hour at :00 and :30" or "every day at noon", because time zone and
      daylight savings transitions may cause linear intervals and apparent clock-face
      intervals to differ.

      Time zone offsets (in the tzdata time zone database, at least) are expressed in
      seconds, so rounding to units of seconds or smaller is not affected by time zones.
      The [round*] functions below provide some straightforward cases. For other small
      units that evenly divide into a second, call with [base = epoch], [after] as the
      time to round, and [interval] as the unit span you are rounding to. *)
  val prev_multiple
    :  ?can_equal_before:bool (** default is [false] *)
    -> base:t
    -> before:t
    -> interval:Span.t
    -> unit
    -> t

  (** [round_up_to_us t] returns [t] rounded up to the next microsecond. *)
  val round_up_to_us : t -> t

  (** [round_up_to_ms t] returns [t] rounded up to the next millisecond. *)
  val round_up_to_ms : t -> t

  (** [round_up_to_sec t] returns [t] rounded up to the next second. *)
  val round_up_to_sec : t -> t

  (** [round_down_to_us t] returns [t] rounded down to the previous microsecond. *)
  val round_down_to_us : t -> t

  (** [round_down_to_ms t] returns [t] rounded down to the previous millisecond. *)
  val round_down_to_ms : t -> t

  (** [round_down_to_sec t] returns [t] rounded down to the previous second. *)
  val round_down_to_sec : t -> t

  val random : ?state:Random.State.t -> unit -> t

  val of_time : Time_float.t -> t
  [@@deprecated
    "[since 2019-01] use [of_time_float_round_nearest] or \
     [of_time_float_round_nearest_microsecond]"]

  val to_time : t -> Time_float.t
  [@@deprecated
    "[since 2019-01] use [to_time_float_round_nearest] or \
     [to_time_float_round_nearest_microsecond]"]

  (** [*_round_nearest] vs [*_round_nearest_microsecond]: If you don't know that you need
      microsecond precision, use the [*_round_nearest] version.
      [*_round_nearest_microsecond] is for historical purposes. *)

  val to_time_float_round_nearest : t -> Time_float.t
  val to_time_float_round_nearest_microsecond : t -> Time_float.t
  val of_time_float_round_nearest : Time_float.t -> t [@@zero_alloc]
  val of_time_float_round_nearest_microsecond : Time_float.t -> t [@@zero_alloc]

  module Utc : sig
    (** [to_date_and_span_since_start_of_day] computes the date and intraday-offset of a
        time in UTC. It may be slower than [Core.Time_ns.to_date_ofday], as this function
        does not cache partial results while the latter does. *)
    val to_date_and_span_since_start_of_day : t -> Date0.t * Span.t

    (** The inverse of [to_date_and_span_since_start_of_day]. *)
    val of_date_and_span_since_start_of_day : Date0.t -> Span.t -> t
    [@@zero_alloc]
  end

  module O : sig
    (** alias for [add] *)
    val ( + ) : t -> Span.t -> t [@@zero_alloc strict]

    (** alias for [diff] *)
    val ( - ) : t -> t -> Span.t [@@zero_alloc strict]

    include Comparisons.Infix_with_zero_alloc with type t := t
  end

  (** String conversions use the local timezone by default. Sexp conversions use
      [get_sexp_zone ()] by default, which can be overridden by calling [set_sexp_zone].
      These default time zones are used when writing a time, and when reading a time with
      no explicit zone or UTC offset.

      Sexps and strings display the date, ofday, and UTC offset of [t] relative to the
      appropriate time zone. *)
  include%template Identifiable.S [@mode local] with type t := t

  (** [Identifiable] masks the comparison functions' [@zero_alloc] annotations *)

  include%template Comparisons.S_with_zero_alloc_strict [@mode local] with type t := t

  module Stable : sig
    module V1 : sig
      type nonrec t = t
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , sexp_grammar]

      include%template
        Stable_int63able_with_witness
        [@mode local]
        with type t := t
         and type comparator_witness = comparator_witness

      include
        Comparable.Stable.V1.With_stable_witness.S
        with type comparable := t
        with type comparator_witness := comparator_witness

      include Diffable.S_atomic with type t := t
      include Quickcheck.S with type t := t
    end

    module Option : sig
      module V1 : sig
        type t = Option.t
        [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

        include%template Stable_int63able_with_witness [@mode local] with type t := t

        include Diffable.S_atomic with type t := t
      end

      module Alternate_sexp : sig
        module V1 : sig
          type nonrec t = Option.Alternate_sexp.t
          [@@deriving bin_io, compare ~localize, hash, sexp, sexp_grammar, stable_witness]

          include
            Comparator.Stable.V1.S
            with type t := t
             and type comparator_witness = Option.Alternate_sexp.comparator_witness

          include
            Comparable.Stable.V1.With_stable_witness.S
            with type comparable := t
            with type comparator_witness := comparator_witness

          include Diffable.S_atomic with type t := t
        end
      end
    end

    (** Provides a sexp representation that is independent of the time zone of the machine
        writing it. *)
    module Alternate_sexp : sig
      module V1 : sig
        type t = Alternate_sexp.t
        [@@deriving
          bin_io
          , compare ~localize
          , equal ~localize
          , hash
          , sexp
          , sexp_grammar
          , stable_witness]

        include
          Comparator.Stable.V1.S
          with type t := t
           and type comparator_witness = Alternate_sexp.comparator_witness

        include
          Comparable.Stable.V1.With_stable_witness.S
          with type comparable := t
          with type comparator_witness := comparator_witness

        include Diffable.S_atomic with type t := t
      end
    end

    module Span : sig
      module V1 : sig
        type nonrec t = Span.t
        [@@deriving
          bin_io ~localize
          , compare ~localize
          , equal ~localize
          , globalize
          , hash
          , sexp_grammar]

        include%template
          Stable_int63able.With_stable_witness.S [@mode local] with type t := t

        include Diffable.S_atomic with type t := t
      end

      module V2 : sig
        type t = Span.t
        [@@deriving
          bin_io ~localize
          , compare ~localize
          , equal ~localize
          , globalize
          , hash
          , sexp_grammar]

        type nonrec comparator_witness = Span.comparator_witness

        include%template
          Stable_int63able.With_stable_witness.S
          [@mode local]
          with type t := t
          with type comparator_witness := comparator_witness

        include
          Comparable.Stable.V1.With_stable_witness.S
          with type comparable := t
          with type comparator_witness := comparator_witness

        include Stringable.S with type t := t
        include Diffable.S_atomic with type t := t
      end

      module Option : sig
        module V1 : sig
          type t = Span.Option.t
          [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

          include%template Stable_int63able_with_witness [@mode local] with type t := t

          include Diffable.S_atomic with type t := t
        end

        module V2 : sig
          type t = Span.Option.t
          [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

          include%template Stable_int63able_with_witness [@mode local] with type t := t

          include Diffable.S_atomic with type t := t
        end
      end
    end

    module Ofday : sig
      module V1 : sig
        type t = Ofday.t
        [@@deriving
          bin_io ~localize
          , compare ~localize
          , equal ~localize
          , globalize
          , hash
          , sexp_grammar]

        include%template
          Stable_int63able.With_stable_witness.S
          [@mode local]
          with type t := t
           and type comparator_witness = Ofday.comparator_witness

        include Diffable.S_atomic with type t := t
      end

      module Zoned : sig
        module V1 : sig
          type nonrec t = Ofday.Zoned.t [@@deriving equal ~localize, hash, sexp_grammar]

          include%template
            Stable_without_comparator_with_witness [@mode local] with type t := t
        end
      end

      module Option : sig
        module V1 : sig
          type t = Ofday.Option.t
          [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

          include%template Stable_int63able_with_witness [@mode local] with type t := t

          include Diffable.S_atomic with type t := t
        end
      end
    end

    module Zone : module type of struct
      include Timezone.Stable
    end
  end

  val arg_type : t Command.Arg_type.t

  val interruptible_pause : [ `Use_Time_ns_unix ]
  [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val pause : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val pause_forever : [ `Use_Time_ns_unix ]
  [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
end
