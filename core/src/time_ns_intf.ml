open! Import

module Rounding_direction = struct
  type t =
    | Down
    | Nearest
    | Up
    | Zero
  [@@deriving equal, enumerate, sexp_of]
end

module type Span = sig
  (** [t] is immediate on 64bit boxes and so plays nicely with the GC write barrier. *)
  type t = private Int63.t [@@deriving hash]

  include Span_intf.S with type underlying = Int63.t and type t := t

  val of_sec_with_microsecond_precision : float -> t
  val of_int_us : int -> t
  val of_int_ms : int -> t
  val to_int_us : t -> int
  val to_int_ms : t -> int
  val to_int_sec : t -> int

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
  val scale_int : t -> int -> t

  (** overflows silently *)
  val scale_int63 : t -> Int63.t -> t

  (** Rounds down, and raises unless denominator is positive. *)
  val div : t -> t -> Int63.t

  (** Fast, implemented as the identity function. *)
  val to_int63_ns : t -> Int63.t

  (** Fast, implemented as the identity function. *)
  val of_int63_ns : Int63.t -> t

  (** Will raise on 32-bit platforms.  Consider [to_int63_ns] instead. *)
  val to_int_ns : t -> int

  val of_int_ns : int -> t
  val since_unix_epoch : unit -> t
  val random : ?state:Random.State.t -> unit -> t

  (** WARNING!!! [to_span] and [of_span] both round to the nearest 1us.

      Around 135y magnitudes [to_span] and [of_span] raise.
  *)
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
    val ( / ) : t -> float -> t
    val ( // ) : t -> t -> float
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t

    (** alias for [neg] *)
    val ( ~- ) : t -> t

    (** alias for [scale] *)
    val ( *. ) : t -> float -> t

    (** alias for [scale_int] *)
    val ( * ) : t -> int -> t

    include Comparisons.Infix with type t := t
  end

  (** [Span.Option.t] is like [Span.t option], except that the value is immediate on
      architectures where [Int63.t] is immediate.  This module should mainly be used to
      avoid allocations. *)
  module Option : sig
    include Immediate_option.S_int63 with type value := t
    include Identifiable.S with type t := t
    include Diffable.S_atomic with type t := t
    include Quickcheck.S with type t := t

    module Stable : sig
      module V1 : sig
        include Stable_int63able.With_stable_witness.S with type t = t
        include Diffable.S_atomic with type t := t
      end

      module V2 : sig
        include Stable_int63able.With_stable_witness.S with type t = t
        include Diffable.S_atomic with type t := t
      end
    end
  end

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving hash, equal, sexp_grammar]

      include Stable_int63able.With_stable_witness.S with type t := t
      include Diffable.S_atomic with type t := t
    end

    module V2 : sig
      type nonrec t = t [@@deriving hash, equal, sexp_grammar]
      type nonrec comparator_witness = comparator_witness

      include
        Stable_int63able.With_stable_witness.S
          with type t := t
          with type comparator_witness := comparator_witness

      include
        Comparable.Stable.V1.With_stable_witness.S
          with type comparable := t
          with type comparator_witness := comparator_witness

      include Stringable.S with type t := t
      include Diffable.S_atomic with type t := t
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

  (** String and sexp output takes the form 'HH:MM:SS.sssssssss'; see
      {!Core.Ofday_intf} for accepted input. If input includes more than 9 decimal
      places in seconds, rounds to the nearest nanosecond, with the midpoint rounded up.
      Allows 60[.sss...] seconds for leap seconds but treats it as exactly 60s regardless
      of fractional part. *)
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

  (** [sub_exn t span] shifts the time of day [t] back by [span]. It raises if the result
      is not in the same 24-hour day. Daylight savings shifts are not accounted for. *)
  val sub_exn : t -> Span.t -> t

  (** [every span ~start ~stop] returns a sorted list of all [t]s that can be expressed as
      [start + (i * span)] without overflow, and satisfying [t >= start && t <= stop].

      If [span <= Span.zero || start > stop], returns an Error.

      The result never crosses the midnight boundary. Constructing a list crossing
      midnight, e.g. every hour from 10pm to 2am, requires multiple calls to [every]. *)
  val every : Span.t -> start:t -> stop:t -> t list Or_error.t

  val to_microsecond_string : t -> string

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving equal, hash, sexp_grammar]

      include
        Stable_int63able.With_stable_witness.S
          with type t := t
           and type comparator_witness = comparator_witness

      include Diffable.S_atomic with type t := t
    end
  end

  val arg_type : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
  val now : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val of_ofday_float_round_nearest : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val of_ofday_float_round_nearest_microsecond : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val to_ofday_float_round_nearest : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val to_ofday_float_round_nearest_microsecond : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  module Option : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
  module Zoned : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
end

(** Time represented as an [Int63.t] number of nanoseconds since the epoch.

    See {!Time_ns_unix} for important user documentation.

    Internally, arithmetic is not overflow-checked. Instead, overflows are silently
    ignored as for [int] arithmetic, unless specifically documented otherwise. Conversions
    may (or may not) raise if prior arithmetic operations overflowed. *)
module type Time_ns = sig
  module Span : Span
  module Ofday : Ofday with module Span := Span

  type t = private Int63.t [@@deriving hash, typerep, bin_io]

  include Comparisons.S with type t := t

  (** Note that we expose a sexp format that is not the one exposed in [Time_ns_unix]. The
      sexp is a single atom rendered as with [to_string_utc], except that all trailing
      zeros are trimmed, rather than trimming in groups of three. *)
  module Alternate_sexp : sig
    type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp, sexp_grammar]

    include Comparable.S with type t := t
    include Diffable.S_atomic with type t := t
  end

  (** [Option.t] is like [t option], except that the value is immediate.  This module
      should mainly be used to avoid allocations. *)
  module Option : sig
    type value := t
    type t = private Span.Option.t [@@deriving compare, bin_io]

    include Immediate_option.S_int63 with type value := value with type t := t
    include Quickcheck.S with type t := t

    module Stable : sig
      module V1 : sig
        type nonrec t = t [@@deriving compare, bin_io, stable_witness]

        val to_int63 : t -> Int63.t
        val of_int63_exn : Int63.t -> t
      end
    end

    val sexp_of_t : [ `Use_Time_ns_unix ]
      [@@deprecated "[since 2023-09] Use [Time_ns_unix.Option]"]

    include Comparisons.S with type t := t

    (** Just like [Time_ns.Alternate_sexp], this is different from the sexp format exposed
        in [Time_ns_unix].  See the comment above. *)
    module Alternate_sexp : sig
      type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp, sexp_grammar]

      include Comparable.S with type t := t
      include Diffable.S_atomic with type t := t
    end
  end

  include
    Time_intf.Shared with type t := t with module Span := Span with module Ofday := Ofday

  val of_string : string -> t
    [@@deprecated
      "[since 2021-04] Use [of_string_with_utc_offset] or [Time_ns_unix.of_string]"]

  (** [of_string_with_utc_offset] requires its input to have an explicit
      UTC offset, e.g. [2000-01-01 12:34:56.789012-23], or use the UTC zone, "Z",
      e.g. [2000-01-01 12:34:56.789012Z]. *)
  val of_string_with_utc_offset : string -> t

  val to_string : t -> string
    [@@deprecated "[since 2021-04] Use [to_string_utc] or [Time_ns_unix.to_string]"]

  (** [to_string_utc] generates a time string with the UTC zone, "Z", e.g. [2000-01-01
      12:34:56.789012Z]. *)
  val to_string_utc : t -> string

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
  val now : unit -> t

  (** overflows silently *)
  val add : t -> Span.t -> t

  (** As [add]; rather than over/underflowing, clamps the result to the closed interval
      between [min_value_representable] and [max_value_representable]. *)
  val add_saturating : t -> Span.t -> t

  (** As [sub]; rather than over/underflowing, clamps the result to the closed interval
      between [min_value_representable] and [max_value_representable]. *)
  val sub_saturating : t -> Span.t -> t

  (** overflows silently *)
  val sub : t -> Span.t -> t

  (** overflows silently *)
  val next : t -> t

  (** overflows silently *)
  val prev : t -> t

  (** overflows silently *)
  val diff : t -> t -> Span.t

  (** overflows silently *)
  val abs_diff : t -> t -> Span.t

  val to_span_since_epoch : t -> Span.t
  val of_span_since_epoch : Span.t -> t
  val to_int63_ns_since_epoch : t -> Int63.t
  val of_int63_ns_since_epoch : Int63.t -> t

  (** Will raise on 32-bit platforms.  Consider [to_int63_ns_since_epoch] instead. *)
  val to_int_ns_since_epoch : t -> int

  val of_int_ns_since_epoch : int -> t

  (** [next_multiple ~base ~after ~interval] returns the smallest [time] of the form:

      {[
        time = base + k * interval
      ]}

      where [k >= 0] and [time > after].  It is an error if [interval <= 0].

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
        time = base + k * interval
      ]}

      where [k >= 0] and [time < before].  It is an error if [interval <= 0].

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
  val of_time_float_round_nearest : Time_float.t -> t
  val of_time_float_round_nearest_microsecond : Time_float.t -> t

  module Utc : sig
    (** [to_date_and_span_since_start_of_day] computes the date and intraday-offset of a
        time in UTC.  It may be slower than [Core.Time_ns.to_date_ofday], as this
        function does not cache partial results while the latter does. *)
    val to_date_and_span_since_start_of_day : t -> Date0.t * Span.t

    (** The inverse of [to_date_and_span_since_start_of_day]. *)
    val of_date_and_span_since_start_of_day : Date0.t -> Span.t -> t
  end

  module O : sig
    (** alias for [add] *)
    val ( + ) : t -> Span.t -> t

    (** alias for [diff] *)
    val ( - ) : t -> t -> Span.t

    include Comparisons.Infix with type t := t
  end

  module Stable : sig
    module V1 : sig end
    [@@deprecated "[since 2021-03] Use [Time_ns_unix] or [Time_ns.Alternate_sexp]"]

    module Option : sig
      module V1 : sig
        type nonrec t = Option.t [@@deriving compare, bin_io, stable_witness]

        val to_int63 : t -> Int63.t
        val of_int63_exn : Int63.t -> t
      end

      module Alternate_sexp : sig
        module V1 : sig
          type nonrec t = Option.Alternate_sexp.t
          [@@deriving bin_io, compare, hash, sexp, sexp_grammar, stable_witness]

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

    module Alternate_sexp : sig
      module V1 : sig
        type t = Alternate_sexp.t
        [@@deriving bin_io, compare, equal, hash, sexp, sexp_grammar, stable_witness]

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
        type nonrec t = Span.t [@@deriving hash, equal, sexp_grammar]

        include Stable_int63able.With_stable_witness.S with type t := t
        include Diffable.S_atomic with type t := t
      end

      module Option : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

      module V2 : sig
        type t = Span.t [@@deriving hash, equal, sexp_grammar]
        type nonrec comparator_witness = Span.comparator_witness

        include
          Stable_int63able.With_stable_witness.S
            with type t := t
            with type comparator_witness := comparator_witness

        include
          Comparable.Stable.V1.With_stable_witness.S
            with type comparable := t
            with type comparator_witness := comparator_witness

        include Stringable.S with type t := t
        include Diffable.S_atomic with type t := t
      end
    end

    module Ofday : sig
      module V1 : sig
        type t = Ofday.t [@@deriving equal, hash, sexp_grammar]

        include
          Stable_int63able.With_stable_witness.S
            with type t := t
             and type comparator_witness = Ofday.comparator_witness

        include Diffable.S_atomic with type t := t
      end

      module Option : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
      module Zoned : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
    end
  end

  module Hash_queue : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
  module Hash_set : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
  module Map : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  module Replace_polymorphic_compare : sig end
  [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  module Set : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
  module Table : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
  module Zone : sig end [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val arg_type : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val comparator : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val get_sexp_zone : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val interruptible_pause : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val of_date_ofday_zoned : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val of_string_abs : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val of_string_fix_proto : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val pause : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val pause_forever : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val pp : [ `Use_Time_ns_unix ] [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val set_sexp_zone : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val sexp_of_t : [ `Use_Time_ns_unix_or_Time_ns_alternate_sexp ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix] or [Time_ns.Alternate_sexp]"]

  val sexp_of_t_abs : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val t_of_sexp : [ `Use_Time_ns_unix_or_Time_ns_alternate_sexp ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix] or [Time_ns.Alternate_sexp]"]

  val t_of_sexp_abs : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val to_date_ofday_zoned : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val to_ofday_zoned : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val to_string_fix_proto : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val validate_bound : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val validate_lbound : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]

  val validate_ubound : [ `Use_Time_ns_unix ]
    [@@deprecated "[since 2021-03] Use [Time_ns_unix]"]
end
