open! Import
open! Import_time

module type Option = sig
  include Immediate_option.S_int63
  include Identifiable with type t := t
end

module type Span = sig
  type t = Core_kernel.Time_ns.Span.t [@@deriving typerep, sexp_of]

  include Identifiable         with type t := t
  include Comparable.With_zero with type t := t

  val arg_type : t Core_kernel.Command.Arg_type.t

  (** Similar to {!Time.Span.Parts}. *)
  module Parts : sig
    type t = private
      { sign : Sign.t
      ; hr   : int
      ; min  : int
      ; sec  : int
      ; ms   : int
      ; us   : int
      ; ns   : int
      }
    [@@deriving sexp]
  end

  val nanosecond  : t
  val microsecond : t
  val millisecond : t
  val second      : t
  val minute      : t
  val hour        : t
  val day         : t

  val of_ns  : float -> t
  val of_us  : float -> t
  val of_ms  : float -> t
  val of_sec : float -> t
  val of_min : float -> t
  val of_hr  : float -> t
  val of_day : float -> t
  val to_ns  : t     -> float
  val to_us  : t     -> float
  val to_ms  : t     -> float
  val to_sec : t     -> float
  val to_min : t     -> float
  val to_hr  : t     -> float
  val to_day : t     -> float

  val of_int_us  : int -> t
  val of_int_ms  : int -> t
  val of_int_sec : int -> t
  val to_int_us  : t -> int
  val to_int_ms  : t -> int
  val to_int_sec : t -> int

  val zero : t
  val min_value : t
  val max_value : t
  val ( + ) : t -> t -> t (** overflows silently *)

  val ( - ) : t -> t -> t (** overflows silently *)

  val abs : t -> t
  val neg : t -> t
  val scale     : t -> float -> t
  val scale_int : t -> int   -> t (** overflows silently *)

  val div : t -> t -> Int63.t
  val ( / ) : t -> float -> t
  val ( // ) : t -> t -> float

  (** Overflows silently. *)
  val create
    :  ?sign : Sign.t
    -> ?day : int
    -> ?hr  : int
    -> ?min : int
    -> ?sec : int
    -> ?ms  : int
    -> ?us  : int
    -> ?ns  : int
    -> unit
    -> t

  val to_short_string : t -> string
  val randomize : t -> percent : Percent.t -> t

  val to_parts : t -> Parts.t

  val to_unit_of_time : t -> Unit_of_time.t
  val of_unit_of_time : Unit_of_time.t -> t

  (** See [Time.Span.to_string_hum]. *)
  val to_string_hum
    :  ?delimiter:char              (** defaults to ['_'] *)
    -> ?decimals:int                (** defaults to 3 *)
    -> ?align_decimal:bool          (** defaults to [false] *)
    -> ?unit_of_time:Unit_of_time.t (** defaults to [to_unit_of_time t] *)
    -> t
    -> string

  (** See [Core_kernel.Time_ns.Span]. *)

  val to_span : t -> Time.Span.t
  [@@deprecated
    "[since 2019-01] use [to_span_float_round_nearest] or \
     [to_span_float_round_nearest_microsecond]"]

  val of_span : Time.Span.t -> t
  [@@deprecated
    "[since 2019-01] use [of_span_float_round_nearest] or \
     [of_span_float_round_nearest_microsecond]"]

  val to_span_float_round_nearest : t -> Time.Span.t
  val to_span_float_round_nearest_microsecond : t -> Time.Span.t
  val of_span_float_round_nearest : Time.Span.t -> t
  val of_span_float_round_nearest_microsecond : Time.Span.t -> t

  include Robustly_comparable with type t := t

  val to_int63_ns : t -> Int63.t (** Fast, implemented as the identity function. *)

  val of_int63_ns : Int63.t -> t (** Somewhat fast, implemented as a range check. *)

  (** Will raise on 32-bit platforms with spans corresponding to contemporary {!now}.
      Consider [to_int63_ns] instead. *)
  val to_int_ns : t   -> int
  val of_int_ns : int -> t

  (** The only condition [to_proportional_float] is supposed to satisfy is that for all
      [t1, t2 : t]: [to_proportional_float t1 /. to_proportional_float t2 = t1 // t2]. *)
  val to_proportional_float : t -> float

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving hash]
      include Stable_int63able with type t := t
    end
    module V2 : sig
      type nonrec t = t [@@deriving hash]
      include Stable_int63able with type t := t
    end
  end

  val random : ?state:Random.State.t -> unit -> t

  (** [Span.Option.t] is like [Span.t option], except that the value is immediate on
      architectures where [Int63.t] is immediate.  This module should mainly be used to
      avoid allocations. *)
  module Option : sig
    include Option with type value := t
    module Stable : sig
      module V1 : Stable_int63able with type t = t
      module V2 : Stable_int63able with type t = t
    end
  end
end

module type Ofday = sig
  include module type of struct include Time_ns.Ofday end

  val arg_type : t Core_kernel.Command.Arg_type.t

  val now : zone:Time.Zone.t -> t

  val to_ofday : t -> Time.Ofday.t
  [@@deprecated
    "[since 2019-01] use [to_ofday_float_round_nearest] or \
     [to_ofday_float_round_nearest_microsecond]"]

  val of_ofday : Time.Ofday.t -> t
  [@@deprecated
    "[since 2019-01] use [of_ofday_float_round_nearest] or \
     [of_ofday_float_round_nearest_microsecond]"]

  val to_ofday_float_round_nearest : t -> Time.Ofday.t
  val to_ofday_float_round_nearest_microsecond : t -> Time.Ofday.t
  val of_ofday_float_round_nearest : Time.Ofday.t -> t
  val of_ofday_float_round_nearest_microsecond : Time.Ofday.t -> t

  module Zoned : sig
    (** Sexps look like "(12:01 nyc)"

        Two [t]'s may or may not correspond to the same times depending on which date
        they're evaluated. *)
    type t [@@deriving bin_io, sexp, hash]

    include Pretty_printer.S with type t := t
    include Stringable       with type t := t (** Strings look like "12:01 nyc" *)

    val arg_type : t Core_kernel.Command.Arg_type.t

    val create       : Time_ns.Ofday.t -> Time.Zone.t -> t
    val create_local : Time_ns.Ofday.t                -> t

    val ofday : t -> Time_ns.Ofday.t
    val zone  : t -> Time.Zone.t

    val to_time_ns : t -> Date.t -> Time_ns.t

    module With_nonchronological_compare : sig
      (** It is possible to consistently compare [t]'s, but due to the complexities of
          time zones and daylight savings, the resulting ordering is not chronological.
          That is, [compare t1 t2 > 0] does not imply [t2] occurs after [t1] every day,
          or any day. *)
      type nonrec t = t [@@deriving bin_io, sexp, compare, hash]
    end
  end

  module Option : sig
    include Option with type value := t
    module Stable : sig
      module V1 : Stable_int63able with type t = t
    end
  end
end

(** An absolute point in time, more efficient and precise than the [float]-based {!Time},
    but representing a narrower range of times.

    This module represents absolute times with nanosecond precision, approximately between
    the years 1823 and 2116 CE.

    You should normally default to using [Time] instead of this module!  The reasons are:

    - Many functions around our libraries expect [Time.t] values, so it will likely be
      much more convenient for you.

    - It leads to greater consistency across different codebases.  It would be bad to end
      up with half our libraries expecting [Time.t] and the other half expecting
      [Time_ns.t].

    - [Time_ns] silently ignores overflow.

    Some reasons you might want want to actually prefer [Time_ns.t] in certain cases:

    - It has superior performance.

    - It uses [int]s rather than [float]s internally, which makes certain things easier to
      reason about, since [int]s respect a bunch of arithmetic identities that [float]s
      don't, e.g., [x + (y + z) = (x + y) + z].

    Neither [Core.Time_ns] nor [Core.Time] are available in Javascript.

    All in all, it would have been nice to have chosen [Time_ns.t] to begin with, but
    we're unlikely to flip everything to [Time_ns.t] in the short term (see comment at the
    end of [time_ns.ml]).

    See {!Core_kernel.Time_ns} for additional low level documentation. *)
module type Time_ns = sig

  type t = Core_kernel.Time_ns.t [@@deriving typerep]

  module Span : Span

  val arg_type : t Core_kernel.Command.Arg_type.t

  (** [Option.t] is like [t option], except that the value is immediate.  This module
      should mainly be used to avoid allocations. *)
  module Option : sig
    include Option with type value := t
    module Stable : sig
      module V1 : Stable_int63able with type t = t
    end
  end

  (** See {!Time.Ofday}. *)
  module Ofday : Ofday


  (** String conversions use the local timezone by default. Sexp conversions use
      [get_sexp_zone ()] by default, which can be overridden by calling [set_sexp_zone].
      These default time zones are used when writing a time, and when reading a time with
      no explicit zone or UTC offset.

      Sexps and strings display the date, ofday, and UTC offset of [t] relative to the
      appropriate time zone. *)
  include Identifiable with type t := t

  module Zone : module type of Time.Zone with type t = Time.Zone.t

  (** These functions are identical to those in [Time] and get/set the same variable. *)

  val get_sexp_zone : unit -> Zone.t
  val set_sexp_zone : Zone.t -> unit

  (** [t_of_sexp_abs sexp] as [t_of_sexp], but demands that [sexp] indicate the timezone
      the time is expressed in. *)
  val t_of_sexp_abs : Sexp.t -> t
  val sexp_of_t_abs : t -> zone:Zone.t -> Sexp.t

  val epoch : t (** Unix epoch (1970-01-01 00:00:00 UTC) *)

  val min_value : t
  val max_value : t

  val now : unit -> t

  val add      : t -> Span.t -> t (** overflows silently *)

  val sub      : t -> Span.t -> t (** overflows silently *)

  val next : t -> t (** overflows silently *)

  val prev : t -> t (** overflows silently *)

  val diff     : t -> t -> Span.t (** overflows silently *)

  val abs_diff : t -> t -> Span.t (** overflows silently *)

  val to_span_since_epoch : t -> Span.t
  val of_span_since_epoch : Span.t -> t

  val to_time : t -> Time.t
  [@@deprecated
    "[since 2019-01] use [to_time_float_round_nearest] or \
     [to_time_float_round_nearest_microsecond]"]

  val of_time : Time.t -> t
  [@@deprecated
    "[since 2019-01] use [of_time_float_round_nearest] or \
     [of_time_float_round_nearest_microsecond]"]

  val to_time_float_round_nearest : t -> Time.t
  val to_time_float_round_nearest_microsecond : t -> Time.t
  val of_time_float_round_nearest : Time.t -> t
  val of_time_float_round_nearest_microsecond : Time.t -> t

  val to_string_fix_proto : [ `Utc | `Local ] -> t -> string
  val of_string_fix_proto : [ `Utc | `Local ] -> string -> t

  (** [to_string_abs ~zone t] is the same as [to_string t] except that it uses the given
      time zone. *)
  val to_string_abs         : t -> zone:Zone.t -> string

  (** [to_string_abs_trimmed] is the same as [to_string_abs], but drops trailing seconds
      and milliseconds if they are 0. *)
  val to_string_abs_trimmed : t -> zone:Zone.t -> string

  val to_string_abs_parts   : t -> zone:Zone.t -> string list

  (** This is like [of_string] except that if the string doesn't specify the zone then it
      raises rather than assume the local timezone. *)
  val of_string_abs : string -> t

  (** Same as [to_string_abs_trimmed], except it leaves off the timezone, so won't
      reliably round trip. *)
  val to_string_trimmed : t -> zone:Zone.t -> string

  (** Same as [to_string_abs], but without milliseconds *)
  val to_sec_string : t -> zone:Zone.t -> string

  (** [of_localized_string ~zone str] read in the given string assuming that it represents
      a time in zone and return the appropriate Time_ns.t *)
  val of_localized_string : zone:Zone.t -> string -> t

  (** [of_string_gen ~if_no_timezone s] attempts to parse [s] to a [t].  If [s] doesn't
      supply a time zone [if_no_timezone] is consulted. *)
  val of_string_gen
    :  if_no_timezone:[ `Fail | `Local | `Use_this_one of Zone.t ]
    -> string
    -> t

  (** [to_string_iso8601_basic] returns a string representation of the following form:
      %Y-%m-%dT%H:%M:%S.%s%Z
      e.g.
      [ to_string_iso8601_basic ~zone:Time.Zone.utc epoch
      = "1970-01-01T00:00:00.000000000Z" ]
  *)
  val to_string_iso8601_basic : t -> zone:Zone.t -> string

  val to_int63_ns_since_epoch : t -> Int63.t
  val of_int63_ns_since_epoch : Int63.t -> t

  (** Will raise on 32-bit platforms.  Consider [to_int63_ns_since_epoch] instead. *)
  val to_int_ns_since_epoch : t -> int
  val of_int_ns_since_epoch : int -> t

  (** [to_filename_string t ~zone] converts [t] to string with format
      YYYY-MM-DD_HH-MM-SS.mmm which is suitable for using in filenames. *)
  val to_filename_string : t      -> zone:Zone.t -> string

  (** [of_filename_string s ~zone] converts [s] that has format YYYY-MM-DD_HH-MM-SS.mmm
      into time_ns. *)
  val of_filename_string : string -> zone:Zone.t -> t

  (** See [Core_kernel.Time_ns].

      Overflows silently. *)
  val next_multiple
    :  ?can_equal_after:bool  (** default is [false] *)
    -> base:t
    -> after:t
    -> interval:Span.t
    -> unit
    -> t

  (** See [Core_kernel.Time_ns].

      Overflows silently. *)
  val prev_multiple
    :  ?can_equal_before:bool  (** default is [false] *)
    -> base:t
    -> before:t
    -> interval:Span.t
    -> unit
    -> t

  val of_date_ofday : zone:Zone.t -> Date.t -> Ofday.t -> t

  (** Because timezone offsets change throughout the year (clocks go forward or back) some
      local times can occur twice or not at all.  In the case that they occur twice, this
      function gives [`Twice] with both occurrences in order; if they do not occur at all,
      this function gives [`Never] with the time at which the local clock skips over the
      desired time of day.

      Note that this is really only intended to work with DST transitions and not unusual or
      dramatic changes, like the calendar change in 1752 (run "cal 9 1752" in a shell to
      see).  In particular it makes the assumption that midnight of each day is unambiguous.

      Most callers should use {!of_date_ofday} rather than this function.  In the [`Twice]
      and [`Never] cases, {!of_date_ofday} will return reasonable times for most uses. *)
  val of_date_ofday_precise
    :  Date.t
    -> Ofday.t
    -> zone:Zone.t
    -> [ `Once of t | `Twice of t * t | `Never of t ]

  val to_ofday : t -> zone:Zone.t -> Ofday.t

  (** Always returns the [Date.t * Ofday.t] that [to_date_ofday] would have returned, and in
      addition returns a variant indicating whether the time is associated with a time zone
      transition.

      {v
      - `Only         -> there is a one-to-one mapping between [t]'s and
                         [Date.t * Ofday.t] pairs
      - `Also_at      -> there is another [t] that maps to the same [Date.t * Ofday.t]
                         (this date/time pair happened twice because the clock fell back)
      - `Also_skipped -> there is another [Date.t * Ofday.t] pair that never happened (due
                         to a jump forward) that [of_date_ofday] would map to the same
                         [t].
    v}
  *)
  val to_date_ofday_precise
    :  t
    -> zone:Zone.t
    -> Date.t * Ofday.t
       * [ `Only
         | `Also_at of t
         | `Also_skipped of Date.t * Ofday.t
         ]

  val to_date  : t -> zone:Zone.t -> Date.t
  val to_date_ofday: t -> zone:Zone.t -> Date.t * Ofday.t
  val occurrence
    :  [ `First_after_or_at | `Last_before_or_at ]
    -> t
    -> ofday:Ofday.t
    -> zone:Time.Zone.t
    -> t

  (** For performance testing only; [reset_date_cache ()] resets an internal cache used to
      speed up [to_date] and related functions when called repeatedly on times that fall
      within the same day. *)
  val reset_date_cache : unit -> unit

  (** It's unspecified what happens if the given date/ofday/zone correspond to more than
      one date/ofday pair in the other zone. *)
  val convert
    :  from_tz:Zone.t
    -> to_tz:Zone.t
    -> Date.t
    -> Ofday.t
    -> (Date.t * Ofday.t)

  val utc_offset
    :  t
    -> zone:Zone.t
    -> Span.t

  (** [pause span] sleeps for [span] time. *)
  val pause : Span.t -> unit

  (** [interruptible_pause span] sleeps for [span] time unless interrupted (e.g. by
      delivery of a signal), in which case the remaining unslept portion of time is
      returned. *)
  val interruptible_pause : Span.t -> [ `Ok | `Remaining of Span.t ]

  (** [pause_forever] sleeps indefinitely. *)
  val pause_forever : unit -> never_returns

  module Stable : sig
    module V1 : Stable_int63able with type t = t
    module Option : sig
      module V1 : Stable_int63able with type t = Option.t
    end
    module Span : sig
      module V1 : sig
        type t = Span.t [@@deriving hash]
        include Stable_int63able with type t := t
      end
      module V2 : sig
        type t = Span.t [@@deriving hash]
        include Stable_int63able with type t := t
      end
      module Option : sig
        module V1 : Stable_int63able with type t = Span.Option.t
        module V2 : Stable_int63able with type t = Span.Option.t
      end
    end
    module Ofday : sig
      module V1 : Stable_int63able with type t = Ofday.t
      module Zoned : sig
        module V1 : sig
          type nonrec t = Ofday.Zoned.t [@@deriving hash]
          include Stable_without_comparator with type t := t
        end
      end
      module Option : sig
        module V1 : Stable_int63able with type t = Ofday.Option.t
      end
    end
  end

  val random : ?state:Random.State.t -> unit -> t

end
