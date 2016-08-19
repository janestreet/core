(** A module for representing absolute points in time, independent of time zone.

    Note that on 32bit architecture, most functions will raise when used on time
    outside the range [1901-12-13 20:45:52 - 2038-01-19 03:14:07].
*)
open! Core_kernel.Std


module Ofday : sig
  include (module type of struct include Ofday end
            with module Zoned := Ofday.Zoned)

  module Zoned : sig
    include module type of struct include Ofday.Zoned end

    val to_time : t -> Date0.t -> Time_internal.T.t
  end

  val now : zone:Zone.t -> t
end

module Span : sig
  include (module type of Span
            with type t = Span.t
            with module Parts := Span.Parts)

  module Parts : sig
    type t = Span.Parts.t = private {
      sign : Sign.t;
      hr   : int;
      min  : int;
      sec  : int;
      ms   : int;
      us   : int;
    }
    [@@deriving sexp]
  end
end

module Zone : module type of Zone with type t = Zone.t

(** A fully qualified point in time, independent of timezone. *)
type t = Time_internal.T.t [@@deriving bin_io, hash, sexp]

(** Sexp conversions use the local timezone by default. This can be overridden by calling
    [set_sexp_zone]. *)
val get_sexp_zone : unit -> Zone.t
val set_sexp_zone : Zone.t -> unit

include Hashable_binable    with type t := t
include Comparable_binable  with type t := t
include Robustly_comparable with type t := t
include Floatable           with type t := t
include Pretty_printer.S    with type t := t

(** The [{to,of}_string] functions in [Time] will produce times with time zone
    indications, but are generous in what they will read in.  String/Sexp.t
    representations without time zone indications are assumed to be in the machine's local
    zone. *)
include Stringable          with type t := t

(** {5 values} *)

(** Unlike [Time_ns], this module purposely omits [max_value] and [min_value]:
    1. They produce unintuitive corner cases because most people's mental models of time
    do not include +/- infinity as concrete values
    2. In practice, when people ask for these values, it is for questionable uses, e.g.,
    as null values to use in place of explicit options. *)

(** midnight, Jan 1, 1970 in UTC *)
val epoch : t


(** {6 Basic operations on times} *)

(** [add t s] adds the span [s] to time [t] and returns the resulting time.

    NOTE: adding spans as a means of adding days is not accurate, and may run into trouble
    due to shifts in daylight savings time, float arithmetic issues, and leap seconds.
    See the comment at the top of Zone.mli for a more complete discussion of some of
    the issues of time-keeping.  For spans that cross date boundaries, use date functions
    instead.
*)
val add : t -> Span.t -> t

(** [sub t s] subtracts the span [s] from time [t] and returns the
    resulting time.  See important note for [add]. *)
val sub : t -> Span.t -> t

(** [diff t1 t2] returns time [t1] minus time [t2]. *)
val diff : t -> t -> Span.t

(** [abs_diff t1 t2] returns the absolute span of time [t1] minus time [t2]. *)
val abs_diff : t -> t -> Span.t

(** {6 Comparisons} *)

(** [is_earlier] and [is_later] are like using [<.], but easier to read. *)
val is_earlier : t -> than:t -> bool
val is_later   : t -> than:t -> bool

(** {6 Constants} *)

(** {6 Conversions} *)
(** All these conversion functions use the current time zone. Unless marked _utc,
    in which case they use Universal Coordinated Time *)

val of_date_ofday : Date0.t -> Ofday.t -> zone:Zone.t -> t
val to_date_ofday : t -> zone:Zone.t -> Date0.t * Ofday.t
val to_date       : t -> zone:Zone.t -> Date0.t
val to_ofday      : t -> zone:Zone.t -> Ofday.t

(** [of_tm] converts a [Unix.tm] (mirroring a [struct tm] from the C stdlib) into a
    [Time.t].  Note that the [tm_wday], [tm_yday], and [tm_isdst] fields are ignored. *)
val of_tm : Core_unix.tm -> zone:Zone.t -> t

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
  :  Date0.t
  -> Ofday.t
  -> zone:Zone.t
  -> [ `Once of t | `Twice of t * t | `Never of t ]

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
  -> Date0.t * Ofday.t
     * [ `Only
       | `Also_at of t
       | `Also_skipped of Date0.t * Ofday.t
       ]

val convert
  :  from_tz:Zone.t
  -> to_tz:Zone.t
  -> Date0.t
  -> Ofday.t
  -> (Date0.t * Ofday.t)

val utc_offset
  :  t
  -> zone:Zone.t
  -> Span.t

(** {6 Other string conversions}  *)

(** [to_filename_string t ~zone] converts [t] to string with format
    YYYY-MM-DD_HH-MM-SS.mmm which is suitable for using in filenames. *)
val to_filename_string : t -> zone:Zone.t -> string

(** [of_filename_string s ~zone] converts [s] that has format YYYY-MM-DD_HH-MM-SS.mmm into
    time. *)
val of_filename_string : string -> zone:Zone.t -> t

val to_string_fix_proto : [`Utc | `Local] -> t -> string
val of_string_fix_proto : [`Utc | `Local] -> string -> t

(** Same as [to_string_abs], but removes trailing seconds and milliseconds
    if they are 0 *)
val to_string_trimmed : t -> zone:Zone.t -> string

(** Same as [to_string_abs], but without milliseconds *)
val to_sec_string : t -> zone:Zone.t -> string

(** [of_localized_string ~zone str] read in the given string assuming that it represents
    a time in zone and return the appropriate Time.t *)
val of_localized_string : zone:Zone.t -> string -> t

(** [to_string_abs ~zone t] returns a string that represents an absolute time, rather than
    a local time with an assumed time zone.  This string can be round-tripped, even on a
    machine in a different time zone than the machine that wrote the string.

    The string will display the date and of-day of [zone] together with [zone] as an
    offset from UTC.

    [to_string_abs_trimmed] is the same as [to_string_abs], but drops trailing seconds and
    milliseconds if they are 0.

    Note that the difference between [to_string] and [to_string_abs] is not that one
    returns an absolute time and one doesn't, but that [to_string_abs] lets you specify
    the time zone, while [to_string] takes it to be the local time zone.
*)
val to_string_abs         : t -> zone:Zone.t -> string
val to_string_abs_trimmed : t -> zone:Zone.t -> string

(** [to_string_iso8601_basic] return a string representation of the following form:
    %Y-%m-%dT%H:%M:%S.%s%Z
    e.g.
    [ to_string_iso8601_basic ~zone:Time.Zone.utc epoch = "1970-01-01T00:00:00.000000Z" ]
*)
val to_string_iso8601_basic : t -> zone:Zone.t -> string

(** [of_string_abs s] is like [of_string], but demands that [s] indicate the timezone the
    time is expressed in. *)
val of_string_abs : string -> t

(** [t_of_sexp_abs sexp] as [t_of_sexp], but demands that [sexp] indicate the timezone the
    time is expressed in. *)
val t_of_sexp_abs : Sexp.t -> t

(** {6 Miscellaneous} *)

(** @return the current time. *)
val now : unit -> t

(** [pause span] sleeps for span time. *)
val pause : Span.t -> unit

(** [interruptible_pause span] sleeps for span time unless interrupted (e.g. by delivery
    of a signal), in which case the remaining unslept portion of time is returned. *)
val interruptible_pause : Span.t -> [`Ok | `Remaining of Span.t]

(** [pause_forever] sleeps indefinitely. *)
val pause_forever : unit -> never_returns

(** [occurrence side time ~ofday ~zone] returns a [Time.t] that is the occurrence of ofday
    (in the given [zone]) that is the latest occurrence (<=) [time] or the earliest
    occurrence (>=) [time], according to [side].

    NOTE: If the given time converted to wall clock time in the given zone is equal to
    ofday then the t returned will be equal to the t given.
*)
val occurrence
  :  [ `First_after_or_at | `Last_before_or_at ]
  -> t
  -> ofday:Ofday.t
  -> zone:Zone.t
  -> t

(** [format t fmt] formats the given time according to fmt, which follows the formatting
    rules given in 'man strftime'.  The time is output in the given timezone.

    {v
      %Y - year (4 digits)
      %y - year (2 digits)
      %m - month
      %d - day
      %H - hour
      %M - minute
      %S - second
    v}

    a common choice would be: %Y-%m-%d %H:%M:%S
*)
val format : t -> string -> zone:Zone.t -> string

(** [parse string ~fmt ~zone] parses [string], according to [fmt], which follows the
    formatting rules given in 'man strptime'.  The time is assumed to be in the given
    timezone.

    {v
      %Y - year (4 digits)
      %y - year (2 digits)
      %m - month
      %d - day
      %H - hour
      %M - minute
      %S - second
    v}
*)
val parse : string -> fmt:string -> zone:Zone.t -> t

(** [to_epoch t] returns the number of seconds since Jan 1, 1970 00:00:00 in UTC *)
val to_epoch : t -> float

(** [of_epoch x] returns the time x seconds after Jan 1, 1970 00:00:00 in UTC *)
val of_epoch : float -> t

(** [next_multiple ~base ~after ~interval] returns the smallest [time] of the form:

    {[
      time = base + k * interval
    ]}

    where [k >= 0] and [time > after].  It is an error if [interval <= 0].

    Supplying [~can_equal_after:true] allows the result to satisfy [time >= after].
*)
val next_multiple
  :  ?can_equal_after:bool  (** default is [false] *)
  -> base:t
  -> after:t
  -> interval:Span.t
  -> unit
  -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t
    type nonrec comparator_witness = comparator_witness
    include Stable
      with type t := t
      with type comparator_witness := comparator_witness
    include Comparable.Stable.V1.S
      with type comparable := t
      with type comparator_witness := comparator_witness
  end

  (** Provides a sexp representation that is independent of the time zone of the machine
      writing it. *)
  module With_utc_sexp : sig
    module V1 : sig
      type nonrec t = t
      type nonrec comparator_witness = comparator_witness
      include Stable
        with type t := t
        with type comparator_witness := comparator_witness
      include Comparable.Stable.V1.S
        with type comparable := t
        with type comparator_witness := comparator_witness
    end
  end

  (** Provides a sexp representation where all sexps must include a timezone (in contrast
      to [V1] above, which will assume local timezone if it's not specified). When
      serializing, it uses the local timezone.  Concretely, this is just [V1] but
      [t_of_sexp] is replaced with [t_of_sexp_abs]. *)
  module With_t_of_sexp_abs : sig
    module V1 : sig
      type nonrec t = t
      type nonrec comparator_witness = comparator_witness
      include Stable
        with type t := t
        with type comparator_witness := comparator_witness
      include Comparable.Stable.V1.S
        with type comparable := t
        with type comparator_witness := comparator_witness
    end
  end
end
