open! Import
open! Std_internal
module Date = Date0

module type Zone = sig
  module Time : Time0_intf.S
  include Zone.S with type t = Zone.t and module Time_in_seconds := Time

  (** [abbreviation t time] returns the abbreviation name (such as EDT, EST, JST) of given
      zone [t] at [time]. This string conversion is one-way only, and cannot reliably be
      turned back into a [t]. This function reads and writes the zone's cached index. *)
  val abbreviation : t -> Time.t -> string

  (** [absolute_time_of_date_and_ofday] and [date_and_ofday_of_absolute_time] convert
      between absolute times and date + ofday forms. These are low level functions not
      intended for most clients. These functions read and write the zone's cached index.
  *)
  val absolute_time_of_date_and_ofday : t -> Time.Date_and_ofday.t -> Time.t

  val date_and_ofday_of_absolute_time : t -> Time.t -> Time.Date_and_ofday.t

  (** Takes a [Time.t] and returns the next [Time.t] strictly after it, if any, that the
      time zone UTC offset changes, and by how much it does so. *)
  val next_clock_shift : t -> strictly_after:Time.t -> (Time.t * Time.Span.t) option

  (** As [next_clock_shift], but *at or before* the given time. *)
  val prev_clock_shift : t -> at_or_before:Time.t -> (Time.t * Time.Span.t) option
end

module type Basic = sig
  module Time : Time0_intf.S

  (*_ necessary to preserve type equality with the Time functor argument *)

  include module type of struct
    include Time
  end [@ocaml.remove_aliases]

  (** [now ()] returns a [t] representing the current time *)
  val now : unit -> t

  module Zone : Zone with module Time := Time

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
end

module type Shared = sig
  type t

  include Quickcheck.S_range with type t := t

  module Span : sig
    type t
  end

  module Ofday : sig
    type t
  end

  (** {6 Comparisons} *)

  val is_earlier : t -> than:t -> bool
  val is_later : t -> than:t -> bool

  (** {6 Conversions} *)

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

  val to_date_ofday : t -> zone:Zone.t -> Date.t * Ofday.t

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
    -> Date.t * Ofday.t * [ `Only | `Also_at of t | `Also_skipped of Date.t * Ofday.t ]

  val to_date : t -> zone:Zone.t -> Date.t
  val to_ofday : t -> zone:Zone.t -> Ofday.t

  (** For performance testing only; [reset_date_cache ()] resets an internal cache used to
      speed up [to_date] and related functions when called repeatedly on times that fall
      within the same day. *)
  val reset_date_cache : unit -> unit

  (** Unlike [Time_ns], this module purposely omits [max_value] and [min_value]:
      1. They produce unintuitive corner cases because most people's mental models of time
      do not include +/- infinity as concrete values
      2. In practice, when people ask for these values, it is for questionable uses, e.g.,
      as null values to use in place of explicit options. *)

  (** midnight, Jan 1, 1970 in UTC *)
  val epoch : t

  (** It's unspecified what happens if the given date/ofday/zone correspond to more than
      one date/ofday pair in the other zone. *)
  val convert : from_tz:Zone.t -> to_tz:Zone.t -> Date.t -> Ofday.t -> Date.t * Ofday.t

  val utc_offset : t -> zone:Zone.t -> Span.t

  (** {6 Other string conversions}  *)

  (** The [{to,of}_string] functions in [Time] convert to UTC time, because a local time
      zone is not necessarily available.  They are generous in what they will read in. *)
  include Stringable with type t := t

  (** [to_filename_string t ~zone] converts [t] to string with format
      YYYY-MM-DD_HH-MM-SS.mmm which is suitable for using in filenames. *)
  val to_filename_string : t -> zone:Zone.t -> string

  (** [of_filename_string s ~zone] converts [s] that has format YYYY-MM-DD_HH-MM-SS.mmm into
      time. *)
  val of_filename_string : string -> zone:Zone.t -> t

  (** [to_string_abs ~zone t] is the same as [to_string t] except that it uses the given
      time zone. *)
  val to_string_abs : t -> zone:Zone.t -> string

  (** [to_string_abs_trimmed] is the same as [to_string_abs], but drops trailing seconds
      and milliseconds if they are 0. *)
  val to_string_abs_trimmed : t -> zone:Zone.t -> string

  val to_string_abs_parts : t -> zone:Zone.t -> string list

  (** Same as [to_string_abs_trimmed], except it leaves off the timezone, so won't
      reliably round trip. *)
  val to_string_trimmed : t -> zone:Zone.t -> string

  (** Same as [to_string_abs], but without milliseconds and the timezone. May raise if
      [zone] offsets move the apparent value beyond [min_value_representable] and
      [max_value_representable]. *)
  val to_sec_string : t -> zone:Zone.t -> string

  (** Same as [to_sec_string] but includes timezone *)
  val to_sec_string_with_zone : t -> zone:Zone.t -> string

  (** [of_localized_string ~zone str] read in the given string assuming that it represents
      a time in zone and return the appropriate Time.t *)
  val of_localized_string : zone:Zone.t -> string -> t

  (** [of_string_gen ~default_zone ~find_zone s] attempts to parse [s] as a [t], calling
      out to [default_zone] and [find_zone] as needed. *)
  val of_string_gen
    :  default_zone:(unit -> Zone.t)
    -> find_zone:(string -> Zone.t)
    -> string
    -> t

  (** [to_string_iso8601_basic] return a string representation of the following form:
      %Y-%m-%dT%H:%M:%S.%s%Z
      e.g.
      [ to_string_iso8601_basic ~zone:Time.Zone.utc epoch = "1970-01-01T00:00:00.000000Z" ]
  *)
  val to_string_iso8601_basic : t -> zone:Zone.t -> string

  (** [occurrence side time ~ofday ~zone] returns a [Time.t] that is the occurrence of
      ofday (in the given [zone]) that is the latest occurrence (<=) [time] or the
      earliest occurrence (>=) [time], according to [side].

      NOTE: If the given time converted to wall clock time in the given zone is equal to
      ofday then the t returned will be equal to the t given.
  *)
  val occurrence
    :  [ `First_after_or_at | `Last_before_or_at ]
    -> t
    -> ofday:Ofday.t
    -> zone:Zone.t
    -> t
end

module type S = sig
  include Basic
  include Shared with type t := t with module Span := Span with module Ofday := Ofday

  val of_string : string -> t
    [@@deprecated
      "[since 2021-04] Use [of_string_with_utc_offset] or [Time_float_unix.of_string]"]

  (** [of_string_with_utc_offset] requires its input to have an explicit
      UTC offset, e.g. [2000-01-01 12:34:56.789012-23], or use the UTC zone, "Z",
      e.g. [2000-01-01 12:34:56.789012Z]. *)
  val of_string_with_utc_offset : string -> t

  val to_string : t -> string
    [@@deprecated "[since 2021-04] Use [to_string_utc] or [Time_float_unix.to_string]"]

  (** [to_string_utc] generates a time string with the UTC zone, "Z", e.g. [2000-01-01
      12:34:56.789012Z]. *)
  val to_string_utc : t -> string
end

module type Time = sig
  module type S = S

  module Make (Time : Time0_intf.S) : S with module Time := Time
end
