(** A module for representing absolute points in time, independent of time zone. *)
open Core_kernel.Std


module Ofday : sig
  include (module type of Ofday
            with type t = Ofday.t
            with module Zoned := Ofday.Zoned)

  module Zoned : sig
    include module type of Ofday.Zoned

    val to_time : t -> Date0.t -> Time_internal.T.t
  end

  val now : unit -> t
end

module Span : sig
  include (module type of Span
            with type t = Span.t
            with module Parts := Span.Parts)

  module Parts : sig
    type t = Span.Parts.t = private {
      sign : Float.Sign.t;
      hr   : int;
      min  : int;
      sec  : int;
      ms   : int;
      us   : int;
    }
    with sexp
  end
end

module Zone : module type of Zone with type t = Zone.t

(** A fully qualified point in time, independent of timezone. *)
type t = Time_internal.T.t with bin_io, sexp

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

(* midnight, Jan 1, 1970 in UTC *)
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

(** {6 Constants} *)

(** {6 Conversions} *)
(** All these conversion functions use the current time zone. Unless marked _utc,
    in which case they use Universal Coordinated Time *)

val of_date_ofday : Zone.t -> Date0.t -> Ofday.t -> t
val to_date_ofday : t -> Zone.t -> Date0.t * Ofday.t
val to_date       : t -> Zone.t -> Date0.t
val to_ofday      : t -> Zone.t -> Ofday.t

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
  :  Zone.t
  -> Date0.t
  -> Ofday.t
  -> [ `Once of t | `Twice of t * t | `Never of t ]

val of_local_date_ofday : Date0.t -> Ofday.t -> t
val to_local_date_ofday : t -> Date0.t * Ofday.t
val to_local_date       : t -> Date0.t
val to_local_ofday      : t -> Ofday.t

val convert
  :  from_tz:Zone.t
  -> to_tz:Zone.t
  -> Date0.t
  -> Ofday.t
  -> (Date0.t * Ofday.t)

val utc_offset
  :  ?zone:Zone.t
  -> t
  -> Span.t

(** Other string conversions  *)

(** [to_filename_string t] converts [t] to string with format YYYY-MM-DD_HH-MM-SS.mmm
    which is suitable for using in filenames *)
val to_filename_string : t -> string

(** [of_filename_string s] converts [s] that has format YYYY-MM-DD_HH-MM-SS.mmm into
    time *)
val of_filename_string : string -> t

val to_string_fix_proto : [`Utc | `Local] -> t -> string
val of_string_fix_proto : [`Utc | `Local] -> string -> t

(** [to_string_trimmed t] Same as to_string, but removes trailing seconds and
  milliseconds if they are 0 *)
val to_string_trimmed : t -> string

(** [to_sec_string t] Same as to_string, but without milliseconds *)
val to_sec_string : t -> string

(** [of_localized_string zone str] read in the given string assuming that it represents
  a time in zone and return the appropriate Time.t *)
val of_localized_string : Zone.t -> string -> t

(** [to_string_abs ?zone t] returns a string that represents an absolute time, rather than
    a local time with an assumed time zone.  This string can be round-tripped, even on a
    machine in a different time zone than the machine that wrote the string.

    The string will display the date and of-day of [zone] together with [zone] as an
    offset from UTC.  The [zone] argument defaults to the machine's timezone.
*)
val to_string_abs
  :  ?zone:Zone.t
  -> t
  -> string

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
    rules given in 'man strftime'.  The time is output in the local timezone.

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
val format : t -> string -> string

(** [to_epoch t] returns the number of seconds since Jan 1, 1970 00:00:00 in UTC *)
val to_epoch : t -> float

(* [next_multiple ~base ~after ~interval] returns the smallest [time] of the form:

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
    type t with bin_io, sexp, compare
  end with type t = t

  (** Provides a sexp representation that is independent of the time zone of the machine
      writing it. *)
  module With_utc_sexp : sig
    module V1 : sig
      type t with bin_io, sexp, compare
    end with type t = t
  end
end
