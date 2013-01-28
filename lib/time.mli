(** Our time module.  This module wraps up unix times, including various
    convenience functions for accessing them.
*)

open Common
open Std_internal

(** A discrete point in time in the universe; not a time in some timezone. *)
type t = Time_internal.T.t


(** If this is ever called then all future calls to to_string and sexp_of_t will produce a
    new format sexp/string that includes enough offset information to reproduce the
    correct Time.t when of_string/t_of_sexp is called, even if they are called on a
    machine with a different timezone than the writing machine.  This should never be
    called in a library, and should only be called in application code. *)
val write_new_string_and_sexp_formats__read_both : unit -> unit

(** This does the same as [write_new_string_and_sexp_formats__read_both] and additionally
    [of_string] and [t_of_sexp] will not accept formats without a timezone. *)
val write_new_string_and_sexp_formats__read_only_new : unit -> unit

(** If this is called it asserts that use_new_string_and_sexp_formats has not been called,
    and will cause use_new_string_and_sexp_formats to throw an exception if it is called
    later *)
val forbid_new_string_and_sexp_formats : unit -> unit

val current_string_and_sexp_format : unit -> [
  | `Old
  | `Force_old
  | `Write_new_read_both
  | `Write_new_read_only_new
]

include Hashable_binable with type t := t
include Comparable_binable with type t := t
include Robustly_comparable with type t := t
include Sexpable with type t := t
include Binable with type t := t
include Stringable with type t := t
include Floatable with type t := t (* seconds since the epoch *)

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

val of_date_ofday : Zone.t -> Date.t -> Ofday.t -> t
val to_date_ofday : t -> Zone.t -> Date.t * Ofday.t
val to_date       : t -> Zone.t -> Date.t
val to_ofday      : t -> Zone.t -> Ofday.t

val of_local_date_ofday : Date.t -> Ofday.t -> t
val to_local_date_ofday : t -> Date.t * Ofday.t
val to_local_date       : t -> Date.t
val to_local_ofday      : t -> Ofday.t

val convert :
  from_tz:Zone.t
  -> to_tz:Zone.t
  -> Date.t
  -> Ofday.t
  -> (Date.t * Ofday.t)

val utc_offset :
  ?zone:Zone.t
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

(** [to_localized_string time zone] returns a string representation of [time]
    in the given zone in the form like "2008-11-18 15:34:56.123". *)
val to_localized_string : t -> Zone.t -> string

(** [of_localized_string zone str] read in the given string assuming that it represents
  a time in zone and return the appropriate Time.t *)
val of_localized_string : Zone.t -> string -> t

(** [to_string_deprecated] returns a string in the old format *)
val to_string_deprecated : t -> string

(** [to_string_abs ?zone t] returns a string that represents an absolute time, rather than
    a local time with an assumed time zone.  This string can be round-tripped, even on a
    machine in a different time zone than the machine that wrote the string.

    The string will display the date and of-day of [zone] together with [zone] as an
    offset from UTC.  The [zone] argument defaults to the machine's timezone.
*)
val to_string_abs :
  ?zone:Zone.t
  -> t
  -> string

val of_string_abs : string -> t

val pp : Format.formatter -> t -> unit

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

(** [ofday_occurrence ofday side now] returns a Time.t that is the occurrence of ofday (in
    the given zone) which is the latest occurrence before now or the earliest occurrence
    after now, according to side.  NOTE: This function is a little bit wrong near daylight
    savings time *)
val ofday_occurrence : t -> Zone.t -> Ofday.t -> [ `right_after | `right_before ] -> t

(** [format t fmt] formats the given time according to fmt, which follows the formatting
    rules given in 'man strftime'.  The time is output in the local timezone. *)
val format : t -> string -> string

(** [to_epoch t] returns the number of seconds since Jan 1, 1970 00:00:00 in UTC *)
val to_epoch : t -> float


