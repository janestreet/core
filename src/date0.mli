open Core_kernel.Std

type t [@@deriving bin_io, sexp]

include Hashable_binable with type t := t
(** converts a string to a date, in formats:
 * m/d/y
 * y-m-d (* valid iso8601_extended *)
  * DD MMM YYYY
  * DDMMMYYYY
  * YYYYMMDD *)
include Stringable         with type t := t
include Comparable_binable with type t := t
include Pretty_printer.S   with type t := t

(** [create_exn ~y ~m ~d] creates the date specified in the arguments.  Arguments are
    validated, and are not normalized in any way.  So, days must be within the limits for
    the month in question, numbers cannot be negative, years must be fully specified, etc.
*)
val create_exn : y:int -> m:Month.t -> d:int -> t

val of_tm : Core_unix.tm -> t

(* For details on this ISO format, see:

   http://www.wikipedia.org/wiki/iso8601
*)
val of_string_iso8601_basic : string -> pos:int -> t (* YYYYMMDD *)
val to_string_iso8601_basic : t -> string            (* YYYYMMDD *)

val to_string_american : t -> string              (* MM/DD/YYYY *)

val day   : t -> int
val month : t -> Month.t
val year  : t -> int

val day_of_week : t -> Day_of_week.t

(** Week of the year, from 1 to 53.  According to ISO 8601, weeks start on Monday, and the
    first week of a year is the week that contains the first Thursday of the year.  Notice
    that this means that dates near the end of the year can have week number 1, and dates
    near the beginning of the year can have week number 52 or 53.

    Warning: the triple (year, week number, week day) does not identify a date -- e.g.
    2012-01-02 and 2012-12-31 are both Mondays of week 1. (However, if instead of the
    year, you use the year of the nearest Thursday, then it does work.) *)
val week_number : t -> int

val is_weekend : t -> bool
val is_weekday : t -> bool

(** Monday through Friday are business days, unless they're a holiday.  Use
    [Pnl_db.Holidays.is_holiday] as a convenient holiday function. *)
val is_business_day : t -> is_holiday:(t -> bool) -> bool

(* [add_days t n] adds n days to [t] and returns the resulting date. *)
val add_days : t -> int -> t

(** [add_months t n] returns date with max days for the month if the date would be
    invalid. e.g. adding 1 month to Jan 30 results in Feb 28 due to Feb 30 being
    an invalid date, Feb 29 is returned in cases of leap year. **)
val add_months : t -> int -> t

(** [diff t1 t2] returns date [t1] minus date [t2] in days. *)
val diff : t -> t -> int
(** [diff_weekdays t1 t2] returns the number of weekdays in the half-open interval
    [t2,t1) if t1 >= t2, and [- diff_weekdays t2 t1] otherwise. *)
val diff_weekdays : t -> t -> int
(** [diff_weekend_days t1 t2] returns the number of days that are weekend days in the
    half-open interval [t2,t1) if t1 >= t2, and [- diff_weekend_days t2 t1] otherwise. *)
val diff_weekend_days : t -> t -> int

(** [add_weekdays t 0] returns the next weekday if [t] is a weekend and [t] otherwise.
    Unlike add_days this is done by looping over the count of days to be added (forward or
    backwards based on the sign), and is O(n) in the number of days to add.
    Beware, [add_weekdays sat 1] or [add_weekdays sun 1] both return the next [tue],
    not the next [mon]. You may want to use [following_weekday] if you want the next
    following weekday, [following_weekday (fri|sat|sun)] would all return the next [mon].
*)
val add_weekdays : t -> int -> t

(** [add_business_days t ~is_holiday n] returns a business day even when
    [n=0]. [add_business_days ~is_holiday:(fun _ -> false) ...] is the same as
    [add_weekdays]. Use [Pnl_db.Holidays.is_holiday] as a convenient holiday
    function. *)
val add_business_days : t -> is_holiday:(t -> bool) -> int -> t

(* the following returns a closed interval (endpoints included) *)
val dates_between : min:t -> max:t -> t list

val business_dates_between : min:t -> max:t -> is_holiday:(t -> bool) -> t list

val weekdays_between : min:t -> max:t -> t list

val previous_weekday : t -> t

val following_weekday : t -> t

(** [first_strictly_after t ~on:day_of_week] returns the first occurrence of [day_of_week]
    strictly after [t]. *)
val first_strictly_after : t -> on:Day_of_week.t -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving sexp, bin_io, compare]
  end
end

module O : sig
  include Comparable.Infix with type t := t
end
