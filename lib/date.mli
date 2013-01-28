open Std_internal

type t = private {
  y: int;
  m: Month.t;
  d: int;
}

include Sexpable with type t := t
include Binable with type t := t
include Hashable_binable with type t := t
(** converts a string to a date, in formats:
 * m/d/y
 * y-m-d (* valid iso8601_extended *)
  * DD MMM YYYY
  * DDMMMYYYY
  * YYYYMMDD *)
include Stringable with type t := t
include Comparable_binable with type t := t

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

val pp : Format.formatter -> t -> unit

val day : t -> int
val month : t -> Month.t
val year : t -> int

(* added to Date in Core.Std to handle the circular dependancy between Time and Date *)
(* val today : unit -> t                   (* based on local timezone *) *)
(* val yesterday : unit -> t               (* based on local timezone *) *)
(* val of_time : Time.t -> t               (* based on local timezone *) *)

val day_of_week : t -> Weekday.t

val is_weekend : t -> bool
val is_weekday : t -> bool

(* Monday through Friday are business days, unless they're a holiday *)
val is_business_day : t -> is_holiday:(t -> bool) -> bool

(* [add_days t n] adds n days to [t] and returns the resulting date. *)
val add_days : t -> int -> t

(** [add_months t n] returns date with max days for the month if the date would be
    invalid. e.g. adding 1 month to Jan 30 results in Feb 28 due to Feb 30 being
    an invalid date, Feb 29 is returned in cases of leap year. **)
val add_months : t -> int -> t

(** [diff t1 t2] returns date [t1] minus date [t2] in days. *)
val diff : t -> t -> int

(** [add_weekdays t 0] returns the next weekday if [t] is a weekend and [t] otherwise.
    Unlike add_days this is done by looping over the count of days to be added (forward or
    backwards based on the sign), and is O(n) in the number of days to add.  *)
val add_weekdays : t -> int -> t

(** [add_business_days t ~is_holiday n] returns a business day even when
    [n=0]. [add_business_days ~is_holiday:(fun _ -> false) ...] is the same as
    [add_weekdays]. Use [Pnl_db.Calendar_events.is_holiday] as a conveninent holiday
    function. *)
val add_business_days : t -> is_holiday:(t -> bool) -> int -> t

(* the following returns a closed interval (endpoints included) *)
val dates_between : min:t -> max:t -> t list

val business_dates_between : min:t -> max:t -> is_holiday:(t -> bool) -> t list

val weekdays_between : min:t -> max:t -> t list

val previous_weekday : t -> t

module Export : sig
  type _date = t = private { y: int; m: Month.t; d: int; }
end
