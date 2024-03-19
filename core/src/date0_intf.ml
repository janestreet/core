open! Import
open Std_internal

module type Date0 = sig
  type t [@@immediate] [@@deriving bin_io ~localize, hash, sexp, sexp_grammar, typerep]

  include Hashable_binable with type t := t

  (** converts a string to a date in the following formats:
      - m/d/y
      - y-m-d (valid iso8601_extended)
      - DD MMM YYYY
      - DDMMMYYYY
      - YYYYMMDD *)
  include Stringable with type t := t

  include Comparable_binable with type t := t
  include Diffable.S_atomic with type t := t
  include Pretty_printer.S with type t := t

  (** [create_exn ~y ~m ~d] creates the date specified in the arguments.  Arguments are
      validated, and are not normalized in any way.  So, days must be within the limits
      for the month in question, numbers cannot be negative, years must be fully
      specified, etc.  *)
  val create_exn : y:int -> m:Month.t -> d:int -> t

  (** For details on this ISO format, see:

      http://www.wikipedia.org/wiki/iso8601
  *)
  val of_string_iso8601_basic : string -> pos:int -> t

  (** YYYYMMDD *)
  val to_string_iso8601_basic : t -> string

  (** MM/DD/YYYY *)
  val to_string_american : t -> string

  val day : t -> int
  val month : t -> Month.t
  val year : t -> int

  (** Only accurate after 1752-09 *)
  val day_of_week : t -> Day_of_week.t

  (** Week of the year, from 1 to 53, along with the week-numbering year to which the week
      belongs. The week-numbering year may not correspond to the calendar year in which
      the provided date occurs.

      According to ISO 8601, weeks start on Monday, and the first week of a year is the
      week that contains the first Thursday of the year. This means that dates near the
      end of the calendar year can have week number 1 and belong to the following
      week-numbering year, and dates near the beginning of the calendar year can have week
      number 52 or 53 and belong to the previous week-numbering year.

      The triple (week-numbering year, week number, week day) uniquely identifies a
      particular date, which is not true if the calendar year is used instead.
  *)
  val week_number_and_year : t -> int * int

  (** See {!week_number_and_year} for the meaning of week number.  *)
  val week_number : t -> int

  (** [is_weekend] and [is_weekday] treat Saturday and Sunday as the weekend, and Monday
      through Friday as weekdays.

      Caveat: Not all cultures, countries, or businesses conform to this particular cycle
      of weekend / weekday. *)

  val is_weekend : t -> bool
  val is_weekday : t -> bool

  (** Monday through Friday are business days, unless they're a holiday.

      See the caveat on [is_weekend] about varying weekend/weekday cycles. *)
  val is_business_day : t -> is_holiday:(t -> bool) -> bool

  (** [add_days t n] adds n days to [t] and returns the resulting date.

      Inaccurate when crossing 1752-09.
  *)
  val add_days : t -> int -> t

  (** [add_months t n] returns date with max days for the month if the date would be
      invalid. e.g. adding 1 month to Jan 30 results in Feb 28 due to Feb 30 being
      an invalid date, Feb 29 is returned in cases of leap year.

      In particular, this means adding [x] months and then adding [y] months isn't the
      same as adding [x + y] months, and in particular adding [x] months and then [-x]
      months won't always get you back where you were. **)
  val add_months : t -> int -> t

  (** [add_years t n] has the same semantics as [add_months] for adding years to Feb 29 of
      a leap year, i.e., when the addition results in a date in a non-leap year, the
      result will be Feb 28 of that year. *)
  val add_years : t -> int -> t

  (** [diff t1 t2] returns date [t1] minus date [t2] in days. *)
  val diff : t -> t -> int

  (** [diff_weekdays t1 t2] returns the number of weekdays in the half-open interval
      \[t2,t1) if t1 >= t2, and [- diff_weekdays t2 t1] otherwise.

      See the caveat on [is_weekend] about varying weekend/weekday cycles. *)
  val diff_weekdays : t -> t -> int

  (** [diff_weekend_days t1 t2] returns the number of days that are weekend days in the
      half-open interval \[t2,t1) if t1 >= t2, and [- diff_weekend_days t2 t1]
      otherwise.

      See the caveat on [is_weekend] about varying weekend/weekday cycles. *)
  val diff_weekend_days : t -> t -> int

  (** First rounds the given date backward to the previous weekday, if it is not already a
      weekday. Then advances by the given number of weekdays, which may be negative.

      See the caveat on [is_weekend] about varying weekend/weekday cycles. *)
  val add_weekdays_rounding_backward : t -> int -> t

  (** First rounds the given date forward to the next weekday, if it is not already a
      weekday. Then advances by the given number of weekdays, which may be negative.

      See the caveat on [is_weekend] about varying weekend/weekday cycles. *)
  val add_weekdays_rounding_forward : t -> int -> t

  (** First rounds the given date backward to the previous business day, i.e. weekday not
      satisfying [is_holiday], if it is not already a business day. Then advances by the
      given number of business days, which may be negative.

      See the caveat on [is_weekend] about varying weekend/weekday cycles. *)
  val add_business_days_rounding_backward : t -> is_holiday:(t -> bool) -> int -> t

  (** First rounds the given date forward to the next business day, i.e. weekday not
      satisfying [is_holiday], if it is not already a business day. Then advances by the
      given number of business days, which may be negative.

      See the caveat on [is_weekend] about varying weekend/weekday cycles. *)
  val add_business_days_rounding_forward : t -> is_holiday:(t -> bool) -> int -> t

  (** [add_weekdays t 0] returns the next weekday if [t] is a weekend and [t] otherwise.
      Unlike [add_days] this is done by looping over the count of days to be added
      (forward or backwards based on the sign), and is O(n) in the number of days to
      add. Beware, [add_weekdays sat 1] or [add_weekdays sun 1] both return the next
      [tue], not the next [mon]. You may want to use [following_weekday] if you want the
      next following weekday, [following_weekday (fri|sat|sun)] would all return the next
      [mon].

      See the caveat on [is_weekend] about varying weekend/weekday cycles. *)
  val add_weekdays : t -> int -> t
    [@@deprecated
      "[since 2019-12] use [add_weekdays_rounding_backward] or \
       [add_weekdays_rounding_forward] as appropriate"]

  val add_weekdays_rounding_in_direction_of_step : t -> int -> t
    [@@alert
      legacy
        "use [add_weekdays_rounding_backward] or [add_weekdays_rounding_forward] as \
         appropriate"]

  (** [add_business_days t ~is_holiday n] returns a business day even when
      [n=0]. [add_business_days ~is_holiday:(fun _ -> false) ...] is the same as
      [add_weekdays].

      If you don't want to skip Saturday or Sunday, use [add_days_skipping].


      See the caveat on [is_weekend] about varying weekend/weekday cycles.
  *)
  val add_business_days : t -> is_holiday:(t -> bool) -> int -> t
    [@@deprecated
      "[since 2019-12] use [add_business_days_rounding_backward] or \
       [add_business_days_rounding_forward] as appropriate"]

  val add_business_days_rounding_in_direction_of_step
    :  t
    -> is_holiday:(t -> bool)
    -> int
    -> t
    [@@alert
      legacy
        "use [add_business_days_rounding_backward] or \
         [add_business_days_rounding_forward] as appropriate"]

  (** [add_days_skipping t ~skip n] adds [n] days to [t], ignoring any date satisfying
      [skip], starting at the first date at or after [t] that does not satisfy [skip].
      For example, if [skip t = true], then [add_days_skipping t ~skip 0 > t].

      [add_business_days] and [add_weekdays] are special cases of [add_days_skipping]. *)
  val add_days_skipping : t -> skip:(t -> bool) -> int -> t

  (** the following returns a closed interval (endpoints included) *)
  val dates_between : min:t -> max:t -> t list

  (** [business_dates_between ~min ~max ~is_holiday] returns the list of dates between
      [min] and [max], inclusive, for which [is_business_day ~is_holiday].

      See the caveat on [is_weekend] about varying weekend/weekday cycles. *)
  val business_dates_between : min:t -> max:t -> is_holiday:(t -> bool) -> t list

  (** See the caveat on [is_weekend] about varying weekend/weekday cycles. *)
  val weekdays_between : min:t -> max:t -> t list

  val previous_weekday : t -> t
  val following_weekday : t -> t
  val round_forward_to_weekday : t -> t
  val round_backward_to_weekday : t -> t
  val round_forward_to_business_day : t -> is_holiday:(t -> bool) -> t
  val round_backward_to_business_day : t -> is_holiday:(t -> bool) -> t

  (** [first_strictly_after t ~on:day_of_week] returns the first occurrence of [day_of_week]
      strictly after [t]. *)
  val first_strictly_after : t -> on:Day_of_week.t -> t

  (** [days_in_month ~year ~month] returns the number of days in [month], using [year]
      only if [month = Month.Feb] to check if there is a leap year.

      Incorrect for September 1752. *)
  val days_in_month : year:int -> month:Month.t -> int

  (** [is_leap_year ~year] returns true if [year] is considered a leap year *)
  val is_leap_year : year:int -> bool

  (** The starting date of the UNIX epoch: 1970-01-01 *)
  val unix_epoch : t

  (** [gen] generates dates between 1900-01-01 and 2100-01-01. *)
  include Quickcheckable with type t := t

  (** [gen_incl d1 d2] generates dates in the range between [d1] and [d2], inclusive, with
      the endpoints having higher weight than the rest.  Raises if [d1 > d2]. *)
  val gen_incl : t -> t -> t Quickcheck.Generator.t

  (** [gen_uniform_incl d1 d2] generates dates chosen uniformly in the range between [d1]
      and [d2], inclusive.  Raises if [d1 > d2]. *)
  val gen_uniform_incl : t -> t -> t Quickcheck.Generator.t

  (** [Days] provides a linear representation of dates that is optimized for arithmetic on
      the number of days between dates, rather than for representing year/month/day
      components. This module is intended for use only in performance-sensitive contexts
      where dates are manipulated more often than they are constructed or deconstructed;
      most clients should use the ordinary [t]. *)
  module Days : sig
    type date = t
    type t [@@immediate]

    val of_date : date -> t
    val to_date : t -> date
    val diff : t -> t -> int
    val add_days : t -> int -> t

    (** The starting date of the UNIX epoch: 1970-01-01 *)
    val unix_epoch : t
  end
  with type date := t

  module Option : sig
    type value := t
    type t [@@immediate] [@@deriving bin_io ~localize, hash, sexp, sexp_grammar]

    include Immediate_option_intf.S with type value := value and type t := t
    include Comparable.S_plain with type t := t
    include Quickcheckable.S with type t := t
  end

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@immediate] [@@deriving equal, hash, sexp_grammar]

      (** [to_int] and [of_int_exn] convert to/from the underlying integer
          representation. *)

      val to_int : t -> int
      val of_int_exn : int -> t

      include
        Stable_comparable.With_stable_witness.V1
          with type t := t
          with type comparator_witness = comparator_witness

      include Hashable.Stable.V1.With_stable_witness.S with type key := t
      include Diffable.S_atomic with type t := t
    end

    module Option : sig
      module V1 : sig
        type nonrec t = Option.t
        [@@immediate]
        [@@deriving bin_io ~localize, compare, equal, sexp, sexp_grammar, stable_witness]

        (** [to_int] and [of_int_exn] convert to/from the underlying integer
            representation. *)

        val to_int : t -> int
        val of_int_exn : int -> t
      end
    end
  end

  module O : sig
    include Comparable.Infix with type t := t
  end

  (*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

    https://opensource.janestreet.com/standards/#private-submodules *)
  module Private : sig
    val leap_year_table : int array
    val non_leap_year_table : int array
    val ordinal_date : t -> int
  end
end
