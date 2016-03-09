open! Core_kernel.Std

(** Times of day.

    [t] represents a clock-face time of day.  Usually this is equivalent to a time-offset
    from midnight, and each [t] occurs exactly once in each calendar day.  However, when
    daylight savings time begins or ends, some clock face times (and therefore [t]'s) can
    occur more than once per day or not at all, and e.g. 04:00 can occur three or five
    hours after midnight, so knowing your current offset from midnight is *not* in general
    equivalent to knowing the current [t].
    (See {!Zone} for tools to help you cope with DST.) *)
type t = private float [@@deriving bin_io, sexp]

module Zoned : sig
  (** A time of day along with a time zone.

      Expresses things like "Seinfeld moved to 6:30pm EST.", while a plain [Ofday]
      expresses something more like "I eat lunch at noon (in whichever timezone I'm in at
      the time).". *)

  (** Sexps look like "(12:01 nyc)"

      Two [t]'s may or may not correspond to the same times depending on which date
      they're evaluated. *)
  type ofday = t
  type t [@@deriving bin_io, sexp]

  (** Strings look like "12:01 nyc" *)
  include Stringable          with type t := t
  include Comparable_binable  with type t := t
  include Hashable_binable    with type t := t
  include Pretty_printer.S    with type t := t

  val create       : ofday -> Zone.t -> t
  val create_local : ofday ->           t

  val ofday : t -> ofday
  val zone  : t -> Zone.t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving sexp, bin_io]
    end
  end
end

include Comparable_binable   with type t := t
include Comparable.With_zero with type t := t
include Floatable            with type t := t
include Hashable_binable     with type t := t
include Pretty_printer.S     with type t := t
include Robustly_comparable  with type t := t
include Stringable           with type t := t

val create : ?hr:int -> ?min:int -> ?sec:int -> ?ms:int -> ?us:int -> unit -> t

val to_parts : t -> Span.Parts.t

(** Smallest valid ofday.  There is no exposed end_of_day value because the upper end of
    the range is not closed. *)
val start_of_day : t

(** Note that these names are only really accurate on days without DST transitions. When
    clocks move forward or back, [of_span_since_start_of_day s] will not necessarily occur
    [s] after that day's midnight. *)
val to_span_since_start_of_day : t -> Span.t
val of_span_since_start_of_day : Span.t -> t

(** Due to a circular reference, this declaration is found in time.mli. *)
(** val now : zone:Zone.t -> t *)

(** [add t s] shifts the time of day [t] by the span [s].  It returns [None] if the result
    is not in the same 24-hour day. *)
val add : t -> Span.t -> t option
val sub : t -> Span.t -> t option

(** [diff t1 t2] returns the difference in time between two ofdays, as if they occurred on
    the same 24-hour day. *)
val diff : t -> t -> Span.t

(** Returns the time-span separating the two of-days, ignoring the hour information, and
    assuming that the of-days represent times that are within a half-hour of each other.
    This is useful for comparing two ofdays in unknown time-zones. *)
val small_diff : t -> t -> Span.t

(** trailing seconds and subseconds are trimmed off if they are 0 *)
val to_string_trimmed : t -> string

(** trailing milliseconds are trimmed *)
val to_sec_string : t -> string

val of_string_iso8601_extended : ?pos:int -> ?len:int -> string -> t

(** with milliseconds *)
val to_millisec_string : t -> string

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, compare, sexp]
  end
end
