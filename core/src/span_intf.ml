open! Import
open Std_internal

(** Parts represents the individual parts of a Span as if it were written out (it is the
    counterpart to [Span.create]). For example, 90 seconds is represented by:

    {[
      {sign = Pos; hr = 0; min = 1; sec = 30; ms = 0; ns = 0}
    ]}

    The fields will always be non-negative, and will never be large enough to form the
    next larger unit (e.g., [min < 60]). *)
module type Parts = sig
  type t = private
    { sign : Sign.t
    ; hr : int
    ; min : int
    ; sec : int
    ; ms : int
    ; us : int
    ; ns : int
    }
  [@@deriving compare, sexp, sexp_grammar]
end

module type S = sig
  (** Span.t represents a span of time (e.g. 7 minutes, 3 hours, 12.8 days).  The span
      may be positive or negative. *)
  type underlying

  type t = private underlying [@@deriving bin_io, hash, sexp, sexp_grammar, typerep]

  module Parts : Parts
  include Comparable_binable with type t := t
  include Comparable.With_zero with type t := t
  include Hashable_binable with type t := t
  include Diffable.S_atomic with type t := t
  include Pretty_printer.S with type t := t
  include Robustly_comparable with type t := t
  include Quickcheck.S_range with type t := t

  (** Time spans are denominated as a float suffixed by a unit of time; the valid suffixes
      are listed below:

      d  - days
      h  - hours
      m  - minutes
      s  - seconds
      ms - milliseconds
      us - microseconds
      ns - nanoseconds

      [to_string] and [sexp_of_t] use a mixed-unit format, which breaks the input span
      into parts and concatenates them in descending order of unit size. For example, pi
      days is rendered as "3d3h23m53.60527015815s". If the span is negative, a single "-"
      precedes the entire string. For extremely large (>10^15 days) or small (<1us) spans,
      a unit may be repeated to ensure the string conversion round-trips.

      [of_string] and [t_of_sexp] accept any combination of (nonnegative float
      string)(unit of time suffix) in any order, without spaces, and sums up the durations
      of each of the parts for the magnitude of the span. The input may be prefixed by "-"
      for negative spans.

      String and sexp conversions round-trip precisely, that is:

      {[ Span.of_string (Span.to_string t) = t ]}
  *)
  val to_string : t -> string

  val of_string : string -> t

  (** {6 values} *)

  val nanosecond : t
  val microsecond : t
  val millisecond : t
  val second : t
  val minute : t
  val hour : t
  val day : t

  (** 10^-6 seconds, used in robustly comparable operators (<., >., =., ...) to determine
      equality *)
  val robust_comparison_tolerance : t

  val zero : t

  (** [?sign] defaults to positive. Setting it to negative is equivalent to negating all
      the integers. *)
  val create
    :  ?sign:Sign.t
    -> ?day:int
    -> ?hr:int
    -> ?min:int
    -> ?sec:int
    -> ?ms:int
    -> ?us:int
    -> ?ns:int
    -> unit
    -> t

  val to_parts : t -> Parts.t

  (** {6 converters} *)

  (** conversions to and from [float] *)

  val of_ns : float -> t
  val of_us : float -> t
  val of_ms : float -> t
  val of_sec : float -> t
  val of_min : float -> t
  val of_hr : float -> t
  val of_day : float -> t
  val to_ns : t -> float
  val to_us : t -> float
  val to_ms : t -> float
  val to_sec : t -> float
  val to_min : t -> float
  val to_hr : t -> float
  val to_day : t -> float

  (** conversions from [int] *)

  val of_int_ns : int -> t
  val of_int_us : int -> t
  val of_int_ms : int -> t
  val of_int_sec : int -> t
  val of_int_min : int -> t
  val of_int_hr : int -> t
  val of_int_day : int -> t

  (** conversions from other size integer types for seconds *)

  val of_int32_seconds : Int32.t -> t
  val of_int63_seconds : Int63.t -> t

  (** [to_int63_seconds_round_down_exn t] returns the number of seconds represented by
      [t], rounded down, raising if the result is not representable as an [Int63.t]. *)
  val to_int63_seconds_round_down_exn : t -> Int63.t

  (** The only condition [to_proportional_float] is supposed to satisfy is that for all
      [t1, t2 : t]: [to_proportional_float t1 /. to_proportional_float t2 = t1 // t2]. *)
  val to_proportional_float : t -> float

  (** {6 Basic operations on spans}

      The arithmetic operations rely on the behavior of the underlying representation of a
      span. For example, if addition overflows with float-represented spans, the result is
      an infinite span; with fixed-width integer-represented spans, the result silently
      wraps around as in two's-complement arithmetic. *)

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t

  (** absolute value *)
  val abs : t -> t

  (** negation *)
  val neg : t -> t

  val scale : t -> float -> t
  val ( / ) : t -> float -> t
  val ( // ) : t -> t -> float

  (** [next t] is the smallest representable span greater than [t] (and therefore
      representation-dependent) *)
  val next : t -> t

  (** [prev t] is the largest representable span less than [t] (and therefore
      representation-dependent) *)
  val prev : t -> t

  (** [to_short_string t] pretty-prints approximate time span using no more than
      five characters if the span is positive, and six if the span is negative.
      Examples
      {ul
      {li ["4h"] = 4 hours}
      {li ["5m"] = 5 minutes}
      {li ["4s"] = 4 seconds}
      {li ["10ms"] = 10 milliseconds}
      }

      only the most significant denomination is shown.
  *)
  val to_short_string : t -> string

  (** [to_unit_of_time t] = [Day] if [abs t >= day], [Hour] if [abs t >= hour], and so on
      down to [Microsecond] if [abs t >= microsecond], and [Nanosecond] otherwise. *)
  val to_unit_of_time : t -> Unit_of_time.t

  (** [of_unit_of_time unit_of_time] produces a [t] representing the corresponding span. *)
  val of_unit_of_time : Unit_of_time.t -> t

  (** [to_string_hum t ~delimiter ~decimals ~align_decimal ~unit_of_time] formats [t] using
      the given unit of time, or the largest appropriate units if none is specified, among
      "d"=day, "h"=hour, "m"=minute, "s"=second, "ms"=millisecond, "us"=microsecond, or
      "ns"=nanosecond.  The magnitude of the time span in the chosen unit is formatted by:

      [Float.to_string_hum ~delimiter ~decimals ~strip_zero:(not align_decimal)]

      If [align_decimal] is true, the single-character suffixes are padded with an extra
      space character.  In combination with not stripping zeroes, this means that the
      decimal point will occur a fixed number of characters from the end of the string. *)
  val to_string_hum
    :  ?delimiter:char (** defaults to ['_'] *)
    -> ?decimals:int (** defaults to 3 *)
    -> ?align_decimal:bool (** defaults to [false] *)
    -> ?unit_of_time:Unit_of_time.t (** defaults to [to_unit_of_time t] *)
    -> t
    -> string

  (** [randomize t ~percent] returns a span +/- percent * original span.  Percent must be
      between 0% and 100% inclusive, and must be positive. *)
  val randomize : ?state:Random.State.t -> t -> percent:Percent.t -> t
end
