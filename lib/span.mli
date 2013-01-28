open Std_internal

type t = private float                  (* number of seconds *)

(* Parts represents the individual parts of a Span as if it were written out (it is the
   counterpart to create).  For example, (sec 90.) is represented by {Parts.hr = 0;
   min = 1; sec = 30; ms = 0}.  The fields will always be positive. *)
module Parts : sig
  type t = private {
      sign : Float.Sign.t;
      hr   : int;
      min  : int;
      sec  : int;
      ms   : int;
      us   : int;
    }
    with sexp
end

include Binable with type t := t
include Comparable_binable with type t := t
include Floatable with type t := t
include Hashable_binable with type t := t
include Robustly_comparable with type t := t
include Sexpable with type t := t

(* String converters and sexp converters allow for specifying of time spans in various
   units after a leading float (e.g. 45s, 3h, or 1d):

    ms - milliseconds
    s - seconds
    m - minutes
    h - hours
    d - days

   The outgoing conversion functions use these units as well, choosing the largest
   available type.  For instance, if it's a bit greater than or equal to 1 hour, the span
   will be rendered in hours, (Time.to_string (Time.of_string "66m") = "1.1h").
*)
val to_string : t -> string
val of_string : string -> t

(* values *)
val nanosecond : t
val microsecond : t
val millisecond : t
val second : t
val minute : t
val hour : t
val day : t
(* 10^-6 seconds, used in robustly comparable operators (<., >., =., ...) to determine
   equality *)
val epsilon : t
val zero : t

(* [create ?sign ?day ?hr ?min ?sec ?ms ?us ()] Create a span from the given parts.  All
   parts are assumed to be positive (no checking is done by the function) and the sign of
   the final span is given by [sign] which is positive by default. *)
val create :
  ?sign:Float.Sign.t
  -> ?day:int
  -> ?hr:int
  -> ?min:int
  -> ?sec:int
  -> ?ms:int
  -> ?us:int
  -> unit
  -> t

val to_parts : t -> Parts.t

(* converters *)
val of_ns      : float -> t
val of_us      : float -> t
val of_ms      : float -> t
val of_sec     : float -> t
val of_int_sec : int   -> t
val of_min     : float -> t
val of_hr      : float -> t
val of_day     : float -> t

val to_ns  : t -> float
val to_us  : t -> float
val to_ms  : t -> float
val to_sec : t -> float
val to_min : t -> float
val to_hr  : t -> float
val to_day : t -> float

(** {6 Basic operations on spans} *)
val (+)   : t -> t -> t
val (-)   : t -> t -> t
val abs   : t -> t (** absolute value *)
val neg   : t -> t (** negation *)
val scale : t -> float -> t
val (/)   : t -> float -> t
val (//)  : t -> t -> float

val pp : Format.formatter -> t -> unit

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

(** [randomize t ~percent] returns a span +/- percent * original span.  Percent must be
    between 0 and 1, and must be positive. *)
val randomize : t -> percent:float -> t
