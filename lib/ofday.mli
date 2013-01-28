open Std_internal

(* Represented as a number of seconds since midnight *)
type t = private float

include Binable with type t := t
include Comparable_binable with type t := t
include Floatable with type t := t
include Hashable_binable with type t := t
include Robustly_comparable with type t := t
include Sexpable with type t := t
include Stringable with type t := t

val create : ?hr:int -> ?min:int -> ?sec:int -> ?ms:int -> ?us:int -> unit -> t

val to_parts : t -> Span.Parts.t

(* smallest and largest valid ofdays *)
val start_of_day : t
val end_of_day : t

val to_span_since_start_of_day : t -> Span.t
val of_span_since_start_of_day : Span.t -> t

(* Due to a circular reference, this function is defined in Core.Std. *)
(* val now : unit -> t *)

(** [add t s] shifts the time of day [t] by the span [s].  It returns None if
    the result is not in the same day.
*)
val add : t -> Span.t -> t option
val sub : t -> Span.t -> t option

(** [diff t1 t2] returns the difference in time between two ofdays, as if they occurred on
    the same day *)
val diff : t -> t -> Span.t

(** since midnight *)
val to_sec : t -> float
val of_sec : float -> t

val pp : Format.formatter -> t -> unit

(* Returns the time-span separating the two of-days, ignoring the hour information, and
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
