(** An absolute point in time, more efficient and precise than the [float]-based {!Time},
    but representing a narrower range of times.

    This module represents absolute times with nanosecond precision, approximately between
    the years 1823 and 2116 CE.

    NOTE: you should normally default to using [Time] instead of this module.  The
    reasons are:

    - Many functions around our libraries expect [Time.t] values, so it will likely be
      much more convenient for you.

    - It leads to greater consistency across different codebases.  It would be bad to end
      up with half our libraries expecting [Time.t] and the other half expecting
      [Time_ns.t].

    Some reasons you might want want to actually prefer [Time_ns.t] in certain cases:

    - It has superior performance.

    - It uses [int]s rather than [float]s internally, which makes certain things easier to
      reason about, since ints respect a bunch of arithmetic identities that floats don't,
      e.g., [x + (y + z) = (x + y) + z].

    - It is available on non-UNIX platforms, including Javascript via js_of_ocaml.

    All in all, it would have been nice to have chosen [Time_ns.t] to begin with, but
    we're unlikely to flip everything to [Time_ns.t] in the short term (see comment at
    the end of time_ns.ml).
*)

open Core_kernel.Std

type t = Core_kernel.Time_ns.t [@@deriving typerep]

module Span : sig
  type t = Core_kernel.Time_ns.Span.t [@@deriving typerep]

  include Identifiable with type t := t

  (** Similar to {!Time.Span.Parts}, but adding [ns]. *)
  module Parts : sig
    type t =
      { sign : Float.Sign.t
      ; hr   : int
      ; min  : int
      ; sec  : int
      ; ms   : int
      ; us   : int
      ; ns   : int
      }
    [@@deriving sexp]
  end

  val nanosecond  : t
  val microsecond : t
  val millisecond : t
  val second      : t
  val minute      : t
  val hour        : t
  val day         : t

  val of_ns  : float -> t
  val of_us  : float -> t
  val of_ms  : float -> t
  val of_sec : float -> t
  val of_min : float -> t
  val of_hr  : float -> t
  val of_day : float -> t
  val to_ns  : t     -> float
  val to_us  : t     -> float
  val to_ms  : t     -> float
  val to_sec : t     -> float
  val to_min : t     -> float
  val to_hr  : t     -> float
  val to_day : t     -> float

  val of_int_us  : int -> t
  val of_int_ms  : int -> t
  val of_int_sec : int -> t
  val to_int_us  : t -> int
  val to_int_ms  : t -> int
  val to_int_sec : t -> int

  val zero : t
  val min_value : t
  val max_value : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val abs : t -> t
  val neg : t -> t
  val scale     : t -> float -> t
  val scale_int : t -> int   -> t
  val div : t -> t -> Int63.t
  val ( / ) : t -> float -> t
  val ( // ) : t -> t -> float

  val create
    :  ?sign : Float.Sign.t
    -> ?day : int
    -> ?hr  : int
    -> ?min : int
    -> ?sec : int
    -> ?ms  : int
    -> ?us  : int
    -> ?ns  : int
    -> unit
    -> t

  val to_short_string : t -> string
  val randomize : t -> percent : float -> t

  val to_parts : t -> Parts.t
  val of_parts : Parts.t -> t

  module Unit_of_time = Time.Span.Unit_of_time

  val to_unit_of_time : t -> Unit_of_time.t
  val of_unit_of_time : Unit_of_time.t -> t

  (** See [Time.Span.to_string_hum]. *)
  val to_string_hum
    :  ?delimiter:char              (** defaults to ['_'] *)
    -> ?decimals:int                (** defaults to 3 *)
    -> ?align_decimal:bool          (** defaults to [false] *)
    -> ?unit_of_time:Unit_of_time.t (** defaults to [to_unit_of_time t] *)
    -> t
    -> string

  (** {!Time.t} is precise to approximately 0.24us in 2014.  If [to_span] converts to the
      closest [Time.Span.t], we have stability problems: converting back yields a
      different [t], sometimes different enough to have a different external
      representation, because the conversion back and forth crosses a rounding boundary.

      To stabilize conversion, we treat [Time.t] as having 1us precision: [to_span] and
      [of_span] both round to the nearest 1us.

      Around 135y magnitudes, [Time.Span.t] no longer has 1us resolution.  At that point,
      [to_span] and [of_span] raise.

      The concern with stability is in part due to an earlier incarnation of
      [Timing_wheel] that had surprising behavior due to rounding of floating-point times.
      Timing_wheel was since re-implemented to use integer [Time_ns], and to treat
      floating-point [Time]s as equivalence classes according to the [Time_ns] that they
      round to.  See [Timing_wheel_float] for details. *)
  val to_span : t -> Time.Span.t
  val of_span : Time.Span.t -> t

  include Robustly_comparable with type t := t

  val to_int63_ns : t       -> Int63.t (** Fast, implemented as the identity function. *)
  val of_int63_ns : Int63.t -> t       (** Fast, implemented as the identity function. *)

  (** Will raise on 32-bit platforms with spans corresponding to contemporary {!now}.
      Consider [to_int63_ns] instead. *)
  val to_int_ns : t   -> int
  val of_int_ns : int -> t

  module Stable : sig
    module V1 : sig
      type nonrec t = t
      include Stable with type t := t
      (** [to_int63] and [of_int63_exn] encode [t] for use in wire protocols; they are
          designed to be efficient on 64-bit machines.  [of_int63_exn (to_int63 t) = t]
          for all [t]; [of_int63_exn] raises for inputs not produced by [to_int63]. *)
      val to_int63     : t -> Int63.t
      val of_int63_exn : Int63.t -> t
    end
  end

  val random : unit -> t

  module Option : sig
    type span

    type t = private Int63.t [@@deriving typerep]

    include Identifiable with type t := t

    val none : t
    val some : span -> t
    val is_none : t -> bool
    val is_some : t -> bool
    val value : t -> default : span -> span
    val value_exn : t -> span

    module Stable : sig
      module V1 : sig
        type nonrec t = t
        include Stable with type t := t
        (** [to_int63] and [of_int63_exn] encode [t] for use in wire protocols; they are
            designed to be efficient on 64-bit machines.  [of_int63_exn (to_int63 t) = t]
            for all [t]; [of_int63_exn] raises for inputs not produced by [to_int63]. *)
        val to_int63     : t -> Int63.t
        val of_int63_exn : Int63.t -> t
      end
    end
  end with type span := t
end

module Option : sig
  type time

  type t = private Int63.t [@@deriving typerep]

  include Identifiable with type t := t

  val none : t
  val some : time -> t
  val is_none : t -> bool
  val is_some : t -> bool
  val value : t -> default : time -> time
  val value_exn : t -> time

  val of_option : time option -> t
  val to_option : t -> time option

  module Stable : sig
    module V1 : sig
      type nonrec t = t
      include Stable with type t := t
      (** [to_int63] and [of_int63_exn] encode [t] for use in wire protocols; they are
          designed to be efficient on 64-bit machines.  [of_int63_exn (to_int63 t) = t]
          for all [t]; [of_int63_exn] raises for inputs not produced by [to_int63]. *)
      val to_int63     : t -> Int63.t
      val of_int63_exn : Int63.t -> t
    end
  end
end with type time := t

module Ofday : sig
  type time

  type t = private Int63.t [@@deriving typerep]

  include Identifiable with type t := t

  val add_exn : t -> Span.t -> t
  val sub_exn : t -> Span.t -> t

  val diff : t -> t -> Span.t

  val to_ofday : t -> Time.Ofday.t
  val of_ofday : Time.Ofday.t -> t

  val local_now : unit -> t
  val of_local_time : time -> t
  val to_millisecond_string : t -> string

  val start_of_day : t
  val end_of_day : t

  val to_span_since_start_of_day : t -> Span.t

  (** [of_span_since_start_of_day_exn] excludes obviously impossible times of day but
      cannot exclude all invalid times of day due to DST, leap seconds, etc. *)
  val of_span_since_start_of_day_exn : Span.t -> t

  module Stable : sig
    module V1 : sig
      type nonrec t = t
      include Stable with type t := t
      (** [to_int63] and [of_int63_exn] encode [t] for use in wire protocols; they are
          designed to be efficient on 64-bit machines.  [of_int63_exn (to_int63 t) = t]
          for all [t]; [of_int63_exn] raises for inputs not produced by [to_int63]. *)
      val to_int63     : t -> Int63.t
      val of_int63_exn : Int63.t -> t
    end
  end
end with type time := t

include Identifiable with type t := t

val epoch : t (** Unix epoch (1970-01-01 00:00:00 UTC) *)

val min_value : t
val max_value : t

val now : unit -> t

val add : t -> Span.t -> t
val sub : t -> Span.t -> t
val diff : t -> t -> Span.t
val abs_diff : t -> t -> Span.t

val to_span_since_epoch : t -> Span.t
val of_span_since_epoch : Span.t -> t

val to_time : t -> Time.t
val of_time : Time.t -> t

val to_string_fix_proto : [ `Utc | `Local ] -> t -> string
val of_string_fix_proto : [ `Utc | `Local ] -> string -> t

(* See [Time] for documentation. *)
val to_string_abs : t -> zone:Zone.t -> string
val of_string_abs : string -> t

val to_int63_ns_since_epoch : t -> Int63.t
val of_int63_ns_since_epoch : Int63.t -> t

(** Will raise on 32-bit platforms.  Consider [to_int63_ns_since_epoch] instead. *)
val to_int_ns_since_epoch : t   -> int
val of_int_ns_since_epoch : int -> t

(** See [Core_kernel.Time_ns] *)
val next_multiple
  :  ?can_equal_after:bool  (** default is [false] *)
  -> base:t
  -> after:t
  -> interval:Span.t
  -> unit
  -> t


val of_date_ofday : zone:Zone.t -> Date.t -> Ofday.t -> t

val to_date : t -> zone:Zone.t -> Date.t

val occurrence
  :  [ `First_after_or_at | `Last_before_or_at ]
  -> t
  -> ofday:Ofday.t
  -> zone:Zone.t
  -> t

(** [pause span] sleeps for [span] time. *)
val pause : Span.t -> unit

(** [interruptible_pause span] sleeps for [span] time unless interrupted (e.g. by delivery
    of a signal), in which case the remaining unslept portion of time is returned. *)
val interruptible_pause : Span.t -> [ `Ok | `Remaining of Span.t ]

(** [pause_forever] sleeps indefinitely. *)
val pause_forever : unit -> never_returns

module Stable : sig
  module V1 : sig
    type nonrec t = t
    include Stable with type t := t
    (** [to_int63] and [of_int63_exn] encode [t] for use in wire protocols; they are
        designed to be efficient on 64-bit machines.  [of_int63_exn (to_int63 t) = t] for
        all [t]; [of_int63_exn] raises for inputs not produced by [to_int63]. *)
    val to_int63     : t -> Int63.t
    val of_int63_exn : Int63.t -> t
  end
  module Span   : module type of Span   .Stable
  module Option : module type of Option .Stable
  module Ofday  : module type of Ofday  .Stable
end

val random : unit -> t
