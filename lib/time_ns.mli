(** An absolute point in time, more efficient and precise than the [float]-based {!Time},
    but representing a narrower range of times.

    This module represents absolute times with nanosecond precision, approximately between
    the years 1823 and 2116 CE. *)
open Core_kernel.Std

(** [int]s are always immediate and so play nicely with the GC write barrier.
    Unfortunately, [private int] is necessary for the compiler to optimize uses. *)
type t = private Int63.t with typerep

module Span : sig
  type t = private Int63.t with typerep

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
    with sexp
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

  val of_int_sec : int -> t
  val to_int_sec : t -> int

  val zero : t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val abs : t -> t
  val neg : t -> t
  val scale : t -> float -> t
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

  val to_span : t -> Time.Span.t
  val of_span : Time.Span.t -> t

  include Robustly_comparable with type t := t

  val to_int63_ns : t       -> Int63.t (** Fast, implemented as the identity function. *)
  val of_int63_ns : Int63.t -> t       (** Fast, implemented as the identity function. *)

  (** Will raise on 32-bit platforms.  Consider [to_int63_ns] instead. *)
  val to_int_ns : t   -> int
  val of_int_ns : int -> t

  module Stable : sig
    module V1 : sig
      type nonrec t = t with sexp, bin_io
    end
  end
end

module Option : sig
  type time

  type t = private int with typerep

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
      type nonrec t = t with sexp, bin_io
    end
  end
end with type time := t

module Ofday : sig
  type time

  type t = private Int63.t with typerep

  include Identifiable with type t := t

  val to_ofday : t -> Time.Ofday.t
  val of_ofday : Time.Ofday.t -> t

  val local_now : unit -> t
  val of_local_time : time -> t
  val to_millisecond_string : t -> string

  val start_of_day : t
  val end_of_day : t

  val to_span_since_start_of_day : t -> Span.t
  val of_span_since_start_of_day : Span.t -> t

  module Stable : sig
    module V1 : sig
      type nonrec t = t with sexp, bin_io
    end
  end
end with type time := t

include Identifiable with type t := t

val epoch : t (** Unix epoch (1970-01-01 00:00:00 UTC) *)

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

val to_int63_ns_since_epoch : t ->      Int63.t
val of_int63_ns_since_epoch : Int63.t ->      t

(** Will raise on 32-bit platforms.  Consider [to_int63_ns_since_epoch] instead. *)
val to_int_ns_since_epoch : t   -> int
val of_int_ns_since_epoch : int -> t

val of_date_ofday : Zone.t -> Date.t -> Ofday.t -> t

val to_date : t -> Zone.t -> Date.t

val occurrence
  :  [ `First_after_or_at | `Last_before_or_at ]
  -> t
  -> ofday:Ofday.t
  -> zone:Zone.t
  -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t with bin_io, compare, sexp
  end
  module Span   : module type of Span   .Stable
  module Option : module type of Option .Stable
  module Ofday  : module type of Ofday  .Stable
end
