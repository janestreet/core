(* Conversions between units of measure based on bytes. *)

module Measure : sig
  type t = [ `Bytes | `Kilobytes | `Megabytes | `Gigabytes | `Words ]
  with sexp, bin_io
end

type t with bin_io, sexp

(** [create measure value] creates a [t] from [value] units of the given measure. *)
val create : Measure.t -> float -> t

include Comparable.S with type t := t
include Hashable  .S with type t := t
include Stringable.S with type t := t

(** [to_string_hum ?measure t] returns a string representation of [t].  If [measure] is
    not given then the largest measure (excluding [`Words]) is used that causes the
    translated value to exceed 1. *)
val to_string_hum : ?measure:Measure.t -> t -> string

val bytes     : t -> float
val kilobytes : t -> float
val megabytes : t -> float
val gigabytes : t -> float
val words     : t -> float

(** [scale t mul] scale the measure [t] by [mul] *)
val scale : t -> float -> t

module Infix : sig
  val ( - ) : t -> t -> t
  val ( + ) : t -> t -> t
  (** [( / ) t mul] scales [t] by [1/mul] *)
  val (/)   : t -> float -> t

  (** [( // ) t1 t2] returns the ratio of t1 to t2 *)
  val (//)  : t -> t -> float
end
