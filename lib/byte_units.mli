(* Conversions between units of measure based on bytes. *)

module Measure : sig
  type t = [ `Bytes | `Kilobytes | `Megabytes | `Gigabytes | `Words ]
end

type t with bin_io, sexp

(* [create measure float] creates a [t] that will use [measure] when converting [t] to
   a string or sexp. *)
val create : Measure.t -> float -> t

include Comparable.S with type t := t
include Hashable  .S with type t := t
include Stringable.S with type t := t

val bytes     : t -> float
val kilobytes : t -> float
val megabytes : t -> float
val gigabytes : t -> float
val words     : t -> float

module Infix : sig
  (* for [-] and [+], the resulting value has the smallest of the
     preferred measures of the two inputs *)
  val ( - ) : t -> t -> t
  val ( + ) : t -> t -> t
  val ( * ) : t -> float -> t
  val ( / ) : t -> float -> t
end
