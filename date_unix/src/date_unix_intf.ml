open! Core

module type Date_unix = sig
  type t := Core_kernel.Date.t

  (** This formats a date using the format patterns available in [strftime]. *)
  val format : t -> string -> string

  (** This parses a date using the format patterns available in [strptime]. *)
  val parse : fmt:string -> string -> t

  val of_tm : Unix.tm -> t
end
