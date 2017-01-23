open! Import

module Date = Core_kernel.Date

module type Core_date = sig
  type t = Date.t
  include module type of Date with type t := t

  (** This formats a date using the format patterns available in [strftime]. *)
  val format : t -> string -> string

  (** This parses a date using the format patterns available in [strptime]. *)
  val parse : fmt:string -> string -> t

  val of_tm : Core_unix.tm -> t
end
