open! Import

type t = Core_kernel.Std.Date.t

include module type of Core_kernel.Std.Date with type t := t

val of_time : Time.t -> zone:Zone.t -> t
val today : zone:Zone.t -> t

(** This formats a date using the format patterns available in [strftime]. *)
val format : t -> string -> string

(** This parses a date using the format patterns available in [strptime]. *)
val parse : fmt:string -> string -> t

val of_tm : Core_unix.tm -> t
