type t = Date0.t = private { y: int; m: Month.t; d: int }

include module type of Date0 with type t := t

val of_time : Time.t -> t
val today : unit -> t

(** This formats a date using the format patterns available in [strftime] *)
val format : t -> string -> string

