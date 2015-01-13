type t = Date0.t

include module type of Date0 with type t := t

val of_time : Time.t -> zone:Zone.t -> t
val today : zone:Zone.t -> t

(** This formats a date using the format patterns available in [strftime]. Uses machine
    timezone where applicable. *)
val format : t -> string -> string

