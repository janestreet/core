type t = Date0.t

include module type of Date0 with type t := t

val of_time : Time.t -> t  (** based on local timezone *)
val today : unit -> t      (** based on local timezone *)

(** This formats a date using the format patterns available in [strftime] *)
val format : t -> string -> string  (** based on local timezone *)

