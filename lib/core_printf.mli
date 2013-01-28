
(* Standard Library *)

val fprintf  : out_channel -> ('r, out_channel, unit) format -> 'r
val printf   :                ('r, out_channel, unit) format -> 'r
val eprintf  :                ('r, out_channel, unit) format -> 'r
val ifprintf : 'a ->                   ('r, 'a, unit) format -> 'r
val sprintf  :                     ('r, unit, string) format -> 'r
val bprintf  :       Buffer.t -> ('r, Buffer.t, unit) format -> 'r

val kfprintf :
  (out_channel -> 'a) -> out_channel -> ('r, out_channel, unit, 'a) format4 -> 'r
val ksprintf :
  (string -> 'a) -> ('r, unit, string, 'a) format4 -> 'r
val kbprintf :
  (Buffer.t -> 'a) -> Buffer.t -> ('r, Buffer.t, unit, 'a) format4 -> 'r

(** {6 Formatting error and exit functions} *)

(** raises Failure *)
val failwithf : ('r, unit, string, unit -> _) format4 -> 'r

(** raises Invalid_arg *)
val invalid_argf : ('r, unit, string, unit -> _) format4 -> 'r

(** print to stderr; exit 1 *)
val exitf : ('r, unit, string, unit -> _) format4 -> 'r
