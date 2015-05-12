(** Crc functions *)

(** Compute the 32-bit crc *)
val crc32 : string -> int64

(** String version of the crc, encoded in hex. *)
val crc32hex : string -> string
