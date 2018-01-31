(** CRC functions. *)

open! Import

(** Computes the 32-bit CRC. *)
val crc32 : string -> Int63.t
val bigstring_crc32 : Bigstring.t -> pos:int -> len:int -> Int63.t

(** String version of the CRC, encoded in hex. *)
val crc32hex : string -> string
