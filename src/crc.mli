(** Crc functions *)

open! Core_kernel.Std

(** Compute the 32-bit crc *)
val crc32 : string -> Int63.t
val bigstring_crc32 : Bigstring.t -> pos:int -> len:int -> Int63.t

(** String version of the crc, encoded in hex. *)
val crc32hex : string -> string
