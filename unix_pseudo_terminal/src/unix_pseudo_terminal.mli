open Core

module Unix = Caml_unix

type openpt_flag =
  | O_RDWR                      (** Open for reading and writing *)
  | O_NOCTTY                    (** Don't make this dev a controlling tty *)

val posix_openpt : (openpt_flag list -> Unix.file_descr) Or_error.t

val grantpt : (Unix.file_descr -> unit) Or_error.t

val unlockpt : (Unix.file_descr -> unit) Or_error.t

val ptsname : (Unix.file_descr -> string) Or_error.t
