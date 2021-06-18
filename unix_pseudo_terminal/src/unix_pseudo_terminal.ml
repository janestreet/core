open Core

module Unix = Caml_unix

type openpt_flag =
  | O_RDWR                      (** Open for reading and writing *)
  | O_NOCTTY                    (** Don't make this dev a controlling tty *)

[%%import "config_ext.h"]
[%%ifdef JSC_UNIX_PTY]
external unix_posix_openpt : openpt_flag list -> Unix.file_descr = "unix_posix_openpt"
external unix_grantpt : Unix.file_descr -> unit = "unix_grantpt"
external unix_unlockpt : Unix.file_descr -> unit = "unix_unlockpt"
external unix_ptsname : Unix.file_descr -> string = "unix_ptsname"

let posix_openpt = Ok unix_posix_openpt
let grantpt = Ok unix_grantpt
let unlockpt = Ok unix_unlockpt
let ptsname = Ok unix_ptsname
[%%else]
let posix_openpt = Or_error.unimplemented "Unix_pseudo_terminal.unix_posix_openpt"
let grantpt = Or_error.unimplemented "Unix_pseudo_terminal.unix_grantpt"
let unlockpt = Or_error.unimplemented "Unix_pseudo_terminal.unix_unlockpt"
let ptsname = Or_error.unimplemented "Unix_pseudo_terminal.unix_ptsname"
[%%endif]
