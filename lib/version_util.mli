(** This module gives access to the same build information as returned by [$0 version
    -build-info] or [$0 -build-info]. *)

val build_info : string
val version : string
val version_list : string list
val arg_spec : (string * Core_arg.spec * string) list

val username           : string
val hostname           : string
val kernel             : string
val time               : Time.t
val x_library_inlining : bool
val compiled_for_speed : bool
val ocaml_version      : string
val executable_path    : string (** Relative to OMakeroot dir *)
