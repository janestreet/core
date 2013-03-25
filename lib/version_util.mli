(** This module gives access to the same version/build information returned by
    [Command]-based executables when called with the [-version] or [-build-info] flags
    by [$0 version (-build-info | -version)] or [$0 (-build-info | -version)].
    Here's how it works: we arrange for the build system to, at link time, include an
    object file that defines symbols that version_util.ml uses to get the strings that
    contain the information that this module provides.  When building with OMake, our
    OMakeroot runs build_info.sh to generate *.build_info.c with the symbols and that is
    linked in.
*)

open Std_internal

val version : string
val version_list : string list
val arg_spec : (string * Core_arg.spec * string) list

(** [Application_specific_fields] is a single field in the build-info sexp that holds
    a [Sexp.t String.Map.t], which can be chosen by the application to hold custom
    fields that it needs. *)
module Application_specific_fields : sig
  type t = Sexp.t String.Map.t with sexp

  (** [putenv t] stores [t] in the process environment so that build_info.sh will see it.
      One calls [putenv t] in a program before calling OMake to set the appropriate
      environment variable so that the application-specific fields in the program being
      compiled will have value [t].   That is, one calls [putenv] in the program building
      the application and [Version_util.application_specific_fields] in the application
      itself. *)
  val putenv : t -> unit
end

val build_info : string
val build_info_as_sexp : Sexp.t

val username                       : string
val hostname                       : string
val kernel                         : string
val time                           : Time.t
val x_library_inlining             : bool
val compiled_for_speed             : bool
val application_specific_fields    : Application_specific_fields.t option
val ocaml_version                  : string
val executable_path                : string (** Relative to OMakeroot dir *)

