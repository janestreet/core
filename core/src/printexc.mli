(** This module is here to ensure that we don't use the functions in [Caml.Printexc]
    inadvertently. *)

open! Import

val to_string : exn -> [ `Deprecated_use_Exn_to_string_instead ]
val print : exn -> [ `Deprecated_use_Exn_to_string_instead ]
val catch : ('a -> _) -> 'a -> [ `Deprecated_use_Exn_handle_uncaught_instead ]


val print_backtrace : out_channel -> unit
val get_backtrace : unit -> string
val record_backtrace : bool -> unit
val backtrace_status : unit -> bool
