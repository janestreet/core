(** This module is here to avoid holding the runtime lock when doing an
    open_in_gen lock. This has been upstreamed as a patch in 3.12.1. *)
(** This module exists only to workaround some undesirable behavior in
    the function [caml_sys_open] in the ocaml runtime.  Delete this
    module in favor of the same functions in [Pervasives] once the
    runtime is patched to our satisfaction *)

(** Reimplementation of [Pervasives.open_in_gen] *)
val open_in_gen : open_flag list -> int -> string -> in_channel

(** Reimplementation of [Pervasives.open_out_gen] *)
val open_out_gen : open_flag list -> int -> string -> out_channel

