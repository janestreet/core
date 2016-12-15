open! Import

(** User code never calls this.  It is called only in [std.ml], as a top-level side
    effect, so that [am_running_inline_test] is set correctly in child processes. *)
val initialize_module : unit -> unit
