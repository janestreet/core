(** Extends {{!Core_kernel.Caml}[Core_kernel.Caml]}. *)

include Core_kernel.Caml (** @open *)

module Condition  = Condition
module Mutex      = Mutex
module Thread     = Thread
module Unix       = Unix
module UnixLabels = UnixLabels
