(** This module extends the {{!Base.Linked_queue}[Base.Linked_queue]} module with bin_io
    support.  As a reminder, the [Base.Linked_queue] module is a wrapper around OCaml's
    standard [Queue] module that follows Base idioms and adds some functions.

    See also {!Core.Queue}, which has different performance characteristics. *)

type 'a t = 'a Base.Linked_queue.t [@@deriving bin_io]

(** @inline *)
include module type of struct
    include Base.Linked_queue
  end
  with type 'a t := 'a t
