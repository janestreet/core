(** Extends {{!Base.Fn}[Base.Fn]}. *)

include Base.Fn (** @open *)

include Deprecate_pipe_bang
