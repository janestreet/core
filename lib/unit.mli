(** Module for the type [unit].  This is mostly useful for building functor arguments. *)

type t = unit

include Identifiable.S with type t := t

module type S = sig end

type m = (module S)
