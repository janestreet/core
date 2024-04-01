(** Module for the type [unit], extended from {{!Base.Unit}[Base.Unit]}.  This is mostly
    useful for building functor arguments. *)

open! Import

type t = unit [@@deriving typerep]

(** @inline *)
include module type of struct
    include Base.Unit
  end
  with type t := t

include Identifiable.S with type t := t and type comparator_witness := comparator_witness
include Quickcheckable.S with type t := t

include sig
    type t [@@deriving bin_io ~localize]
  end
  with type t := t

module type S = sig end

type m = (module S)

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io ~localize]

    include Stable_module_types.With_stable_witness.S0 with type t := t
  end

  (** Zero-length bin_prot format.

      The default converter for the type [unit] is the V1 converter, not the V2.  That's
      because there's an assumption that primitive types, which include [unit], are stable
      whether or not they say so, so we can't change the [unit] bin-io converter without
      breaking many stable types.  *)
  module V2 : sig
    type nonrec t = t [@@deriving bin_io ~localize, equal]

    include Stable_module_types.With_stable_witness.S0 with type t := t
  end
end
