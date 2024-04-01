(** This module extends {{!Base.Source_code_position}[Base.Source_code_position]}. *)

(** @inline *)
include module type of struct
  include Base.Source_code_position
end

type t = Base.Source_code_position.t =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving fields ~getters]

include Comparable.S with type t := t and type comparator_witness := comparator_witness
include Hashable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving equal]

    include Stable_module_types.With_stable_witness.S0 with type t := t
  end
end
