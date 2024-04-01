open! Import

include module type of struct
  include Base.Source_code_position
end

type t = Base.Source_code_position.t =
  { pos_fname : string
  ; pos_lnum : int
  ; pos_bol : int
  ; pos_cnum : int
  }
[@@deriving bin_io, compare, fields ~getters, hash, sexp]

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io, compare, equal, hash, sexp, stable_witness]

    include Comparator.Stable.V1.S with type t := t
  end
end
