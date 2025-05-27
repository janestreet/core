@@ portable

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
[@@deriving
  bin_io ~localize
  , compare ~localize
  , fields ~getters
  , globalize
  , hash
  , sexp
  , sexp_grammar]

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , hash
      , sexp
      , sexp_grammar
      , stable_witness]

    include Comparator.Stable.V1.S with type t := t
  end
end
