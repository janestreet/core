open! Import

module Stable = struct
  module V1 = struct
    include Base.Source_code_position

    type t = Base.Source_code_position.t =
      { pos_fname : string
      ; pos_lnum : int
      ; pos_bol : int
      ; pos_cnum : int
      }
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , fields ~getters
      , globalize
      , hash
      , sexp
      , sexp_grammar
      , stable_witness]
  end
end

include Stable.V1

let to_string = Base.Source_code_position.to_string
let sexp_of_t = Base.Source_code_position.sexp_of_t
