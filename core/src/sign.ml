open! Import
module Sign = Base.Sign

module Stable = struct
  module V1 = struct
    type t = Sign.t =
      | Neg
      | Zero
      | Pos
    [@@deriving
      sexp, sexp_grammar, bin_io ~localize, compare ~localize, hash, typerep, enumerate]
  end
end

include Stable.V1
include Sign

include%template Identifiable.Extend [@mode local] [@modality portable] (Sign) (Stable.V1)
