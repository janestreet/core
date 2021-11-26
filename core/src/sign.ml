open! Import
module Sign = Base.Sign

module Stable = struct
  module V1 = struct
    type t = Sign.t =
      | Neg
      | Zero
      | Pos
    [@@deriving sexp, bin_io, compare, hash, typerep, enumerate]
  end
end

include Stable.V1
include Sign
include Identifiable.Extend (Sign) (Stable.V1)
