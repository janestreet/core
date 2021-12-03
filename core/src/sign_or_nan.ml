open! Import
module Sign_or_nan = Base.Sign_or_nan

module Stable = struct
  module V1 = struct
    type t = Sign_or_nan.t =
      | Neg
      | Zero
      | Pos
      | Nan
    [@@deriving sexp, bin_io, compare, hash, typerep, enumerate]
  end
end

include Stable.V1
include Sign_or_nan
include Identifiable.Extend (Sign_or_nan) (Stable.V1)
