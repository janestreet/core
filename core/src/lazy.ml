open! Import
open Base_quickcheck.Export

module Stable = struct
  module V1 = struct
    open Sexplib.Std

    type 'a t = 'a lazy_t
    [@@deriving
      bin_io ~localize, quickcheck, sexp ~localize, sexp_grammar, typerep, stable_witness]

    let map = Base.Lazy.map
    let compare = Base.Lazy.compare
    let compare__local = Base.Lazy.compare__local
    let equal = Base.Lazy.equal
    let equal__local = Base.Lazy.equal__local
  end
end

module type Base_mask = module type of Base.Lazy with type 'a t := 'a Stable.V1.t

include Stable.V1
include (Base.Lazy : Base_mask)
