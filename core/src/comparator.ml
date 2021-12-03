open! Import
module Comparator = Base.Comparator

type ('a, 'witness) t = ('a, 'witness) Comparator.t = private
  { compare : 'a -> 'a -> int
  ; sexp_of_t : 'a -> Base.Sexp.t
  }

module type Base_mask = module type of Comparator with type ('a, 'b) t := ('a, 'b) t

include (Comparator : Base_mask)

module Stable = struct
  module V1 = struct
    type nonrec ('a, 'witness) t = ('a, 'witness) t = private
      { compare : 'a -> 'a -> int
      ; sexp_of_t : 'a -> Base.Sexp.t
      }

    type ('a, 'b) comparator = ('a, 'b) t

    module type S = S
    module type S1 = S1

    let make = make

    module Make = Make
    module Make1 = Make1
  end
end
