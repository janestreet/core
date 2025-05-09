open! Import
module Comparator = Base.Comparator

type ('a, 'witness) t = ('a, 'witness) Comparator.t

module type Base_mask = module type of Comparator with type ('a, 'b) t := ('a, 'b) t

include (Comparator : Base_mask)

module%template Stable = struct
  module V1 = struct
    type nonrec ('a, 'witness) t = ('a, 'witness) t
    type ('a, 'b) comparator = ('a, 'b) t

    module type S = S
    module type S1 = S1

    [@@@modality.default p = (portable, nonportable)]

    let make = (make [@modality p])

    module Make = Make [@modality p]
    module Make1 = Make1 [@modality p]
  end
end
