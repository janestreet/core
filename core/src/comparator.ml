open! Import
module Comparator = Base.Comparator

type ('a, 'witness) t = ('a, 'witness) Comparator.t

module type Base_mask = module type of Comparator with type ('a, 'b) t := ('a, 'b) t

include (Comparator : Base_mask)

module%template Stable = struct
  module V1 = struct
    include Comparator_intf.Definitions.Stable.V1

    type nonrec ('a, 'witness) t = ('a, 'witness) t
    type ('a, 'b) comparator = ('a, 'b) t

    [@@@modality.default p = (portable, nonportable)]

    let make = (make [@modality p])

    [@@@mode.default m = (global, local)]

    module Make = Make [@mode m] [@modality p]
    module Make1 = Make1 [@mode m] [@modality p]
  end
end
