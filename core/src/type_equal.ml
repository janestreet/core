include Base.Type_equal

module Id = struct
  include Id

  module Uid = struct
    module Upstream = Base.Type_equal.Id.Uid
    include Base.Type_equal.Id.Uid

    include%template
      Comparable.Extend_plain [@mode local] [@modality portable]
        (Upstream)
        (struct
          type t = Base.Type_equal.Id.Uid.t [@@deriving sexp_of]
        end)

    include%template Hashable.Make_plain [@modality portable] (Upstream)
  end
end
