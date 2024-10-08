include Base.Stack

include Bin_prot.Utils.Make_binable1_without_uuid [@alert "-legacy"] (struct
    type nonrec 'a t = 'a t

    module Binable = List

    let to_binable = to_list
    let of_binable = of_list
  end)

include
  Quickcheckable.Of_quickcheckable1
    (List)
    (struct
      type nonrec 'a t = 'a t

      let to_quickcheckable = to_list
      let of_quickcheckable = of_list
    end)
