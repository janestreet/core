include Base.Stack

include%template
  Bin_prot.Utils.Make_binable1_without_uuid [@modality portable] [@alert "-legacy"] (struct
    type nonrec 'a t = 'a t

    module Binable = List

    let to_binable = to_list
    let of_binable = of_list
  end)

include%template
  Quickcheckable.Of_quickcheckable1 [@modality portable]
    (List)
    (struct
      type nonrec 'a t = 'a t

      let to_quickcheckable = to_list
      let of_quickcheckable = of_list
    end)
