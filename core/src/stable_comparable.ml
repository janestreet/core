module type%template
  [@mode m = (global, local)] [@modality p = (portable, nonportable)] V1 = sig
  include Stable_module_types.S0 [@mode m] [@modality p]

  include
    Comparable.Stable.V1.S
    with type comparable := t
    with type comparator_witness := comparator_witness
end

module With_stable_witness = struct
  module type%template
    [@mode m = (global, local)] [@modality p = (portable, nonportable)] V1 = sig
    include Stable_module_types.With_stable_witness.S0 [@mode m] [@modality p]

    include
      Comparable.Stable.V1.With_stable_witness.S
      with type comparable := t
      with type comparator_witness := comparator_witness
  end
end
