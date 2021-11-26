module type V1 = sig
  include Stable_module_types.S0

  include
    Comparable.Stable.V1.S
    with type comparable := t
    with type comparator_witness := comparator_witness
end
