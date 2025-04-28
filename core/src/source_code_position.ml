open! Import
include Base.Source_code_position
include Source_code_position0

include%template
  Comparable.Extend [@modality portable]
    (Base.Source_code_position)
    (Source_code_position0)

include%template Hashable.Make [@modality portable] (Source_code_position0)
