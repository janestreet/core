open! Import
include Base.Source_code_position
include Source_code_position0
include Comparable.Extend (Base.Source_code_position) (Source_code_position0)
include Hashable.Make (Source_code_position0)
