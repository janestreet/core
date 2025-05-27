open! Import

module T = struct
  include Base.Source_code_position
  include Source_code_position0

  include%template
    Comparable.Extend [@mode local] [@modality portable]
      (Base.Source_code_position)
      (Source_code_position0)

  include%template Hashable.Make [@modality portable] (Source_code_position0)
end

include T

module With_hiding = struct
  include T

  let to_string t =
    if am_running_test then String.concat [ t.pos_fname; ":LINE:COL" ] else to_string t
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)
end
