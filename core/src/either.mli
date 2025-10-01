@@ portable

(** This module extends {{!Base.Either} [Base.Either]}. *)

type ('f, 's) t = ('f, 's) Base.Either.t =
  | First of 'f
  | Second of 's
[@@deriving bin_io ~localize, typerep]

(** @inline *)
include module type of struct
    include Base.Either
  end
  with type ('f, 's) t := ('f, 's) t

include Comparator.Derived2 with type ('a, 'b) t := ('a, 'b) t

include%template Quickcheckable.S2 [@modality portable] with type ('a, 'b) t := ('a, 'b) t

module Stable : sig
  module V1 : sig
    type nonrec ('f, 's) t = ('f, 's) t =
      | First of 'f
      | Second of 's
    [@@deriving bin_io ~localize, equal ~localize, sexp_grammar]

    include%template
      Stable_module_types.With_stable_witness.S2
      [@mode local]
      with type ('f, 's) t := ('f, 's) t
  end
end
