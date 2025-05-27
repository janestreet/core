@@ portable

open! Import
open! Std_internal
include module type of Filename_base

include%template Binable.S [@mode local] with type t := t

include%template
  Comparable.S
  [@mode local]
  with type t := t
   and type comparator_witness := comparator_witness

include Hashable.S with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t [@@deriving bin_io ~localize, equal ~localize, hash, sexp_grammar]

    include%template
      Stable_comparable.With_stable_witness.V1
      [@mode local]
      with type t := t
      with type comparator_witness = comparator_witness

    include Hashable.Stable.V1.With_stable_witness.S with type key := t
  end
end
