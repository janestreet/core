module Stable = struct
  module V1 = struct
    include (
      String.Stable.V1 :
        sig
          type t = string
          [@@deriving
            bin_io ~localize, compare, equal, hash, sexp, sexp_grammar, stable_witness]

          include
            Comparable.Stable.V1.With_stable_witness.S
              with type comparable := t
              with type comparator_witness = String.Stable.V1.comparator_witness

          val comparator : (t, comparator_witness) Comparator.t

          include Hashable.Stable.V1.With_stable_witness.S with type key := t
        end)
  end
end

open! Import
open! Std_internal
include Filename_base

include (
  String :
    sig
      type t = string [@@deriving bin_io ~localize]

      include
        Comparable.S with type t := t and type comparator_witness := comparator_witness

      include Hashable.S with type t := t
    end)
