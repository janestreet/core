@@ portable

open! Import
include Time_ns_intf.Span

include%template Quickcheck.S_int [@mode portable] with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving
      bin_io ~localize, compare ~localize, equal ~localize, globalize, hash, sexp_grammar]

    include%template Stable_int63able.With_stable_witness.S [@mode local] with type t := t

    include Diffable.S_atomic with type t := t
  end

  module Option : sig
    module V1 : sig
      type nonrec t = t
      [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

      include%template
        Stable_int63able.With_stable_witness.S [@mode local] with type t := t

      include Diffable.S_atomic with type t := t
    end

    module V2 : sig
      type nonrec t = t
      [@@deriving bin_io ~localize, compare ~localize, equal ~localize, globalize]

      include%template
        Stable_int63able.With_stable_witness.S [@mode local] with type t := t

      include Diffable.S with type t := t and type Diff.t = t
    end
  end

  module V2 : sig
    type nonrec t = t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , sexp_grammar
      , stable_witness
      , typerep]

    type nonrec comparator_witness = comparator_witness

    include%template
      Stable_int63able.With_stable_witness.S
      [@mode local]
      with type t := t
      with type comparator_witness := comparator_witness

    include
      Comparable.Stable.V1.With_stable_witness.S
      with type comparable := t
      with type comparator_witness := comparator_witness

    include Stringable.S with type t := t
    include Diffable.S_atomic with type t := t
    include Quickcheck.S with type t := t
  end
end
