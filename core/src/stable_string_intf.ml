open! Import

module type Identifiable_without_binio = sig
  type t [@@deriving equal, hash, sexp_grammar]
  type comparator_witness

  include Base.Stringable.S with type t := t

  include
    Stable_comparable.With_stable_witness.V1
    with type t := t
    with type comparator_witness := comparator_witness

  include Hashable.Stable.V1.With_stable_witness.S with type key := t
end

module type Stable_string = sig
  module type Identifiable_without_binio = Identifiable_without_binio

  module V1 : sig
    type t = string
    [@@deriving
      bin_io, compare, diff ~extra_derive:[ sexp ], equal, globalize, quickcheck, sexp_of]

    include
      Identifiable_without_binio
      with type t := t
       and type comparator_witness = Base.String.comparator_witness
  end
end
