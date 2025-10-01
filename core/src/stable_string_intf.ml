open! Import

module type Identifiable_without_binio = sig
  type t [@@deriving equal ~localize, hash, sexp_grammar]
  type comparator_witness

  include%template Base.Stringable.S [@alloc stack] with type t := t

  include%template
    Stable_comparable.With_stable_witness.V1
    [@mode local]
    with type t := t
    with type comparator_witness := comparator_witness

  include Hashable.Stable.V1.With_stable_witness.S with type key := t
end

module type Stable_string = sig @@ portable
  module type Identifiable_without_binio = Identifiable_without_binio

  module V1 : sig
    type t = string
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , diff ~extra_derive:[ sexp ]
      , equal ~localize
      , globalize
      , quickcheck]

    include
      Identifiable_without_binio
      with type t := t
       and type comparator_witness = Base.String.comparator_witness
  end
end
