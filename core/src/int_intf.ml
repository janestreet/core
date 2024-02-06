(** This module extends {{!Base.Int_intf}[Base.Int_intf]}. *)

module type Round = Base.Int.Round

module type Stable = sig
  module V1 : sig
    type t [@@deriving equal, hash, sexp_grammar]

    include Stable_comparable.With_stable_witness.V1 with type t := t
  end
end

module type Binaryable = sig
  type t

  module Binary : sig
    type nonrec t = t [@@deriving bin_io, sexp_of, compare ~localize, hash, typerep]

    val to_string : t -> string
    val to_string_hum : ?delimiter:char -> t -> string
  end

  (*_ Ensure that this module is an extension of [Base.Int.Binary]. *)
  include Base.Int.Binaryable with type t := t and module Binary := Binary
end

module type Hexable = sig
  type t

  module Hex : sig
    type nonrec t = t
    [@@deriving bin_io, sexp, sexp_grammar, compare ~localize, hash, typerep]

    include Base.Stringable.S with type t := t

    val to_string_hum : ?delimiter:char -> t -> string
  end

  (*_ Ensure that this module is an extension of [Base.Int.Hexable]. *)
  include Base.Int.Hexable with type t := t and module Hex := Hex
end

module type Extension = sig
  type t [@@deriving bin_io, typerep]

  include Binaryable with type t := t
  include Hexable with type t := t
  include Identifiable.S with type t := t
  include Comparable.Validate_with_zero with type t := t
  include Quickcheckable.S_int with type t := t
end

module type S_unbounded = sig
  include Base.Int.S_unbounded
  include Extension with type t := t with type comparator_witness := comparator_witness
end

module type S = sig
  include Base.Int.S
  include Extension with type t := t with type comparator_witness := comparator_witness
end

module type Extension_with_stable = sig
  include Extension

  module Stable :
    Stable with type V1.t = t and type V1.comparator_witness = comparator_witness
end
