(** This module extends {{!Base.Int_intf} [Base.Int_intf]}. *)

module type Round = Base.Int.Round

module type Stable = sig @@ portable
  module V1 : sig
    type t [@@deriving equal ~localize, globalize, hash, sexp_grammar]

    include%template
      Stable_comparable.With_stable_witness.V1 [@mode local] with type t := t
  end
end

module type Binaryable = sig
  type t

  module Binary : sig
    type nonrec t = t [@@deriving bin_io, typerep]

    include Base.Int.To_string_format with type t := t
  end

  (*_ Ensure that this module is an extension of [Base.Int.Binary]. *)
  include Base.Int.Binaryable with type t := t and module Binary := Binary
end

module type Hexable = sig
  type t

  module Hex : sig
    type nonrec t = t [@@deriving bin_io, typerep]

    include Base.Int.String_format with type t := t
  end

  (*_ Ensure that this module is an extension of [Base.Int.Hexable]. *)
  include Base.Int.Hexable with type t := t and module Hex := Hex
end

module type Extension = sig @@ portable
  type t [@@deriving bin_io ~localize, typerep]

  include Binaryable with type t := t
  include Hexable with type t := t

  include%template Identifiable.S [@mode local] with type t := t

  include Base.Stringable.S_local_input with type t := t
  include Comparable.Validate_with_zero with type t := t

  include%template Quickcheckable.S_int [@mode portable] with type t := t
end

module type S_unbounded = sig @@ portable
  include Base.Int.S_unbounded
  include Extension with type t := t with type comparator_witness := comparator_witness
end

module type S = sig @@ portable
  include Base.Int.S
  include Extension with type t := t with type comparator_witness := comparator_witness
end

module type Extension_with_stable = sig @@ portable
  include Extension

  module Stable :
    Stable with type V1.t = t and type V1.comparator_witness = comparator_witness
end
