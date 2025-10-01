(** This module extends {{!Base.Int63} [Base.Int63]}. *)

(** {2 Interface from Base} *)

(** @inline *)
include module type of struct
  include Base.Int63
end

(** {2 Extensions} *)

(** @inline *)
include
  Int_intf.Extension with type t := t and type comparator_witness := comparator_witness

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving
      bin_io ~localize, compare ~localize, equal ~localize, globalize, hash, sexp_grammar]
    [@@immediate64]

    include
      Stable_comparable.With_stable_witness.V1
      with type t := t
       and type comparator_witness = comparator_witness
  end
end
