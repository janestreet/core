(** This module extends {{!Base.Int63} [Base.Int63]}. *)

(** {2 Interface from Base} *)

(** @inline *)
include module type of struct
  include Base.Int63
end

(** {2 Extensions} *)

(** @inline *)
include
  Int_intf.Extension with type t := t and type comparator_witness := comparator_witness @@ 
portable

module Stable : sig @@ portable
  module V1 : sig
    type nonrec t : immediate64 = t
    [@@deriving
      bin_io ~localize ~portable
      , compare ~localize ~portable
      , equal ~localize ~portable
      , globalize
      , hash
      , sexp_grammar]

    include
      Stable_comparable.With_stable_witness.V1
      with type t := t
       and type comparator_witness = comparator_witness
  end
end
