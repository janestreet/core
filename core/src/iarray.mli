@@ portable

open! Import

type 'a t = 'a Base.Iarray.t [@@deriving bin_io ~localize, quickcheck, typerep]

include Base.Iarray.Public with type 'a t := 'a t (** @inline *)

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t
    [@@deriving compare ~localize, equal ~localize, globalize, hash, sexp, sexp_grammar]

    include%template
      Stable_module_types.With_stable_witness.S1 [@mode local] with type 'a t := 'a t
  end
end
