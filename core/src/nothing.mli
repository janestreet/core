@@ portable

(** This module extends {{!Base.Nothing} [Base.Nothing]}. *)

open! Import

(** @inline *)
include module type of struct
  include Base.Nothing
end

(** It may seem weird that this is identifiable, but we're just trying to anticipate all
    the contexts in which people may need this. It would be a crying shame if you had some
    variant type involving [Nothing.t] that you wished to make identifiable, but were
    prevented for lack of [Identifiable.S] here.

    Obviously, [of_string] and [t_of_sexp] will raise an exception. *)
include%template
  Identifiable.S
  [@mode local]
  with type t := t
   and type comparator_witness := comparator_witness

include Diffable.S_atomic with type t := t

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , enumerate
      , equal ~localize
      , hash
      , sexp
      , stable_witness
      , sexp_grammar]

    include Diffable.S_atomic with type t := t
  end
end
