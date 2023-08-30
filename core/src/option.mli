(** This module extends {{!Base.Option}[Base.Option]} with bin_io, quickcheck, and support
    for ppx_optional. *)

type 'a t = 'a Base.Option.t [@@deriving bin_io ~localize, typerep]

(** @inline *)
include module type of struct
    include Base.Option
  end
  with type 'a t := 'a option

include Comparator.Derived with type 'a t := 'a t
include Quickcheckable.S1 with type 'a t := 'a t

val validate : none:unit Validate.check -> some:'a Validate.check -> 'a t Validate.check

module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t
    [@@deriving
      bin_io ~localize, compare, equal, hash, sexp, sexp_grammar, stable_witness]
  end
end

(** You might think that it's pointless to have [Optional_syntax] on options because OCaml
    already has nice syntax for matching on options.  The reason to have this here is that
    you might have, for example, a tuple of an option and some other type that supports
    [Optional_syntax].  Since [Optional_syntax] can only be opted into at the granularity
    of the whole match expression, we need this [Optional_syntax] support for options in
    order to use it for the other half of the tuple. *)
module Optional_syntax : Optional_syntax.S1 with type 'a t := 'a t and type 'a value := 'a
