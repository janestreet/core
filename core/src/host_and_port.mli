@@ portable

(** Type for the commonly-used notion of host and port in networking. *)

open! Std_internal

type t =
  { host : string
  ; port : int
  }
[@@deriving globalize, hash]

val create : host:string -> port:int -> t
val host : t -> string
val port : t -> int
val tuple : t -> string * int

include%template Comparator.S [@modality portable] with type t := t

include%template
  Identifiable.S
  [@mode local]
  with type t := t
   and type comparator_witness := comparator_witness

include%template Base.Stringable.S [@alloc stack] with type t := t

include Sexplib.Sexp_grammar.S with type t := t

module Hide_port_in_test : sig
  include%template
    Identifiable.S
    [@mode local]
    with type t = t
     and type comparator_witness = comparator_witness
end

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving
      sexp
      , sexp_grammar
      , bin_io
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , quickcheck]

    include%template Base.Stringable.S [@alloc stack] with type t := t

    include%template
      Stable_comparable.With_stable_witness.V1
      [@mode local]
      with type t := t
       and type comparator_witness = comparator_witness
  end
end

val type_id : t Type_equal.Id.t
