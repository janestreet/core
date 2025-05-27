@@ portable

open! Import
include Time_ns_intf.Ofday with module Span := Span_ns

module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving
      bin_io ~localize
      , compare ~localize
      , equal ~localize
      , globalize
      , hash
      , sexp_grammar
      , typerep]

    include%template
      Stable_int63able.With_stable_witness.S
      [@mode local]
      with type t := t
       and type comparator_witness = comparator_witness

    include Diffable.S_atomic with type t := t
  end

  module Option : sig end [@@deprecated "[since 2021-02] Use [Time_ns_unix.Stable]"]
  module Zoned : sig end [@@deprecated "[since 2021-02] Use [Time_ns_unix.Stable]"]
end
