open! Import
open Std_internal
include Time_ns_intf.Ofday with module Span := Span_ns

module Stable : sig
  module V1 :
    Stable_int63able with type t = t and type comparator_witness = comparator_witness

  module Option : sig end [@@deprecated "[since 2021-02] Use [Time_ns_unix.Stable]"]
  module Zoned : sig end [@@deprecated "[since 2021-02] Use [Time_ns_unix.Stable]"]
end
