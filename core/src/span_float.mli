open! Import
include Span_intf.S with type underlying = float

module Stable : sig
  (** [V1]'s sexps use single-unit format and support units from [d] to [ms]; it does not
      support [us] or [ns]. [V1]'s sexp conversions do not round-trip precisely. *)
  module V1 : sig
    type nonrec t = t
    [@@deriving sexp, sexp_grammar, bin_io, compare, hash, equal, stable_witness, diff]
  end

  (** [V2]'s sexps use single-unit format and support units from [d] to [ns]. [V2] can
      read [V1] sexps but not vice versa. [V2]'s sexp conversions do not round-trip
      precisely. *)
  module V2 : sig
    type nonrec t = t
    [@@deriving sexp, sexp_grammar, bin_io, compare, hash, equal, stable_witness, diff]
  end

  (** [V3] uses mixed-unit format and supports units from [d] to [ns]. [V3] can read [V2]
      and [V1] sexps but not vice versa. [V3]'s sexp conversions round-trip precisely. *)
  module V3 : sig
    type nonrec t = t
    [@@deriving
      sexp, sexp_grammar, bin_io, compare, hash, typerep, equal, stable_witness, diff]
  end
end

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val parse_suffix : string -> index:int -> Unit_of_time.t
  val suffix_of_unit_of_time : Unit_of_time.t -> string
end
