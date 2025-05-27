open! Import

module type Month = sig @@ portable
  type t =
    | Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec
  [@@deriving
    bin_io ~localize
    , compare ~localize
    , equal ~localize
    , globalize
    , hash
    , quickcheck
    , sexp
    , sexp_grammar
    , typerep
    , variants]

  include%template Comparable.S_binable [@mode local] with type t := t

  include Hashable.S_binable with type t := t

  (** [of_string s] accepts three-character abbreviations with three capitalizations (e.g.
      Jan, JAN, and jan). *)
  include Stringable.S with type t := t

  val all : t list

  (** [of_int i] returns the [i]th month if [i] is in 1, 2, ... , 12. Otherwise it returns
      [None]. *)
  val of_int : int -> t option

  val of_int_exn : int -> t

  (** [to_int t] returns an int in 1, 2, ... 12. *)
  val to_int : t -> int

  (** [shift t i] goes forward (or backward) the specified number of months. *)
  val shift : t -> int -> t

  module Export : sig
    type month = t =
      | Jan
      | Feb
      | Mar
      | Apr
      | May
      | Jun
      | Jul
      | Aug
      | Sep
      | Oct
      | Nov
      | Dec
    [@@deprecated
      "[since 2016-06] no longer needed since ocaml is now better at inferring the \
       module where a constructor is defined"]
  end

  module Stable : sig
    module V1 : sig
      type nonrec t = t =
        | Jan
        | Feb
        | Mar
        | Apr
        | May
        | Jun
        | Jul
        | Aug
        | Sep
        | Oct
        | Nov
        | Dec
      [@@deriving equal ~localize, globalize, hash, sexp_grammar]

      include%template
        Stable_module_types.With_stable_witness.S0
        [@mode local]
        with type comparator_witness = comparator_witness
         and type t := t
    end
  end
end
