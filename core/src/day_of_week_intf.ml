(** For representing a day of the week. *)

open! Import

module type Day_of_week = sig
  type t =
    | Sun
    | Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
  [@@deriving bin_io ~localize, compare, hash, quickcheck, sexp, sexp_grammar, typerep]

  include Comparable.S_binable with type t := t
  include Hashable.S_binable with type t := t

  (** [of_string s] accepts three-character abbreviations and full day names
      with any capitalization, and strings of the integers 0-6. *)
  include Stringable.S with type t := t

  (** Capitalized full day names rather than all-caps 3-letter abbreviations. *)
  val to_string_long : t -> string

  (** These use the same mapping as [Unix.tm_wday]: 0 <-> Sun, ... 6 <-> Sat *)
  val of_int_exn : int -> t

  val of_int : int -> t option
  val to_int : t -> int

  (** As per ISO 8601, Mon->1, Tue->2, ... Sun->7 *)
  val iso_8601_weekday_number : t -> int

  (** [shift] goes forward (or backward) the specified number of days. *)
  val shift : t -> int -> t

  (** [num_days ~from ~to_] gives the number of days that must elapse from a [from] to get
      to a [to_], i.e., the smallest non-negative number [i] such that [shift from i =
      to_].
  *)
  val num_days : from:t -> to_:t -> int

  val is_sun_or_sat : t -> bool
  val all : t list

  (** [ Mon; Tue; Wed; Thu; Fri ] *)
  val weekdays : t list

  (** [ Sat; Sun ] *)
  val weekends : t list

  module Stable : sig
    module V1 : sig
      type nonrec t = t
      [@@deriving
        bin_io ~localize, equal, sexp, sexp_grammar, compare, hash, stable_witness]

      include
        Comparable.Stable.V1.With_stable_witness.S
          with type comparable := t
          with type comparator_witness := comparator_witness

      include Hashable.Stable.V1.With_stable_witness.S with type key := t
    end
  end
end
