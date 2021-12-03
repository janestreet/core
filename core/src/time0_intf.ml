open! Import
open Std_internal

module type Basic = sig
  module Span : Span_intf.S

  type t

  module Replace_polymorphic_compare : Comparable.Polymorphic_compare with type t := t
  include Comparable.Polymorphic_compare with type t := t
  include Robustly_comparable with type t := t

  val add : t -> Span.t -> t
  val sub : t -> Span.t -> t
  val diff : t -> t -> Span.t

  (** [next t] returns the next t (forwards in time) *)
  val next : t -> t

  (** [prev t] returns the previous t (backwards in time) *)
  val prev : t -> t

  val to_span_since_epoch : t -> Span.t
  val of_span_since_epoch : Span.t -> t
end

module type S = sig
  type underlying
  type t = private underlying [@@deriving bin_io, compare, hash, typerep]

  module Span : Span_intf.S with type underlying = underlying
  module Ofday : Ofday_intf.S with type underlying := underlying and module Span := Span
  include Basic with type t := t and module Span := Span

  include
    Comparable.S_common
    with type t := t
     and module Replace_polymorphic_compare := Replace_polymorphic_compare

  (** Equivalent to a [Date.t] and an [Ofday.t] with no time zone. A [Date_and_ofday.t]
      does not correspond to a single, unambiguous point in time. *)
  module Date_and_ofday : sig
    type absolute = t
    type t = private underlying

    (** {2 Constructors and accessors} *)

    val of_date_ofday : Date0.t -> Ofday.t -> t
    val to_date_ofday : t -> Date0.t * Ofday.t
    val to_date : t -> Date0.t
    val to_ofday : t -> Ofday.t

    (** {2 Conversions between absolute times and date + ofday}

        Based on the offset from UTC at the given time. It is usually simpler to use the
        [Time.Zone] wrappers of these conversions. *)

    val of_absolute : absolute -> offset_from_utc:Span.t -> t
    val to_absolute : t -> offset_from_utc:Span.t -> absolute

    (** {2 Low-level conversions}

        Convert between [t] and a synthetic span representing the difference in date from
        epoch, times the length of a day, plus the ofday's distance from midnight.

        These spans do not correspond with any actual duration of time. Arithmetic on
        these spans is only meaningful within a range where no DST transitions can occur.
        For example, rounding to the nearest second makes sense but adding arbitrary spans
        does not.

        These functions are intended for low-level DST transition arithmetic. Most clients
        should not call these functions directly. *)

    val of_synthetic_span_since_epoch : Span.t -> t
    val to_synthetic_span_since_epoch : t -> Span.t
  end
  with type absolute := t

  (** [next_multiple ~base ~after ~interval] returns the smallest [time] of the form:

      {[
        time = base + k * interval
      ]}

      where [k >= 0] and [time > after].  It is an error if [interval <= 0].

      Supplying [~can_equal_after:true] allows the result to satisfy [time >= after].
  *)
  val next_multiple
    :  ?can_equal_after:bool (** default is [false] *)
    -> base:t
    -> after:t
    -> interval:Span.t
    -> unit
    -> t

  (** [prev_multiple ~base ~before ~interval] returns the largest [time] of the form:

      {[
        time = base + k * interval
      ]}

      where [k >= 0] and [time < before].  It is an error if [interval <= 0].

      Supplying [~can_equal_before:true] allows the result to satisfy [time <= before].
  *)
  val prev_multiple
    :  ?can_equal_before:bool (** default is [false] *)
    -> base:t
    -> before:t
    -> interval:Span.t
    -> unit
    -> t

  val now : unit -> t
end
