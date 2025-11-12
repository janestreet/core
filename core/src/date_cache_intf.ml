(** A mechanism for cached calculations of the date and start-of-day for a given time. *)

open! Import

module Definitions = struct
  (** The pieces of [Time] and [Time_ns] needed for calculating the date and start-of-day. *)
  module type Time = sig @@ portable
    type underlying : value mod contended many stateless unyielding
    type t = private underlying

    val epoch : t

    include Comparisons.S with type t := t
    module Span : Span_intf.S with type underlying = underlying

    val add : t -> Span.t -> t
    val sub : t -> Span.t -> t

    module Ofday : Ofday_intf.S with type underlying := underlying and module Span := Span

    module Zone : sig
      type time := t
      type t = Zone.t

      module Index : sig
        type t
      end

      val utc : t
      val index : Zone.t -> time -> Index.t
      val index_offset_from_utc_exn : t -> Index.t -> Span.t
      val index_has_prev_clock_shift : t -> Index.t -> bool
      val index_prev_clock_shift_time_exn : t -> Index.t -> time
      val index_has_next_clock_shift : t -> Index.t -> bool
      val index_next_clock_shift_time_exn : t -> Index.t -> time
    end

    module Date_and_ofday : sig
      type absolute := t
      type t = private underlying

      val to_date : t -> Date0.t
      val to_ofday : t -> Ofday.t
      val of_absolute : absolute -> offset_from_utc:Span.t -> t
      val to_absolute : t -> offset_from_utc:Span.t -> absolute
    end
  end

  (** The shared cache for calculating date and start-of-day from a given time. Contains a
      synchronized mutable state that is exposed via [get] and [reset]. *)
  module type Synchronized_date_cache = sig @@ portable
    type time

    (** [get_date time ~zone] and [get_day_start time ~zone] calculate the date and
        start-of-day time for [time] in [zone], and return whichever one is requested.

        The most recent date/day-start result is cached so that consecutive calls for the
        same timezone and date are much faster (~2ns hit vs ~20ns miss). *)

    val get_date : time -> zone:Zone.t -> Date0.t
    val get_day_start : time -> zone:Zone.t -> time

    (** Clears the top-level cache for [get]; specifically intended for
        testing/benchmarking. *)
    val reset : unit -> unit
  end
end

module type Date_cache = sig
  include module type of struct
    include Definitions
  end

  module Make (Time : Time) () : Synchronized_date_cache with type time := Time.t
end
