(** Time-zone handling. *)

open! Import

(** The internal time representation of [Zone.t]. This is a tiny subset of [Time0_intf.S],
    see that interface for details such as the meaning of [Span] and [Date_and_ofday].

    The name of the interface reflects the fact that the interface only gives you access
    to the seconds of the [t]. But you can use this interface with types that have higher
    precision than that, hence the rounding implied in the name of
    [to_int63_seconds_round_down_exn]. *)
module type Time_in_seconds = sig
  module Span : sig
    type t

    val of_int63_seconds : Int63.t -> t
    val to_int63_seconds_round_down_exn : t -> Int63.t
  end

  module Date_and_ofday : sig
    type t

    val of_synthetic_span_since_epoch : Span.t -> t
    val to_synthetic_span_since_epoch : t -> Span.t
  end

  type t

  val of_span_since_epoch : Span.t -> t
  val to_span_since_epoch : t -> Span.t
end

(** When there is a backwards DST transition, some local times can occur twice. Some
    functions that operate on local times accept an optional [Earlier_or_later.t], which
    allows the user to explicitly specify whether the earlier or later of the two absolute
    times corresponding to an ambiguous local time is intended. *)
module Earlier_or_later = struct
  type t =
    | Earlier
    | Later
  [@@deriving compare ~localize, enumerate, equal ~localize, hash, sexp_of]
end

(** This is the interface of [Zone], but not the interface of [Time.Zone] or
    [Time_ns.Zone]. For those, look at [Time_intf.Zone] *)
module type S = sig
  (** {1 User-friendly interface} *)

  (** The type of a time-zone.

      bin_io and sexp representations of Zone.t are the name of the zone, and not the full
      data that is read from disk when Zone.find is called. The full Zone.t is
      reconstructed on the receiving/reading side by reloading the zone file from disk.
      Any zone name that is accepted by [find] is acceptable in the bin_io and sexp
      representations. *)
  type t : sync_data [@@deriving sexp_of, compare ~localize]

  (** [input_tz_file ~zonename ~filename] read in [filename] and return [t] with [name t]
      = [zonename] *)
  val input_tz_file : zonename:string -> filename:string -> t

  (** [likely_machine_zones] is a list of zone names that will be searched first when
      trying to determine the machine zone of a box. Setting this to a likely set of zones
      for your application will speed the very first use of the local timezone. *)
  val likely_machine_zones : string list Atomic.t

  module Time_in_seconds : Time_in_seconds
  module Earlier_or_later = Earlier_or_later

  (** [of_utc_offset offset] returns a timezone with a static UTC offset (given in hours). *)
  val of_utc_offset : hours:int -> t

  (** Like [of_utc_offset], but overriding the default name. These zones can only be
      reliably transferred over sexp or bin-io using [Stable.Full_data]; see below. *)
  val of_utc_offset_explicit_name : name:string -> hours:int -> t

  (** Returns a timezone with a static UTC offset in units of seconds. Rounds input to the
      next lower unit of seconds if necessary. These zones can only be reliably
      transferred over sexp or bin-io using [Stable.Full_data]; see below. *)
  val of_utc_offset_in_seconds_round_down : ?name:string -> Time_in_seconds.Span.t -> t

  (** Returns a timezone with a fixed offset relative to the given [t] in units of
      seconds. This time zone doesn't represent any real place, but may be convenient for
      testing or for other non-real-time purposes. Rounds [span] to the next lower unit of
      seconds if necessary. These zones can only be reliably transferred over sexp or
      bin-io using [Stable.Full_data]; see below. *)
  val add_offset_in_seconds_round_down
    :  t
    -> name:string
    -> span:Time_in_seconds.Span.t
    -> t

  (** [utc] the UTC time zone. Included for convenience *)
  val utc : t

  val%template name : t @ m -> string @ m [@@mode m = (local, global)]

  (** [original_filename t] return the filename [t] was loaded from (if any) *)
  val original_filename : t -> string option

  (** [digest t] return the MD5 digest of the file the t was created from (if any) *)
  val digest : t -> Md5.t option

  (** For performance testing only; [reset_transition_cache t] resets an internal cache in
      [t] used to speed up repeated lookups of the same clock shift transition. *)
  val reset_transition_cache : t -> unit

  (** A time zone index refers to a range of times delimited by DST transitions at one or
      both ends. Every time belongs to exactly one such range. The times of DST
      transitions themselves belong to the range for which they are the lower bound. *)
  module Index : sig
    type t : immediate

    val next : t -> t
    val prev : t -> t
  end

  (** Gets the index of a time. *)
  val index : t -> Time_in_seconds.t -> Index.t

  (** Gets the index of an date and time of day in this zone. When there are two
      occurrences, the result is determined by [prefer]. *)
  val index_of_date_and_ofday
    :  ?prefer:Earlier_or_later.t (** default: [Later] *)
    -> t
    -> Time_in_seconds.Date_and_ofday.t
    -> Index.t

  (** Gets the UTC offset of times in a specific range.

      This can raise if you use an [Index.t] that is out of bounds for this [t]. *)
  val index_offset_from_utc_exn : t -> Index.t -> Time_in_seconds.Span.t

  (** [index_abbreviation_exn t index] returns the abbreviation name (such as EDT, EST,
      JST) of given zone [t] for the range of [index]. This string conversion is one-way
      only, and cannot reliably be turned back into a [t]. This function reads and writes
      the zone's cached index. Raises if [index] is out of bounds for [t]. *)
  val index_abbreviation_exn : t -> Index.t -> string

  (** Accessors for the DST transitions delimiting the start and end of a range, if any.
      The [_exn] accessors raise if there is no such transition. These accessors are split
      up to increase performance and improve allocation; they are intended as a low-level
      back-end for commonly-used time conversion functions. See [Time.Zone] and
      [Time_ns.Zone] for higher-level accessors that return an optional tuple for clock
      shifts in either direction. *)
  val index_has_prev_clock_shift : t -> Index.t -> bool

  val index_prev_clock_shift_time_exn : t -> Index.t -> Time_in_seconds.t
  val index_prev_clock_shift_amount_exn : t -> Index.t -> Time_in_seconds.Span.t
  val index_has_next_clock_shift : t -> Index.t -> bool
  val index_next_clock_shift_time_exn : t -> Index.t -> Time_in_seconds.t
  val index_next_clock_shift_amount_exn : t -> Index.t -> Time_in_seconds.Span.t
end

module type S_stable = sig
  type t

  (** Transfers the full contents of a time zone including all DST transitions, while
      other protocols such as [Timezone.Stable.V1] serialize only the name. This protocol
      is required when talking to platforms that do not have access to a time zone
      database, or for time zones that do not come from either [Timezone.find] or
      [of_utc_offset]. *)
  module Full_data : sig
    module%template V1 :
      Stable_module_types.With_stable_witness.S0_without_comparator
      [@mode local]
      with type t = t
  end
end

module type Zone = sig @@ portable
  module type S = S
  module type S_stable = S_stable

  include S
  module Stable : S_stable with type t := t
end
