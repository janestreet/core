open! Import
open! Import_time

module type Option = sig
  include Immediate_option.S_int63
  include Identifiable with type t := t
end

module type Span = sig
  include module type of struct include Time_ns.Span end [@ocaml.remove_aliases]
  with module Private := Time_ns.Span.Private

  include Comparable.With_zero
    with type t := t

  val arg_type : t Core_kernel.Command.Arg_type.t

  include Robustly_comparable with type t := t

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving hash]
      include Stable_int63able with type t := t
    end
    module V2 : sig
      type nonrec t = t [@@deriving hash]
      type nonrec comparator_witness = comparator_witness

      include
        Stable_int63able
        with type t := t
        with type comparator_witness := comparator_witness
      include
        Comparable.Stable.V1.S
        with type comparable := t
        with type comparator_witness := comparator_witness
    end
  end

  (** [Span.Option.t] is like [Span.t option], except that the value is immediate on
      architectures where [Int63.t] is immediate.  This module should mainly be used to
      avoid allocations. *)
  module Option : sig
    include Option with type value := t
    include Quickcheck.S with type t := t
    module Stable : sig
      module V1 : Stable_int63able with type t = t
      module V2 : Stable_int63able with type t = t
    end
  end
end

module type Ofday = sig
  include module type of struct include Time_ns.Ofday end

  val arg_type : t Core_kernel.Command.Arg_type.t

  val now : zone:Time.Zone.t -> t

  val to_ofday : t -> Time.Ofday.t
  [@@deprecated
    "[since 2019-01] use [to_ofday_float_round_nearest] or \
     [to_ofday_float_round_nearest_microsecond]"]

  val of_ofday : Time.Ofday.t -> t
  [@@deprecated
    "[since 2019-01] use [of_ofday_float_round_nearest] or \
     [of_ofday_float_round_nearest_microsecond]"]

  val to_ofday_float_round_nearest : t -> Time.Ofday.t
  val to_ofday_float_round_nearest_microsecond : t -> Time.Ofday.t
  val of_ofday_float_round_nearest : Time.Ofday.t -> t
  val of_ofday_float_round_nearest_microsecond : Time.Ofday.t -> t

  module Zoned : sig
    (** Sexps look like "(12:01 nyc)"

        Two [t]'s may or may not correspond to the same times depending on which date
        they're evaluated. *)
    type t [@@deriving bin_io, sexp, hash]

    include Pretty_printer.S with type t := t
    include Stringable       with type t := t (** Strings look like "12:01 nyc" *)

    val arg_type : t Core_kernel.Command.Arg_type.t

    val create       : Time_ns.Ofday.t -> Time.Zone.t -> t
    val create_local : Time_ns.Ofday.t                -> t

    val ofday : t -> Time_ns.Ofday.t
    val zone  : t -> Time.Zone.t

    val to_time_ns : t -> Date.t -> Time_ns.t

    module With_nonchronological_compare : sig
      (** It is possible to consistently compare [t]'s, but due to the complexities of
          time zones and daylight savings, the resulting ordering is not chronological.
          That is, [compare t1 t2 > 0] does not imply [t2] occurs after [t1] every day,
          or any day. *)
      type nonrec t = t [@@deriving bin_io, sexp, compare, equal, hash]
    end
  end

  module Option : sig
    include Option with type value := t
    include Quickcheck.S with type t := t
    module Stable : sig
      module V1 : Stable_int63able with type t = t
    end

    (** Returns [some] if the given span is a valid time since start of day, and [none]
        otherwise. *)
    val of_span_since_start_of_day : Time_ns.Span.t -> t
  end
end

(** An absolute point in time, more efficient and precise than the [float]-based {!Time},
    but representing a narrower range of times.

    This module represents absolute times with nanosecond precision, approximately between
    the years 1823 and 2116 CE.

    You should normally default to using [Time] instead of this module!  The reasons are:

    - Many functions around our libraries expect [Time.t] values, so it will likely be
      much more convenient for you.

    - It leads to greater consistency across different codebases.  It would be bad to end
      up with half our libraries expecting [Time.t] and the other half expecting
      [Time_ns.t].

    - [Time_ns] silently ignores overflow.

    Some reasons you might want want to actually prefer [Time_ns.t] in certain cases:

    - It has superior performance.

    - It uses [int]s rather than [float]s internally, which makes certain things easier to
      reason about, since [int]s respect a bunch of arithmetic identities that [float]s
      don't, e.g., [x + (y + z) = (x + y) + z].

    Neither [Core.Time_ns] nor [Core.Time] are available in Javascript.

    All in all, it would have been nice to have chosen [Time_ns.t] to begin with, but
    we're unlikely to flip everything to [Time_ns.t] in the short term (see comment at the
    end of [time_ns.ml]).

    See {!Core_kernel.Time_ns} for additional low level documentation. *)
module type Time_ns = sig
  include module type of struct include Time_ns end [@ocaml.remove_aliases]
  with module Span := Time_ns.Span
  with module Ofday := Time_ns.Ofday
  with module Stable := Time_ns.Stable

  module Span : Span

  val arg_type : t Core_kernel.Command.Arg_type.t

  (** [Option.t] is like [t option], except that the value is immediate.  This module
      should mainly be used to avoid allocations. *)
  module Option : sig
    include Option with type value := t
    include Quickcheck.S with type t := t
    module Stable : sig
      module V1 : Stable_int63able with type t = t
    end
  end

  (** See {!Time.Ofday}. *)
  module Ofday : Ofday


  (** String conversions use the local timezone by default. Sexp conversions use
      [get_sexp_zone ()] by default, which can be overridden by calling [set_sexp_zone].
      These default time zones are used when writing a time, and when reading a time with
      no explicit zone or UTC offset.

      Sexps and strings display the date, ofday, and UTC offset of [t] relative to the
      appropriate time zone. *)
  include Identifiable with type t := t

  module Zone : module type of Time.Zone with type t = Time.Zone.t

  (** These functions are identical to those in [Time] and get/set the same variable. *)

  val get_sexp_zone : unit -> Zone.t
  val set_sexp_zone : Zone.t -> unit

  (** [t_of_sexp_abs sexp] as [t_of_sexp], but demands that [sexp] indicate the timezone
      the time is expressed in. *)
  val t_of_sexp_abs : Sexp.t -> t
  val sexp_of_t_abs : t -> zone:Zone.t -> Sexp.t

  (** Conversion functions that involved Ofday.Zoned.t, exactly analogous to the
      conversion functions that involve Ofday.t *)
  val of_date_ofday_zoned : Date.t -> Ofday.Zoned.t -> t
  val to_date_ofday_zoned : t -> zone:Time.Zone.t -> Date.t * Ofday.Zoned.t
  val to_ofday_zoned : t -> zone:Time.Zone.t -> Ofday.Zoned.t

  val to_string_fix_proto : [ `Utc | `Local ] -> t -> string
  val of_string_fix_proto : [ `Utc | `Local ] -> string -> t

  (** This is like [of_string] except that if the string doesn't specify the zone then it
      raises rather than assume the local timezone. *)
  val of_string_abs : string -> t

  (** [of_string_gen ~if_no_timezone s] attempts to parse [s] to a [t].  If [s] doesn't
      supply a time zone [if_no_timezone] is consulted. *)
  val of_string_gen
    :  if_no_timezone:[ `Fail | `Local | `Use_this_one of Zone.t ]
    -> string
    -> t

  (** [pause span] sleeps for [span] time. *)
  val pause : Span.t -> unit

  (** [interruptible_pause span] sleeps for [span] time unless interrupted (e.g. by
      delivery of a signal), in which case the remaining unslept portion of time is
      returned. *)
  val interruptible_pause : Span.t -> [ `Ok | `Remaining of Span.t ]

  (** [pause_forever] sleeps indefinitely. *)
  val pause_forever : unit -> never_returns

  module Stable : sig
    module V1 : sig
      include Stable_int63able
        with type t = t
         and type comparator_witness = comparator_witness

      include
        Comparable.Stable.V1.S
        with type comparable := t
        with type comparator_witness := comparator_witness
    end

    (** Provides a sexp representation that is independent of the time zone of the machine
        writing it. *)
    module Alternate_sexp : sig
      module V1 : Stable_without_comparator with type t = t
    end
    module Option : sig
      module V1 : Stable_int63able with type t = Option.t
    end
    module Span : sig
      module V1 : sig
        type t = Span.t [@@deriving hash]
        include Stable_int63able with type t := t
      end
      module V2 : sig
        type t = Span.t [@@deriving hash]
        type nonrec comparator_witness = Span.comparator_witness
        include Stable_int63able with type t := t
          with type comparator_witness := comparator_witness
        include
          Comparable.Stable.V1.S
          with type comparable := t
          with type comparator_witness := comparator_witness
      end
      module Option : sig
        module V1 : Stable_int63able with type t = Span.Option.t
        module V2 : Stable_int63able with type t = Span.Option.t
      end
    end
    module Ofday : sig
      module V1 : Stable_int63able with type t = Ofday.t
        with type comparator_witness = Time_ns.Stable.Ofday.V1.comparator_witness
      module Zoned : sig
        module V1 : sig
          type nonrec t = Ofday.Zoned.t [@@deriving hash]
          include Stable_without_comparator with type t := t
        end
      end
      module Option : sig
        module V1 : Stable_int63able with type t = Ofday.Option.t
      end
    end
  end
end
