(** This module type is basically the intersection of the module types of [Core.Time]
    and [Core.Time_ns].  We verify that that relation holds in check_std.ml. *)

open! Import
open! Import_time

module type S = sig
  type t

  include Binable.S    with type t := t
  include Comparable.S with type t := t
  include Hashable.S   with type t := t
  include Sexpable.S   with type t := t
  include Stringable.S with type t := t

  module Span : sig
    type t

    include Binable.S            with type t := t
    include Comparable.S         with type t := t
    include Comparable.With_zero with type t := t
    include Hashable.S           with type t := t
    include Sexpable.S           with type t := t
    include Stringable.S         with type t := t

    val nanosecond  : t
    val microsecond : t
    val millisecond : t
    val second      : t
    val minute      : t
    val hour        : t
    val day         : t

    val of_ns  : float -> t
    val of_us  : float -> t
    val of_ms  : float -> t
    val of_sec : float -> t
    val of_min : float -> t
    val of_hr  : float -> t
    val of_day : float -> t

    val to_ns  : t -> float
    val to_us  : t -> float
    val to_ms  : t -> float
    val to_sec : t -> float
    val to_min : t -> float
    val to_hr  : t -> float
    val to_day : t -> float

    val zero   : t
    val ( + )  : t -> t -> t
    val ( - )  : t -> t -> t
    val abs    : t -> t
    val neg    : t -> t
    val scale  : t -> float -> t
    val ( / )  : t -> float -> t
    val ( // ) : t -> t -> float

    (** The only condition [to_proportional_float] is supposed to satisfy is that for all
        [t1, t2 : t]: [to_proportional_float t1 /. to_proportional_float t2 = t1 // t2]. *)
    val to_proportional_float : t -> float

    val to_short_string : t -> string
    val to_string_hum
      :  ?delimiter    :char
      -> ?decimals     :int
      -> ?align_decimal:bool
      -> ?unit_of_time :Unit_of_time.t
      -> t -> string

    val randomize : t -> percent:Percent.t -> t

    val to_unit_of_time : t -> Unit_of_time.t
    val of_unit_of_time : Unit_of_time.t -> t
  end

  module Ofday : sig
    type t

    include Binable      with type t := t
    include Comparable.S with type t := t
    include Hashable.S   with type t := t
    include Sexpable.S   with type t := t

    val start_of_day : t
  end

  val epoch : t
  val now : unit -> t
  val add : t -> Span.t -> t
  val sub : t -> Span.t -> t
  val diff : t -> t -> Span.t
  val abs_diff : t -> t -> Span.t

  val to_string_fix_proto : [ `Local | `Utc ] -> t -> string
  val of_string_fix_proto : [ `Local | `Utc ] -> string -> t

  val to_string_abs : t -> zone:Time.Zone.t -> string
  val of_string_abs : string -> t

  val next_multiple
    :  ?can_equal_after:bool
    -> base:t
    -> after:t
    -> interval:Span.t
    -> unit
    -> t

  val of_date_ofday : zone:Time.Zone.t -> Date.t -> Ofday.t -> t
  val to_ofday : t -> zone:Time.Zone.t -> Ofday.t
  val to_date : t -> zone:Time.Zone.t -> Date.t

  val occurrence
    :  [ `First_after_or_at | `Last_before_or_at ]
    -> t
    -> ofday:Ofday.t
    -> zone:Time.Zone.t
    -> t

  val pause : Span.t -> unit
  val interruptible_pause : Span.t -> [ `Ok | `Remaining of Span.t ]
  val pause_forever : unit -> never_returns
end
