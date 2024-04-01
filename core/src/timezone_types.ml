open! Base

module Regime = struct
  (** When used from javascript, daylight savings and abbreviation information
      aren't available, so on that platform, [is_dst] is always false, and
      [abbrv] is always the empty string. *)
  type t =
    { utc_offset_in_seconds : Int63.t
    ; is_dst : bool
    ; abbrv : string
    }
end

module Transition = struct
  type t =
    { start_time_in_seconds_since_epoch : Int63.t
    ; new_regime : Regime.t
    }
end
