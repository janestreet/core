open! Base
open Timezone_types

external should_use_timezone_js_loader
  :  [ `Yes ]
  -> [ `Platform_not_supported ]
  -> [ `Disabled ]
  -> [ `Yes | `Platform_not_supported | `Disabled ]
  = "should_use_timezone_js_loader"

module Instant = struct
  type t

  external from_epoch_seconds : int64 -> t = "timezone_js_loader_from_epoch_seconds"
  external epoch_seconds : t -> int64 = "timezone_js_loader_epoch_seconds"
  external now : unit -> t = "timezone_js_loader_now"
  external plus_hours : t -> int64 -> t = "timezone_js_loader_instant_plus_hours"
  external compare : t -> t -> int = "timezone_js_loader_compare_instants"
end

module Zone = struct
  type t

  external create : string -> t = "timezone_js_loader_create_zone"

  external get_offset_nanos_for
    :  t
    -> Instant.t
    -> int64
    = "timezone_js_loader_get_offset_nanos_for"

  external next_transition_or_this_time_if_none
    :  t
    -> Instant.t
    -> Instant.t
    = "timezone_js_loader_get_next_transition_or_this_time_if_none"

  let next_transition t instant =
    let transition = next_transition_or_this_time_if_none t instant in
    if phys_equal instant transition then None else Some transition
  ;;
end

(* Mom: "we have Nonempty_list.t at home."
   The Nonempty_list.t at home: *)
type t =
  { first_transition : Timezone_types.Transition.t
  ; remaining_transitions : Timezone_types.Transition.t list
  }
(* Nonempty_list depends on Core, which this file is a part of... *)

let utc_offset_s_at_instant tz instant =
  let offset_ns = Zone.get_offset_nanos_for tz instant in
  let ns_per_s = 1_000_000_000L in
  let offset_ns = Int64.round_up ~to_multiple_of:ns_per_s offset_ns in
  Int63.of_int64_exn Int64.(offset_ns / ns_per_s)
;;

let make_transition ~start_time_in_seconds_since_epoch ~utc_offset_in_seconds =
  (* The javascript API does not have access to abbreviations.
     Abbreviations do not appear to be used in applications.

     We also don't know if the transition is caused by daylight saving time, but this
     information is never even exposed by Core.zone. *)
  let new_regime = { Regime.abbrv = ""; is_dst = false; utc_offset_in_seconds } in
  { Transition.start_time_in_seconds_since_epoch; new_regime }
;;

let load_exn s =
  (* From https://tc39.es/proposal-temporal/docs/instant.html:
     > The range of allowed values for this type is the same as the old-style JavaScript
     > Date, 100 million (10^8) days before or after the Unix epoch. This range covers
     > approximately half a million years. If epochNanoseconds is outside of this range, a
     > RangeError will be thrown.
     (100_000_000) * (60 * 60 * 24) = 8_640_000_000_000L *)
  let a_long_long_time_ago_s = -8_640_000_000_000L in
  let a_long_long_time_ago_instant = Instant.from_epoch_seconds a_long_long_time_ago_s in
  let about_15_years_from_now =
    (* the timezone database on linux only extends forward about 15 years, so copy that
       logic here. Without this logic, the browser might decide to return an infinite
       number of transitions. *)
    let now = Instant.now () in
    Instant.plus_hours now 131_490L
  in
  let tz = Zone.create s in
  let rec build_transitions acc ~starting_at =
    if Instant.compare starting_at about_15_years_from_now > 0
    then List.rev acc
    else (
      match Zone.next_transition tz starting_at with
      | None -> List.rev acc
      | Some transition_point ->
        let transition =
          make_transition
            ~start_time_in_seconds_since_epoch:
              (Int63.of_int64_exn (Instant.epoch_seconds transition_point))
            ~utc_offset_in_seconds:(utc_offset_s_at_instant tz transition_point)
        in
        build_transitions (transition :: acc) ~starting_at:transition_point)
  in
  let first_transition =
    make_transition
      ~start_time_in_seconds_since_epoch:(Int63.of_int64_exn a_long_long_time_ago_s)
      ~utc_offset_in_seconds:(utc_offset_s_at_instant tz a_long_long_time_ago_instant)
  in
  let remaining_transitions =
    build_transitions [] ~starting_at:a_long_long_time_ago_instant
  in
  { first_transition; remaining_transitions }
;;

module Load_error = struct
  type t =
    | Disabled
    | Platform_not_supported
    | Failed of exn
  [@@deriving sexp_of]
end

let load s =
  match should_use_timezone_js_loader `Yes `Platform_not_supported `Disabled with
  | `Disabled -> Error Load_error.Disabled
  | `Platform_not_supported -> Error Load_error.Platform_not_supported
  | `Yes ->
    (match load_exn s with
     | t -> Ok t
     | exception exn -> Error (Load_error.Failed exn))
;;

module For_testing = struct
  external disable : unit -> unit = "timezone_js_loader_disable_for_testing"
  external enable : unit -> unit = "timezone_js_loader_enable_for_testing"
end
