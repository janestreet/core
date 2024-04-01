open! Core
module Timezone_js_loader = Core.Core_private.Timezone_js_loader
module Timezone_types = Core.Core_private.Timezone_types

module Regime = struct
  type t = Timezone_types.Regime.t =
    { utc_offset_in_seconds : Int63.t
    ; is_dst : bool
    ; abbrv : string
    }
  [@@deriving sexp_of]
end

module Transition = struct
  type t = Timezone_types.Transition.t =
    { start_time_in_seconds_since_epoch : Int63.t
    ; new_regime : Regime.t
    }
  [@@deriving sexp_of]
end

let print_result_kind zone =
  match Timezone_js_loader.load zone with
  | Ok _ -> print_endline "platform supported"
  | Error Platform_not_supported -> print_endline "platform *not* supported"
  | Error Disabled -> print_endline "platform supported, but disabled"
  | Error (Failed exn) -> Exn.raise_without_backtrace exn
;;

let%expect_test ("attempt to use from native" [@tags "no-js"]) =
  print_result_kind "America/New_York";
  [%expect {| platform *not* supported |}]
;;

let%expect_test ("attempt to use from javascript" [@tags "js-only"]) =
  print_result_kind "America/New_York";
  [%expect {| platform supported |}]
;;

let%expect_test ("disable javascript support" [@tags "js-only"]) =
  Exn.protect
    ~f:(fun () ->
      Timezone_js_loader.For_testing.disable ();
      print_result_kind "America/New_York";
      [%expect {| platform supported, but disabled |}])
    ~finally:Timezone_js_loader.For_testing.enable
;;

let load_exn zone =
  match Timezone_js_loader.load zone with
  | Ok result -> result
  | Error Platform_not_supported -> failwith "platform not supported"
  | Error Disabled -> failwith "platform supported but was disabled"
  | Error (Failed exn) -> Exn.raise_without_backtrace exn
;;

let%expect_test ("load named zone" [@tags "js-only"]) =
  (* Zone content changes over time, so we can't print the output of
     these functions deterministically *)
  let ({ first_transition; remaining_transitions } : Timezone_js_loader.t) =
    load_exn "America/New_York"
  in
  (* This first transition should be deterministic *)
  print_s [%message (first_transition : Transition.t)];
  [%expect
    {|
    (first_transition
     ((start_time_in_seconds_since_epoch -8_640_000_000_000)
      (new_regime ((utc_offset_in_seconds -17_762) (is_dst false) (abbrv "")))))
    |}];
  (* There should be a lot of transitions, and this number should grow over
     time, so this should be deterministic *)
  assert (List.length remaining_transitions > 100)
;;

let%expect_test ("load utc" [@tags "js-only"]) =
  let ({ first_transition; remaining_transitions } : Timezone_js_loader.t) =
    load_exn "UTC"
  in
  print_s
    [%message
      "" (first_transition : Transition.t) (remaining_transitions : Transition.t list)];
  [%expect
    {|
    ((first_transition
      ((start_time_in_seconds_since_epoch -8_640_000_000_000)
       (new_regime ((utc_offset_in_seconds 0) (is_dst false) (abbrv "")))))
     (remaining_transitions ()))
    |}]
;;

let%expect_test ("load utc offset" [@tags "js-only"]) =
  let ({ first_transition; remaining_transitions } : Timezone_js_loader.t) =
    load_exn "+05:00"
  in
  print_s
    [%message
      "" (first_transition : Transition.t) (remaining_transitions : Transition.t list)];
  [%expect
    {|
    ((first_transition
      ((start_time_in_seconds_since_epoch -8_640_000_000_000)
       (new_regime ((utc_offset_in_seconds 18_000) (is_dst false) (abbrv "")))))
     (remaining_transitions ()))
    |}]
;;
