open! Core.Std

(* module Timing_wheel =
   Core_kernel_unit_tests.Timing_wheel_debug.Make (Time) (Timing_wheel) *)

open Timing_wheel

include Core_kernel_test.Timing_wheel_unit_tests.Make (Timing_wheel)

let sec = Time.Span.of_sec

let%test_unit _ =
  let t = create_unit () in
  let start = start t in
  List.iter
    [ Time.sub start (sec (2. *. Float.of_int Int.max_value))
    ; Time.add start (sec (2. *. Float.of_int Int.max_value))
    ; Time.of_float Float.max_value
    ]
    ~f:(fun time ->
      assert (does_raise (fun () -> interval_num t time));
      assert (does_raise (fun () -> interval_start t time)))
;;

(* Check that default [level_bits] gives desired range of times. *)
let%test_unit _ =
  let zone = Time.Zone.find_exn "America/New_York" in
  let start =
    Time.of_date_ofday ~zone
      (Date.create_exn ~y:2000 ~m:Month.Jan ~d:1)
      Time.Ofday.start_of_day
  in
  let alarm_precision = Time.Span.microsecond in
  let max_alarm_lower_bound = Date.create_exn ~y:2073 ~m:Month.Jan ~d:1 in
  let level_bits = Level_bits.default in
  let t = create ~config:(Config.create ~level_bits ~alarm_precision ()) ~start in
  assert (Date.(>=)
            (Time.to_date ~zone (alarm_upper_bound t))
            max_alarm_lower_bound)
;;
