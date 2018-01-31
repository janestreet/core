open! Core
open! Expect_test_helpers_kernel

open Timing_wheel_float

let sec = Time.Span.of_sec

let create_unit ?level_bits ?(start = Time.epoch) ?(alarm_precision = sec 1.) () =
  create
    ~config:(
      Config.create ?level_bits ()
        ~alarm_precision:(alarm_precision |> Alarm_precision.of_span_floor_pow2_ns))
    ~start
;;

let%test_unit _ =
  let t = create_unit () in
  let start = start t in
  List.iter
    [ Time.sub start (sec (2. *. Float.of_int64 (Int63.(to_int64 max_value))))
    ; Time.add start (sec (2. *. Float.of_int64 (Int63.(to_int64 max_value))))
    ; Time.of_span_since_epoch (Time.Span.of_sec Float.max_value)
    ]
    ~f:(fun time ->
      assert (does_raise (fun () -> interval_num t time));
      assert (does_raise (fun () -> interval_start t time)))
;;

(* Check that default [level_bits] gives desired range of times. *)
let%expect_test _ =
  let zone = Time.Zone.find_exn "America/New_York" in
  let start =
    Time.of_date_ofday ~zone
      (Date.create_exn ~y:2000 ~m:Month.Jan ~d:1)
      Time.Ofday.start_of_day
  in
  let alarm_precision = Alarm_precision.about_one_microsecond in
  let max_alarm_lower_bound =
    match Word_size.word_size with
    | W32 -> Date.create_exn ~y:2035 ~m:Jan ~d:1
    | W64 -> Date.create_exn ~y:2073 ~m:Jan ~d:1
  in
  let level_bits = Level_bits.default in
  let t = create ~config:(Config.create ~level_bits ~alarm_precision ()) ~start in
  let alarm_upper_bound = Time.to_date ~zone (alarm_upper_bound t) in
  require [%here] (Date.( >= ) alarm_upper_bound max_alarm_lower_bound)
    ~if_false_then_print_s:(
      lazy [%message
        ""
          (alarm_upper_bound : Date.t)
          (max_alarm_lower_bound : Date.t)]);
;;
