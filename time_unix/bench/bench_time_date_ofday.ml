(* This module adds benchmarks for various patterns of converting between times and
   date/ofday pairs that may trigger different kinds of caching. In order to maintain
   uniformity, we build lists of input values that set up the expected cache transitions
   when iterated forward and backward, then create benchmarks that run back and forth
   through the lists.

   This benchmarking scheme does have some overhead beyond just running the conversion
   functions, but we expect it is very little compared to the conversion itself, and the
   purpose is not so much to figure out exactly what the transition costs as it is to see
   how much the cost of the transition changes between one revision and another. *)
open! Core
module Time = Time_unix
module Time_ns = Time_ns_unix

let rec all_but_last_exn = function
  | [] -> assert false
  | [ _ ] -> []
  | hd :: tl -> hd :: all_but_last_exn tl
;;

let back_and_forth_list = function
  | [] -> assert false
  | [ elt ] -> [ elt ]
  | list ->
    let fwd = list in
    let rev = List.rev fwd in
    let prefix = all_but_last_exn fwd in
    let suffix = all_but_last_exn rev in
    prefix @ suffix
;;

let time_example (zone_string, date_string, ofday_string) =
  let zone = Time.Zone.find_exn zone_string in
  let date = Date.of_string date_string in
  let ofday = Time.Ofday.of_string ofday_string in
  let time = Time.of_date_ofday ~zone date ofday in
  zone, date, ofday, time
;;

let time_ns_example (zone_string, date_string, ofday_string) =
  let zone = Time_ns.Zone.find_exn zone_string in
  let date = Date.of_string date_string in
  let ofday = Time_ns.Ofday.of_string ofday_string in
  let time = Time_ns.of_date_ofday ~zone date ofday in
  zone, date, ofday, time
;;

let bench list ~example ~f =
  let array = list |> List.map ~f:example |> back_and_forth_list |> Array.of_list in
  let len = Array.length array in
  let index = ref 0 in
  fun () ->
    incr index;
    if !index = len then index := 0;
    f array.(!index)
;;

let bench_time list ~f = bench list ~example:time_example ~f
let bench_time_ns list ~f = bench list ~example:time_ns_example ~f

let bench_time_of_date_ofday list =
  bench_time list ~f:(fun (zone, date, ofday, _) -> Time.of_date_ofday ~zone date ofday)
;;

let bench_time_of_date_ofday_precise list =
  bench_time list ~f:(fun (zone, date, ofday, _) ->
    Time.of_date_ofday_precise ~zone date ofday)
;;

let bench_time_ns_of_date_ofday list =
  bench_time_ns list ~f:(fun (zone, date, ofday, _) ->
    Time_ns.of_date_ofday ~zone date ofday)
;;

let bench_time_to_date list =
  bench_time list ~f:(fun (zone, _, _, time) -> Time.to_date ~zone time)
;;

let bench_time_ns_to_date list =
  bench_time_ns list ~f:(fun (zone, _, _, time) -> Time_ns.to_date ~zone time)
;;

let bench_time_to_ofday list =
  bench_time list ~f:(fun (zone, _, _, time) -> Time.to_ofday ~zone time)
;;

let bench_time_ns_to_ofday list =
  bench_time_ns list ~f:(fun (zone, _, _, time) -> Time_ns.to_ofday ~zone time)
;;

let bench_time_to_date_ofday list =
  bench_time list ~f:(fun (zone, _, _, time) -> Time.to_date_ofday ~zone time)
;;

let bench_time_to_date_ofday_precise list =
  bench_time list ~f:(fun (zone, _, _, time) -> Time.to_date_ofday_precise ~zone time)
;;

let bench_time_ns_to_date_ofday list =
  bench_time_ns list ~f:(fun (zone, _, _, time) -> Time_ns.to_date_ofday ~zone time)
;;

(* a set of time zone transitions to keep in mind when reading below *)
let%expect_test "sample transitions" =
  let zone = Time.Zone.find_exn "America/New_York" in
  let time =
    Time.of_date_ofday ~zone (Date.of_string "2013-10-07") (Time.Ofday.of_string "09:30")
  in
  let prev_clock_shift = Time.Zone.prev_clock_shift zone ~at_or_before:time in
  print_s [%sexp (prev_clock_shift : (Time.t * Time.Span.t) option)];
  [%expect {| (((2013-03-10 03:00:00.000000-04:00) 1h)) |}];
  let next_clock_shift = Time.Zone.next_clock_shift zone ~strictly_after:time in
  print_s [%sexp (next_clock_shift : (Time.t * Time.Span.t) option)];
  [%expect {| (((2013-11-03 01:00:00.000000-05:00) -1h)) |}]
;;


let same_day =
  [ "America/New_York", "2013-10-07", "01:00"
  ; "America/New_York", "2013-10-07", "02:00"
  ; "America/New_York", "2013-10-07", "03:00"
  ; "America/New_York", "2013-10-07", "04:00"
  ; "America/New_York", "2013-10-07", "05:00"
  ; "America/New_York", "2013-10-07", "06:00"
  ; "America/New_York", "2013-10-07", "07:00"
  ; "America/New_York", "2013-10-07", "08:00"
  ; "America/New_York", "2013-10-07", "09:00"
  ]
;;

let%bench_fun "Time.   of_date_ofday,         same day" =
  bench_time_of_date_ofday same_day
;;

let%bench_fun "Time.   of_date_ofday_precise, same day" =
  bench_time_of_date_ofday_precise same_day
;;

let%bench_fun "Time_ns.of_date_ofday,         same day" =
  bench_time_ns_of_date_ofday same_day
;;

let%bench_fun "Time.   to_date,               same day" = bench_time_to_date same_day
let%bench_fun "Time_ns.to_date,               same day" = bench_time_ns_to_date same_day
let%bench_fun "Time.   to_ofday,              same day" = bench_time_to_ofday same_day
let%bench_fun "Time_ns.to_ofday,              same day" = bench_time_ns_to_ofday same_day

let%bench_fun "Time.   to_date_ofday,         same day" =
  bench_time_to_date_ofday same_day
;;

let%bench_fun "Time.   to_date_ofday_precise, same day" =
  bench_time_to_date_ofday_precise same_day
;;

let%bench_fun "Time_ns.to_date_ofday,         same day" =
  bench_time_ns_to_date_ofday same_day
;;

let consecutive_days =
  [ "America/New_York", "2013-10-01", "09:30"
  ; "America/New_York", "2013-10-02", "09:30"
  ; "America/New_York", "2013-10-03", "09:30"
  ; "America/New_York", "2013-10-04", "09:30"
  ; "America/New_York", "2013-10-05", "09:30"
  ; "America/New_York", "2013-10-06", "09:30"
  ; "America/New_York", "2013-10-07", "09:30"
  ; "America/New_York", "2013-10-08", "09:30"
  ; "America/New_York", "2013-10-09", "09:30"
  ]
;;

let%bench_fun "Time.   of_date_ofday,         consecutive days" =
  bench_time_of_date_ofday consecutive_days
;;

let%bench_fun "Time.   of_date_ofday_precise, consecutive days" =
  bench_time_of_date_ofday_precise consecutive_days
;;

let%bench_fun "Time_ns.of_date_ofday,         consecutive days" =
  bench_time_ns_of_date_ofday consecutive_days
;;

let%bench_fun "Time.   to_date,               consecutive days" =
  bench_time_to_date consecutive_days
;;

let%bench_fun "Time_ns.to_date,               consecutive days" =
  bench_time_ns_to_date consecutive_days
;;

let%bench_fun "Time.   to_ofday,              consecutive days" =
  bench_time_to_ofday consecutive_days
;;

let%bench_fun "Time_ns.to_ofday,              consecutive days" =
  bench_time_ns_to_ofday consecutive_days
;;

let%bench_fun "Time.   to_date_ofday,         consecutive days" =
  bench_time_to_date_ofday consecutive_days
;;

let%bench_fun "Time.   to_date_ofday_precise, consecutive days" =
  bench_time_to_date_ofday_precise consecutive_days
;;

let%bench_fun "Time_ns.to_date_ofday,         consecutive days" =
  bench_time_ns_to_date_ofday consecutive_days
;;

let non_consecutive_days =
  [ "America/New_York", "2013-04-01", "09:30"
  ; "America/New_York", "2013-05-01", "09:30"
  ; "America/New_York", "2013-06-01", "09:30"
  ; "America/New_York", "2013-07-01", "09:30"
  ; "America/New_York", "2013-08-01", "09:30"
  ; "America/New_York", "2013-09-01", "09:30"
  ; "America/New_York", "2013-10-01", "09:30"
  ]
;;

let%bench_fun "Time.   of_date_ofday,         non-consecutive days" =
  bench_time_of_date_ofday non_consecutive_days
;;

let%bench_fun "Time.   of_date_ofday_precise, non-consecutive days" =
  bench_time_of_date_ofday_precise non_consecutive_days
;;

let%bench_fun "Time_ns.of_date_ofday,         non-consecutive days" =
  bench_time_ns_of_date_ofday non_consecutive_days
;;

let%bench_fun "Time.   to_date,               non-consecutive days" =
  bench_time_to_date non_consecutive_days
;;

let%bench_fun "Time_ns.to_date,               non-consecutive days" =
  bench_time_ns_to_date non_consecutive_days
;;

let%bench_fun "Time.   to_ofday,              non-consecutive days" =
  bench_time_to_ofday non_consecutive_days
;;

let%bench_fun "Time_ns.to_ofday,              non-consecutive days" =
  bench_time_ns_to_ofday non_consecutive_days
;;

let%bench_fun "Time.   to_date_ofday,         non-consecutive days" =
  bench_time_to_date_ofday non_consecutive_days
;;

let%bench_fun "Time.   to_date_ofday_precise, non-consecutive days" =
  bench_time_to_date_ofday_precise non_consecutive_days
;;

let%bench_fun "Time_ns.to_date_ofday,         non-consecutive days" =
  bench_time_ns_to_date_ofday non_consecutive_days
;;

let same_day_across_transition =
  [ "America/New_York", "2013-03-10", "00:30"; "America/New_York", "2013-03-10", "23:30" ]
;;

let%bench_fun "Time.   of_date_ofday,         same day, across transition" =
  bench_time_of_date_ofday same_day_across_transition
;;

let%bench_fun "Time.   of_date_ofday_precise, same day, across transition" =
  bench_time_of_date_ofday_precise same_day_across_transition
;;

let%bench_fun "Time_ns.of_date_ofday,         same day, across transition" =
  bench_time_ns_of_date_ofday same_day_across_transition
;;

let%bench_fun "Time.   to_date,               same day, across transition" =
  bench_time_to_date same_day_across_transition
;;

let%bench_fun "Time_ns.to_date,               same day, across transition" =
  bench_time_ns_to_date same_day_across_transition
;;

let%bench_fun "Time.   to_ofday,              same day, across transition" =
  bench_time_to_ofday same_day_across_transition
;;

let%bench_fun "Time_ns.to_ofday,              same day, across transition" =
  bench_time_ns_to_ofday same_day_across_transition
;;

let%bench_fun "Time.   to_date_ofday,         same day, across transition" =
  bench_time_to_date_ofday same_day_across_transition
;;

let%bench_fun "Time.   to_date_ofday_precise, same day, across transition" =
  bench_time_to_date_ofday_precise same_day_across_transition
;;

let%bench_fun "Time_ns.to_date_ofday,         same day, across transition" =
  bench_time_ns_to_date_ofday same_day_across_transition
;;

let consecutive_days_across_transition =
  [ "America/New_York", "2013-03-09", "09:30"; "America/New_York", "2013-03-10", "09:30" ]
;;

let%bench_fun "Time.   of_date_ofday,         consecutive days, across transition" =
  bench_time_of_date_ofday consecutive_days_across_transition
;;

let%bench_fun "Time.   of_date_ofday_precise, consecutive days, across transition" =
  bench_time_of_date_ofday_precise consecutive_days_across_transition
;;

let%bench_fun "Time_ns.of_date_ofday,         consecutive days, across transition" =
  bench_time_ns_of_date_ofday consecutive_days_across_transition
;;

let%bench_fun "Time.   to_date,               consecutive days, across transition" =
  bench_time_to_date consecutive_days_across_transition
;;

let%bench_fun "Time_ns.to_date,               consecutive days, across transition" =
  bench_time_ns_to_date consecutive_days_across_transition
;;

let%bench_fun "Time.   to_ofday,              consecutive days, across transition" =
  bench_time_to_ofday consecutive_days_across_transition
;;

let%bench_fun "Time_ns.to_ofday,              consecutive days, across transition" =
  bench_time_ns_to_ofday consecutive_days_across_transition
;;

let%bench_fun "Time.   to_date_ofday,         consecutive days, across transition" =
  bench_time_to_date_ofday consecutive_days_across_transition
;;

let%bench_fun "Time.   to_date_ofday_precise, consecutive days, across transition" =
  bench_time_to_date_ofday_precise consecutive_days_across_transition
;;

let%bench_fun "Time_ns.to_date_ofday,         consecutive days, across transition" =
  bench_time_ns_to_date_ofday consecutive_days_across_transition
;;

let consecutive_transitions =
  [ "America/New_York", "2011-10-07", "09:30"
  ; "America/New_York", "2011-12-11", "09:30"
  ; "America/New_York", "2012-10-07", "09:30"
  ; "America/New_York", "2012-12-11", "09:30"
  ; "America/New_York", "2013-10-07", "09:30"
  ; "America/New_York", "2013-12-11", "09:30"
  ; "America/New_York", "2014-10-07", "09:30"
  ; "America/New_York", "2014-12-11", "09:30"
  ; "America/New_York", "2015-10-07", "09:30"
  ; "America/New_York", "2015-12-11", "09:30"
  ; "America/New_York", "2016-10-07", "09:30"
  ; "America/New_York", "2016-12-11", "09:30"
  ; "America/New_York", "2017-10-07", "09:30"
  ; "America/New_York", "2017-12-11", "09:30"
  ; "America/New_York", "2018-10-07", "09:30"
  ; "America/New_York", "2018-12-11", "09:30"
  ; "America/New_York", "2019-10-07", "09:30"
  ; "America/New_York", "2019-12-11", "09:30"
  ]
;;

let%bench_fun "Time.   of_date_ofday,         consecutive transitions" =
  bench_time_of_date_ofday consecutive_transitions
;;

let%bench_fun "Time.   of_date_ofday_precise, consecutive transitions" =
  bench_time_of_date_ofday_precise consecutive_transitions
;;

let%bench_fun "Time_ns.of_date_ofday,         consecutive transitions" =
  bench_time_ns_of_date_ofday consecutive_transitions
;;

let%bench_fun "Time.   to_date,               consecutive transitions" =
  bench_time_to_date consecutive_transitions
;;

let%bench_fun "Time_ns.to_date,               consecutive transitions" =
  bench_time_ns_to_date consecutive_transitions
;;

let%bench_fun "Time.   to_ofday,              consecutive transitions" =
  bench_time_to_ofday consecutive_transitions
;;

let%bench_fun "Time_ns.to_ofday,              consecutive transitions" =
  bench_time_ns_to_ofday consecutive_transitions
;;

let%bench_fun "Time.   to_date_ofday,         consecutive transitions" =
  bench_time_to_date_ofday consecutive_transitions
;;

let%bench_fun "Time.   to_date_ofday_precise, consecutive transitions" =
  bench_time_to_date_ofday_precise consecutive_transitions
;;

let%bench_fun "Time_ns.to_date_ofday,         consecutive transitions" =
  bench_time_ns_to_date_ofday consecutive_transitions
;;

let non_consecutive_transitions =
  [ "America/New_York", "2011-10-07", "09:30"
  ; "America/New_York", "2012-10-07", "09:30"
  ; "America/New_York", "2013-10-07", "09:30"
  ; "America/New_York", "2014-10-07", "09:30"
  ; "America/New_York", "2015-10-07", "09:30"
  ; "America/New_York", "2016-10-07", "09:30"
  ; "America/New_York", "2017-10-07", "09:30"
  ; "America/New_York", "2018-10-07", "09:30"
  ; "America/New_York", "2019-10-07", "09:30"
  ]
;;

let%bench_fun "Time.   of_date_ofday,         non-consecutive transitions" =
  bench_time_of_date_ofday non_consecutive_transitions
;;

let%bench_fun "Time.   of_date_ofday_precise, non-consecutive transitions" =
  bench_time_of_date_ofday_precise non_consecutive_transitions
;;

let%bench_fun "Time_ns.of_date_ofday,         non-consecutive transitions" =
  bench_time_ns_of_date_ofday non_consecutive_transitions
;;

let%bench_fun "Time.   to_date,               non-consecutive transitions" =
  bench_time_to_date non_consecutive_transitions
;;

let%bench_fun "Time_ns.to_date,               non-consecutive transitions" =
  bench_time_ns_to_date non_consecutive_transitions
;;

let%bench_fun "Time.   to_ofday,              non-consecutive transitions" =
  bench_time_to_ofday non_consecutive_transitions
;;

let%bench_fun "Time_ns.to_ofday,              non-consecutive transitions" =
  bench_time_ns_to_ofday non_consecutive_transitions
;;

let%bench_fun "Time.   to_date_ofday,         non-consecutive transitions" =
  bench_time_to_date_ofday non_consecutive_transitions
;;

let%bench_fun "Time.   to_date_ofday_precise, non-consecutive transitions" =
  bench_time_to_date_ofday_precise non_consecutive_transitions
;;

let%bench_fun "Time_ns.to_date_ofday,         non-consecutive transitions" =
  bench_time_ns_to_date_ofday non_consecutive_transitions
;;

let different_zones =
  [ "Europe/London", "2011-10-07", "09:30"
  ; "America/New_York", "2012-10-07", "09:30"
  ; "Europe/London", "2013-10-07", "09:30"
  ; "America/New_York", "2014-10-07", "09:30"
  ; "Europe/London", "2015-10-07", "09:30"
  ; "America/New_York", "2016-10-07", "09:30"
  ; "Europe/London", "2017-10-07", "09:30"
  ; "America/New_York", "2018-10-07", "09:30"
  ; "Europe/London", "2019-10-07", "09:30"
  ]
;;

let%bench_fun "Time.   of_date_ofday,         different zones" =
  bench_time_of_date_ofday different_zones
;;

let%bench_fun "Time.   of_date_ofday_precise, different zones" =
  bench_time_of_date_ofday_precise different_zones
;;

let%bench_fun "Time_ns.of_date_ofday,         different zones" =
  bench_time_ns_of_date_ofday different_zones
;;

let%bench_fun "Time.   to_date,               different zones" =
  bench_time_to_date different_zones
;;

let%bench_fun "Time_ns.to_date,               different zones" =
  bench_time_ns_to_date different_zones
;;

let%bench_fun "Time.   to_ofday,              different zones" =
  bench_time_to_ofday different_zones
;;

let%bench_fun "Time_ns.to_ofday,              different zones" =
  bench_time_ns_to_ofday different_zones
;;

let%bench_fun "Time.   to_date_ofday,         different zones" =
  bench_time_to_date_ofday different_zones
;;

let%bench_fun "Time.   to_date_ofday_precise, different zones" =
  bench_time_to_date_ofday_precise different_zones
;;

let%bench_fun "Time_ns.to_date_ofday,         different zones" =
  bench_time_ns_to_date_ofday different_zones
;;
