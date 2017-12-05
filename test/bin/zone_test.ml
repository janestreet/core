open OUnit
open Core

(* We don't test Feb 29th because generating proper leap year dates is
   trickier.  Also, there are no time zone changes on leap dates. *)
let month_limits = Map.Poly.of_alist_exn [
  1, 31;
  2, 28;
  3, 31;
  4, 30;
  5, 31;
  6, 30;
  7, 31;
  8, 31;
  9, 30;
  10, 31;
  11, 30;
  12, 31
]

let random_time state =
  (* If we go out much further then floating point errors at the microsecond
     level start to creep in.  We can change this when Time.t = int64 *)
  let year  = 1970 + Random.State.int state 67 in
  let month = 1 + (Random.State.int state 12) in
  let day   = 1 + (Random.State.int state (Map.find_exn month_limits month)) in
  let hour  = Random.State.int state 12 + 8 in
  let min   = Random.State.int state 60 in
  let sec   = Random.State.int state 60 in
  let ms    = Random.State.int state 1_000 in
  let mic   = Random.State.int state 1_000 in
  (year,month,day,hour,min,sec,ms,mic)
;;

let random_time_str state =
  let year,month,day,hour,min,sec,ms,_mic = random_time state in
  sprintf "%d-%.2d-%.2d %.2d:%.2d:%.2d.%.3d000" year month day hour min sec ms
;;

let random_tm state =
  let (year,month,day,hour,min,sec,_,_) = random_time state in
  {Unix.
    tm_sec   = sec;
    tm_min   = min;
    tm_hour  = hour;
    tm_mday  = day;
    tm_mon   = month;
    tm_year  = year - 1900;
    tm_wday  = 0;
    tm_yday  = 0;
    tm_isdst = false;
  }

let zone_tests = ref []
let add name test = zone_tests := (name >:: test) :: !zone_tests

let add_random_string_round_trip_test state s1 =
  let pos_neg = if Random.State.bool state then "+" else "-" in
  let distance = Int.to_string (Random.State.int state 10 + 1) in
  let s2 = String.concat [s1; pos_neg; distance; ":00"] in
  let zone = (force Time.Zone.local) in
  let s1 =
    let t = Time.of_string s1 in
    let utc_epoch =
      Time.Zone.relative_time_of_absolute_time zone t
      |> Time.Zone.absolute_time_of_relative_time Time.Zone.utc
    in
    let f =
      Time.diff utc_epoch t
      |> Time.Span.to_sec
      |> Float.to_int
    in
    s1 ^ (if f = 0 then "Z" else Printf.sprintf "%+03d:00" (f / 3600))
  in
  add ("roundtrip string " ^ s1) (fun () ->
    "s1" @? (s1 = (Time.to_string_abs (Time.of_string s1) ~zone));
    "s2-time" @? (
      let s2_time1 = Time.of_string s2 in
      let s2_time2 = Time.of_string (Time.to_string_abs s2_time1 ~zone) in
      Time.(=.) s2_time1 s2_time2)
  )

let add_random_string_round_trip_tests state =
  for _ = 1 to 100 do
    add_random_string_round_trip_test state (random_time_str state)
  done;
;;

let add_roundtrip_conversion_test state (zone_name,(zone:Time.Zone.t)) =
  add ("roundtrip conversion " ^ zone_name) (fun () ->
    let tm        = random_tm state in
    let unix_time = 1664476678.000 in
    let time      = Time.of_span_since_epoch (Time.Span.of_sec unix_time) in
    let (zone_date, zone_ofday) =
      let date,ofday = Time.to_date_ofday ~zone:(force Time.Zone.local) time in
      Time.convert
        ~from_tz:(force Time.Zone.local)
        ~to_tz:zone
        date
        ofday
    in
    let round_trip_time =
      let round_date,round_ofday =
        Time.convert
          ~from_tz:zone
          ~to_tz:(force Time.Zone.local)
          zone_date
          zone_ofday
      in
      Time.of_date_ofday ~zone:(force Time.Zone.local) round_date round_ofday
    in
    "time" @?
    (if time = round_trip_time then true
     else begin
       failwith (String.concat [
         sprintf "tm: %s\n" (Sexp.to_string_hum (Unix.sexp_of_tm tm));
         sprintf "unix_time: %.20f\n" unix_time;
         sprintf "our_time: %.20f\n" (Time.to_span_since_epoch time |> Time.Span.to_sec);
         sprintf "date, ofday: %s, %s\n"
           (Date.to_string zone_date) (Time.Ofday.to_string zone_ofday);
         sprintf "round_trip: %.20f\n" (Time.to_span_since_epoch round_trip_time |> Time.Span.to_sec)
       ])
     end))

module Localtime_test_data = struct
  type t = {
    zone_name              : string;
    unix_time              : float;
    localtime_date_string  : string;
    localtime_ofday_string : string;
    our_date_string        : string;
    our_ofday_string       : string;
  } [@@deriving sexp]
end

let add_random_localtime_tests state =
  List.iter (Time.Zone.initialized_zones ()) ~f:(fun (zone_name, zone) ->
    add ("localtime " ^ zone_name) (fun () ->
      let tm          = random_tm state in
      let tm          = Unix.gmtime (Unix.timegm tm) in

      (* goes through the dance of setting the env variable, then calling localtime, then
         setting the TZ back.  We call localtime on 1000. each time to reset the internal
         state of localtime, which matters when we convert indeterminate times. *)
      Unix.putenv ~key:"TZ" ~data:zone_name;
      ignore (Unix.localtime 1000.);
      let unix_time,_ = Unix.mktime tm in
      let localtime = Unix.localtime unix_time in
      let localtime_date_string  = Unix.strftime localtime "%Y-%m-%d" in
      let localtime_ofday_string = Unix.strftime localtime "%H:%M:%S.000000" in
      Unix.unsetenv ("TZ");
      ignore (Unix.localtime 1000.);

      let our_date,our_ofday = Time.to_date_ofday (Time.of_span_since_epoch (Time.Span.of_sec unix_time)) ~zone in
      let test_data          =
        {Localtime_test_data.
          zone_name;
          unix_time;
          our_date_string  = Date.to_string our_date;
          our_ofday_string = Time.Ofday.to_string our_ofday;
          localtime_date_string;
          localtime_ofday_string;
        }
      in
      "date" @?
      (if Localtime_test_data.(
         test_data.localtime_date_string = test_data.our_date_string)
       then
         true
       else
         failwith (Sexp.to_string (Localtime_test_data.sexp_of_t test_data)));
      "ofday" @?
      (if Localtime_test_data.(
         test_data.localtime_ofday_string = test_data.our_ofday_string)
       then
         true
       else
         failwith (Sexp.to_string (Localtime_test_data.sexp_of_t test_data)))))
;;

let add_roundtrip_conversion_tests state =
  List.iter (Time.Zone.initialized_zones ()) ~f:(add_roundtrip_conversion_test state)
;;

let add_randomized_tests () =
  let state = Random.State.make [| 1; 2; 3; 4 |] in
  add_random_string_round_trip_tests state;
  add_random_localtime_tests state;
  add_roundtrip_conversion_tests state
;;

let () =
  add_randomized_tests ();
;;

let test = "zone" >::: !zone_tests
