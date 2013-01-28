open OUnit
open Core.Std

let my_tz = Time.Zone.machine_zone ()

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

let random_time () =
  (* dpowers: if we go out much further then floating point errors at the microsecond
     level start to creep in.  We can change this when Time.t = int64 *)
  let year  = 1970 + Random.int 67 in
  let month = 1 + (Random.int 12) in
  let day   = 1 + (Random.int (Map.find_exn month_limits month)) in
  let hour  = Random.int 12 + 8 in
  let min   = Random.int 60 in
  let sec   = Random.int 60 in
  let ms    = Random.int 1_000 in
  let mic   = Random.int 1_000 in
  (year,month,day,hour,min,sec,ms,mic)
;;

let random_time_str () =
  let year,month,day,hour,min,sec,ms,_mic = random_time () in
  sprintf "%d-%0.2d-%0.2d %0.2d:%0.2d:%0.2d.%0.3d000" year month day hour min sec ms
;;

let random_tm () =
  let (year,month,day,hour,min,sec,_,_) = random_time () in
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

let add_random_string_round_trip_tests () =
  for i = 1 to 100 do
    let s1 = random_time_str () in
    let pos_neg = if Random.bool () then "+" else "-" in
    let distance = Int.to_string (Random.int 10 + 1) in
    let s2 = String.concat [s1; pos_neg; distance; ":00"] in
    add ("roundtrip string " ^ s1) (fun () ->
      let t = Time.of_string s1 in
      let c1 = (Time.to_string_deprecated t) in
      if s1 <> c1 then begin
        exit 7;
      end;
      "s1" @? (s1 = (Time.to_string_deprecated (Time.of_string s1)));
      "s2-time" @? (
        let s2_time1 = Time.of_string s2 in
        let s2_time2 = Time.of_string (Time.to_string_abs s2_time1) in
        Time.(=) s2_time1 s2_time2)
    )
  done

let add_roundtrip_conversion_test (zone_name,(zone:Zone.t)) =
  add ("roundtrip conversion " ^ zone_name) (fun () ->
    let tm = random_tm () in
    let unix_time = 1664476678.000 in
    let time = Time.of_float unix_time in
    let (zone_date, zone_ofday) =
      let date,ofday = Time.to_local_date_ofday time in
      Time.convert
        ~from_tz:(Zone.machine_zone ())
        ~to_tz:zone
        date
        ofday
    in
    let round_trip_time =
      let round_date,round_ofday =
        Time.convert
        ~from_tz:zone
        ~to_tz:(Zone.machine_zone ())
        zone_date
        zone_ofday
      in
      Time.of_local_date_ofday round_date round_ofday
    in
    "time" @?
      (if time = round_trip_time then true
      else begin
        failwith (String.concat [
          sprintf "tm: %s\n" (Sexp.to_string_hum (Unix.sexp_of_tm tm));
          sprintf "unix_time: %0.20f\n" unix_time;
          sprintf "our_time: %0.20f\n" (Time.to_float time);
          sprintf "date, ofday: %s, %s\n"
            (Date.to_string zone_date) (Ofday.to_string zone_ofday);
          sprintf "round_trip: %0.20f\n" (Time.to_float round_trip_time)
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
  } with sexp
end

let add_random_localtime_tests () =
  List.iter (Zone.initialized_zones ()) ~f:(fun (zone_name, zone) ->
    add ("localtime " ^ zone_name) (fun () ->
      let tm          = random_tm () in
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

      let our_date,our_ofday = Time.to_date_ofday (Time.of_float unix_time) zone in
      let test_data          =
        {Localtime_test_data.
          zone_name;
          unix_time;
          our_date_string  = Date.to_string our_date;
          our_ofday_string = Ofday.to_string our_ofday;
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
          failwithf "%s" (Sexp.to_string (Localtime_test_data.sexp_of_t test_data)) ());
      "ofday" @?
        (if Localtime_test_data.(
          test_data.localtime_ofday_string = test_data.our_ofday_string)
        then
          true
        else
          failwithf "%s" (Sexp.to_string (Localtime_test_data.sexp_of_t test_data)) ())))
;;

let add_roundtrip_conversion_tests () =
  List.iter (Zone.initialized_zones ()) ~f:add_roundtrip_conversion_test

let add_random_tests () =
  add_random_string_round_trip_tests ();
  add_random_localtime_tests ()
;;

let () =
  add_random_tests ();
  add_roundtrip_conversion_tests ()
;;

let test = "zone" >::: !zone_tests
