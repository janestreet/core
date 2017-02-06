open Core
open OUnit;;

module Date = Date
module Ofday = Time.Ofday
module Span = Time.Span

let teq t1 t2 =
  if Sys.word_size = 64 then
    Float.iround ~dir:`Zero (Time.Span.to_sec (Time.to_span_since_epoch t1) *. 1000.)
    = Float.iround ~dir:`Zero (Time.Span.to_sec (Time.to_span_since_epoch t2) *. 1000.)
  else
    true (* milliseconds since 1970 too large for truncate *)
let speq s1 s2 = round (Time.Span.to_ms s1) = round (Time.Span.to_ms s2)
let convtest ?tol f1 f2 =
  let x = float (Random.int 1_000_000) /. 1000. in
  let tol = match tol with
    | None -> Float.robust_comparison_tolerance
    | Some pct -> x *. pct
  in
  Float.abs (f1 (f2 x) -. x) <= tol

let mintime_str = "0000-01-01 00:00:00.000000"
let maxtime_str = "3000-01-01 00:00:00.000000"

let time_gen () = Time.of_span_since_epoch (Time.Span.of_sec (Quickcheck_deprecated.fg ()))

let reasonable_time time = (* between about 1970 and 2070 *)
  let time = Time.to_span_since_epoch time |> Time.Span.to_sec in
  time > 0. && time < 100. *. 52. *. 24. *. 60. *. 60.

let similar_time time time' =
  let time = Time.to_span_since_epoch time |> Time.Span.to_sec in
  let time' = Time.to_span_since_epoch time' |> Time.Span.to_sec in
  Float.abs (time -. time') < 0.01

let test_list = ref []
let add name test = test_list := (name >:: test) :: !test_list

let () =
  add "t"
    (fun () ->
       let s1 = "2005-05-25 12:46-4:00" in
       let s2 = "2005-05-25 12:46:15-4:00" in
       let s3 = "2005-05-25 12:46:15.232-4:00" in
       let s4 = "2005-05-25 12:46:15.232338-4:00" in
       let time1 = Time.of_string s1 in
       let time2 = Time.of_string s2 in
       let time3 = Time.of_string s3 in
       let time4 = Time.of_string s4 in
       let now1 = Time.now () in
       let now2 = Time.now () in
       "diff1"  @? (Float.iround_exn ~dir:`Nearest (Time.Span.to_sec (Time.diff time2 time1)) = 15);
       "diff1'" @? (Float.iround_exn ~dir:`Nearest (Time.Span.to_ms (Time.diff time2 time1)) = 15 * 1000);
       "diff2"  @? (Float.iround_exn ~dir:`Nearest (Time.Span.to_ms (Time.diff time3 time2)) = 232);
       "diff3"  @? (Float.iround_exn ~dir:`Nearest (Time.Span.to_us (Time.diff time4 time3)) = 338);
       "ord"    @? (now2 >= now1);
       "sexp1"  @? (Time.t_of_sexp (Time.sexp_of_t time1) = time1);
       "sexp2"  @? (Time.t_of_sexp (Time.sexp_of_t time2) = time2);
       "sexp3"  @? (Time.t_of_sexp (Time.sexp_of_t time3) = time3);
       let zone = Time.Zone.find_exn "America/New_York" in
       let date, ofday = Time.to_date_ofday time3 ~zone in
       "date"   @? (date = Date.of_string "2005-05-25");
       "ofday"  @? (Ofday.(=.) ofday (Time.Ofday.of_string "12:46:15.232"));
       "ofday1" @? (Time.Ofday.of_string "09:13" = Time.Ofday.of_string "0913");
       "add1"   @? teq (Time.add time1 (sec 15.)) time2;
       "add2"   @? teq (Time.add time2 (Time.Span.of_ms 232.)) time3;
    )

let () =
  add "Ofday_string_conversion"
    (fun () ->
       (* We want to test a number of times during the day, but testing all
          possible times is too expensive.  We also want the test to always be
          the same, so this uses a specific Random.State to generate a repeatable
          series of random times to test *)
       let rand_state = Random.State.make [| 1; 2; 3; 4; 5; 6; 7 |] in
       for _ = 0 to 100_000 do
         let secs = Random.State.int rand_state 86_400_000 in
         let ofday = Ofday.of_span_since_start_of_day (Time.Span.of_ms (float secs)) in
         let ofday_string = Ofday.to_string ofday in
         let ofday' = Ofday.of_string ofday_string in
         if Ofday.(<>.) ofday ofday' then
           failwithf "(%d seconds) %s (%.20f) <> Ofday.of_string %s (%.20f)"
             secs ofday_string (Ofday.to_span_since_start_of_day ofday |> Time.Span.to_sec) (Ofday.to_string ofday')
             (Ofday.to_span_since_start_of_day ofday' |> Time.Span.to_sec) ();
         let ofday' = Ofday.of_string ofday_string in
         if Ofday.(<>.) ofday ofday' then
           failwithf "%s <> Ofday.of_string %s"
             ofday_string (Ofday.to_string ofday') ();
       done)


let () =
  add "date"
    (fun () ->
       let zone = (force Time.Zone.local) in
       let start =
         Time.of_date_ofday ~zone
           (Date.create_exn ~y:1999 ~m:Month.Jan ~d:1)
           Ofday.start_of_day
       in
       let day = Span.of_day 1. in
       let number_of_days =
         match Word_size.word_size with
         | W64 -> 100_000
         | W32 -> 365 * 39 (* do not go pass 2038 *)
       in
       for i = 0 to number_of_days do
         let date =
           Time.to_date ~zone (Time.add start (Time.Span.scale day (float i)))
         in
         let date_string = Date.to_string date in
         let date' = Date.of_string date_string in
         if Date.(<>) date date' then
           failwithf "%s <> Date.of_string %s"
             date_string (Date.to_string date') ();
       done)

module Old_date_impl = struct
  let of_tm tm =
    Date.create_exn
      ~y:(tm.Unix.tm_year + 1900)
      ~m:(Month.of_int_exn (tm.Unix.tm_mon + 1))
      ~d:tm.Unix.tm_mday
  ;;

  let to_tm t =
    { Unix.
      tm_sec = 0;
      tm_min = 0;
      tm_hour = 12;
      tm_mday = Date.day t;
      tm_mon = Month.to_int (Date.month t) - 1;
      tm_year = Date.year t - 1900;
      tm_wday = 0;
      tm_yday = 0;
      tm_isdst = false;
    }
  ;;

  let to_time_internal t =
    let tm_date = to_tm t in
    let time = fst (Unix.mktime tm_date) in
    Time.of_span_since_epoch (Time.Span.of_sec time)
  ;;

  let of_time_internal time = of_tm (Unix.localtime (Float.round ~dir:`Down (Time.to_span_since_epoch time |> Time.Span.to_sec)))

  let add_days t n =
    let time = to_time_internal t in
    of_time_internal (Time.add time (Span.of_day (Float.of_int n)))
  ;;

  let day_of_week t =
    let uday = to_tm t in
    let sec, _ = Unix.mktime uday in
    let unix_wday = (Unix.localtime sec).Unix.tm_wday in
    Day_of_week.of_int_exn unix_wday
  ;;

  let exhaustive_date_range =
    if Sys.c_int_size () < 64 then
      Date.create_exn ~y:1970 ~m:Month.Jan ~d:1, 365 * 68
    else
      Date.create_exn ~y:1900 ~m:Month.Jan ~d:1, 365 * 100

  let exhaustive_day_of_week_test () =
    let start_date, ndays = exhaustive_date_range in
    let rec loop n current_date =
      if n = ndays then true
      else begin
        let old_method = day_of_week current_date in
        let new_method = Date.day_of_week current_date in
        if Day_of_week.(=) new_method old_method
        then loop (n + 1) (Date.add_days current_date 1)
        else false
      end
    in
    loop 1 start_date
  ;;

  let exhaustive_add_days_test () =
    let start_date, ndays = exhaustive_date_range in
    let rec loop n current_date =
      if n = ndays then true
      else begin
        let old_method = add_days current_date 1 in
        let new_method = Date.add_days current_date 1 in
        if (Date.(=) old_method new_method)
        then loop (n + 1) new_method
        else false
      end
    in
    loop 1 start_date;
  ;;
end

let () =
  add "day_of_week"
    (fun () ->
       "exhaustive" @? Old_date_impl.exhaustive_day_of_week_test ())

let () =
  add "add_days"
    (fun () ->
       "one" @? (Date.add_days (Date.of_string "2008-11-02") 1 =
                 Date.of_string "2008-11-03");
       "two" @? (Date.add_days (Date.of_string "2008-11-02") 2 =
                 Date.of_string "2008-11-04");
       "leap" @? (Date.add_days (Date.of_string "2000-02-28") 1 =
                  Date.of_string "2000-02-29");
       "exhaustive" @? Old_date_impl.exhaustive_add_days_test ()
    )

let () =
  add "add_months"
    (fun () ->
       "zero" @? (Date.add_months (Date.of_string "2009-02-28") 0 =
                  Date.of_string "2009-02-28");
       "one" @? (Date.add_months (Date.of_string "2009-01-30") 1 =
                 Date.of_string "2009-02-28");
       "two" @? (Date.add_months (Date.of_string "2009-01-30") 2 =
                 Date.of_string "2009-03-30");
       "december" @? (Date.add_months (Date.of_string "2009-02-28") 10 =
                      Date.of_string "2009-12-28");
       "neg" @? (Date.add_months (Date.of_string "2009-01-30") (-11) =
                 Date.of_string "2008-02-29");
    )

let () =
  add "add_weekdays"
    (fun () ->
       let test lbl d1 n d2 =
         lbl @? (Date.add_weekdays
                   (Date.of_string d1) n = Date.of_string d2)
       in
       test "one" "2009-01-01" 1 "2009-01-02";
       test "one_weekend" "2009-01-02" 1 "2009-01-05";
       test "neg_one" "2009-01-02" (-1) "2009-01-01";
       test "neg_one_weekend" "2009-01-05" (-1) "2009-01-02";
       test "neg_two_weekend" "2009-01-06" (-2) "2009-01-02";
       test "non_leap_weekend" "2009-02-27" 1 "2009-03-02";
       test "leap_weekend" "2008-02-28" 2 "2008-03-03";
    )
;;

let () =
  add "add_business_days"
    (fun () ->
       let test lbl d1 n d2 =
         let is_holiday d =
           List.mem ~equal:Date.equal (List.map [
             "2009-01-01";
             "2009-03-01";
             "2009-03-02";
           ] ~f:Date.of_string) d
         in
         let res = Date.add_business_days ~is_holiday (Date.of_string d1) n in
         lbl @? (res = Date.of_string d2)
       in
       test "one" "2009-01-01" 1 "2009-01-05";
       test "one_weekend" "2009-01-02" 1 "2009-01-05";
       test "neg_one" "2009-01-02" (-1) "2008-12-31";
       test "neg_one_weekend" "2009-01-05" (-1) "2009-01-02";
       test "neg_two_weekend" "2009-01-06" (-2) "2009-01-02";
       test "non_leap_weekend" "2009-02-27" 1 "2009-03-03";
       test "leap_weekend" "2008-02-28" 2 "2008-03-03";
    )
;;

let () =
  add "span_scale"
    (fun () ->
       "ms" @? speq (Time.Span.scale (sec 10.) 0.001) (Time.Span.of_ms 10.);
       "min" @? speq (Time.Span.scale (sec 10.) 60.) (Time.Span.of_min 10.);
       "hr" @? speq (Time.Span.scale (sec 10.) (60. *. 60.)) (Time.Span.of_hr 10.);
    );
  add "span_conv"
    (fun () ->
       for _ = 1 to 100 do
         "sec" @? convtest Time.Span.to_sec sec;
         "ms" @? convtest Time.Span.to_ms Span.of_ms;
         "min" @? convtest (fun x -> Time.Span.to_sec x /. 60.) Span.of_min;
         "hr" @? convtest (fun x -> Time.Span.to_sec x /. 60. /. 60.) Span.of_hr;
         "sexp" @?
         convtest ~tol:0.0001
           (fun x -> Time.Span.to_sec (Time.Span.t_of_sexp x))
           (fun x -> Span.sexp_of_t (sec x));
       done
    );
  add "date"
    (fun () ->
       let d = Date.create_exn ~y:2004 ~m:Month.Apr ~d:15 in
       "conv1" @? (Date.to_string d = "2004-04-15");
       "conv2" @? (d = Date.of_string "2004-04-15");
       "conv3" @? (d = Date.of_string "20040415");
       "conv4" @? (d = Date.of_string "15APR2004");
       "conv5" @? (d = Date.of_string "04/15/2004");
       "conv6" @? (d = Date.of_string "2004/04/15");
       "conv7" @? (d = Date.of_string "4/15/4");
    );
  add "norollover"
    (fun () ->
       let zone = Time.Zone.of_string "nyc" in
       let t1   = Time.of_localized_string ~zone "2005-05-25 12:46:59.900" in
       let t2   = Time.add t1 (Time.Span.of_ms 99.9) in
       (* within 1 mic *)
       "60secspr" @? ((Time.to_string_abs ~zone t2) = "2005-05-25 12:46:59.999900-04:00");
    );
  add "to_string,of_string"
    (fun () ->
       let check time =
         if reasonable_time time
         then begin
           let time' = Time.of_string (Time.to_string time) in
           if similar_time time time' then true
           else begin
             Printf.printf "\nbad time: %f\n%!" (Time.to_span_since_epoch time |> Time.Span.to_sec);
             exit 7;
           end;
         end else true
       in
       Quickcheck_deprecated.laws_exn "string" 100 time_gen check
    );
  add "to_string,of_string2"
    (fun () ->
       let zone = Time.Zone.find_exn "America/New_York" in
       let s = "2005-06-01 10:15:08.047123-04:00" in
       let t = Time.of_string s in
       "foo" @? (Time.to_string_abs t ~zone = s)
    );
  add "to_string,of_string3"
    (fun () ->
       let zone = Time.Zone.find_exn "America/New_York" in
       let s = "2006-06-16 04:37:07.082945-04:00" in
       let t = Time.of_string s in
       "foo" @? (Time.to_string_abs t ~zone = s)
    );

  add "of_string without colon, negative offset"
    (fun () ->

       let t = "2015-07-14 10:31:55.564871-04:00" |> Time.of_string_abs in
       let s = "2015-07-14 10:31:55.564871-0400" in
       "no-colon-abs-negative-offset" @? (Time.of_string_abs s = t)
    );

  add "of_string without colon, positive offset"
    (fun () ->
       let t = "2015-07-14 10:31:55.564871+04:00" |> Time.of_string_abs in
       let s = "2015-07-14 10:31:55.564871+0400" in
       "no-colon-abs-positive-offset" @? (Time.of_string_abs s = t)
    );

  add "of_string with leap second"
    (fun () ->
       let expected_time_at_leap_second =
         Time.of_date_ofday
           ~zone:Time.Zone.utc
           (Date.create_exn ~y:2015 ~m:Jul ~d:1)
           Time.Ofday.start_of_day
       in
       "foo" @?
       List.for_all
         ~f:(fun s -> Time.of_string s = expected_time_at_leap_second)
         [ "2015-06-30 23:59:60Z"
         ; "2015-06-30 23:59:60.500Z" ]);
  add "to_filename_string,of_filename_string"
    (fun () ->
       let zone = (force Time.Zone.local) in
       let check time =
         if reasonable_time time
         then
           let time' =
             Time.of_filename_string ~zone
               (Time.to_filename_string time ~zone)
           in
           similar_time time time'
         else true
       in
       Quickcheck_deprecated.laws_exn "string" 100 time_gen check;
    );
  add "to_filename_string,of_filename_string2"
    (fun () ->
       let zone = (force Time.Zone.local) in
       let s = "2005-06-01_10-15-08.047983" in
       let t = Time.of_filename_string s ~zone in
       "foo" @? (Time.to_filename_string t ~zone = s)
    );
  add "of_sexp,to_sexp"
    (fun () ->
       let check time =
         if reasonable_time time
         then
           let time' = Time.t_of_sexp (Time.sexp_of_t time) in
           similar_time time time'
         else true
       in
       Quickcheck_deprecated.laws_exn "sexp" 100 time_gen check
    );
  add "daylight_saving_time"
    (fun () ->
       let zone = Time.Zone.find_exn "America/New_York" in
       let s = "2006-04-02 23:00:00.000000-04:00" in
       let time = Time.of_string s in
       "dst" @? (Time.to_string_abs ~zone time = s)
    );
  add "weird_date_in_time"
    (fun () ->
       let zone = Time.Zone.find_exn "America/New_York" in
       let t1 = Time.of_string "01 JAN 2008 10:37:22.551-05:00" in
       "rnse1" @? (Time.to_string_abs t1 ~zone = "2008-01-01 10:37:22.551000-05:00");
       let t2 = Time.of_string "01 FEB 2008 17:38:44.031-05:00" in
       "rnse2" @? (Time.to_string_abs t2 ~zone = "2008-02-01 17:38:44.031000-05:00")
    );
  add "ofday_small_diff"
    (fun () ->
       let same x y = Float.abs (x -. y) < sqrt Float.epsilon_float in
       let check (s1,s2,d) =
         let t1 = Time.Ofday.of_string s1 in
         let t2 = Time.Ofday.of_string s2 in
         same (Time.Span.to_sec (Time.Ofday.small_diff t1 t2)) d
         && same (Time.Span.to_sec (Time.Ofday.small_diff t2 t1)) (~-. d)
       in
       "foo" @? List.for_all ~f:check
                  ["10:00:01.298", "14:59:55.000", 6.298;
                   "08:59:54.000", "10:00:01.555", (-7.555);
                   "12:48:55.787", "17:48:55.000", 0.787;
                  ]);
  add "occurrence_right_side"
    (fun () ->
       let times = [
         "00:00:00";
         "00:00:01";
         "09:00:00";
         "11:59:59";
         "12:00:00";
         "12:00:01";
         "18:30:30";
         "23:59:59";
       ] in
       let now    = Time.now () in
       let now_f  = Time.to_span_since_epoch now |> Time.Span.to_sec in
       let zone   = (force Time.Zone.local) in
       let utimes = Time.to_ofday ~zone now :: List.map times ~f:(Time.Ofday.of_string) in
       let after_times =
         List.map utimes ~f:(fun ut ->
           Time.occurrence `First_after_or_at now ~zone ~ofday:ut)
       in
       let before_times =
         List.map utimes ~f:(fun ut ->
           Time.occurrence `Last_before_or_at now ~zone ~ofday:ut)
       in
       "right-side-after" @? List.for_all after_times
                               ~f:(fun t -> Time.Span.to_sec (Time.to_span_since_epoch t) >= now_f);
       "right-side-before" @? List.for_all before_times
                                ~f:(fun t -> Time.Span.to_sec (Time.to_span_since_epoch t) <= now_f);
    );
  add "occurrence_distance"
    (fun () ->
       let now = Time.of_string "2007-05-04 13:00:00.000" in
       let after_times = [
         ("13:00:00.000", "2007-05-04 13:00:00.000");
         ("13:00:00.001", "2007-05-04 13:00:00.001");
         ("11:59:59.999", "2007-05-05 11:59:59.999");
         ("00:00:00.000", "2007-05-05 00:00:00.000");
         ("12:59:59.000", "2007-05-05 12:59:59.000");
       ] in
       let before_times = [
         ("13:00:00.000", "2007-05-04 13:00:00.000");
         ("13:00:00.001", "2007-05-03 13:00:00.001");
         ("11:59:59.999", "2007-05-04 11:59:59.999");
         ("00:00:00.000", "2007-05-04 00:00:00.000");
         ("12:59:59.000", "2007-05-04 12:59:59.000");
       ] in
       List.iter after_times ~f:(fun (od_s,prediction_s) ->
         let od         = Time.Ofday.of_string od_s in
         let prediction = Time.of_string prediction_s in
         let real       =
           Time.occurrence `First_after_or_at now ~zone:(force Time.Zone.local) ~ofday:od
         in
         ("right-distance - " ^ od_s ^ "," ^ prediction_s) @?
         if Time.Span.to_ms (Time.diff prediction real) = 0. then true
         else false
       );
       List.iter before_times ~f:(fun (od_s,prediction_s) ->
         let od         = Time.Ofday.of_string od_s in
         let prediction = Time.of_string prediction_s in
         let real       =
           Time.occurrence `Last_before_or_at now ~zone:(force Time.Zone.local) ~ofday:od
         in
         ("right-distance - " ^ od_s ^ "," ^ prediction_s) @?
         if Time.Span.to_ms (Time.diff prediction real) = 0. then true
         else false
       )
    );
  add "diff"
    (fun () ->
       let d1 = Date.create_exn ~y:2000 ~m:Month.Jan ~d:1 in
       let d2 = Date.create_exn ~y:2000 ~m:Month.Jan ~d:2 in
       let d3 = Date.create_exn ~y:2000 ~m:Month.Feb ~d:28 in
       let d4 = Date.create_exn ~y:2000 ~m:Month.Mar ~d:1 in
       "normal-diff" @? (Date.diff d2 d1 = 1);
       "leap-diff" @? (Date.diff d4 d3 = 2)
    )
;;


let roundtrip s =
  let t = Span.of_string s in
  ("string roundtrip " ^ s) @?
  Span.(=) t (Time.Span.of_string (Time.Span.to_string t));
  ("span roundtrip " ^ s) @?
  String.(=) s (Time.Span.to_string (Time.Span.of_string s))
;;


exception Finished
let () =
  let assert_raises f =
    try f (); raise Finished with
    | Finished -> assert false
    | _ -> ()
  in
  let extensions = ["ms";"s";"m";"h"] in
  add "roundtrip span<->string" (fun () ->
    List.iter extensions ~f:(fun ext ->
      let t x = roundtrip (x ^ ext) in
      t "1";
      t "5";
      t "1.34";
    );
    let t x = roundtrip (x ^ "s") in
    t "59.9999";
    t "59";
  );
  add "Span.of_string (nan)" (fun () ->
    assert_raises (fun () -> ignore (Time.Span.of_string "nans")));
  add "Span.of_string (inf)" (fun () ->
    assert_raises (fun () -> ignore (Time.Span.of_string "infs")));
  add "Span.of_string" (fun () ->
    let test string secs =
      ("sec " ^ string) @? (Time.Span.to_sec (Time.Span.of_string string) = secs)
    in
    test "1ms" 0.001;
    test "95ms" 0.095;
    test "1222ms" 1.222;
    test "1.222s" 1.222;
    test "0.5m" 30.;
    test "1m" 60.;
    test "1h" (60. *. 60.);
  );
  add "Time.of_string_fix_proto" (fun () ->
    let test s t =
      ("fix proto time " ^ s) @? (Time.of_string_fix_proto `Utc s = t)
    in
    test
      "20080603-13:55:35.577"
      (Time.of_span_since_epoch (Time.Span.of_sec (Int64.float_of_bits 4742872407195577745L))))
;;



let test = "time" >::: !test_list
