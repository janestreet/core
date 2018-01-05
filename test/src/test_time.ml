open Core
open Expect_test_helpers_kernel

open Time
open Exposed_for_tests

let of_sec_since_epoch sec = of_span_since_epoch (Span.of_sec sec)

let%expect_test "[to_string_iso8601] in zulu" =
  let s = to_string_iso8601_basic ~zone:Zone.utc (of_sec_since_epoch 12.345678) in
  printf !"%s" s;
  [%expect {| 1970-01-01T00:00:12.345678Z |}];
;;

let%expect_test "[to_string_iso8601] in local (which is set to NYC in tests)" =
  let s =
    to_string_iso8601_basic ~zone:(Lazy.force Zone.local) (of_sec_since_epoch 12.345678)
  in
  printf !"%s" s;
  [%expect {| 1969-12-31T19:00:12.345678-05:00 |}];
;;

let%test_module "ensure_colon_in_offset" =
  (module struct
    let gen_digit_string ~length =
      let open Quickcheck.Let_syntax in
      let%bind len = length in
      String.gen_with_length len Char.gen_digit

    let%test_unit "add colon" =
      let gen = gen_digit_string ~length:(Int.gen_incl 3 4) in
      Quickcheck.test gen ~sexp_of:String.sexp_of_t ~f:(fun digits ->
        assert (not (String.mem digits ':'));
        let output = ensure_colon_in_offset digits in
        assert (Int.(=) (String.count output ~f:(Char.(=) ':')) 1);
        let prefix, suffix = String.lsplit2_exn output ~on:':' in
        assert (String.(<>) prefix "");
        assert (Int.(>=) (String.length suffix) (String.length prefix))
      )

    let gen_offset_with_colon =
      let open Quickcheck.Generator in
      let gen_prefix = gen_digit_string ~length:(Int.gen_incl 1 2) in
      let gen_suffix = gen_digit_string ~length:(return 2) in
      (tuple2 gen_prefix gen_suffix)
      >>| (fun (a, b) -> String.concat [a; ":"; b])

    let%test_unit "do not add colon" =
      Quickcheck.test gen_offset_with_colon ~sexp_of:String.sexp_of_t ~f:(fun offset ->
        assert (Int.(=) (String.count offset ~f:(Char.(=) ':')) 1);
        assert (String.equal offset (ensure_colon_in_offset offset)))
  end)

let%test_unit _ =
  let unzoned_sexp = Sexp.of_string "(2015-07-03 16:27:00)" in
  set_sexp_zone Zone.utc;
  let in_utc = t_of_sexp unzoned_sexp in
  set_sexp_zone (Zone.of_utc_offset ~hours:8);
  let in_plus8 = t_of_sexp unzoned_sexp in
  set_sexp_zone (Lazy.force Zone.local);
  [%test_result: Span.t]
    ~expect:(Span.of_hr 8.)
    (diff in_utc in_plus8)

let%test _ =
  Set.equal (Set.of_list [epoch])
    (Set.t_of_sexp (Sexp.List [Float.sexp_of_t (to_span_since_epoch epoch |> Span.to_sec)]))
;;

let%test_unit _ =
  let expected_next_multiple ~base ~after ~interval =
    let rec loop at =
      if (>) at after then
        at
      else
        loop (add at interval)
    in
    loop base
  in
  List.iter ~f:(fun (since_base, interval) ->
    let base = epoch in
    let sec = Span.of_sec in
    let interval = sec interval in
    let after = add base (sec since_base) in
    let actual_next_multiple = next_multiple ~base ~after ~interval () in
    let expected_next_multiple = expected_next_multiple ~base ~after ~interval in
    let relativize time = diff time base in
    let times_are_close t1 t2 = Float.(<) (Float.abs (Span.to_us (diff t1 t2))) 1. in
    if not (times_are_close actual_next_multiple expected_next_multiple) then
      failwiths "Time.next_multiple" (since_base, interval,
                                      relativize expected_next_multiple,
                                      relativize actual_next_multiple)
        ([%sexp_of: float * Span.t * Span.t * Span.t]))
    [
      0.    , 1.;
      0.1   , 1.;
      0.9   , 1.;
      1.    , 1.;
      1.1   , 1.;
      1.9   , 1.;
      1000.1, 1.;
      (-1.) , 1.;
      (-1.) , 0.1;
      1.    , 0.2;
      1E-5  , 1E-6;
    ]
;;

let%test_module "of_tm" =
  (module struct
    let unix_epoch_t =
      of_date_ofday ~zone:Zone.utc
        (Date.create_exn ~y:1970 ~m:Jan ~d:1)
        (Ofday.create ())

    let%test_unit _ =
      [%test_result: t] ~expect:unix_epoch_t
        (of_tm ~zone:(Lazy.force Zone.local) (Unix.localtime 0.))

    let%test_unit _ =
      [%test_result: t] ~expect:unix_epoch_t
        (of_tm ~zone:Zone.utc (Unix.gmtime 0.))
  end)

let%test_module "format" =
  (module struct
    let hkg = Time.Zone.find_exn "Asia/Hong_Kong"
    let ldn = Time.Zone.find_exn "Europe/London"
    let nyc = Time.Zone.find_exn "America/New_York"

    let zones = [hkg; ldn; nyc]

    let test_time time =
      print_endline (Time.to_string_abs time ~zone:Zone.utc);
      List.iter zones ~f:(fun zone ->
        print_endline (format time ~zone "%F %T" ^ " -- " ^ Time.Zone.name zone))

    let time1 = of_string_abs ("2015-01-01 10:00:00 Europe/London")
    let time2 = of_string_abs ("2015-06-06 10:00:00 Europe/London")

    let%expect_test _ =
      test_time time1;
      [%expect {|
        2015-01-01 10:00:00.000000Z
        2015-01-01 18:00:00 -- Asia/Hong_Kong
        2015-01-01 10:00:00 -- Europe/London
        2015-01-01 05:00:00 -- America/New_York |}]

    let%expect_test _ =
      test_time time2;
      [%expect {|
        2015-06-06 09:00:00.000000Z
        2015-06-06 17:00:00 -- Asia/Hong_Kong
        2015-06-06 10:00:00 -- Europe/London
        2015-06-06 05:00:00 -- America/New_York |}]

    let list_of_transition = function
      | None           -> []
      | Some (time, _) -> [time]

    let transitions_of_time time zone =
      List.concat_map ~f:list_of_transition [
        Zone.prev_clock_shift zone ~at_or_before:time;
        Zone.next_clock_shift zone ~strictly_after:time;
      ]

    let times_around time =
      List.map [-2.;-1.;+0.;+1.;+2.] ~f:(fun min ->
        Time.add time (Time.Span.of_min min))

    let test_transition time =
      List.iter (times_around time) ~f:test_time

    let test_transitions time zone =
      List.iter (transitions_of_time time zone) ~f:(fun time ->
        print_endline "";
        test_transition time)

    let%expect_test _ =
      test_transitions time1 ldn;
      [%expect {|
        2014-10-26 00:58:00.000000Z
        2014-10-26 08:58:00 -- Asia/Hong_Kong
        2014-10-26 01:58:00 -- Europe/London
        2014-10-25 20:58:00 -- America/New_York
        2014-10-26 00:59:00.000000Z
        2014-10-26 08:59:00 -- Asia/Hong_Kong
        2014-10-26 01:59:00 -- Europe/London
        2014-10-25 20:59:00 -- America/New_York
        2014-10-26 01:00:00.000000Z
        2014-10-26 09:00:00 -- Asia/Hong_Kong
        2014-10-26 01:00:00 -- Europe/London
        2014-10-25 21:00:00 -- America/New_York
        2014-10-26 01:01:00.000000Z
        2014-10-26 09:01:00 -- Asia/Hong_Kong
        2014-10-26 01:01:00 -- Europe/London
        2014-10-25 21:01:00 -- America/New_York
        2014-10-26 01:02:00.000000Z
        2014-10-26 09:02:00 -- Asia/Hong_Kong
        2014-10-26 01:02:00 -- Europe/London
        2014-10-25 21:02:00 -- America/New_York

        2015-03-29 00:58:00.000000Z
        2015-03-29 08:58:00 -- Asia/Hong_Kong
        2015-03-29 00:58:00 -- Europe/London
        2015-03-28 20:58:00 -- America/New_York
        2015-03-29 00:59:00.000000Z
        2015-03-29 08:59:00 -- Asia/Hong_Kong
        2015-03-29 00:59:00 -- Europe/London
        2015-03-28 20:59:00 -- America/New_York
        2015-03-29 01:00:00.000000Z
        2015-03-29 09:00:00 -- Asia/Hong_Kong
        2015-03-29 02:00:00 -- Europe/London
        2015-03-28 21:00:00 -- America/New_York
        2015-03-29 01:01:00.000000Z
        2015-03-29 09:01:00 -- Asia/Hong_Kong
        2015-03-29 02:01:00 -- Europe/London
        2015-03-28 21:01:00 -- America/New_York
        2015-03-29 01:02:00.000000Z
        2015-03-29 09:02:00 -- Asia/Hong_Kong
        2015-03-29 02:02:00 -- Europe/London
        2015-03-28 21:02:00 -- America/New_York |}]

    let%expect_test _ =
      test_transitions time1 nyc;
      [%expect {|
        2014-11-02 05:58:00.000000Z
        2014-11-02 13:58:00 -- Asia/Hong_Kong
        2014-11-02 05:58:00 -- Europe/London
        2014-11-02 01:58:00 -- America/New_York
        2014-11-02 05:59:00.000000Z
        2014-11-02 13:59:00 -- Asia/Hong_Kong
        2014-11-02 05:59:00 -- Europe/London
        2014-11-02 01:59:00 -- America/New_York
        2014-11-02 06:00:00.000000Z
        2014-11-02 14:00:00 -- Asia/Hong_Kong
        2014-11-02 06:00:00 -- Europe/London
        2014-11-02 01:00:00 -- America/New_York
        2014-11-02 06:01:00.000000Z
        2014-11-02 14:01:00 -- Asia/Hong_Kong
        2014-11-02 06:01:00 -- Europe/London
        2014-11-02 01:01:00 -- America/New_York
        2014-11-02 06:02:00.000000Z
        2014-11-02 14:02:00 -- Asia/Hong_Kong
        2014-11-02 06:02:00 -- Europe/London
        2014-11-02 01:02:00 -- America/New_York

        2015-03-08 06:58:00.000000Z
        2015-03-08 14:58:00 -- Asia/Hong_Kong
        2015-03-08 06:58:00 -- Europe/London
        2015-03-08 01:58:00 -- America/New_York
        2015-03-08 06:59:00.000000Z
        2015-03-08 14:59:00 -- Asia/Hong_Kong
        2015-03-08 06:59:00 -- Europe/London
        2015-03-08 01:59:00 -- America/New_York
        2015-03-08 07:00:00.000000Z
        2015-03-08 15:00:00 -- Asia/Hong_Kong
        2015-03-08 07:00:00 -- Europe/London
        2015-03-08 03:00:00 -- America/New_York
        2015-03-08 07:01:00.000000Z
        2015-03-08 15:01:00 -- Asia/Hong_Kong
        2015-03-08 07:01:00 -- Europe/London
        2015-03-08 03:01:00 -- America/New_York
        2015-03-08 07:02:00.000000Z
        2015-03-08 15:02:00 -- Asia/Hong_Kong
        2015-03-08 07:02:00 -- Europe/London
        2015-03-08 03:02:00 -- America/New_York |}]
  end)

let%test_module "parse" =
  (module struct
    let unix_epoch_t =
      of_date_ofday ~zone:Zone.utc
        (Date.create_exn ~y:1970 ~m:Jan ~d:1)
        (Ofday.create ())

    let%test_unit _ =
      [%test_result: t] ~expect:unix_epoch_t
        (parse ~zone:Zone.utc ~fmt:"%Y-%m-%d %H:%M:%S" "1970-01-01 00:00:00")

    let%test_unit _ =
      [%test_result: t] ~expect:unix_epoch_t
        (parse
           ~zone:(Zone.find_exn "Asia/Hong_Kong")
           ~fmt:"%Y-%m-%d %H:%M:%S"
           "1970-01-01 08:00:00")
  end)

let%expect_test "accept float instead of time/span/ofday for hash tables and hash sets" =
  let module Of_string (M : Sexpable.S1) = struct
    type t = string M.t [@@deriving sexp]
  end in
  let test (module M : Sexpable) string =
    print_s (M.sexp_of_t (M.t_of_sexp (Sexp.of_string string)))
  in
  test (module Time.Hash_set) {| (0 0.05 946746000 1381152600) |};
  [%expect {|
    ((1969-12-31 19:00:00.000000-05:00)
     (1969-12-31 19:00:00.050000-05:00)
     (2000-01-01 12:00:00.000000-05:00)
     (2013-10-07 09:30:00.000000-04:00)) |}];
  test (module Of_string (Time.Table)) {|
    ((0          "arbitrary value")
     (0.05       "arbitrary value")
     (946746000  "arbitrary value")
     (1381152600 "arbitrary value")) |};
  [%expect {|
    (((1969-12-31 19:00:00.000000-05:00) "arbitrary value")
     ((1969-12-31 19:00:00.050000-05:00) "arbitrary value")
     ((2000-01-01 12:00:00.000000-05:00) "arbitrary value")
     ((2013-10-07 09:30:00.000000-04:00) "arbitrary value")) |}];
  test (module Time.Span.Hash_set) {| (0 1E-09 1E-06 0.001 1 60 3600 86400) |};
  [%expect {| (0s 1e-06ms 0.001ms 1ms 1s 1m 1h 1d) |}];
  test (module Of_string (Time.Span.Table)) {|
    ((0     "arbitrary value")
     (1E-09 "arbitrary value")
     (1E-06 "arbitrary value")
     (0.001 "arbitrary value")
     (1     "arbitrary value")
     (60    "arbitrary value")
     (3600  "arbitrary value")
     (86400 "arbitrary value")) |};
  [%expect {|
    ((0s      "arbitrary value")
     (1e-06ms "arbitrary value")
     (0.001ms "arbitrary value")
     (1ms     "arbitrary value")
     (1s      "arbitrary value")
     (1m      "arbitrary value")
     (1h      "arbitrary value")
     (1d      "arbitrary value")) |}];
  test (module Time.Ofday.Hash_set) {| (0 0.05 34200 43200) |};
  [%expect {| (00:00:00.000000 00:00:00.050000 09:30:00.000000 12:00:00.000000) |}];
  test (module Of_string (Time.Ofday.Table)) {|
    ((0     "arbitrary value")
     (0.05  "arbitrary value")
     (34200 "arbitrary value")
     (43200 "arbitrary value")) |};
  [%expect {|
    ((00:00:00.000000 "arbitrary value")
     (00:00:00.050000 "arbitrary value")
     (09:30:00.000000 "arbitrary value")
     (12:00:00.000000 "arbitrary value")) |}];
;;

let%test_module "Time.Stable.V1" =
  (module struct
    let%test_module "Time.Stable.V1 functor application" = (module Stable_unit_test.Make (struct
        include Time.Stable.V1

        let equal = Time.equal

        let zone = Zone.find_exn "America/New_York"
        let sexp_of_t t = sexp_of_t_abs ~zone t

        let tests =
          let time ~y ~m ~d ofday =
            of_date_ofday ~zone (Date.create_exn ~y ~m ~d) ofday
          in
          [ time ~y:2012 ~m:Month.Apr ~d:9 (Ofday.create ~hr:12 ()),
            "(2012-04-09 12:00:00.000000-04:00)",
            "\000\000\000\224\193\224\211\065";
            time ~y:1985 ~m:Month.Jun ~d:5 (Ofday.create ~hr:5 ~min:25 ()),
            "(1985-06-05 05:25:00.000000-04:00)",
            "\000\000\000\108\039\004\189\065";
          ] @ if Int.(Sys.c_int_size () < 64) then [] else [
            time ~y:2222 ~m:Month.Nov ~d:22 (Ofday.create ~hr:17 ~min:17 ~sec:17 ()),
            "(2222-11-22 17:17:17.000000-05:00)",
            "\000\000\208\230\204\186\253\065";
          ]
      end))

    (* test that t_of_sexp accepts sexps qualified with time zones in two formats *)
    let%test_unit _ =
      ignore (Time.Stable.V1.t_of_sexp (Sexp.of_string "(2012-04-09 12:00:00.000000-04:00:00)"))

    let%test_unit _ =
      ignore
        (Time.Stable.V1.t_of_sexp (Sexp.of_string "(2012-04-09 12:00:00.000000 America/New_York)"))
  end)

let%test_module "Time robustly compare" = (module struct
                                            let%test _ = of_sec_since_epoch 0.0 =. of_sec_since_epoch 0.000_000_99
                                            let%test _ = of_sec_since_epoch 0.0 <. of_sec_since_epoch 0.000_001_1

                                            let%test_unit _ =
                                              for i = 0 to 100 do
                                                let time = of_sec_since_epoch (Float.of_int i /. 17.) in
                                                assert ((=.) time (sexp_of_t time |> t_of_sexp))
                                              done
                                          end)

let%expect_test "in tests, [to_string] uses NYC's time zone" =
  printf "%s" (to_string epoch);
  [%expect {| 1969-12-31 19:00:00.000000-05:00 |}];
;;

let%expect_test "in tests, [sexp_of_t] uses NYC's time zone" =
  printf !"%{Sexp}" [%sexp (epoch : t)];
  [%expect {| (1969-12-31 19:00:00.000000-05:00) |}];
;;

module Ofday_zoned = struct
  open Time.Ofday.Zoned

  let (=) = [%compare.equal : With_nonchronological_compare.t]

  let%test_unit _ =
    List.iter
      [ "12:00 nyc";
        "12:00 America/New_York";
      ] ~f:(fun string ->
        let t = of_string string in
        assert (t = of_string (to_string t));
        assert (t = t_of_sexp (sexp_of_t t)))
  ;;
end

let%expect_test "our gmtime matches Unix.gmtime" =
  let unix_date_ofday (sec_since_epoch : float) =
    let parts  = Float.modf sec_since_epoch in
    let sec    = Float.Parts.integral parts in
    let subsec = Float.Parts.fractional parts in
    let sec, subsec =
      if Float.(<) subsec 0. then (sec -. 1., 1. +. subsec)
      else (sec, subsec)
    in
    let tm     = Unix.gmtime sec in
    let unix_date =
      Date.create_exn
        ~y:(tm.tm_year + 1900)
        ~m:(Month.of_int_exn (tm.tm_mon + 1))
        ~d:tm.tm_mday
    in
    let integral_ofday =
      ((tm.tm_hour * 60 * 60)
       + (tm.tm_min * 60)
       + tm.tm_sec)
      |> Float.of_int
    in
    let unix_ofday =
      (* put back the subseconds *)
      integral_ofday +. (Float.abs subsec)
      |> Time.Span.of_sec
      |> Time.Ofday.of_span_since_start_of_day
    in
    (unix_date, unix_ofday)
  in
  let generator =
    let open Quickcheck.Generator.Let_syntax in
    let one_hundred_years = 86_400 * 365 * 100 * 1_000 * 1_000 in
    let upper_bound       = one_hundred_years in
    let lower_bound       = Int.neg one_hundred_years in
    let%map mics          = Int.gen_incl lower_bound upper_bound in
    Float.of_int mics /. (1_000. *. 1_000.)
  in
  let gmtime time = Time.to_date_ofday ~zone:Time.Zone.utc time in
  Quickcheck.test generator
    ~sexp_of:[%sexp_of: float]
    ~trials:100_000
    ~examples:[ 0.; 100.; -100.; 86_400.; -86_400.; 90_000.; -90_000. ]
    ~f:(fun sec_since_epoch ->
      let time = Time.of_span_since_epoch (Time.Span.of_sec sec_since_epoch) in
      let (my_date, my_ofday) = gmtime time in
      let (unix_date, unix_ofday) = unix_date_ofday sec_since_epoch in
      let results = ((my_date, my_ofday), (unix_date, unix_ofday)) in
      if not (Tuple.T2.equal ~eq1:Date.equal ~eq2:Time.Ofday.equal
                (fst results) (snd results))
      then raise_s [%message "our gmtime doesn't match Unix.gmtime"
                               (sec_since_epoch: float)
                               (results : ((Date.t * Time.Ofday.t)
                                           * (Date.t * Time.Ofday.t)))])
;;

(* we expose the private type of Timish things to help the compiler optimize things
   like records of all floats.  This is not exactly an expect test in that we expect
   compilation to simply fail rather than a runtime test failure. *)
let%expect_test "time/span/ofday can be cast to their underlying type" =
  let _ = (Time.epoch :> float) in
  let _ = (Time.Span.zero :> float) in
  let _ = (Time.Ofday.start_of_day :> float) in
  ()
;;

let%expect_test "end-of-day constants" =
  let zones = List.map !Time.Zone.likely_machine_zones ~f:Time.Zone.find_exn in
  let test_round_trip zone date ofday ~expect =
    require_equal [%here] (module Date)
      (Time.of_date_ofday ~zone date ofday |> Time.to_date ~zone)
      expect
      ~message:(Time.Zone.name zone)
  in
  let test date_string =
    let date = Date.of_string date_string in
    List.iter zones ~f:(fun zone ->
      test_round_trip zone date Time.Ofday.approximate_end_of_day
        ~expect:date;
      test_round_trip zone date Time.Ofday.start_of_next_day
        ~expect:(Date.add_days date 1));
  in
  test "1970-01-01";
  test "2013-10-07";
  test "2099-12-31";
  test "2121-04-01";
  [%expect {||}];
;;

module Specialize_to_int (Poly : Stable1) = struct
  type t = int Poly.t [@@deriving bin_io, compare, sexp]
end

let%test_module "Time.Stable" =
  (module struct
    let zone_new_york = Zone.find_exn "America/New_York"

    let mk date ofday =
      of_date_ofday
        ~zone:zone_new_york
        (Date.of_string date)
        (Ofday.of_string ofday)

    let examples =
      [ Time.epoch
      ; mk "1999-12-31" "23:59:59"
      ; mk "2000-01-01" "00:00:00"
      ; mk "2013-10-07" "09:30:00"
      ; mk "2037-07-22" "14:23:37"
      ]

    let set_examples =
      [ Set.empty ]
      @
      List.map examples ~f:Set.singleton
      @
      [ Set.of_list examples ]

    let map_examples =
      [ Map.empty ]
      @
      List.mapi examples ~f:(fun i example ->
        Map.singleton example i)
      @
      [ Map.of_alist_exn (List.mapi examples ~f:(fun i example ->
          (example, i))) ]

    let%expect_test "V1" =
      print_and_check_stable_type [%here]
        (module Time.Stable.V1)
        examples;
      [%expect {|
        (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
        ((sexp (1969-12-31 19:00:00.000000-05:00))
         (bin_io "\000\000\000\000\000\000\000\000"))
        ((sexp (1999-12-31 23:59:59.000000-05:00))
         (bin_io "\000\000\128\231\1966\204A"))
        ((sexp (2000-01-01 00:00:00.000000-05:00))
         (bin_io "\000\000\000\232\1966\204A"))
        ((sexp (2013-10-07 09:30:00.000000-04:00))
         (bin_io "\000\000\000\214\173\148\212A"))
        ((sexp (2037-07-22 14:23:37.000000-04:00)) (bin_io "\000\000@j\141\196\223A")) |}];
    ;;

    let%expect_test "V1.Set" =
      print_and_check_stable_type [%here]
        (module Time.Stable.V1.Set)
        set_examples;
      [%expect {|
        (bin_shape_digest 4e7cbf6fe56bd628b963b7f8259e58bf)
        ((sexp ()) (bin_io "\000"))
        ((sexp ((1969-12-31 19:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\000\000\000\000\000"))
        ((sexp ((1999-12-31 23:59:59.000000-05:00)))
         (bin_io "\001\000\000\128\231\1966\204A"))
        ((sexp ((2000-01-01 00:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\232\1966\204A"))
        ((sexp ((2013-10-07 09:30:00.000000-04:00)))
         (bin_io "\001\000\000\000\214\173\148\212A"))
        ((sexp ((2037-07-22 14:23:37.000000-04:00)))
         (bin_io "\001\000\000@j\141\196\223A"))
        ((sexp (
           (1969-12-31 19:00:00.000000-05:00)
           (1999-12-31 23:59:59.000000-05:00)
           (2000-01-01 00:00:00.000000-05:00)
           (2013-10-07 09:30:00.000000-04:00)
           (2037-07-22 14:23:37.000000-04:00)))
         (bin_io
          "\005\000\000\000\000\000\000\000\000\000\000\128\231\1966\204A\000\000\000\232\1966\204A\000\000\000\214\173\148\212A\000\000@j\141\196\223A")) |}];
    ;;

    let%expect_test "V1.Map" =
      print_and_check_stable_type [%here]
        (module Specialize_to_int (Time.Stable.V1.Map))
        map_examples;
      [%expect {|
        (bin_shape_digest 31404094f08cdbe1f9fca07a1a1e5303)
        ((sexp ()) (bin_io "\000"))
        ((sexp (((1969-12-31 19:00:00.000000-05:00) 0)))
         (bin_io "\001\000\000\000\000\000\000\000\000\000"))
        ((sexp (((1999-12-31 23:59:59.000000-05:00) 1)))
         (bin_io "\001\000\000\128\231\1966\204A\001"))
        ((sexp (((2000-01-01 00:00:00.000000-05:00) 2)))
         (bin_io "\001\000\000\000\232\1966\204A\002"))
        ((sexp (((2013-10-07 09:30:00.000000-04:00) 3)))
         (bin_io "\001\000\000\000\214\173\148\212A\003"))
        ((sexp (((2037-07-22 14:23:37.000000-04:00) 4)))
         (bin_io "\001\000\000@j\141\196\223A\004"))
        ((sexp (
           ((1969-12-31 19:00:00.000000-05:00) 0)
           ((1999-12-31 23:59:59.000000-05:00) 1)
           ((2000-01-01 00:00:00.000000-05:00) 2)
           ((2013-10-07 09:30:00.000000-04:00) 3)
           ((2037-07-22 14:23:37.000000-04:00) 4)))
         (bin_io
          "\005\000\000\000\000\000\000\000\000\000\000\000\128\231\1966\204A\001\000\000\000\232\1966\204A\002\000\000\000\214\173\148\212A\003\000\000@j\141\196\223A\004")) |}];
    ;;

    (* [With_utc_sexp] *)

    let%expect_test "With_utc_sexp.V1" =
      print_and_check_stable_type [%here]
        (module Time.Stable.With_utc_sexp.V1)
        examples;
      [%expect {|
        (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
        ((sexp (1970-01-01 00:00:00.000000Z))
         (bin_io "\000\000\000\000\000\000\000\000"))
        ((sexp (2000-01-01 04:59:59.000000Z)) (bin_io "\000\000\128\231\1966\204A"))
        ((sexp (2000-01-01 05:00:00.000000Z)) (bin_io "\000\000\000\232\1966\204A"))
        ((sexp (2013-10-07 13:30:00.000000Z))
         (bin_io "\000\000\000\214\173\148\212A"))
        ((sexp (2037-07-22 18:23:37.000000Z)) (bin_io "\000\000@j\141\196\223A")) |}];
    ;;

    let%expect_test "With_utc_sexp.V1.Set" =
      print_and_check_stable_type [%here]
        (module Time.Stable.With_utc_sexp.V1.Set)
        set_examples;
      [%expect {|
        (bin_shape_digest 4e7cbf6fe56bd628b963b7f8259e58bf)
        ((sexp ()) (bin_io "\000"))
        ((sexp ((1970-01-01 00:00:00.000000Z)))
         (bin_io "\001\000\000\000\000\000\000\000\000"))
        ((sexp ((2000-01-01 04:59:59.000000Z)))
         (bin_io "\001\000\000\128\231\1966\204A"))
        ((sexp ((2000-01-01 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\232\1966\204A"))
        ((sexp ((2013-10-07 13:30:00.000000Z)))
         (bin_io "\001\000\000\000\214\173\148\212A"))
        ((sexp ((2037-07-22 18:23:37.000000Z)))
         (bin_io "\001\000\000@j\141\196\223A"))
        ((sexp (
           (1970-01-01 00:00:00.000000Z)
           (2000-01-01 04:59:59.000000Z)
           (2000-01-01 05:00:00.000000Z)
           (2013-10-07 13:30:00.000000Z)
           (2037-07-22 18:23:37.000000Z)))
         (bin_io
          "\005\000\000\000\000\000\000\000\000\000\000\128\231\1966\204A\000\000\000\232\1966\204A\000\000\000\214\173\148\212A\000\000@j\141\196\223A")) |}];
    ;;

    let%expect_test "With_utc_sexp.V1.Map" =
      print_and_check_stable_type [%here]
        (module Specialize_to_int (Time.Stable.With_utc_sexp.V1.Map))
        map_examples;
      [%expect {|
        (bin_shape_digest 31404094f08cdbe1f9fca07a1a1e5303)
        ((sexp ()) (bin_io "\000"))
        ((sexp (((1970-01-01 00:00:00.000000Z) 0)))
         (bin_io "\001\000\000\000\000\000\000\000\000\000"))
        ((sexp (((2000-01-01 04:59:59.000000Z) 1)))
         (bin_io "\001\000\000\128\231\1966\204A\001"))
        ((sexp (((2000-01-01 05:00:00.000000Z) 2)))
         (bin_io "\001\000\000\000\232\1966\204A\002"))
        ((sexp (((2013-10-07 13:30:00.000000Z) 3)))
         (bin_io "\001\000\000\000\214\173\148\212A\003"))
        ((sexp (((2037-07-22 18:23:37.000000Z) 4)))
         (bin_io "\001\000\000@j\141\196\223A\004"))
        ((sexp (
           ((1970-01-01 00:00:00.000000Z) 0)
           ((2000-01-01 04:59:59.000000Z) 1)
           ((2000-01-01 05:00:00.000000Z) 2)
           ((2013-10-07 13:30:00.000000Z) 3)
           ((2037-07-22 18:23:37.000000Z) 4)))
         (bin_io
          "\005\000\000\000\000\000\000\000\000\000\000\000\128\231\1966\204A\001\000\000\000\232\1966\204A\002\000\000\000\214\173\148\212A\003\000\000@j\141\196\223A\004")) |}];
    ;;

    let%expect_test "With_utc_sexp.V2" =
      print_and_check_stable_type [%here]
        (module Time.Stable.With_utc_sexp.V2)
        examples;
      [%expect {|
        (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
        ((sexp (1970-01-01 00:00:00.000000Z))
         (bin_io "\000\000\000\000\000\000\000\000"))
        ((sexp (2000-01-01 04:59:59.000000Z)) (bin_io "\000\000\128\231\1966\204A"))
        ((sexp (2000-01-01 05:00:00.000000Z)) (bin_io "\000\000\000\232\1966\204A"))
        ((sexp (2013-10-07 13:30:00.000000Z))
         (bin_io "\000\000\000\214\173\148\212A"))
        ((sexp (2037-07-22 18:23:37.000000Z)) (bin_io "\000\000@j\141\196\223A")) |}];
    ;;

    let%expect_test "With_utc_sexp.V2.Set" =
      print_and_check_stable_type [%here]
        (module Time.Stable.With_utc_sexp.V2.Set)
        set_examples;
      [%expect {|
        (bin_shape_digest 4e7cbf6fe56bd628b963b7f8259e58bf)
        ((sexp ()) (bin_io "\000"))
        ((sexp ((1970-01-01 00:00:00.000000Z)))
         (bin_io "\001\000\000\000\000\000\000\000\000"))
        ((sexp ((2000-01-01 04:59:59.000000Z)))
         (bin_io "\001\000\000\128\231\1966\204A"))
        ((sexp ((2000-01-01 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\232\1966\204A"))
        ((sexp ((2013-10-07 13:30:00.000000Z)))
         (bin_io "\001\000\000\000\214\173\148\212A"))
        ((sexp ((2037-07-22 18:23:37.000000Z)))
         (bin_io "\001\000\000@j\141\196\223A"))
        ((sexp (
           (1970-01-01 00:00:00.000000Z)
           (2000-01-01 04:59:59.000000Z)
           (2000-01-01 05:00:00.000000Z)
           (2013-10-07 13:30:00.000000Z)
           (2037-07-22 18:23:37.000000Z)))
         (bin_io
          "\005\000\000\000\000\000\000\000\000\000\000\128\231\1966\204A\000\000\000\232\1966\204A\000\000\000\214\173\148\212A\000\000@j\141\196\223A")) |}];
    ;;

    let%expect_test "With_utc_sexp.V2.Map" =
      print_and_check_stable_type [%here]
        (module Specialize_to_int (Time.Stable.With_utc_sexp.V2.Map))
        map_examples;
      [%expect {|
        (bin_shape_digest 31404094f08cdbe1f9fca07a1a1e5303)
        ((sexp ()) (bin_io "\000"))
        ((sexp (((1970-01-01 00:00:00.000000Z) 0)))
         (bin_io "\001\000\000\000\000\000\000\000\000\000"))
        ((sexp (((2000-01-01 04:59:59.000000Z) 1)))
         (bin_io "\001\000\000\128\231\1966\204A\001"))
        ((sexp (((2000-01-01 05:00:00.000000Z) 2)))
         (bin_io "\001\000\000\000\232\1966\204A\002"))
        ((sexp (((2013-10-07 13:30:00.000000Z) 3)))
         (bin_io "\001\000\000\000\214\173\148\212A\003"))
        ((sexp (((2037-07-22 18:23:37.000000Z) 4)))
         (bin_io "\001\000\000@j\141\196\223A\004"))
        ((sexp (
           ((1970-01-01 00:00:00.000000Z) 0)
           ((2000-01-01 04:59:59.000000Z) 1)
           ((2000-01-01 05:00:00.000000Z) 2)
           ((2013-10-07 13:30:00.000000Z) 3)
           ((2037-07-22 18:23:37.000000Z) 4)))
         (bin_io
          "\005\000\000\000\000\000\000\000\000\000\000\000\128\231\1966\204A\001\000\000\000\232\1966\204A\002\000\000\000\214\173\148\212A\003\000\000@j\141\196\223A\004")) |}];
    ;;

    (* [With_t_of_sexp_abs] *)

    let%expect_test "With_t_of_sexp_abs.V1" =
      print_and_check_stable_type [%here]
        (module Time.Stable.With_t_of_sexp_abs.V1)
        examples;
      [%expect {|
        (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
        ((sexp (1969-12-31 19:00:00.000000-05:00))
         (bin_io "\000\000\000\000\000\000\000\000"))
        ((sexp (1999-12-31 23:59:59.000000-05:00))
         (bin_io "\000\000\128\231\1966\204A"))
        ((sexp (2000-01-01 00:00:00.000000-05:00))
         (bin_io "\000\000\000\232\1966\204A"))
        ((sexp (2013-10-07 09:30:00.000000-04:00))
         (bin_io "\000\000\000\214\173\148\212A"))
        ((sexp (2037-07-22 14:23:37.000000-04:00)) (bin_io "\000\000@j\141\196\223A")) |}];
      show_raise (fun () ->
        Time.Stable.With_t_of_sexp_abs.V1.t_of_sexp
          (Sexp.of_string "(2000-01-01 00:00:00.000000)"));
      [%expect {|
        (raised (
          Sexplib.Conv.Of_sexp_error
          (Failure
           "Time.t_of_sexp: (time.ml.Make.Time_of_string \"2000-01-01 00:00:00.000000\"\n  (core_time.ml.Make.Time_string_not_absolute \"2000-01-01 00:00:00.000000\"))")
          (2000-01-01 00:00:00.000000))) |}];
    ;;
  end)

let%test_module "Time.Stable.Span" =
  (module struct
    let units =
      [ Span.nanosecond
      ; Span.microsecond
      ; Span.millisecond
      ; Span.second
      ; Span.minute
      ; Span.hour
      ; Span.day
      ]

    let examples =
      [ Span.zero ]
      @ units @
      [ List.sum (module Span) units ~f:ident ]

    let%expect_test "V1" =
      print_and_check_stable_type [%here]
        (* V1 round-trips imprecisely in some cases, so we document them and note that
           they are still reasonably close. *)
        ~cr:Comment
        (module Time.Stable.Span.V1)
        examples;
      [%expect {|
        (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
        ((sexp   0s)
         (bin_io "\000\000\000\000\000\000\000\000"))
        ((sexp   1e-06ms)
         (bin_io "\149\214&\232\011.\017>"))
        (* require-failed: lib/core/test/src/test_time.ml:LINE:COL. *)
        ("sexp serialization failed to round-trip"
          (original       1e-06ms)
          (sexp           1e-06ms)
          (sexp_roundtrip 1e-06ms))
        ((sexp   0.001ms)
         (bin_io "\141\237\181\160\247\198\176>"))
        ((sexp   1ms)
         (bin_io "\252\169\241\210MbP?"))
        ((sexp   1s)
         (bin_io "\000\000\000\000\000\000\240?"))
        ((sexp   1m)
         (bin_io "\000\000\000\000\000\000N@"))
        ((sexp   1h)
         (bin_io "\000\000\000\000\000 \172@"))
        ((sexp   1d)
         (bin_io "\000\000\000\000\000\024\245@"))
        ((sexp   1.04237d)
         (bin_io ")\160\025\004\208\252\245@"))
        (* require-failed: lib/core/test/src/test_time.ml:LINE:COL. *)
        ("sexp serialization failed to round-trip"
          (original       1.04237d)
          (sexp           1.04237d)
          (sexp_roundtrip 1.04237d)) |}];
    ;;

    let%expect_test "V2" =
      print_and_check_stable_type [%here]
        (module Time.Stable.Span.V2)
        examples;
      [%expect {|
        (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
        ((sexp   0s)
         (bin_io "\000\000\000\000\000\000\000\000"))
        ((sexp   1ns)
         (bin_io "\149\214&\232\011.\017>"))
        ((sexp   1us)
         (bin_io "\141\237\181\160\247\198\176>"))
        ((sexp   1ms)
         (bin_io "\252\169\241\210MbP?"))
        ((sexp   1s)
         (bin_io "\000\000\000\000\000\000\240?"))
        ((sexp   1m)
         (bin_io "\000\000\000\000\000\000N@"))
        ((sexp   1h)
         (bin_io "\000\000\000\000\000 \172@"))
        ((sexp   1d)
         (bin_io "\000\000\000\000\000\024\245@"))
        ((sexp   1.0423726967708449d)
         (bin_io ")\160\025\004\208\252\245@")) |}];
    ;;
  end)

let%test_module "Time.Stable.Ofday" =
  (module struct
    let examples =
      [ Ofday.start_of_day
      ; Ofday.create ~hr:12 ()
      ; Ofday.create ~hr:23 ~min:59 ~sec:29 ~ms:999 ~us:999 ()
      (* individual units *)
      ; Ofday.create ~us: 1 ()
      ; Ofday.create ~ms: 1 ()
      ; Ofday.create ~sec:1 ()
      ; Ofday.create ~min:1 ()
      ; Ofday.create ~hr: 1 ()
      ]

    let%expect_test "V1" =
      print_and_check_stable_type [%here]
        (* V1 round-trips imprecisely in some cases, so we document them and note that
           they are still reasonably close. *)
        ~cr:Comment
        (module Time.Stable.Ofday.V1)
        examples;
      [%expect {|
        (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
        ((sexp   00:00:00.000000)
         (bin_io "\000\000\000\000\000\000\000\000"))
        ((sexp   12:00:00.000000)
         (bin_io "\000\000\000\000\000\024\229@"))
        ((sexp   23:59:29.999999)
         (bin_io "\144\243\254\255\031\022\245@"))
        (* require-failed: lib/core/test/src/test_time.ml:LINE:COL. *)
        ("sexp serialization failed to round-trip"
          (original       23:59:29.999999)
          (sexp           23:59:29.999999)
          (sexp_roundtrip 23:59:29.999999))
        ((sexp   00:00:00.000001)
         (bin_io "\141\237\181\160\247\198\176>"))
        ((sexp   00:00:00.001000)
         (bin_io "\252\169\241\210MbP?"))
        ((sexp   00:00:01.000000)
         (bin_io "\000\000\000\000\000\000\240?"))
        ((sexp   00:01:00.000000)
         (bin_io "\000\000\000\000\000\000N@"))
        ((sexp   01:00:00.000000)
         (bin_io "\000\000\000\000\000 \172@")) |}];
    ;;

    let zoned_examples =
      let zone_new_york = Zone.find_exn "America/New_York" in
      List.map examples ~f:(fun example ->
        Ofday.Zoned.create example Zone.utc)
      @
      List.map examples ~f:(fun example ->
        Ofday.Zoned.create example zone_new_york)

    let%expect_test "Zoned.V1" =
      print_and_check_stable_type [%here]
        (* V1 round-trips imprecisely in some cases, so we document them and note that
           they are still reasonably close. *)
        ~cr:Comment
        (module Time.Stable.Ofday.Zoned.V1)
        zoned_examples;
      [%expect {|
        (bin_shape_digest 490573c3397b4fe37e8ade0086fb4759)
        ((sexp (00:00:00.000000 UTC))
         (bin_io "\000\000\000\000\000\000\000\000\003UTC"))
        ((sexp (12:00:00.000000 UTC)) (bin_io "\000\000\000\000\000\024\229@\003UTC"))
        ((sexp (23:59:29.999999 UTC)) (bin_io "\144\243\254\255\031\022\245@\003UTC"))
        (* require-failed: lib/core/test/src/test_time.ml:LINE:COL. *)
        ("sexp serialization failed to round-trip"
          (original       (23:59:29.999999 UTC))
          (sexp           (23:59:29.999999 UTC))
          (sexp_roundtrip (23:59:29.999999 UTC)))
        ((sexp (00:00:00.000001 UTC)) (bin_io "\141\237\181\160\247\198\176>\003UTC"))
        ((sexp (00:00:00.001000 UTC)) (bin_io "\252\169\241\210MbP?\003UTC"))
        ((sexp (00:00:01.000000 UTC)) (bin_io "\000\000\000\000\000\000\240?\003UTC"))
        ((sexp (00:01:00.000000 UTC)) (bin_io "\000\000\000\000\000\000N@\003UTC"))
        ((sexp (01:00:00.000000 UTC)) (bin_io "\000\000\000\000\000 \172@\003UTC"))
        ((sexp (00:00:00.000000 America/New_York))
         (bin_io "\000\000\000\000\000\000\000\000\016America/New_York"))
        ((sexp (12:00:00.000000 America/New_York))
         (bin_io "\000\000\000\000\000\024\229@\016America/New_York"))
        ((sexp (23:59:29.999999 America/New_York))
         (bin_io "\144\243\254\255\031\022\245@\016America/New_York"))
        (* require-failed: lib/core/test/src/test_time.ml:LINE:COL. *)
        ("sexp serialization failed to round-trip"
          (original       (23:59:29.999999 America/New_York))
          (sexp           (23:59:29.999999 America/New_York))
          (sexp_roundtrip (23:59:29.999999 America/New_York)))
        ((sexp (00:00:00.000001 America/New_York))
         (bin_io "\141\237\181\160\247\198\176>\016America/New_York"))
        ((sexp (00:00:00.001000 America/New_York))
         (bin_io "\252\169\241\210MbP?\016America/New_York"))
        ((sexp (00:00:01.000000 America/New_York))
         (bin_io "\000\000\000\000\000\000\240?\016America/New_York"))
        ((sexp (00:01:00.000000 America/New_York))
         (bin_io "\000\000\000\000\000\000N@\016America/New_York"))
        ((sexp (01:00:00.000000 America/New_York))
         (bin_io "\000\000\000\000\000 \172@\016America/New_York")) |}];
    ;;
  end)

let%test_module "Time.Stable.Zone" =
  (module struct
    let examples =
      [ Zone.utc
      ; Zone.find_exn "hkg"
      ; Zone.find_exn "ldn"
      ; Zone.find_exn "nyc"
      ]

    let%expect_test "V1" =
      print_and_check_stable_type [%here]
        (module Time.Stable.Zone.V1)
        examples;
      [%expect {|
        (bin_shape_digest d9a8da25d5656b016fb4dbdc2e4197fb)
        ((sexp   UTC)
         (bin_io "\003UTC"))
        ((sexp   Asia/Hong_Kong)
         (bin_io "\014Asia/Hong_Kong"))
        ((sexp   Europe/London)
         (bin_io "\rEurope/London"))
        ((sexp   America/New_York)
         (bin_io "\016America/New_York")) |}];
    ;;
  end)
