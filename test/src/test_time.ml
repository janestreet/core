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

let%test_module "Time robustly compare" =
  (module struct
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

    let date_examples =
      [ "1912-06-23" (* before epoch *)
      ; "1970-01-01" (* date of epoch *)
      ; "1999-12-31" (* right before 19xx->20xx millennium shift *)
      ; "2000-01-01" (* right after 19xx->20xx millennium shift *)
      ; "2013-10-07" (* date during JS's history *)
      ; "2222-11-22" (* far future date *)
      ]
      |> List.map ~f:Date.of_string

    let ofday_examples =
      (* significant day-boundary defined constants *)
      [ Ofday.start_of_day
      ; Ofday.approximate_end_of_day
      ; Ofday.start_of_next_day
      (* noon *)
      ; Ofday.create ~hr:12 ()
      (* single units, down to microseconds *)
      ; Ofday.create ~us: 1 ()
      ; Ofday.create ~ms: 1 ()
      ; Ofday.create ~sec:1 ()
      ; Ofday.create ~min:1 ()
      ; Ofday.create ~hr: 1 ()
      (* some value in each unit, down to microseconds *)
      ; Ofday.create ~hr:13 ~min:42 ~sec:23 ~ms:622 ~us:933 ()
      ]

    (* We construct and test lots of values because the stability of Time conversions is
       complex, and depends a lot on how comprehensive these tests are. *)
    let examples =
      let constructed_examples =
        List.concat_map date_examples ~f:(fun date ->
          List.map ofday_examples ~f:(fun ofday ->
            of_date_ofday ~zone:zone_new_york date ofday))
        |> List.dedup_and_sort ~compare:Time.compare
      in
      let pseudo_random_examples =
        let state = Random.State.make [| 1; 6; 1; 8; 0; 3; 3; 9; 8; 8; 7; 5 |] in
        List.init 10 ~f:(fun _ ->
          (* any digits further and the times fail to sexpify due to [gmtime] bounds *)
          Random.State.float_range state (-1e10) 1e10
          |> Span.of_sec
          |> of_span_since_epoch)
        |> List.dedup_and_sort ~compare:Time.compare
      in
      constructed_examples @ pseudo_random_examples

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

    module type S = sig
      type t [@@deriving bin_io, compare, sexp]
      val examples : t list
      val gen : t Quickcheck.Generator.t
    end

    let test_stability (module M : S) =
      require_does_not_raise [%here] (fun () ->
        (* For the pre-written examples, test round-tripping, and also print out the
           converted values so we will see if they change. *)
        print_and_check_stable_type [%here] (module M) M.examples;
        (* Test lots more pseudo-randomly generated examples for round-tripping. Do not
           print them out, as we don't want to read thousands of examples, so we won't
           know if their representation changes, but at least we will know they
           round-trip. *)
        quickcheck [%here] M.gen ~sexp_of:M.sexp_of_t ~f:(fun example ->
          require_does_not_raise [%here] (fun () ->
            let sexp            = M.sexp_of_t example in
            let sexp_round_trip = M.t_of_sexp sexp    in
            require_compare_equal [%here] (module M) example sexp_round_trip;
            let string           = Binable.to_string (module M) example in
            let binio_round_trip = Binable.of_string (module M) string  in
            require_compare_equal [%here] (module M) example binio_round_trip;
          )))
    ;;

    module For_time = struct
      let compare = Time.robustly_compare
      let examples = examples

      let gen =
        (* If we add another digit, the times start to exceed the range of [gmtime] and
           sexp conversion raises. *)
        Int64.gen_uniform_incl
          (-10_000_000_000_000_000L)
          (+10_000_000_000_000_000L)
        (* We generate in units of microseconds because our current sexp representation is
           no more precise than that. *)
        |> Quickcheck.Generator.map ~f:(fun int64 ->
          Time.of_span_since_epoch (Span.of_us (Int64.to_float int64)))
    end

    module For_set = struct
      let compare x y =
        List.compare For_time.compare
          (Set.to_list x)
          (Set.to_list y)

      let examples = set_examples

      let gen =
        Quickcheck.Generator.map (List.gen For_time.gen) ~f:Time.Set.of_list
    end

    module For_map = struct
      let compare x y =
        List.compare (Tuple2.compare ~cmp1:For_time.compare ~cmp2:Int.compare)
          (Map.to_alist x)
          (Map.to_alist y)

      let examples = map_examples

      let gen =
        List.gen (Quickcheck.Generator.tuple2 For_time.gen Int.gen)
        |> Quickcheck.Generator.filter_map ~f:(fun alist ->
          match Time.Map.of_alist alist with
          | `Ok map          -> Some map
          | `Duplicate_key _ -> None)
    end

    let%expect_test "V1" =
      test_stability (module struct
        include Time.Stable.V1
        include For_time
      end);
      [%expect {|
        (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
        ((sexp (1912-06-23 00:00:00.000000-05:00))
         (bin_io "\000\000\000\140\241\012\219\193"))
        ((sexp (1912-06-23 00:00:00.000001-05:00))
         (bin_io "\252\255\255\139\241\012\219\193"))
        ((sexp (1912-06-23 00:00:00.001000-05:00))
         (bin_io "\158\239\255\139\241\012\219\193"))
        ((sexp (1912-06-23 00:00:01.000000-05:00))
         (bin_io "\000\000\192\139\241\012\219\193"))
        ((sexp (1912-06-23 00:01:00.000000-05:00))
         (bin_io "\000\000\000}\241\012\219\193"))
        ((sexp (1912-06-23 01:00:00.000000-05:00))
         (bin_io "\000\000\000\b\238\012\219\193"))
        ((sexp (1912-06-23 12:00:00.000000-05:00))
         (bin_io "\000\000\000\\\199\012\219\193"))
        ((sexp (1912-06-23 13:42:23.622933-05:00))
         (bin_io "\222!\024\\\193\012\219\193"))
        ((sexp (1912-06-23 23:59:59.999999-05:00))
         (bin_io "\004\000\000,\157\012\219\193"))
        ((sexp (1912-06-24 00:00:00.000000-05:00))
         (bin_io "\000\000\000,\157\012\219\193"))
        ((sexp (1970-01-01 00:00:00.000000-05:00))
         (bin_io "\000\000\000\000\000\148\209@"))
        ((sexp (1970-01-01 00:00:00.000001-05:00))
         (bin_io "\1901\004\000\000\148\209@"))
        ((sexp (1970-01-01 00:00:00.001000-05:00)) (bin_io "\211Mb\016\000\148\209@"))
        ((sexp (1970-01-01 00:00:01.000000-05:00))
         (bin_io "\000\000\000\000@\148\209@"))
        ((sexp (1970-01-01 00:01:00.000000-05:00))
         (bin_io "\000\000\000\000\000\163\209@"))
        ((sexp (1970-01-01 01:00:00.000000-05:00))
         (bin_io "\000\000\000\000\000\024\213@"))
        ((sexp (1970-01-01 12:00:00.000000-05:00))
         (bin_io "\000\000\000\000\000\226\237@"))
        ((sexp (1970-01-01 13:42:23.622933-05:00))
         (bin_io "\234\151\136\247\249p\240@"))
        ((sexp (1970-01-01 23:59:59.999999-05:00))
         (bin_io "\145\243\254\255\255|\249@"))
        ((sexp (1970-01-02 00:00:00.000000-05:00))
         (bin_io "\000\000\000\000\000}\249@"))
        ((sexp (1999-12-31 00:00:00.000000-05:00)) (bin_io "\000\000\000(\0286\204A"))
        ((sexp (1999-12-31 00:00:00.000001-05:00)) (bin_io "\b\000\000(\0286\204A"))
        ((sexp (1999-12-31 00:00:00.001000-05:00)) (bin_io "\197 \000(\0286\204A"))
        ((sexp (1999-12-31 00:00:01.000000-05:00)) (bin_io "\000\000\128(\0286\204A"))
        ((sexp (1999-12-31 00:01:00.000000-05:00)) (bin_io "\000\000\000F\0286\204A"))
        ((sexp (1999-12-31 01:00:00.000000-05:00)) (bin_io "\000\000\0000#6\204A"))
        ((sexp (1999-12-31 12:00:00.000000-05:00)) (bin_io "\000\000\000\136p6\204A"))
        ((sexp (1999-12-31 13:42:23.622933-05:00)) (bin_io "E\188\207\135|6\204A"))
        ((sexp (1999-12-31 23:59:59.999999-05:00))
         (bin_io "\248\255\255\231\1966\204A"))
        ((sexp (2000-01-01 00:00:00.000000-05:00))
         (bin_io "\000\000\000\232\1966\204A"))
        ((sexp (2000-01-01 00:00:00.000001-05:00))
         (bin_io "\b\000\000\232\1966\204A"))
        ((sexp (2000-01-01 00:00:00.001000-05:00)) (bin_io "\197 \000\232\1966\204A"))
        ((sexp (2000-01-01 00:00:01.000000-05:00))
         (bin_io "\000\000\128\232\1966\204A"))
        ((sexp (2000-01-01 00:01:00.000000-05:00))
         (bin_io "\000\000\000\006\1976\204A"))
        ((sexp (2000-01-01 01:00:00.000000-05:00))
         (bin_io "\000\000\000\240\2036\204A"))
        ((sexp (2000-01-01 12:00:00.000000-05:00)) (bin_io "\000\000\000H\0257\204A"))
        ((sexp (2000-01-01 13:42:23.622933-05:00)) (bin_io "E\188\207G%7\204A"))
        ((sexp (2000-01-01 23:59:59.999999-05:00)) (bin_io "\248\255\255\167m7\204A"))
        ((sexp (2000-01-02 00:00:00.000000-05:00)) (bin_io "\000\000\000\168m7\204A"))
        ((sexp (2013-10-07 00:00:00.000000-04:00))
         (bin_io "\000\000\000p\140\148\212A"))
        ((sexp (2013-10-07 00:00:00.000001-04:00))
         (bin_io "\004\000\000p\140\148\212A"))
        ((sexp (2013-10-07 00:00:00.001000-04:00)) (bin_io "b\016\000p\140\148\212A"))
        ((sexp (2013-10-07 00:00:01.000000-04:00)) (bin_io "\000\000@p\140\148\212A"))
        ((sexp (2013-10-07 00:01:00.000000-04:00))
         (bin_io "\000\000\000\127\140\148\212A"))
        ((sexp (2013-10-07 01:00:00.000000-04:00))
         (bin_io "\000\000\000\244\143\148\212A"))
        ((sexp (2013-10-07 12:00:00.000000-04:00))
         (bin_io "\000\000\000\160\182\148\212A"))
        ((sexp (2013-10-07 13:42:23.622933-04:00))
         (bin_io "\"\222\231\159\188\148\212A"))
        ((sexp (2013-10-07 23:59:59.999999-04:00))
         (bin_io "\252\255\255\207\224\148\212A"))
        ((sexp (2013-10-08 00:00:00.000000-04:00))
         (bin_io "\000\000\000\208\224\148\212A"))
        ((sexp (2222-11-22 00:00:00.000000-05:00))
         (bin_io "\000\000\000\181\189\186\253A"))
        ((sexp (2222-11-22 00:00:00.000001-05:00))
         (bin_io "\001\000\000\181\189\186\253A"))
        ((sexp (2222-11-22 00:00:00.001000-05:00))
         (bin_io "\025\004\000\181\189\186\253A"))
        ((sexp (2222-11-22 00:00:01.000000-05:00))
         (bin_io "\000\000\016\181\189\186\253A"))
        ((sexp (2222-11-22 00:01:00.000000-05:00))
         (bin_io "\000\000\192\184\189\186\253A"))
        ((sexp (2222-11-22 01:00:00.000000-05:00))
         (bin_io "\000\000\000\150\190\186\253A"))
        ((sexp (2222-11-22 12:00:00.000000-05:00))
         (bin_io "\000\000\000A\200\186\253A"))
        ((sexp (2222-11-22 13:42:23.622933-05:00))
         (bin_io "\137\247\249\192\201\186\253A"))
        ((sexp (2222-11-22 23:59:59.999999-05:00))
         (bin_io "\255\255\255\204\210\186\253A"))
        ((sexp (2222-11-23 00:00:00.000000-05:00))
         (bin_io "\000\000\000\205\210\186\253A"))
        ((sexp (1665-09-11 05:34:21.163040-04:56:02))
         (bin_io "\024\178\006\206\003\227\001\194"))
        ((sexp (1749-05-17 01:44:58.057413-04:56:02))
         (bin_io "\214\020?\176\186\239\249\193"))
        ((sexp (1799-10-01 16:35:57.622579-04:56:02))
         (bin_io "\235\t\006\187\165\003\244\193"))
        ((sexp (1833-12-02 03:31:51.104713-04:56:02))
         (bin_io "0\166\220n\190\254\239\193"))
        ((sexp (1876-08-10 08:56:53.681209-04:56:02))
         (bin_io "\1383\ni9\245\229\193"))
        ((sexp (1876-08-29 21:01:31.672122-04:56:02))
         (bin_io "\250}J\158\002\242\229\193"))
        ((sexp (1896-03-25 09:54:28.564831-05:00)) (bin_io "\232\236m\203!X\225\193"))
        ((sexp (1952-04-04 18:31:13.284826-05:00))
         (bin_io "\208\138[_\127\175\192\193"))
        ((sexp (2045-05-01 15:40:18.240950-05:00)) (bin_io "\220\181G6O\182\225A"))
        ((sexp (2157-05-06 22:33:36.270771-05:00)) (bin_io "\020U\004)9\006\246A")) |}];
      (* test that t_of_sexp accepts sexps qualified with time zones in two formats *)
      let test string =
        require_does_not_raise [%here] (fun () ->
          print_s
            [%sexp (Time.Stable.V1.t_of_sexp (Sexp.of_string string) : Time.Stable.V1.t)])
      in
      test "(2012-04-09 12:00:00.000000-04:00:00)";
      test "(2012-04-09 12:00:00.000000 America/New_York)";
      [%expect {|
        (2012-04-09 12:00:00.000000-04:00)
        (2012-04-09 12:00:00.000000-04:00) |}];
      (* test that t_of_sexp accepts leap seconds
         NB. there are such things as non-integer hour offset timezones, so we need to be
         able to accept leap seconds even when the minutes aren't zero. This obviously
         isn't such a timezone, but the output sexp will be in UTC-4, so the input sexp
         should be so as well so that the result is obviously as intended.
      *)
      test "(2012-04-09 05:14:60.000000-04:00)";
      test "(2012-04-09 05:14:60.123456-04:00)";
      [%expect {|
        (2012-04-09 05:15:00.000000-04:00)
        (2012-04-09 05:15:00.000000-04:00) |}]
    ;;

    let%expect_test "V1.Set" =
      test_stability (module struct
        include Time.Stable.V1.Set
        include For_set
      end);
      [%expect {|
        (bin_shape_digest 4e7cbf6fe56bd628b963b7f8259e58bf)
        ((sexp ()) (bin_io "\000"))
        ((sexp ((1912-06-23 00:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\140\241\012\219\193"))
        ((sexp ((1912-06-23 00:00:00.000001-05:00)))
         (bin_io "\001\252\255\255\139\241\012\219\193"))
        ((sexp ((1912-06-23 00:00:00.001000-05:00)))
         (bin_io "\001\158\239\255\139\241\012\219\193"))
        ((sexp ((1912-06-23 00:00:01.000000-05:00)))
         (bin_io "\001\000\000\192\139\241\012\219\193"))
        ((sexp ((1912-06-23 00:01:00.000000-05:00)))
         (bin_io "\001\000\000\000}\241\012\219\193"))
        ((sexp ((1912-06-23 01:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\b\238\012\219\193"))
        ((sexp ((1912-06-23 12:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\\\199\012\219\193"))
        ((sexp ((1912-06-23 13:42:23.622933-05:00)))
         (bin_io "\001\222!\024\\\193\012\219\193"))
        ((sexp ((1912-06-23 23:59:59.999999-05:00)))
         (bin_io "\001\004\000\000,\157\012\219\193"))
        ((sexp ((1912-06-24 00:00:00.000000-05:00)))
         (bin_io "\001\000\000\000,\157\012\219\193"))
        ((sexp ((1970-01-01 00:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\000\000\148\209@"))
        ((sexp ((1970-01-01 00:00:00.000001-05:00)))
         (bin_io "\001\1901\004\000\000\148\209@"))
        ((sexp ((1970-01-01 00:00:00.001000-05:00)))
         (bin_io "\001\211Mb\016\000\148\209@"))
        ((sexp ((1970-01-01 00:00:01.000000-05:00)))
         (bin_io "\001\000\000\000\000@\148\209@"))
        ((sexp ((1970-01-01 00:01:00.000000-05:00)))
         (bin_io "\001\000\000\000\000\000\163\209@"))
        ((sexp ((1970-01-01 01:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\000\000\024\213@"))
        ((sexp ((1970-01-01 12:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\000\000\226\237@"))
        ((sexp ((1970-01-01 13:42:23.622933-05:00)))
         (bin_io "\001\234\151\136\247\249p\240@"))
        ((sexp ((1970-01-01 23:59:59.999999-05:00)))
         (bin_io "\001\145\243\254\255\255|\249@"))
        ((sexp ((1970-01-02 00:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\000\000}\249@"))
        ((sexp ((1999-12-31 00:00:00.000000-05:00)))
         (bin_io "\001\000\000\000(\0286\204A"))
        ((sexp ((1999-12-31 00:00:00.000001-05:00)))
         (bin_io "\001\b\000\000(\0286\204A"))
        ((sexp ((1999-12-31 00:00:00.001000-05:00)))
         (bin_io "\001\197 \000(\0286\204A"))
        ((sexp ((1999-12-31 00:00:01.000000-05:00)))
         (bin_io "\001\000\000\128(\0286\204A"))
        ((sexp ((1999-12-31 00:01:00.000000-05:00)))
         (bin_io "\001\000\000\000F\0286\204A"))
        ((sexp ((1999-12-31 01:00:00.000000-05:00)))
         (bin_io "\001\000\000\0000#6\204A"))
        ((sexp ((1999-12-31 12:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\136p6\204A"))
        ((sexp ((1999-12-31 13:42:23.622933-05:00)))
         (bin_io "\001E\188\207\135|6\204A"))
        ((sexp ((1999-12-31 23:59:59.999999-05:00)))
         (bin_io "\001\248\255\255\231\1966\204A"))
        ((sexp ((2000-01-01 00:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\232\1966\204A"))
        ((sexp ((2000-01-01 00:00:00.000001-05:00)))
         (bin_io "\001\b\000\000\232\1966\204A"))
        ((sexp ((2000-01-01 00:00:00.001000-05:00)))
         (bin_io "\001\197 \000\232\1966\204A"))
        ((sexp ((2000-01-01 00:00:01.000000-05:00)))
         (bin_io "\001\000\000\128\232\1966\204A"))
        ((sexp ((2000-01-01 00:01:00.000000-05:00)))
         (bin_io "\001\000\000\000\006\1976\204A"))
        ((sexp ((2000-01-01 01:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\240\2036\204A"))
        ((sexp ((2000-01-01 12:00:00.000000-05:00)))
         (bin_io "\001\000\000\000H\0257\204A"))
        ((sexp ((2000-01-01 13:42:23.622933-05:00))) (bin_io "\001E\188\207G%7\204A"))
        ((sexp ((2000-01-01 23:59:59.999999-05:00)))
         (bin_io "\001\248\255\255\167m7\204A"))
        ((sexp ((2000-01-02 00:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\168m7\204A"))
        ((sexp ((2013-10-07 00:00:00.000000-04:00)))
         (bin_io "\001\000\000\000p\140\148\212A"))
        ((sexp ((2013-10-07 00:00:00.000001-04:00)))
         (bin_io "\001\004\000\000p\140\148\212A"))
        ((sexp ((2013-10-07 00:00:00.001000-04:00)))
         (bin_io "\001b\016\000p\140\148\212A"))
        ((sexp ((2013-10-07 00:00:01.000000-04:00)))
         (bin_io "\001\000\000@p\140\148\212A"))
        ((sexp ((2013-10-07 00:01:00.000000-04:00)))
         (bin_io "\001\000\000\000\127\140\148\212A"))
        ((sexp ((2013-10-07 01:00:00.000000-04:00)))
         (bin_io "\001\000\000\000\244\143\148\212A"))
        ((sexp ((2013-10-07 12:00:00.000000-04:00)))
         (bin_io "\001\000\000\000\160\182\148\212A"))
        ((sexp ((2013-10-07 13:42:23.622933-04:00)))
         (bin_io "\001\"\222\231\159\188\148\212A"))
        ((sexp ((2013-10-07 23:59:59.999999-04:00)))
         (bin_io "\001\252\255\255\207\224\148\212A"))
        ((sexp ((2013-10-08 00:00:00.000000-04:00)))
         (bin_io "\001\000\000\000\208\224\148\212A"))
        ((sexp ((2222-11-22 00:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\181\189\186\253A"))
        ((sexp ((2222-11-22 00:00:00.000001-05:00)))
         (bin_io "\001\001\000\000\181\189\186\253A"))
        ((sexp ((2222-11-22 00:00:00.001000-05:00)))
         (bin_io "\001\025\004\000\181\189\186\253A"))
        ((sexp ((2222-11-22 00:00:01.000000-05:00)))
         (bin_io "\001\000\000\016\181\189\186\253A"))
        ((sexp ((2222-11-22 00:01:00.000000-05:00)))
         (bin_io "\001\000\000\192\184\189\186\253A"))
        ((sexp ((2222-11-22 01:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\150\190\186\253A"))
        ((sexp ((2222-11-22 12:00:00.000000-05:00)))
         (bin_io "\001\000\000\000A\200\186\253A"))
        ((sexp ((2222-11-22 13:42:23.622933-05:00)))
         (bin_io "\001\137\247\249\192\201\186\253A"))
        ((sexp ((2222-11-22 23:59:59.999999-05:00)))
         (bin_io "\001\255\255\255\204\210\186\253A"))
        ((sexp ((2222-11-23 00:00:00.000000-05:00)))
         (bin_io "\001\000\000\000\205\210\186\253A"))
        ((sexp ((1665-09-11 05:34:21.163040-04:56:02)))
         (bin_io "\001\024\178\006\206\003\227\001\194"))
        ((sexp ((1749-05-17 01:44:58.057413-04:56:02)))
         (bin_io "\001\214\020?\176\186\239\249\193"))
        ((sexp ((1799-10-01 16:35:57.622579-04:56:02)))
         (bin_io "\001\235\t\006\187\165\003\244\193"))
        ((sexp ((1833-12-02 03:31:51.104713-04:56:02)))
         (bin_io "\0010\166\220n\190\254\239\193"))
        ((sexp ((1876-08-10 08:56:53.681209-04:56:02)))
         (bin_io "\001\1383\ni9\245\229\193"))
        ((sexp ((1876-08-29 21:01:31.672122-04:56:02)))
         (bin_io "\001\250}J\158\002\242\229\193"))
        ((sexp ((1896-03-25 09:54:28.564831-05:00)))
         (bin_io "\001\232\236m\203!X\225\193"))
        ((sexp ((1952-04-04 18:31:13.284826-05:00)))
         (bin_io "\001\208\138[_\127\175\192\193"))
        ((sexp ((2045-05-01 15:40:18.240950-05:00)))
         (bin_io "\001\220\181G6O\182\225A"))
        ((sexp ((2157-05-06 22:33:36.270771-05:00)))
         (bin_io "\001\020U\004)9\006\246A"))
        ((sexp (
           (1665-09-11 05:34:21.163040-04:56:02)
           (1749-05-17 01:44:58.057413-04:56:02)
           (1799-10-01 16:35:57.622579-04:56:02)
           (1833-12-02 03:31:51.104713-04:56:02)
           (1876-08-10 08:56:53.681209-04:56:02)
           (1876-08-29 21:01:31.672122-04:56:02)
           (1896-03-25 09:54:28.564831-05:00)
           (1912-06-23 00:00:00.000000-05:00)
           (1912-06-23 00:00:00.000001-05:00)
           (1912-06-23 00:00:00.001000-05:00)
           (1912-06-23 00:00:01.000000-05:00)
           (1912-06-23 00:01:00.000000-05:00)
           (1912-06-23 01:00:00.000000-05:00)
           (1912-06-23 12:00:00.000000-05:00)
           (1912-06-23 13:42:23.622933-05:00)
           (1912-06-23 23:59:59.999999-05:00)
           (1912-06-24 00:00:00.000000-05:00)
           (1952-04-04 18:31:13.284826-05:00)
           (1970-01-01 00:00:00.000000-05:00)
           (1970-01-01 00:00:00.000001-05:00)
           (1970-01-01 00:00:00.001000-05:00)
           (1970-01-01 00:00:01.000000-05:00)
           (1970-01-01 00:01:00.000000-05:00)
           (1970-01-01 01:00:00.000000-05:00)
           (1970-01-01 12:00:00.000000-05:00)
           (1970-01-01 13:42:23.622933-05:00)
           (1970-01-01 23:59:59.999999-05:00)
           (1970-01-02 00:00:00.000000-05:00)
           (1999-12-31 00:00:00.000000-05:00)
           (1999-12-31 00:00:00.000001-05:00)
           (1999-12-31 00:00:00.001000-05:00)
           (1999-12-31 00:00:01.000000-05:00)
           (1999-12-31 00:01:00.000000-05:00)
           (1999-12-31 01:00:00.000000-05:00)
           (1999-12-31 12:00:00.000000-05:00)
           (1999-12-31 13:42:23.622933-05:00)
           (1999-12-31 23:59:59.999999-05:00)
           (2000-01-01 00:00:00.000000-05:00)
           (2000-01-01 00:00:00.000001-05:00)
           (2000-01-01 00:00:00.001000-05:00)
           (2000-01-01 00:00:01.000000-05:00)
           (2000-01-01 00:01:00.000000-05:00)
           (2000-01-01 01:00:00.000000-05:00)
           (2000-01-01 12:00:00.000000-05:00)
           (2000-01-01 13:42:23.622933-05:00)
           (2000-01-01 23:59:59.999999-05:00)
           (2000-01-02 00:00:00.000000-05:00)
           (2013-10-07 00:00:00.000000-04:00)
           (2013-10-07 00:00:00.000001-04:00)
           (2013-10-07 00:00:00.001000-04:00)
           (2013-10-07 00:00:01.000000-04:00)
           (2013-10-07 00:01:00.000000-04:00)
           (2013-10-07 01:00:00.000000-04:00)
           (2013-10-07 12:00:00.000000-04:00)
           (2013-10-07 13:42:23.622933-04:00)
           (2013-10-07 23:59:59.999999-04:00)
           (2013-10-08 00:00:00.000000-04:00)
           (2045-05-01 15:40:18.240950-05:00)
           (2157-05-06 22:33:36.270771-05:00)
           (2222-11-22 00:00:00.000000-05:00)
           (2222-11-22 00:00:00.000001-05:00)
           (2222-11-22 00:00:00.001000-05:00)
           (2222-11-22 00:00:01.000000-05:00)
           (2222-11-22 00:01:00.000000-05:00)
           (2222-11-22 01:00:00.000000-05:00)
           (2222-11-22 12:00:00.000000-05:00)
           (2222-11-22 13:42:23.622933-05:00)
           (2222-11-22 23:59:59.999999-05:00)
           (2222-11-23 00:00:00.000000-05:00)))
         (bin_io
          "E\024\178\006\206\003\227\001\194\214\020?\176\186\239\249\193\235\t\006\187\165\003\244\1930\166\220n\190\254\239\193\1383\ni9\245\229\193\250}J\158\002\242\229\193\232\236m\203!X\225\193\000\000\000\140\241\012\219\193\252\255\255\139\241\012\219\193\158\239\255\139\241\012\219\193\000\000\192\139\241\012\219\193\000\000\000}\241\012\219\193\000\000\000\b\238\012\219\193\000\000\000\\\199\012\219\193\222!\024\\\193\012\219\193\004\000\000,\157\012\219\193\000\000\000,\157\012\219\193\208\138[_\127\175\192\193\000\000\000\000\000\148\209@\1901\004\000\000\148\209@\211Mb\016\000\148\209@\000\000\000\000@\148\209@\000\000\000\000\000\163\209@\000\000\000\000\000\024\213@\000\000\000\000\000\226\237@\234\151\136\247\249p\240@\145\243\254\255\255|\249@\000\000\000\000\000}\249@\000\000\000(\0286\204A\b\000\000(\0286\204A\197 \000(\0286\204A\000\000\128(\0286\204A\000\000\000F\0286\204A\000\000\0000#6\204A\000\000\000\136p6\204AE\188\207\135|6\204A\248\255\255\231\1966\204A\000\000\000\232\1966\204A\b\000\000\232\1966\204A\197 \000\232\1966\204A\000\000\128\232\1966\204A\000\000\000\006\1976\204A\000\000\000\240\2036\204A\000\000\000H\0257\204AE\188\207G%7\204A\248\255\255\167m7\204A\000\000\000\168m7\204A\000\000\000p\140\148\212A\004\000\000p\140\148\212Ab\016\000p\140\148\212A\000\000@p\140\148\212A\000\000\000\127\140\148\212A\000\000\000\244\143\148\212A\000\000\000\160\182\148\212A\"\222\231\159\188\148\212A\252\255\255\207\224\148\212A\000\000\000\208\224\148\212A\220\181G6O\182\225A\020U\004)9\006\246A\000\000\000\181\189\186\253A\001\000\000\181\189\186\253A\025\004\000\181\189\186\253A\000\000\016\181\189\186\253A\000\000\192\184\189\186\253A\000\000\000\150\190\186\253A\000\000\000A\200\186\253A\137\247\249\192\201\186\253A\255\255\255\204\210\186\253A\000\000\000\205\210\186\253A")) |}];
    ;;

    let%expect_test "V1.Map" =
      test_stability (module struct
        include Specialize_to_int (Time.Stable.V1.Map)
        include For_map
      end);
      [%expect {|
        (bin_shape_digest 31404094f08cdbe1f9fca07a1a1e5303)
        ((sexp ()) (bin_io "\000"))
        ((sexp (((1912-06-23 00:00:00.000000-05:00) 0)))
         (bin_io "\001\000\000\000\140\241\012\219\193\000"))
        ((sexp (((1912-06-23 00:00:00.000001-05:00) 1)))
         (bin_io "\001\252\255\255\139\241\012\219\193\001"))
        ((sexp (((1912-06-23 00:00:00.001000-05:00) 2)))
         (bin_io "\001\158\239\255\139\241\012\219\193\002"))
        ((sexp (((1912-06-23 00:00:01.000000-05:00) 3)))
         (bin_io "\001\000\000\192\139\241\012\219\193\003"))
        ((sexp (((1912-06-23 00:01:00.000000-05:00) 4)))
         (bin_io "\001\000\000\000}\241\012\219\193\004"))
        ((sexp (((1912-06-23 01:00:00.000000-05:00) 5)))
         (bin_io "\001\000\000\000\b\238\012\219\193\005"))
        ((sexp (((1912-06-23 12:00:00.000000-05:00) 6)))
         (bin_io "\001\000\000\000\\\199\012\219\193\006"))
        ((sexp (((1912-06-23 13:42:23.622933-05:00) 7)))
         (bin_io "\001\222!\024\\\193\012\219\193\007"))
        ((sexp (((1912-06-23 23:59:59.999999-05:00) 8)))
         (bin_io "\001\004\000\000,\157\012\219\193\b"))
        ((sexp (((1912-06-24 00:00:00.000000-05:00) 9)))
         (bin_io "\001\000\000\000,\157\012\219\193\t"))
        ((sexp (((1970-01-01 00:00:00.000000-05:00) 10)))
         (bin_io "\001\000\000\000\000\000\148\209@\n"))
        ((sexp (((1970-01-01 00:00:00.000001-05:00) 11)))
         (bin_io "\001\1901\004\000\000\148\209@\011"))
        ((sexp (((1970-01-01 00:00:00.001000-05:00) 12)))
         (bin_io "\001\211Mb\016\000\148\209@\012"))
        ((sexp (((1970-01-01 00:00:01.000000-05:00) 13)))
         (bin_io "\001\000\000\000\000@\148\209@\r"))
        ((sexp (((1970-01-01 00:01:00.000000-05:00) 14)))
         (bin_io "\001\000\000\000\000\000\163\209@\014"))
        ((sexp (((1970-01-01 01:00:00.000000-05:00) 15)))
         (bin_io "\001\000\000\000\000\000\024\213@\015"))
        ((sexp (((1970-01-01 12:00:00.000000-05:00) 16)))
         (bin_io "\001\000\000\000\000\000\226\237@\016"))
        ((sexp (((1970-01-01 13:42:23.622933-05:00) 17)))
         (bin_io "\001\234\151\136\247\249p\240@\017"))
        ((sexp (((1970-01-01 23:59:59.999999-05:00) 18)))
         (bin_io "\001\145\243\254\255\255|\249@\018"))
        ((sexp (((1970-01-02 00:00:00.000000-05:00) 19)))
         (bin_io "\001\000\000\000\000\000}\249@\019"))
        ((sexp (((1999-12-31 00:00:00.000000-05:00) 20)))
         (bin_io "\001\000\000\000(\0286\204A\020"))
        ((sexp (((1999-12-31 00:00:00.000001-05:00) 21)))
         (bin_io "\001\b\000\000(\0286\204A\021"))
        ((sexp (((1999-12-31 00:00:00.001000-05:00) 22)))
         (bin_io "\001\197 \000(\0286\204A\022"))
        ((sexp (((1999-12-31 00:00:01.000000-05:00) 23)))
         (bin_io "\001\000\000\128(\0286\204A\023"))
        ((sexp (((1999-12-31 00:01:00.000000-05:00) 24)))
         (bin_io "\001\000\000\000F\0286\204A\024"))
        ((sexp (((1999-12-31 01:00:00.000000-05:00) 25)))
         (bin_io "\001\000\000\0000#6\204A\025"))
        ((sexp (((1999-12-31 12:00:00.000000-05:00) 26)))
         (bin_io "\001\000\000\000\136p6\204A\026"))
        ((sexp (((1999-12-31 13:42:23.622933-05:00) 27)))
         (bin_io "\001E\188\207\135|6\204A\027"))
        ((sexp (((1999-12-31 23:59:59.999999-05:00) 28)))
         (bin_io "\001\248\255\255\231\1966\204A\028"))
        ((sexp (((2000-01-01 00:00:00.000000-05:00) 29)))
         (bin_io "\001\000\000\000\232\1966\204A\029"))
        ((sexp (((2000-01-01 00:00:00.000001-05:00) 30)))
         (bin_io "\001\b\000\000\232\1966\204A\030"))
        ((sexp (((2000-01-01 00:00:00.001000-05:00) 31)))
         (bin_io "\001\197 \000\232\1966\204A\031"))
        ((sexp (((2000-01-01 00:00:01.000000-05:00) 32)))
         (bin_io "\001\000\000\128\232\1966\204A "))
        ((sexp (((2000-01-01 00:01:00.000000-05:00) 33)))
         (bin_io "\001\000\000\000\006\1976\204A!"))
        ((sexp (((2000-01-01 01:00:00.000000-05:00) 34)))
         (bin_io "\001\000\000\000\240\2036\204A\""))
        ((sexp (((2000-01-01 12:00:00.000000-05:00) 35)))
         (bin_io "\001\000\000\000H\0257\204A#"))
        ((sexp (((2000-01-01 13:42:23.622933-05:00) 36)))
         (bin_io "\001E\188\207G%7\204A$"))
        ((sexp (((2000-01-01 23:59:59.999999-05:00) 37)))
         (bin_io "\001\248\255\255\167m7\204A%"))
        ((sexp (((2000-01-02 00:00:00.000000-05:00) 38)))
         (bin_io "\001\000\000\000\168m7\204A&"))
        ((sexp (((2013-10-07 00:00:00.000000-04:00) 39)))
         (bin_io "\001\000\000\000p\140\148\212A'"))
        ((sexp (((2013-10-07 00:00:00.000001-04:00) 40)))
         (bin_io "\001\004\000\000p\140\148\212A("))
        ((sexp (((2013-10-07 00:00:00.001000-04:00) 41)))
         (bin_io "\001b\016\000p\140\148\212A)"))
        ((sexp (((2013-10-07 00:00:01.000000-04:00) 42)))
         (bin_io "\001\000\000@p\140\148\212A*"))
        ((sexp (((2013-10-07 00:01:00.000000-04:00) 43)))
         (bin_io "\001\000\000\000\127\140\148\212A+"))
        ((sexp (((2013-10-07 01:00:00.000000-04:00) 44)))
         (bin_io "\001\000\000\000\244\143\148\212A,"))
        ((sexp (((2013-10-07 12:00:00.000000-04:00) 45)))
         (bin_io "\001\000\000\000\160\182\148\212A-"))
        ((sexp (((2013-10-07 13:42:23.622933-04:00) 46)))
         (bin_io "\001\"\222\231\159\188\148\212A."))
        ((sexp (((2013-10-07 23:59:59.999999-04:00) 47)))
         (bin_io "\001\252\255\255\207\224\148\212A/"))
        ((sexp (((2013-10-08 00:00:00.000000-04:00) 48)))
         (bin_io "\001\000\000\000\208\224\148\212A0"))
        ((sexp (((2222-11-22 00:00:00.000000-05:00) 49)))
         (bin_io "\001\000\000\000\181\189\186\253A1"))
        ((sexp (((2222-11-22 00:00:00.000001-05:00) 50)))
         (bin_io "\001\001\000\000\181\189\186\253A2"))
        ((sexp (((2222-11-22 00:00:00.001000-05:00) 51)))
         (bin_io "\001\025\004\000\181\189\186\253A3"))
        ((sexp (((2222-11-22 00:00:01.000000-05:00) 52)))
         (bin_io "\001\000\000\016\181\189\186\253A4"))
        ((sexp (((2222-11-22 00:01:00.000000-05:00) 53)))
         (bin_io "\001\000\000\192\184\189\186\253A5"))
        ((sexp (((2222-11-22 01:00:00.000000-05:00) 54)))
         (bin_io "\001\000\000\000\150\190\186\253A6"))
        ((sexp (((2222-11-22 12:00:00.000000-05:00) 55)))
         (bin_io "\001\000\000\000A\200\186\253A7"))
        ((sexp (((2222-11-22 13:42:23.622933-05:00) 56)))
         (bin_io "\001\137\247\249\192\201\186\253A8"))
        ((sexp (((2222-11-22 23:59:59.999999-05:00) 57)))
         (bin_io "\001\255\255\255\204\210\186\253A9"))
        ((sexp (((2222-11-23 00:00:00.000000-05:00) 58)))
         (bin_io "\001\000\000\000\205\210\186\253A:"))
        ((sexp (((1665-09-11 05:34:21.163040-04:56:02) 59)))
         (bin_io "\001\024\178\006\206\003\227\001\194;"))
        ((sexp (((1749-05-17 01:44:58.057413-04:56:02) 60)))
         (bin_io "\001\214\020?\176\186\239\249\193<"))
        ((sexp (((1799-10-01 16:35:57.622579-04:56:02) 61)))
         (bin_io "\001\235\t\006\187\165\003\244\193="))
        ((sexp (((1833-12-02 03:31:51.104713-04:56:02) 62)))
         (bin_io "\0010\166\220n\190\254\239\193>"))
        ((sexp (((1876-08-10 08:56:53.681209-04:56:02) 63)))
         (bin_io "\001\1383\ni9\245\229\193?"))
        ((sexp (((1876-08-29 21:01:31.672122-04:56:02) 64)))
         (bin_io "\001\250}J\158\002\242\229\193@"))
        ((sexp (((1896-03-25 09:54:28.564831-05:00) 65)))
         (bin_io "\001\232\236m\203!X\225\193A"))
        ((sexp (((1952-04-04 18:31:13.284826-05:00) 66)))
         (bin_io "\001\208\138[_\127\175\192\193B"))
        ((sexp (((2045-05-01 15:40:18.240950-05:00) 67)))
         (bin_io "\001\220\181G6O\182\225AC"))
        ((sexp (((2157-05-06 22:33:36.270771-05:00) 68)))
         (bin_io "\001\020U\004)9\006\246AD"))
        ((sexp (
           ((1665-09-11 05:34:21.163040-04:56:02) 59)
           ((1749-05-17 01:44:58.057413-04:56:02) 60)
           ((1799-10-01 16:35:57.622579-04:56:02) 61)
           ((1833-12-02 03:31:51.104713-04:56:02) 62)
           ((1876-08-10 08:56:53.681209-04:56:02) 63)
           ((1876-08-29 21:01:31.672122-04:56:02) 64)
           ((1896-03-25 09:54:28.564831-05:00)    65)
           ((1912-06-23 00:00:00.000000-05:00)    0)
           ((1912-06-23 00:00:00.000001-05:00)    1)
           ((1912-06-23 00:00:00.001000-05:00)    2)
           ((1912-06-23 00:00:01.000000-05:00)    3)
           ((1912-06-23 00:01:00.000000-05:00)    4)
           ((1912-06-23 01:00:00.000000-05:00)    5)
           ((1912-06-23 12:00:00.000000-05:00)    6)
           ((1912-06-23 13:42:23.622933-05:00)    7)
           ((1912-06-23 23:59:59.999999-05:00)    8)
           ((1912-06-24 00:00:00.000000-05:00)    9)
           ((1952-04-04 18:31:13.284826-05:00)    66)
           ((1970-01-01 00:00:00.000000-05:00)    10)
           ((1970-01-01 00:00:00.000001-05:00)    11)
           ((1970-01-01 00:00:00.001000-05:00)    12)
           ((1970-01-01 00:00:01.000000-05:00)    13)
           ((1970-01-01 00:01:00.000000-05:00)    14)
           ((1970-01-01 01:00:00.000000-05:00)    15)
           ((1970-01-01 12:00:00.000000-05:00)    16)
           ((1970-01-01 13:42:23.622933-05:00)    17)
           ((1970-01-01 23:59:59.999999-05:00)    18)
           ((1970-01-02 00:00:00.000000-05:00)    19)
           ((1999-12-31 00:00:00.000000-05:00)    20)
           ((1999-12-31 00:00:00.000001-05:00)    21)
           ((1999-12-31 00:00:00.001000-05:00)    22)
           ((1999-12-31 00:00:01.000000-05:00)    23)
           ((1999-12-31 00:01:00.000000-05:00)    24)
           ((1999-12-31 01:00:00.000000-05:00)    25)
           ((1999-12-31 12:00:00.000000-05:00)    26)
           ((1999-12-31 13:42:23.622933-05:00)    27)
           ((1999-12-31 23:59:59.999999-05:00)    28)
           ((2000-01-01 00:00:00.000000-05:00)    29)
           ((2000-01-01 00:00:00.000001-05:00)    30)
           ((2000-01-01 00:00:00.001000-05:00)    31)
           ((2000-01-01 00:00:01.000000-05:00)    32)
           ((2000-01-01 00:01:00.000000-05:00)    33)
           ((2000-01-01 01:00:00.000000-05:00)    34)
           ((2000-01-01 12:00:00.000000-05:00)    35)
           ((2000-01-01 13:42:23.622933-05:00)    36)
           ((2000-01-01 23:59:59.999999-05:00)    37)
           ((2000-01-02 00:00:00.000000-05:00)    38)
           ((2013-10-07 00:00:00.000000-04:00)    39)
           ((2013-10-07 00:00:00.000001-04:00)    40)
           ((2013-10-07 00:00:00.001000-04:00)    41)
           ((2013-10-07 00:00:01.000000-04:00)    42)
           ((2013-10-07 00:01:00.000000-04:00)    43)
           ((2013-10-07 01:00:00.000000-04:00)    44)
           ((2013-10-07 12:00:00.000000-04:00)    45)
           ((2013-10-07 13:42:23.622933-04:00)    46)
           ((2013-10-07 23:59:59.999999-04:00)    47)
           ((2013-10-08 00:00:00.000000-04:00)    48)
           ((2045-05-01 15:40:18.240950-05:00)    67)
           ((2157-05-06 22:33:36.270771-05:00)    68)
           ((2222-11-22 00:00:00.000000-05:00)    49)
           ((2222-11-22 00:00:00.000001-05:00)    50)
           ((2222-11-22 00:00:00.001000-05:00)    51)
           ((2222-11-22 00:00:01.000000-05:00)    52)
           ((2222-11-22 00:01:00.000000-05:00)    53)
           ((2222-11-22 01:00:00.000000-05:00)    54)
           ((2222-11-22 12:00:00.000000-05:00)    55)
           ((2222-11-22 13:42:23.622933-05:00)    56)
           ((2222-11-22 23:59:59.999999-05:00)    57)
           ((2222-11-23 00:00:00.000000-05:00)    58)))
         (bin_io
          "E\024\178\006\206\003\227\001\194;\214\020?\176\186\239\249\193<\235\t\006\187\165\003\244\193=0\166\220n\190\254\239\193>\1383\ni9\245\229\193?\250}J\158\002\242\229\193@\232\236m\203!X\225\193A\000\000\000\140\241\012\219\193\000\252\255\255\139\241\012\219\193\001\158\239\255\139\241\012\219\193\002\000\000\192\139\241\012\219\193\003\000\000\000}\241\012\219\193\004\000\000\000\b\238\012\219\193\005\000\000\000\\\199\012\219\193\006\222!\024\\\193\012\219\193\007\004\000\000,\157\012\219\193\b\000\000\000,\157\012\219\193\t\208\138[_\127\175\192\193B\000\000\000\000\000\148\209@\n\1901\004\000\000\148\209@\011\211Mb\016\000\148\209@\012\000\000\000\000@\148\209@\r\000\000\000\000\000\163\209@\014\000\000\000\000\000\024\213@\015\000\000\000\000\000\226\237@\016\234\151\136\247\249p\240@\017\145\243\254\255\255|\249@\018\000\000\000\000\000}\249@\019\000\000\000(\0286\204A\020\b\000\000(\0286\204A\021\197 \000(\0286\204A\022\000\000\128(\0286\204A\023\000\000\000F\0286\204A\024\000\000\0000#6\204A\025\000\000\000\136p6\204A\026E\188\207\135|6\204A\027\248\255\255\231\1966\204A\028\000\000\000\232\1966\204A\029\b\000\000\232\1966\204A\030\197 \000\232\1966\204A\031\000\000\128\232\1966\204A \000\000\000\006\1976\204A!\000\000\000\240\2036\204A\"\000\000\000H\0257\204A#E\188\207G%7\204A$\248\255\255\167m7\204A%\000\000\000\168m7\204A&\000\000\000p\140\148\212A'\004\000\000p\140\148\212A(b\016\000p\140\148\212A)\000\000@p\140\148\212A*\000\000\000\127\140\148\212A+\000\000\000\244\143\148\212A,\000\000\000\160\182\148\212A-\"\222\231\159\188\148\212A.\252\255\255\207\224\148\212A/\000\000\000\208\224\148\212A0\220\181G6O\182\225AC\020U\004)9\006\246AD\000\000\000\181\189\186\253A1\001\000\000\181\189\186\253A2\025\004\000\181\189\186\253A3\000\000\016\181\189\186\253A4\000\000\192\184\189\186\253A5\000\000\000\150\190\186\253A6\000\000\000A\200\186\253A7\137\247\249\192\201\186\253A8\255\255\255\204\210\186\253A9\000\000\000\205\210\186\253A:")) |}];
    ;;

    (* [With_utc_sexp] *)

    let%expect_test "With_utc_sexp.V1" =
      test_stability (module struct
        include Time.Stable.With_utc_sexp.V1
        include For_time
      end);
      [%expect {|
        (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
        ((sexp (1912-06-23 05:00:00.000000Z))
         (bin_io "\000\000\000\140\241\012\219\193"))
        ((sexp (1912-06-23 05:00:00.000001Z))
         (bin_io "\252\255\255\139\241\012\219\193"))
        ((sexp (1912-06-23 05:00:00.001000Z))
         (bin_io "\158\239\255\139\241\012\219\193"))
        ((sexp (1912-06-23 05:00:01.000000Z))
         (bin_io "\000\000\192\139\241\012\219\193"))
        ((sexp (1912-06-23 05:01:00.000000Z))
         (bin_io "\000\000\000}\241\012\219\193"))
        ((sexp (1912-06-23 06:00:00.000000Z))
         (bin_io "\000\000\000\b\238\012\219\193"))
        ((sexp (1912-06-23 17:00:00.000000Z))
         (bin_io "\000\000\000\\\199\012\219\193"))
        ((sexp (1912-06-23 18:42:23.622933Z)) (bin_io "\222!\024\\\193\012\219\193"))
        ((sexp (1912-06-24 04:59:59.999999Z))
         (bin_io "\004\000\000,\157\012\219\193"))
        ((sexp (1912-06-24 05:00:00.000000Z))
         (bin_io "\000\000\000,\157\012\219\193"))
        ((sexp (1970-01-01 05:00:00.000000Z))
         (bin_io "\000\000\000\000\000\148\209@"))
        ((sexp (1970-01-01 05:00:00.000001Z)) (bin_io "\1901\004\000\000\148\209@"))
        ((sexp (1970-01-01 05:00:00.001000Z)) (bin_io "\211Mb\016\000\148\209@"))
        ((sexp (1970-01-01 05:00:01.000000Z)) (bin_io "\000\000\000\000@\148\209@"))
        ((sexp (1970-01-01 05:01:00.000000Z))
         (bin_io "\000\000\000\000\000\163\209@"))
        ((sexp (1970-01-01 06:00:00.000000Z))
         (bin_io "\000\000\000\000\000\024\213@"))
        ((sexp (1970-01-01 17:00:00.000000Z))
         (bin_io "\000\000\000\000\000\226\237@"))
        ((sexp (1970-01-01 18:42:23.622933Z)) (bin_io "\234\151\136\247\249p\240@"))
        ((sexp (1970-01-02 04:59:59.999999Z)) (bin_io "\145\243\254\255\255|\249@"))
        ((sexp (1970-01-02 05:00:00.000000Z)) (bin_io "\000\000\000\000\000}\249@"))
        ((sexp (1999-12-31 05:00:00.000000Z)) (bin_io "\000\000\000(\0286\204A"))
        ((sexp (1999-12-31 05:00:00.000001Z)) (bin_io "\b\000\000(\0286\204A"))
        ((sexp (1999-12-31 05:00:00.001000Z)) (bin_io "\197 \000(\0286\204A"))
        ((sexp (1999-12-31 05:00:01.000000Z)) (bin_io "\000\000\128(\0286\204A"))
        ((sexp (1999-12-31 05:01:00.000000Z)) (bin_io "\000\000\000F\0286\204A"))
        ((sexp (1999-12-31 06:00:00.000000Z)) (bin_io "\000\000\0000#6\204A"))
        ((sexp (1999-12-31 17:00:00.000000Z)) (bin_io "\000\000\000\136p6\204A"))
        ((sexp (1999-12-31 18:42:23.622933Z)) (bin_io "E\188\207\135|6\204A"))
        ((sexp (2000-01-01 04:59:59.999999Z)) (bin_io "\248\255\255\231\1966\204A"))
        ((sexp (2000-01-01 05:00:00.000000Z)) (bin_io "\000\000\000\232\1966\204A"))
        ((sexp (2000-01-01 05:00:00.000001Z)) (bin_io "\b\000\000\232\1966\204A"))
        ((sexp (2000-01-01 05:00:00.001000Z)) (bin_io "\197 \000\232\1966\204A"))
        ((sexp (2000-01-01 05:00:01.000000Z)) (bin_io "\000\000\128\232\1966\204A"))
        ((sexp (2000-01-01 05:01:00.000000Z)) (bin_io "\000\000\000\006\1976\204A"))
        ((sexp (2000-01-01 06:00:00.000000Z)) (bin_io "\000\000\000\240\2036\204A"))
        ((sexp (2000-01-01 17:00:00.000000Z)) (bin_io "\000\000\000H\0257\204A"))
        ((sexp (2000-01-01 18:42:23.622933Z)) (bin_io "E\188\207G%7\204A"))
        ((sexp (2000-01-02 04:59:59.999999Z)) (bin_io "\248\255\255\167m7\204A"))
        ((sexp (2000-01-02 05:00:00.000000Z)) (bin_io "\000\000\000\168m7\204A"))
        ((sexp (2013-10-07 04:00:00.000000Z)) (bin_io "\000\000\000p\140\148\212A"))
        ((sexp (2013-10-07 04:00:00.000001Z)) (bin_io "\004\000\000p\140\148\212A"))
        ((sexp (2013-10-07 04:00:00.001000Z)) (bin_io "b\016\000p\140\148\212A"))
        ((sexp (2013-10-07 04:00:01.000000Z)) (bin_io "\000\000@p\140\148\212A"))
        ((sexp (2013-10-07 04:01:00.000000Z))
         (bin_io "\000\000\000\127\140\148\212A"))
        ((sexp (2013-10-07 05:00:00.000000Z))
         (bin_io "\000\000\000\244\143\148\212A"))
        ((sexp (2013-10-07 16:00:00.000000Z))
         (bin_io "\000\000\000\160\182\148\212A"))
        ((sexp (2013-10-07 17:42:23.622933Z)) (bin_io "\"\222\231\159\188\148\212A"))
        ((sexp (2013-10-08 03:59:59.999999Z))
         (bin_io "\252\255\255\207\224\148\212A"))
        ((sexp (2013-10-08 04:00:00.000000Z))
         (bin_io "\000\000\000\208\224\148\212A"))
        ((sexp (2222-11-22 05:00:00.000000Z))
         (bin_io "\000\000\000\181\189\186\253A"))
        ((sexp (2222-11-22 05:00:00.000001Z))
         (bin_io "\001\000\000\181\189\186\253A"))
        ((sexp (2222-11-22 05:00:00.001000Z))
         (bin_io "\025\004\000\181\189\186\253A"))
        ((sexp (2222-11-22 05:00:01.000000Z))
         (bin_io "\000\000\016\181\189\186\253A"))
        ((sexp (2222-11-22 05:01:00.000000Z))
         (bin_io "\000\000\192\184\189\186\253A"))
        ((sexp (2222-11-22 06:00:00.000000Z))
         (bin_io "\000\000\000\150\190\186\253A"))
        ((sexp (2222-11-22 17:00:00.000000Z)) (bin_io "\000\000\000A\200\186\253A"))
        ((sexp (2222-11-22 18:42:23.622933Z))
         (bin_io "\137\247\249\192\201\186\253A"))
        ((sexp (2222-11-23 04:59:59.999999Z))
         (bin_io "\255\255\255\204\210\186\253A"))
        ((sexp (2222-11-23 05:00:00.000000Z))
         (bin_io "\000\000\000\205\210\186\253A"))
        ((sexp (1665-09-11 10:30:23.163040Z))
         (bin_io "\024\178\006\206\003\227\001\194"))
        ((sexp (1749-05-17 06:41:00.057413Z))
         (bin_io "\214\020?\176\186\239\249\193"))
        ((sexp (1799-10-01 21:31:59.622579Z))
         (bin_io "\235\t\006\187\165\003\244\193"))
        ((sexp (1833-12-02 08:27:53.104713Z)) (bin_io "0\166\220n\190\254\239\193"))
        ((sexp (1876-08-10 13:52:55.681209Z)) (bin_io "\1383\ni9\245\229\193"))
        ((sexp (1876-08-30 01:57:33.672122Z)) (bin_io "\250}J\158\002\242\229\193"))
        ((sexp (1896-03-25 14:54:28.564831Z)) (bin_io "\232\236m\203!X\225\193"))
        ((sexp (1952-04-04 23:31:13.284826Z)) (bin_io "\208\138[_\127\175\192\193"))
        ((sexp (2045-05-01 20:40:18.240950Z)) (bin_io "\220\181G6O\182\225A"))
        ((sexp (2157-05-07 03:33:36.270771Z)) (bin_io "\020U\004)9\006\246A")) |}];
    ;;

    let%expect_test "With_utc_sexp.V1.Set" =
      test_stability (module struct
        include Time.Stable.With_utc_sexp.V1.Set
        include For_set
      end);
      [%expect {|
        (bin_shape_digest 4e7cbf6fe56bd628b963b7f8259e58bf)
        ((sexp ()) (bin_io "\000"))
        ((sexp ((1912-06-23 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\140\241\012\219\193"))
        ((sexp ((1912-06-23 05:00:00.000001Z)))
         (bin_io "\001\252\255\255\139\241\012\219\193"))
        ((sexp ((1912-06-23 05:00:00.001000Z)))
         (bin_io "\001\158\239\255\139\241\012\219\193"))
        ((sexp ((1912-06-23 05:00:01.000000Z)))
         (bin_io "\001\000\000\192\139\241\012\219\193"))
        ((sexp ((1912-06-23 05:01:00.000000Z)))
         (bin_io "\001\000\000\000}\241\012\219\193"))
        ((sexp ((1912-06-23 06:00:00.000000Z)))
         (bin_io "\001\000\000\000\b\238\012\219\193"))
        ((sexp ((1912-06-23 17:00:00.000000Z)))
         (bin_io "\001\000\000\000\\\199\012\219\193"))
        ((sexp ((1912-06-23 18:42:23.622933Z)))
         (bin_io "\001\222!\024\\\193\012\219\193"))
        ((sexp ((1912-06-24 04:59:59.999999Z)))
         (bin_io "\001\004\000\000,\157\012\219\193"))
        ((sexp ((1912-06-24 05:00:00.000000Z)))
         (bin_io "\001\000\000\000,\157\012\219\193"))
        ((sexp ((1970-01-01 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\000\000\148\209@"))
        ((sexp ((1970-01-01 05:00:00.000001Z)))
         (bin_io "\001\1901\004\000\000\148\209@"))
        ((sexp ((1970-01-01 05:00:00.001000Z)))
         (bin_io "\001\211Mb\016\000\148\209@"))
        ((sexp ((1970-01-01 05:00:01.000000Z)))
         (bin_io "\001\000\000\000\000@\148\209@"))
        ((sexp ((1970-01-01 05:01:00.000000Z)))
         (bin_io "\001\000\000\000\000\000\163\209@"))
        ((sexp ((1970-01-01 06:00:00.000000Z)))
         (bin_io "\001\000\000\000\000\000\024\213@"))
        ((sexp ((1970-01-01 17:00:00.000000Z)))
         (bin_io "\001\000\000\000\000\000\226\237@"))
        ((sexp ((1970-01-01 18:42:23.622933Z)))
         (bin_io "\001\234\151\136\247\249p\240@"))
        ((sexp ((1970-01-02 04:59:59.999999Z)))
         (bin_io "\001\145\243\254\255\255|\249@"))
        ((sexp ((1970-01-02 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\000\000}\249@"))
        ((sexp ((1999-12-31 05:00:00.000000Z)))
         (bin_io "\001\000\000\000(\0286\204A"))
        ((sexp ((1999-12-31 05:00:00.000001Z))) (bin_io "\001\b\000\000(\0286\204A"))
        ((sexp ((1999-12-31 05:00:00.001000Z))) (bin_io "\001\197 \000(\0286\204A"))
        ((sexp ((1999-12-31 05:00:01.000000Z)))
         (bin_io "\001\000\000\128(\0286\204A"))
        ((sexp ((1999-12-31 05:01:00.000000Z)))
         (bin_io "\001\000\000\000F\0286\204A"))
        ((sexp ((1999-12-31 06:00:00.000000Z))) (bin_io "\001\000\000\0000#6\204A"))
        ((sexp ((1999-12-31 17:00:00.000000Z)))
         (bin_io "\001\000\000\000\136p6\204A"))
        ((sexp ((1999-12-31 18:42:23.622933Z))) (bin_io "\001E\188\207\135|6\204A"))
        ((sexp ((2000-01-01 04:59:59.999999Z)))
         (bin_io "\001\248\255\255\231\1966\204A"))
        ((sexp ((2000-01-01 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\232\1966\204A"))
        ((sexp ((2000-01-01 05:00:00.000001Z)))
         (bin_io "\001\b\000\000\232\1966\204A"))
        ((sexp ((2000-01-01 05:00:00.001000Z)))
         (bin_io "\001\197 \000\232\1966\204A"))
        ((sexp ((2000-01-01 05:00:01.000000Z)))
         (bin_io "\001\000\000\128\232\1966\204A"))
        ((sexp ((2000-01-01 05:01:00.000000Z)))
         (bin_io "\001\000\000\000\006\1976\204A"))
        ((sexp ((2000-01-01 06:00:00.000000Z)))
         (bin_io "\001\000\000\000\240\2036\204A"))
        ((sexp ((2000-01-01 17:00:00.000000Z)))
         (bin_io "\001\000\000\000H\0257\204A"))
        ((sexp ((2000-01-01 18:42:23.622933Z))) (bin_io "\001E\188\207G%7\204A"))
        ((sexp ((2000-01-02 04:59:59.999999Z)))
         (bin_io "\001\248\255\255\167m7\204A"))
        ((sexp ((2000-01-02 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\168m7\204A"))
        ((sexp ((2013-10-07 04:00:00.000000Z)))
         (bin_io "\001\000\000\000p\140\148\212A"))
        ((sexp ((2013-10-07 04:00:00.000001Z)))
         (bin_io "\001\004\000\000p\140\148\212A"))
        ((sexp ((2013-10-07 04:00:00.001000Z)))
         (bin_io "\001b\016\000p\140\148\212A"))
        ((sexp ((2013-10-07 04:00:01.000000Z)))
         (bin_io "\001\000\000@p\140\148\212A"))
        ((sexp ((2013-10-07 04:01:00.000000Z)))
         (bin_io "\001\000\000\000\127\140\148\212A"))
        ((sexp ((2013-10-07 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\244\143\148\212A"))
        ((sexp ((2013-10-07 16:00:00.000000Z)))
         (bin_io "\001\000\000\000\160\182\148\212A"))
        ((sexp ((2013-10-07 17:42:23.622933Z)))
         (bin_io "\001\"\222\231\159\188\148\212A"))
        ((sexp ((2013-10-08 03:59:59.999999Z)))
         (bin_io "\001\252\255\255\207\224\148\212A"))
        ((sexp ((2013-10-08 04:00:00.000000Z)))
         (bin_io "\001\000\000\000\208\224\148\212A"))
        ((sexp ((2222-11-22 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\181\189\186\253A"))
        ((sexp ((2222-11-22 05:00:00.000001Z)))
         (bin_io "\001\001\000\000\181\189\186\253A"))
        ((sexp ((2222-11-22 05:00:00.001000Z)))
         (bin_io "\001\025\004\000\181\189\186\253A"))
        ((sexp ((2222-11-22 05:00:01.000000Z)))
         (bin_io "\001\000\000\016\181\189\186\253A"))
        ((sexp ((2222-11-22 05:01:00.000000Z)))
         (bin_io "\001\000\000\192\184\189\186\253A"))
        ((sexp ((2222-11-22 06:00:00.000000Z)))
         (bin_io "\001\000\000\000\150\190\186\253A"))
        ((sexp ((2222-11-22 17:00:00.000000Z)))
         (bin_io "\001\000\000\000A\200\186\253A"))
        ((sexp ((2222-11-22 18:42:23.622933Z)))
         (bin_io "\001\137\247\249\192\201\186\253A"))
        ((sexp ((2222-11-23 04:59:59.999999Z)))
         (bin_io "\001\255\255\255\204\210\186\253A"))
        ((sexp ((2222-11-23 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\205\210\186\253A"))
        ((sexp ((1665-09-11 10:30:23.163040Z)))
         (bin_io "\001\024\178\006\206\003\227\001\194"))
        ((sexp ((1749-05-17 06:41:00.057413Z)))
         (bin_io "\001\214\020?\176\186\239\249\193"))
        ((sexp ((1799-10-01 21:31:59.622579Z)))
         (bin_io "\001\235\t\006\187\165\003\244\193"))
        ((sexp ((1833-12-02 08:27:53.104713Z)))
         (bin_io "\0010\166\220n\190\254\239\193"))
        ((sexp ((1876-08-10 13:52:55.681209Z))) (bin_io "\001\1383\ni9\245\229\193"))
        ((sexp ((1876-08-30 01:57:33.672122Z)))
         (bin_io "\001\250}J\158\002\242\229\193"))
        ((sexp ((1896-03-25 14:54:28.564831Z)))
         (bin_io "\001\232\236m\203!X\225\193"))
        ((sexp ((1952-04-04 23:31:13.284826Z)))
         (bin_io "\001\208\138[_\127\175\192\193"))
        ((sexp ((2045-05-01 20:40:18.240950Z))) (bin_io "\001\220\181G6O\182\225A"))
        ((sexp ((2157-05-07 03:33:36.270771Z))) (bin_io "\001\020U\004)9\006\246A"))
        ((sexp (
           (1665-09-11 10:30:23.163040Z)
           (1749-05-17 06:41:00.057413Z)
           (1799-10-01 21:31:59.622579Z)
           (1833-12-02 08:27:53.104713Z)
           (1876-08-10 13:52:55.681209Z)
           (1876-08-30 01:57:33.672122Z)
           (1896-03-25 14:54:28.564831Z)
           (1912-06-23 05:00:00.000000Z)
           (1912-06-23 05:00:00.000001Z)
           (1912-06-23 05:00:00.001000Z)
           (1912-06-23 05:00:01.000000Z)
           (1912-06-23 05:01:00.000000Z)
           (1912-06-23 06:00:00.000000Z)
           (1912-06-23 17:00:00.000000Z)
           (1912-06-23 18:42:23.622933Z)
           (1912-06-24 04:59:59.999999Z)
           (1912-06-24 05:00:00.000000Z)
           (1952-04-04 23:31:13.284826Z)
           (1970-01-01 05:00:00.000000Z)
           (1970-01-01 05:00:00.000001Z)
           (1970-01-01 05:00:00.001000Z)
           (1970-01-01 05:00:01.000000Z)
           (1970-01-01 05:01:00.000000Z)
           (1970-01-01 06:00:00.000000Z)
           (1970-01-01 17:00:00.000000Z)
           (1970-01-01 18:42:23.622933Z)
           (1970-01-02 04:59:59.999999Z)
           (1970-01-02 05:00:00.000000Z)
           (1999-12-31 05:00:00.000000Z)
           (1999-12-31 05:00:00.000001Z)
           (1999-12-31 05:00:00.001000Z)
           (1999-12-31 05:00:01.000000Z)
           (1999-12-31 05:01:00.000000Z)
           (1999-12-31 06:00:00.000000Z)
           (1999-12-31 17:00:00.000000Z)
           (1999-12-31 18:42:23.622933Z)
           (2000-01-01 04:59:59.999999Z)
           (2000-01-01 05:00:00.000000Z)
           (2000-01-01 05:00:00.000001Z)
           (2000-01-01 05:00:00.001000Z)
           (2000-01-01 05:00:01.000000Z)
           (2000-01-01 05:01:00.000000Z)
           (2000-01-01 06:00:00.000000Z)
           (2000-01-01 17:00:00.000000Z)
           (2000-01-01 18:42:23.622933Z)
           (2000-01-02 04:59:59.999999Z)
           (2000-01-02 05:00:00.000000Z)
           (2013-10-07 04:00:00.000000Z)
           (2013-10-07 04:00:00.000001Z)
           (2013-10-07 04:00:00.001000Z)
           (2013-10-07 04:00:01.000000Z)
           (2013-10-07 04:01:00.000000Z)
           (2013-10-07 05:00:00.000000Z)
           (2013-10-07 16:00:00.000000Z)
           (2013-10-07 17:42:23.622933Z)
           (2013-10-08 03:59:59.999999Z)
           (2013-10-08 04:00:00.000000Z)
           (2045-05-01 20:40:18.240950Z)
           (2157-05-07 03:33:36.270771Z)
           (2222-11-22 05:00:00.000000Z)
           (2222-11-22 05:00:00.000001Z)
           (2222-11-22 05:00:00.001000Z)
           (2222-11-22 05:00:01.000000Z)
           (2222-11-22 05:01:00.000000Z)
           (2222-11-22 06:00:00.000000Z)
           (2222-11-22 17:00:00.000000Z)
           (2222-11-22 18:42:23.622933Z)
           (2222-11-23 04:59:59.999999Z)
           (2222-11-23 05:00:00.000000Z)))
         (bin_io
          "E\024\178\006\206\003\227\001\194\214\020?\176\186\239\249\193\235\t\006\187\165\003\244\1930\166\220n\190\254\239\193\1383\ni9\245\229\193\250}J\158\002\242\229\193\232\236m\203!X\225\193\000\000\000\140\241\012\219\193\252\255\255\139\241\012\219\193\158\239\255\139\241\012\219\193\000\000\192\139\241\012\219\193\000\000\000}\241\012\219\193\000\000\000\b\238\012\219\193\000\000\000\\\199\012\219\193\222!\024\\\193\012\219\193\004\000\000,\157\012\219\193\000\000\000,\157\012\219\193\208\138[_\127\175\192\193\000\000\000\000\000\148\209@\1901\004\000\000\148\209@\211Mb\016\000\148\209@\000\000\000\000@\148\209@\000\000\000\000\000\163\209@\000\000\000\000\000\024\213@\000\000\000\000\000\226\237@\234\151\136\247\249p\240@\145\243\254\255\255|\249@\000\000\000\000\000}\249@\000\000\000(\0286\204A\b\000\000(\0286\204A\197 \000(\0286\204A\000\000\128(\0286\204A\000\000\000F\0286\204A\000\000\0000#6\204A\000\000\000\136p6\204AE\188\207\135|6\204A\248\255\255\231\1966\204A\000\000\000\232\1966\204A\b\000\000\232\1966\204A\197 \000\232\1966\204A\000\000\128\232\1966\204A\000\000\000\006\1976\204A\000\000\000\240\2036\204A\000\000\000H\0257\204AE\188\207G%7\204A\248\255\255\167m7\204A\000\000\000\168m7\204A\000\000\000p\140\148\212A\004\000\000p\140\148\212Ab\016\000p\140\148\212A\000\000@p\140\148\212A\000\000\000\127\140\148\212A\000\000\000\244\143\148\212A\000\000\000\160\182\148\212A\"\222\231\159\188\148\212A\252\255\255\207\224\148\212A\000\000\000\208\224\148\212A\220\181G6O\182\225A\020U\004)9\006\246A\000\000\000\181\189\186\253A\001\000\000\181\189\186\253A\025\004\000\181\189\186\253A\000\000\016\181\189\186\253A\000\000\192\184\189\186\253A\000\000\000\150\190\186\253A\000\000\000A\200\186\253A\137\247\249\192\201\186\253A\255\255\255\204\210\186\253A\000\000\000\205\210\186\253A")) |}];
    ;;

    let%expect_test "With_utc_sexp.V1.Map" =
      test_stability (module struct
        include Specialize_to_int (Time.Stable.With_utc_sexp.V1.Map)
        include For_map
      end);
      [%expect {|
        (bin_shape_digest 31404094f08cdbe1f9fca07a1a1e5303)
        ((sexp ()) (bin_io "\000"))
        ((sexp (((1912-06-23 05:00:00.000000Z) 0)))
         (bin_io "\001\000\000\000\140\241\012\219\193\000"))
        ((sexp (((1912-06-23 05:00:00.000001Z) 1)))
         (bin_io "\001\252\255\255\139\241\012\219\193\001"))
        ((sexp (((1912-06-23 05:00:00.001000Z) 2)))
         (bin_io "\001\158\239\255\139\241\012\219\193\002"))
        ((sexp (((1912-06-23 05:00:01.000000Z) 3)))
         (bin_io "\001\000\000\192\139\241\012\219\193\003"))
        ((sexp (((1912-06-23 05:01:00.000000Z) 4)))
         (bin_io "\001\000\000\000}\241\012\219\193\004"))
        ((sexp (((1912-06-23 06:00:00.000000Z) 5)))
         (bin_io "\001\000\000\000\b\238\012\219\193\005"))
        ((sexp (((1912-06-23 17:00:00.000000Z) 6)))
         (bin_io "\001\000\000\000\\\199\012\219\193\006"))
        ((sexp (((1912-06-23 18:42:23.622933Z) 7)))
         (bin_io "\001\222!\024\\\193\012\219\193\007"))
        ((sexp (((1912-06-24 04:59:59.999999Z) 8)))
         (bin_io "\001\004\000\000,\157\012\219\193\b"))
        ((sexp (((1912-06-24 05:00:00.000000Z) 9)))
         (bin_io "\001\000\000\000,\157\012\219\193\t"))
        ((sexp (((1970-01-01 05:00:00.000000Z) 10)))
         (bin_io "\001\000\000\000\000\000\148\209@\n"))
        ((sexp (((1970-01-01 05:00:00.000001Z) 11)))
         (bin_io "\001\1901\004\000\000\148\209@\011"))
        ((sexp (((1970-01-01 05:00:00.001000Z) 12)))
         (bin_io "\001\211Mb\016\000\148\209@\012"))
        ((sexp (((1970-01-01 05:00:01.000000Z) 13)))
         (bin_io "\001\000\000\000\000@\148\209@\r"))
        ((sexp (((1970-01-01 05:01:00.000000Z) 14)))
         (bin_io "\001\000\000\000\000\000\163\209@\014"))
        ((sexp (((1970-01-01 06:00:00.000000Z) 15)))
         (bin_io "\001\000\000\000\000\000\024\213@\015"))
        ((sexp (((1970-01-01 17:00:00.000000Z) 16)))
         (bin_io "\001\000\000\000\000\000\226\237@\016"))
        ((sexp (((1970-01-01 18:42:23.622933Z) 17)))
         (bin_io "\001\234\151\136\247\249p\240@\017"))
        ((sexp (((1970-01-02 04:59:59.999999Z) 18)))
         (bin_io "\001\145\243\254\255\255|\249@\018"))
        ((sexp (((1970-01-02 05:00:00.000000Z) 19)))
         (bin_io "\001\000\000\000\000\000}\249@\019"))
        ((sexp (((1999-12-31 05:00:00.000000Z) 20)))
         (bin_io "\001\000\000\000(\0286\204A\020"))
        ((sexp (((1999-12-31 05:00:00.000001Z) 21)))
         (bin_io "\001\b\000\000(\0286\204A\021"))
        ((sexp (((1999-12-31 05:00:00.001000Z) 22)))
         (bin_io "\001\197 \000(\0286\204A\022"))
        ((sexp (((1999-12-31 05:00:01.000000Z) 23)))
         (bin_io "\001\000\000\128(\0286\204A\023"))
        ((sexp (((1999-12-31 05:01:00.000000Z) 24)))
         (bin_io "\001\000\000\000F\0286\204A\024"))
        ((sexp (((1999-12-31 06:00:00.000000Z) 25)))
         (bin_io "\001\000\000\0000#6\204A\025"))
        ((sexp (((1999-12-31 17:00:00.000000Z) 26)))
         (bin_io "\001\000\000\000\136p6\204A\026"))
        ((sexp (((1999-12-31 18:42:23.622933Z) 27)))
         (bin_io "\001E\188\207\135|6\204A\027"))
        ((sexp (((2000-01-01 04:59:59.999999Z) 28)))
         (bin_io "\001\248\255\255\231\1966\204A\028"))
        ((sexp (((2000-01-01 05:00:00.000000Z) 29)))
         (bin_io "\001\000\000\000\232\1966\204A\029"))
        ((sexp (((2000-01-01 05:00:00.000001Z) 30)))
         (bin_io "\001\b\000\000\232\1966\204A\030"))
        ((sexp (((2000-01-01 05:00:00.001000Z) 31)))
         (bin_io "\001\197 \000\232\1966\204A\031"))
        ((sexp (((2000-01-01 05:00:01.000000Z) 32)))
         (bin_io "\001\000\000\128\232\1966\204A "))
        ((sexp (((2000-01-01 05:01:00.000000Z) 33)))
         (bin_io "\001\000\000\000\006\1976\204A!"))
        ((sexp (((2000-01-01 06:00:00.000000Z) 34)))
         (bin_io "\001\000\000\000\240\2036\204A\""))
        ((sexp (((2000-01-01 17:00:00.000000Z) 35)))
         (bin_io "\001\000\000\000H\0257\204A#"))
        ((sexp (((2000-01-01 18:42:23.622933Z) 36)))
         (bin_io "\001E\188\207G%7\204A$"))
        ((sexp (((2000-01-02 04:59:59.999999Z) 37)))
         (bin_io "\001\248\255\255\167m7\204A%"))
        ((sexp (((2000-01-02 05:00:00.000000Z) 38)))
         (bin_io "\001\000\000\000\168m7\204A&"))
        ((sexp (((2013-10-07 04:00:00.000000Z) 39)))
         (bin_io "\001\000\000\000p\140\148\212A'"))
        ((sexp (((2013-10-07 04:00:00.000001Z) 40)))
         (bin_io "\001\004\000\000p\140\148\212A("))
        ((sexp (((2013-10-07 04:00:00.001000Z) 41)))
         (bin_io "\001b\016\000p\140\148\212A)"))
        ((sexp (((2013-10-07 04:00:01.000000Z) 42)))
         (bin_io "\001\000\000@p\140\148\212A*"))
        ((sexp (((2013-10-07 04:01:00.000000Z) 43)))
         (bin_io "\001\000\000\000\127\140\148\212A+"))
        ((sexp (((2013-10-07 05:00:00.000000Z) 44)))
         (bin_io "\001\000\000\000\244\143\148\212A,"))
        ((sexp (((2013-10-07 16:00:00.000000Z) 45)))
         (bin_io "\001\000\000\000\160\182\148\212A-"))
        ((sexp (((2013-10-07 17:42:23.622933Z) 46)))
         (bin_io "\001\"\222\231\159\188\148\212A."))
        ((sexp (((2013-10-08 03:59:59.999999Z) 47)))
         (bin_io "\001\252\255\255\207\224\148\212A/"))
        ((sexp (((2013-10-08 04:00:00.000000Z) 48)))
         (bin_io "\001\000\000\000\208\224\148\212A0"))
        ((sexp (((2222-11-22 05:00:00.000000Z) 49)))
         (bin_io "\001\000\000\000\181\189\186\253A1"))
        ((sexp (((2222-11-22 05:00:00.000001Z) 50)))
         (bin_io "\001\001\000\000\181\189\186\253A2"))
        ((sexp (((2222-11-22 05:00:00.001000Z) 51)))
         (bin_io "\001\025\004\000\181\189\186\253A3"))
        ((sexp (((2222-11-22 05:00:01.000000Z) 52)))
         (bin_io "\001\000\000\016\181\189\186\253A4"))
        ((sexp (((2222-11-22 05:01:00.000000Z) 53)))
         (bin_io "\001\000\000\192\184\189\186\253A5"))
        ((sexp (((2222-11-22 06:00:00.000000Z) 54)))
         (bin_io "\001\000\000\000\150\190\186\253A6"))
        ((sexp (((2222-11-22 17:00:00.000000Z) 55)))
         (bin_io "\001\000\000\000A\200\186\253A7"))
        ((sexp (((2222-11-22 18:42:23.622933Z) 56)))
         (bin_io "\001\137\247\249\192\201\186\253A8"))
        ((sexp (((2222-11-23 04:59:59.999999Z) 57)))
         (bin_io "\001\255\255\255\204\210\186\253A9"))
        ((sexp (((2222-11-23 05:00:00.000000Z) 58)))
         (bin_io "\001\000\000\000\205\210\186\253A:"))
        ((sexp (((1665-09-11 10:30:23.163040Z) 59)))
         (bin_io "\001\024\178\006\206\003\227\001\194;"))
        ((sexp (((1749-05-17 06:41:00.057413Z) 60)))
         (bin_io "\001\214\020?\176\186\239\249\193<"))
        ((sexp (((1799-10-01 21:31:59.622579Z) 61)))
         (bin_io "\001\235\t\006\187\165\003\244\193="))
        ((sexp (((1833-12-02 08:27:53.104713Z) 62)))
         (bin_io "\0010\166\220n\190\254\239\193>"))
        ((sexp (((1876-08-10 13:52:55.681209Z) 63)))
         (bin_io "\001\1383\ni9\245\229\193?"))
        ((sexp (((1876-08-30 01:57:33.672122Z) 64)))
         (bin_io "\001\250}J\158\002\242\229\193@"))
        ((sexp (((1896-03-25 14:54:28.564831Z) 65)))
         (bin_io "\001\232\236m\203!X\225\193A"))
        ((sexp (((1952-04-04 23:31:13.284826Z) 66)))
         (bin_io "\001\208\138[_\127\175\192\193B"))
        ((sexp (((2045-05-01 20:40:18.240950Z) 67)))
         (bin_io "\001\220\181G6O\182\225AC"))
        ((sexp (((2157-05-07 03:33:36.270771Z) 68)))
         (bin_io "\001\020U\004)9\006\246AD"))
        ((sexp (
           ((1665-09-11 10:30:23.163040Z) 59)
           ((1749-05-17 06:41:00.057413Z) 60)
           ((1799-10-01 21:31:59.622579Z) 61)
           ((1833-12-02 08:27:53.104713Z) 62)
           ((1876-08-10 13:52:55.681209Z) 63)
           ((1876-08-30 01:57:33.672122Z) 64)
           ((1896-03-25 14:54:28.564831Z) 65)
           ((1912-06-23 05:00:00.000000Z) 0)
           ((1912-06-23 05:00:00.000001Z) 1)
           ((1912-06-23 05:00:00.001000Z) 2)
           ((1912-06-23 05:00:01.000000Z) 3)
           ((1912-06-23 05:01:00.000000Z) 4)
           ((1912-06-23 06:00:00.000000Z) 5)
           ((1912-06-23 17:00:00.000000Z) 6)
           ((1912-06-23 18:42:23.622933Z) 7)
           ((1912-06-24 04:59:59.999999Z) 8)
           ((1912-06-24 05:00:00.000000Z) 9)
           ((1952-04-04 23:31:13.284826Z) 66)
           ((1970-01-01 05:00:00.000000Z) 10)
           ((1970-01-01 05:00:00.000001Z) 11)
           ((1970-01-01 05:00:00.001000Z) 12)
           ((1970-01-01 05:00:01.000000Z) 13)
           ((1970-01-01 05:01:00.000000Z) 14)
           ((1970-01-01 06:00:00.000000Z) 15)
           ((1970-01-01 17:00:00.000000Z) 16)
           ((1970-01-01 18:42:23.622933Z) 17)
           ((1970-01-02 04:59:59.999999Z) 18)
           ((1970-01-02 05:00:00.000000Z) 19)
           ((1999-12-31 05:00:00.000000Z) 20)
           ((1999-12-31 05:00:00.000001Z) 21)
           ((1999-12-31 05:00:00.001000Z) 22)
           ((1999-12-31 05:00:01.000000Z) 23)
           ((1999-12-31 05:01:00.000000Z) 24)
           ((1999-12-31 06:00:00.000000Z) 25)
           ((1999-12-31 17:00:00.000000Z) 26)
           ((1999-12-31 18:42:23.622933Z) 27)
           ((2000-01-01 04:59:59.999999Z) 28)
           ((2000-01-01 05:00:00.000000Z) 29)
           ((2000-01-01 05:00:00.000001Z) 30)
           ((2000-01-01 05:00:00.001000Z) 31)
           ((2000-01-01 05:00:01.000000Z) 32)
           ((2000-01-01 05:01:00.000000Z) 33)
           ((2000-01-01 06:00:00.000000Z) 34)
           ((2000-01-01 17:00:00.000000Z) 35)
           ((2000-01-01 18:42:23.622933Z) 36)
           ((2000-01-02 04:59:59.999999Z) 37)
           ((2000-01-02 05:00:00.000000Z) 38)
           ((2013-10-07 04:00:00.000000Z) 39)
           ((2013-10-07 04:00:00.000001Z) 40)
           ((2013-10-07 04:00:00.001000Z) 41)
           ((2013-10-07 04:00:01.000000Z) 42)
           ((2013-10-07 04:01:00.000000Z) 43)
           ((2013-10-07 05:00:00.000000Z) 44)
           ((2013-10-07 16:00:00.000000Z) 45)
           ((2013-10-07 17:42:23.622933Z) 46)
           ((2013-10-08 03:59:59.999999Z) 47)
           ((2013-10-08 04:00:00.000000Z) 48)
           ((2045-05-01 20:40:18.240950Z) 67)
           ((2157-05-07 03:33:36.270771Z) 68)
           ((2222-11-22 05:00:00.000000Z) 49)
           ((2222-11-22 05:00:00.000001Z) 50)
           ((2222-11-22 05:00:00.001000Z) 51)
           ((2222-11-22 05:00:01.000000Z) 52)
           ((2222-11-22 05:01:00.000000Z) 53)
           ((2222-11-22 06:00:00.000000Z) 54)
           ((2222-11-22 17:00:00.000000Z) 55)
           ((2222-11-22 18:42:23.622933Z) 56)
           ((2222-11-23 04:59:59.999999Z) 57)
           ((2222-11-23 05:00:00.000000Z) 58)))
         (bin_io
          "E\024\178\006\206\003\227\001\194;\214\020?\176\186\239\249\193<\235\t\006\187\165\003\244\193=0\166\220n\190\254\239\193>\1383\ni9\245\229\193?\250}J\158\002\242\229\193@\232\236m\203!X\225\193A\000\000\000\140\241\012\219\193\000\252\255\255\139\241\012\219\193\001\158\239\255\139\241\012\219\193\002\000\000\192\139\241\012\219\193\003\000\000\000}\241\012\219\193\004\000\000\000\b\238\012\219\193\005\000\000\000\\\199\012\219\193\006\222!\024\\\193\012\219\193\007\004\000\000,\157\012\219\193\b\000\000\000,\157\012\219\193\t\208\138[_\127\175\192\193B\000\000\000\000\000\148\209@\n\1901\004\000\000\148\209@\011\211Mb\016\000\148\209@\012\000\000\000\000@\148\209@\r\000\000\000\000\000\163\209@\014\000\000\000\000\000\024\213@\015\000\000\000\000\000\226\237@\016\234\151\136\247\249p\240@\017\145\243\254\255\255|\249@\018\000\000\000\000\000}\249@\019\000\000\000(\0286\204A\020\b\000\000(\0286\204A\021\197 \000(\0286\204A\022\000\000\128(\0286\204A\023\000\000\000F\0286\204A\024\000\000\0000#6\204A\025\000\000\000\136p6\204A\026E\188\207\135|6\204A\027\248\255\255\231\1966\204A\028\000\000\000\232\1966\204A\029\b\000\000\232\1966\204A\030\197 \000\232\1966\204A\031\000\000\128\232\1966\204A \000\000\000\006\1976\204A!\000\000\000\240\2036\204A\"\000\000\000H\0257\204A#E\188\207G%7\204A$\248\255\255\167m7\204A%\000\000\000\168m7\204A&\000\000\000p\140\148\212A'\004\000\000p\140\148\212A(b\016\000p\140\148\212A)\000\000@p\140\148\212A*\000\000\000\127\140\148\212A+\000\000\000\244\143\148\212A,\000\000\000\160\182\148\212A-\"\222\231\159\188\148\212A.\252\255\255\207\224\148\212A/\000\000\000\208\224\148\212A0\220\181G6O\182\225AC\020U\004)9\006\246AD\000\000\000\181\189\186\253A1\001\000\000\181\189\186\253A2\025\004\000\181\189\186\253A3\000\000\016\181\189\186\253A4\000\000\192\184\189\186\253A5\000\000\000\150\190\186\253A6\000\000\000A\200\186\253A7\137\247\249\192\201\186\253A8\255\255\255\204\210\186\253A9\000\000\000\205\210\186\253A:")) |}];
    ;;

    let%expect_test "With_utc_sexp.V2" =
      test_stability (module struct
        include Time.Stable.With_utc_sexp.V2
        include For_time
      end);
      [%expect {|
        (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
        ((sexp (1912-06-23 05:00:00.000000Z))
         (bin_io "\000\000\000\140\241\012\219\193"))
        ((sexp (1912-06-23 05:00:00.000001Z))
         (bin_io "\252\255\255\139\241\012\219\193"))
        ((sexp (1912-06-23 05:00:00.001000Z))
         (bin_io "\158\239\255\139\241\012\219\193"))
        ((sexp (1912-06-23 05:00:01.000000Z))
         (bin_io "\000\000\192\139\241\012\219\193"))
        ((sexp (1912-06-23 05:01:00.000000Z))
         (bin_io "\000\000\000}\241\012\219\193"))
        ((sexp (1912-06-23 06:00:00.000000Z))
         (bin_io "\000\000\000\b\238\012\219\193"))
        ((sexp (1912-06-23 17:00:00.000000Z))
         (bin_io "\000\000\000\\\199\012\219\193"))
        ((sexp (1912-06-23 18:42:23.622933Z)) (bin_io "\222!\024\\\193\012\219\193"))
        ((sexp (1912-06-24 04:59:59.999999Z))
         (bin_io "\004\000\000,\157\012\219\193"))
        ((sexp (1912-06-24 05:00:00.000000Z))
         (bin_io "\000\000\000,\157\012\219\193"))
        ((sexp (1970-01-01 05:00:00.000000Z))
         (bin_io "\000\000\000\000\000\148\209@"))
        ((sexp (1970-01-01 05:00:00.000001Z)) (bin_io "\1901\004\000\000\148\209@"))
        ((sexp (1970-01-01 05:00:00.001000Z)) (bin_io "\211Mb\016\000\148\209@"))
        ((sexp (1970-01-01 05:00:01.000000Z)) (bin_io "\000\000\000\000@\148\209@"))
        ((sexp (1970-01-01 05:01:00.000000Z))
         (bin_io "\000\000\000\000\000\163\209@"))
        ((sexp (1970-01-01 06:00:00.000000Z))
         (bin_io "\000\000\000\000\000\024\213@"))
        ((sexp (1970-01-01 17:00:00.000000Z))
         (bin_io "\000\000\000\000\000\226\237@"))
        ((sexp (1970-01-01 18:42:23.622933Z)) (bin_io "\234\151\136\247\249p\240@"))
        ((sexp (1970-01-02 04:59:59.999999Z)) (bin_io "\145\243\254\255\255|\249@"))
        ((sexp (1970-01-02 05:00:00.000000Z)) (bin_io "\000\000\000\000\000}\249@"))
        ((sexp (1999-12-31 05:00:00.000000Z)) (bin_io "\000\000\000(\0286\204A"))
        ((sexp (1999-12-31 05:00:00.000001Z)) (bin_io "\b\000\000(\0286\204A"))
        ((sexp (1999-12-31 05:00:00.001000Z)) (bin_io "\197 \000(\0286\204A"))
        ((sexp (1999-12-31 05:00:01.000000Z)) (bin_io "\000\000\128(\0286\204A"))
        ((sexp (1999-12-31 05:01:00.000000Z)) (bin_io "\000\000\000F\0286\204A"))
        ((sexp (1999-12-31 06:00:00.000000Z)) (bin_io "\000\000\0000#6\204A"))
        ((sexp (1999-12-31 17:00:00.000000Z)) (bin_io "\000\000\000\136p6\204A"))
        ((sexp (1999-12-31 18:42:23.622933Z)) (bin_io "E\188\207\135|6\204A"))
        ((sexp (2000-01-01 04:59:59.999999Z)) (bin_io "\248\255\255\231\1966\204A"))
        ((sexp (2000-01-01 05:00:00.000000Z)) (bin_io "\000\000\000\232\1966\204A"))
        ((sexp (2000-01-01 05:00:00.000001Z)) (bin_io "\b\000\000\232\1966\204A"))
        ((sexp (2000-01-01 05:00:00.001000Z)) (bin_io "\197 \000\232\1966\204A"))
        ((sexp (2000-01-01 05:00:01.000000Z)) (bin_io "\000\000\128\232\1966\204A"))
        ((sexp (2000-01-01 05:01:00.000000Z)) (bin_io "\000\000\000\006\1976\204A"))
        ((sexp (2000-01-01 06:00:00.000000Z)) (bin_io "\000\000\000\240\2036\204A"))
        ((sexp (2000-01-01 17:00:00.000000Z)) (bin_io "\000\000\000H\0257\204A"))
        ((sexp (2000-01-01 18:42:23.622933Z)) (bin_io "E\188\207G%7\204A"))
        ((sexp (2000-01-02 04:59:59.999999Z)) (bin_io "\248\255\255\167m7\204A"))
        ((sexp (2000-01-02 05:00:00.000000Z)) (bin_io "\000\000\000\168m7\204A"))
        ((sexp (2013-10-07 04:00:00.000000Z)) (bin_io "\000\000\000p\140\148\212A"))
        ((sexp (2013-10-07 04:00:00.000001Z)) (bin_io "\004\000\000p\140\148\212A"))
        ((sexp (2013-10-07 04:00:00.001000Z)) (bin_io "b\016\000p\140\148\212A"))
        ((sexp (2013-10-07 04:00:01.000000Z)) (bin_io "\000\000@p\140\148\212A"))
        ((sexp (2013-10-07 04:01:00.000000Z))
         (bin_io "\000\000\000\127\140\148\212A"))
        ((sexp (2013-10-07 05:00:00.000000Z))
         (bin_io "\000\000\000\244\143\148\212A"))
        ((sexp (2013-10-07 16:00:00.000000Z))
         (bin_io "\000\000\000\160\182\148\212A"))
        ((sexp (2013-10-07 17:42:23.622933Z)) (bin_io "\"\222\231\159\188\148\212A"))
        ((sexp (2013-10-08 03:59:59.999999Z))
         (bin_io "\252\255\255\207\224\148\212A"))
        ((sexp (2013-10-08 04:00:00.000000Z))
         (bin_io "\000\000\000\208\224\148\212A"))
        ((sexp (2222-11-22 05:00:00.000000Z))
         (bin_io "\000\000\000\181\189\186\253A"))
        ((sexp (2222-11-22 05:00:00.000001Z))
         (bin_io "\001\000\000\181\189\186\253A"))
        ((sexp (2222-11-22 05:00:00.001000Z))
         (bin_io "\025\004\000\181\189\186\253A"))
        ((sexp (2222-11-22 05:00:01.000000Z))
         (bin_io "\000\000\016\181\189\186\253A"))
        ((sexp (2222-11-22 05:01:00.000000Z))
         (bin_io "\000\000\192\184\189\186\253A"))
        ((sexp (2222-11-22 06:00:00.000000Z))
         (bin_io "\000\000\000\150\190\186\253A"))
        ((sexp (2222-11-22 17:00:00.000000Z)) (bin_io "\000\000\000A\200\186\253A"))
        ((sexp (2222-11-22 18:42:23.622933Z))
         (bin_io "\137\247\249\192\201\186\253A"))
        ((sexp (2222-11-23 04:59:59.999999Z))
         (bin_io "\255\255\255\204\210\186\253A"))
        ((sexp (2222-11-23 05:00:00.000000Z))
         (bin_io "\000\000\000\205\210\186\253A"))
        ((sexp (1665-09-11 10:30:23.163040Z))
         (bin_io "\024\178\006\206\003\227\001\194"))
        ((sexp (1749-05-17 06:41:00.057413Z))
         (bin_io "\214\020?\176\186\239\249\193"))
        ((sexp (1799-10-01 21:31:59.622579Z))
         (bin_io "\235\t\006\187\165\003\244\193"))
        ((sexp (1833-12-02 08:27:53.104713Z)) (bin_io "0\166\220n\190\254\239\193"))
        ((sexp (1876-08-10 13:52:55.681209Z)) (bin_io "\1383\ni9\245\229\193"))
        ((sexp (1876-08-30 01:57:33.672122Z)) (bin_io "\250}J\158\002\242\229\193"))
        ((sexp (1896-03-25 14:54:28.564831Z)) (bin_io "\232\236m\203!X\225\193"))
        ((sexp (1952-04-04 23:31:13.284826Z)) (bin_io "\208\138[_\127\175\192\193"))
        ((sexp (2045-05-01 20:40:18.240950Z)) (bin_io "\220\181G6O\182\225A"))
        ((sexp (2157-05-07 03:33:36.270771Z)) (bin_io "\020U\004)9\006\246A")) |}];
    ;;

    let%expect_test "With_utc_sexp.V2.Set" =
      test_stability (module struct
        include Time.Stable.With_utc_sexp.V2.Set
        include For_set
      end);
      [%expect {|
        (bin_shape_digest 4e7cbf6fe56bd628b963b7f8259e58bf)
        ((sexp ()) (bin_io "\000"))
        ((sexp ((1912-06-23 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\140\241\012\219\193"))
        ((sexp ((1912-06-23 05:00:00.000001Z)))
         (bin_io "\001\252\255\255\139\241\012\219\193"))
        ((sexp ((1912-06-23 05:00:00.001000Z)))
         (bin_io "\001\158\239\255\139\241\012\219\193"))
        ((sexp ((1912-06-23 05:00:01.000000Z)))
         (bin_io "\001\000\000\192\139\241\012\219\193"))
        ((sexp ((1912-06-23 05:01:00.000000Z)))
         (bin_io "\001\000\000\000}\241\012\219\193"))
        ((sexp ((1912-06-23 06:00:00.000000Z)))
         (bin_io "\001\000\000\000\b\238\012\219\193"))
        ((sexp ((1912-06-23 17:00:00.000000Z)))
         (bin_io "\001\000\000\000\\\199\012\219\193"))
        ((sexp ((1912-06-23 18:42:23.622933Z)))
         (bin_io "\001\222!\024\\\193\012\219\193"))
        ((sexp ((1912-06-24 04:59:59.999999Z)))
         (bin_io "\001\004\000\000,\157\012\219\193"))
        ((sexp ((1912-06-24 05:00:00.000000Z)))
         (bin_io "\001\000\000\000,\157\012\219\193"))
        ((sexp ((1970-01-01 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\000\000\148\209@"))
        ((sexp ((1970-01-01 05:00:00.000001Z)))
         (bin_io "\001\1901\004\000\000\148\209@"))
        ((sexp ((1970-01-01 05:00:00.001000Z)))
         (bin_io "\001\211Mb\016\000\148\209@"))
        ((sexp ((1970-01-01 05:00:01.000000Z)))
         (bin_io "\001\000\000\000\000@\148\209@"))
        ((sexp ((1970-01-01 05:01:00.000000Z)))
         (bin_io "\001\000\000\000\000\000\163\209@"))
        ((sexp ((1970-01-01 06:00:00.000000Z)))
         (bin_io "\001\000\000\000\000\000\024\213@"))
        ((sexp ((1970-01-01 17:00:00.000000Z)))
         (bin_io "\001\000\000\000\000\000\226\237@"))
        ((sexp ((1970-01-01 18:42:23.622933Z)))
         (bin_io "\001\234\151\136\247\249p\240@"))
        ((sexp ((1970-01-02 04:59:59.999999Z)))
         (bin_io "\001\145\243\254\255\255|\249@"))
        ((sexp ((1970-01-02 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\000\000}\249@"))
        ((sexp ((1999-12-31 05:00:00.000000Z)))
         (bin_io "\001\000\000\000(\0286\204A"))
        ((sexp ((1999-12-31 05:00:00.000001Z))) (bin_io "\001\b\000\000(\0286\204A"))
        ((sexp ((1999-12-31 05:00:00.001000Z))) (bin_io "\001\197 \000(\0286\204A"))
        ((sexp ((1999-12-31 05:00:01.000000Z)))
         (bin_io "\001\000\000\128(\0286\204A"))
        ((sexp ((1999-12-31 05:01:00.000000Z)))
         (bin_io "\001\000\000\000F\0286\204A"))
        ((sexp ((1999-12-31 06:00:00.000000Z))) (bin_io "\001\000\000\0000#6\204A"))
        ((sexp ((1999-12-31 17:00:00.000000Z)))
         (bin_io "\001\000\000\000\136p6\204A"))
        ((sexp ((1999-12-31 18:42:23.622933Z))) (bin_io "\001E\188\207\135|6\204A"))
        ((sexp ((2000-01-01 04:59:59.999999Z)))
         (bin_io "\001\248\255\255\231\1966\204A"))
        ((sexp ((2000-01-01 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\232\1966\204A"))
        ((sexp ((2000-01-01 05:00:00.000001Z)))
         (bin_io "\001\b\000\000\232\1966\204A"))
        ((sexp ((2000-01-01 05:00:00.001000Z)))
         (bin_io "\001\197 \000\232\1966\204A"))
        ((sexp ((2000-01-01 05:00:01.000000Z)))
         (bin_io "\001\000\000\128\232\1966\204A"))
        ((sexp ((2000-01-01 05:01:00.000000Z)))
         (bin_io "\001\000\000\000\006\1976\204A"))
        ((sexp ((2000-01-01 06:00:00.000000Z)))
         (bin_io "\001\000\000\000\240\2036\204A"))
        ((sexp ((2000-01-01 17:00:00.000000Z)))
         (bin_io "\001\000\000\000H\0257\204A"))
        ((sexp ((2000-01-01 18:42:23.622933Z))) (bin_io "\001E\188\207G%7\204A"))
        ((sexp ((2000-01-02 04:59:59.999999Z)))
         (bin_io "\001\248\255\255\167m7\204A"))
        ((sexp ((2000-01-02 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\168m7\204A"))
        ((sexp ((2013-10-07 04:00:00.000000Z)))
         (bin_io "\001\000\000\000p\140\148\212A"))
        ((sexp ((2013-10-07 04:00:00.000001Z)))
         (bin_io "\001\004\000\000p\140\148\212A"))
        ((sexp ((2013-10-07 04:00:00.001000Z)))
         (bin_io "\001b\016\000p\140\148\212A"))
        ((sexp ((2013-10-07 04:00:01.000000Z)))
         (bin_io "\001\000\000@p\140\148\212A"))
        ((sexp ((2013-10-07 04:01:00.000000Z)))
         (bin_io "\001\000\000\000\127\140\148\212A"))
        ((sexp ((2013-10-07 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\244\143\148\212A"))
        ((sexp ((2013-10-07 16:00:00.000000Z)))
         (bin_io "\001\000\000\000\160\182\148\212A"))
        ((sexp ((2013-10-07 17:42:23.622933Z)))
         (bin_io "\001\"\222\231\159\188\148\212A"))
        ((sexp ((2013-10-08 03:59:59.999999Z)))
         (bin_io "\001\252\255\255\207\224\148\212A"))
        ((sexp ((2013-10-08 04:00:00.000000Z)))
         (bin_io "\001\000\000\000\208\224\148\212A"))
        ((sexp ((2222-11-22 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\181\189\186\253A"))
        ((sexp ((2222-11-22 05:00:00.000001Z)))
         (bin_io "\001\001\000\000\181\189\186\253A"))
        ((sexp ((2222-11-22 05:00:00.001000Z)))
         (bin_io "\001\025\004\000\181\189\186\253A"))
        ((sexp ((2222-11-22 05:00:01.000000Z)))
         (bin_io "\001\000\000\016\181\189\186\253A"))
        ((sexp ((2222-11-22 05:01:00.000000Z)))
         (bin_io "\001\000\000\192\184\189\186\253A"))
        ((sexp ((2222-11-22 06:00:00.000000Z)))
         (bin_io "\001\000\000\000\150\190\186\253A"))
        ((sexp ((2222-11-22 17:00:00.000000Z)))
         (bin_io "\001\000\000\000A\200\186\253A"))
        ((sexp ((2222-11-22 18:42:23.622933Z)))
         (bin_io "\001\137\247\249\192\201\186\253A"))
        ((sexp ((2222-11-23 04:59:59.999999Z)))
         (bin_io "\001\255\255\255\204\210\186\253A"))
        ((sexp ((2222-11-23 05:00:00.000000Z)))
         (bin_io "\001\000\000\000\205\210\186\253A"))
        ((sexp ((1665-09-11 10:30:23.163040Z)))
         (bin_io "\001\024\178\006\206\003\227\001\194"))
        ((sexp ((1749-05-17 06:41:00.057413Z)))
         (bin_io "\001\214\020?\176\186\239\249\193"))
        ((sexp ((1799-10-01 21:31:59.622579Z)))
         (bin_io "\001\235\t\006\187\165\003\244\193"))
        ((sexp ((1833-12-02 08:27:53.104713Z)))
         (bin_io "\0010\166\220n\190\254\239\193"))
        ((sexp ((1876-08-10 13:52:55.681209Z))) (bin_io "\001\1383\ni9\245\229\193"))
        ((sexp ((1876-08-30 01:57:33.672122Z)))
         (bin_io "\001\250}J\158\002\242\229\193"))
        ((sexp ((1896-03-25 14:54:28.564831Z)))
         (bin_io "\001\232\236m\203!X\225\193"))
        ((sexp ((1952-04-04 23:31:13.284826Z)))
         (bin_io "\001\208\138[_\127\175\192\193"))
        ((sexp ((2045-05-01 20:40:18.240950Z))) (bin_io "\001\220\181G6O\182\225A"))
        ((sexp ((2157-05-07 03:33:36.270771Z))) (bin_io "\001\020U\004)9\006\246A"))
        ((sexp (
           (1665-09-11 10:30:23.163040Z)
           (1749-05-17 06:41:00.057413Z)
           (1799-10-01 21:31:59.622579Z)
           (1833-12-02 08:27:53.104713Z)
           (1876-08-10 13:52:55.681209Z)
           (1876-08-30 01:57:33.672122Z)
           (1896-03-25 14:54:28.564831Z)
           (1912-06-23 05:00:00.000000Z)
           (1912-06-23 05:00:00.000001Z)
           (1912-06-23 05:00:00.001000Z)
           (1912-06-23 05:00:01.000000Z)
           (1912-06-23 05:01:00.000000Z)
           (1912-06-23 06:00:00.000000Z)
           (1912-06-23 17:00:00.000000Z)
           (1912-06-23 18:42:23.622933Z)
           (1912-06-24 04:59:59.999999Z)
           (1912-06-24 05:00:00.000000Z)
           (1952-04-04 23:31:13.284826Z)
           (1970-01-01 05:00:00.000000Z)
           (1970-01-01 05:00:00.000001Z)
           (1970-01-01 05:00:00.001000Z)
           (1970-01-01 05:00:01.000000Z)
           (1970-01-01 05:01:00.000000Z)
           (1970-01-01 06:00:00.000000Z)
           (1970-01-01 17:00:00.000000Z)
           (1970-01-01 18:42:23.622933Z)
           (1970-01-02 04:59:59.999999Z)
           (1970-01-02 05:00:00.000000Z)
           (1999-12-31 05:00:00.000000Z)
           (1999-12-31 05:00:00.000001Z)
           (1999-12-31 05:00:00.001000Z)
           (1999-12-31 05:00:01.000000Z)
           (1999-12-31 05:01:00.000000Z)
           (1999-12-31 06:00:00.000000Z)
           (1999-12-31 17:00:00.000000Z)
           (1999-12-31 18:42:23.622933Z)
           (2000-01-01 04:59:59.999999Z)
           (2000-01-01 05:00:00.000000Z)
           (2000-01-01 05:00:00.000001Z)
           (2000-01-01 05:00:00.001000Z)
           (2000-01-01 05:00:01.000000Z)
           (2000-01-01 05:01:00.000000Z)
           (2000-01-01 06:00:00.000000Z)
           (2000-01-01 17:00:00.000000Z)
           (2000-01-01 18:42:23.622933Z)
           (2000-01-02 04:59:59.999999Z)
           (2000-01-02 05:00:00.000000Z)
           (2013-10-07 04:00:00.000000Z)
           (2013-10-07 04:00:00.000001Z)
           (2013-10-07 04:00:00.001000Z)
           (2013-10-07 04:00:01.000000Z)
           (2013-10-07 04:01:00.000000Z)
           (2013-10-07 05:00:00.000000Z)
           (2013-10-07 16:00:00.000000Z)
           (2013-10-07 17:42:23.622933Z)
           (2013-10-08 03:59:59.999999Z)
           (2013-10-08 04:00:00.000000Z)
           (2045-05-01 20:40:18.240950Z)
           (2157-05-07 03:33:36.270771Z)
           (2222-11-22 05:00:00.000000Z)
           (2222-11-22 05:00:00.000001Z)
           (2222-11-22 05:00:00.001000Z)
           (2222-11-22 05:00:01.000000Z)
           (2222-11-22 05:01:00.000000Z)
           (2222-11-22 06:00:00.000000Z)
           (2222-11-22 17:00:00.000000Z)
           (2222-11-22 18:42:23.622933Z)
           (2222-11-23 04:59:59.999999Z)
           (2222-11-23 05:00:00.000000Z)))
         (bin_io
          "E\024\178\006\206\003\227\001\194\214\020?\176\186\239\249\193\235\t\006\187\165\003\244\1930\166\220n\190\254\239\193\1383\ni9\245\229\193\250}J\158\002\242\229\193\232\236m\203!X\225\193\000\000\000\140\241\012\219\193\252\255\255\139\241\012\219\193\158\239\255\139\241\012\219\193\000\000\192\139\241\012\219\193\000\000\000}\241\012\219\193\000\000\000\b\238\012\219\193\000\000\000\\\199\012\219\193\222!\024\\\193\012\219\193\004\000\000,\157\012\219\193\000\000\000,\157\012\219\193\208\138[_\127\175\192\193\000\000\000\000\000\148\209@\1901\004\000\000\148\209@\211Mb\016\000\148\209@\000\000\000\000@\148\209@\000\000\000\000\000\163\209@\000\000\000\000\000\024\213@\000\000\000\000\000\226\237@\234\151\136\247\249p\240@\145\243\254\255\255|\249@\000\000\000\000\000}\249@\000\000\000(\0286\204A\b\000\000(\0286\204A\197 \000(\0286\204A\000\000\128(\0286\204A\000\000\000F\0286\204A\000\000\0000#6\204A\000\000\000\136p6\204AE\188\207\135|6\204A\248\255\255\231\1966\204A\000\000\000\232\1966\204A\b\000\000\232\1966\204A\197 \000\232\1966\204A\000\000\128\232\1966\204A\000\000\000\006\1976\204A\000\000\000\240\2036\204A\000\000\000H\0257\204AE\188\207G%7\204A\248\255\255\167m7\204A\000\000\000\168m7\204A\000\000\000p\140\148\212A\004\000\000p\140\148\212Ab\016\000p\140\148\212A\000\000@p\140\148\212A\000\000\000\127\140\148\212A\000\000\000\244\143\148\212A\000\000\000\160\182\148\212A\"\222\231\159\188\148\212A\252\255\255\207\224\148\212A\000\000\000\208\224\148\212A\220\181G6O\182\225A\020U\004)9\006\246A\000\000\000\181\189\186\253A\001\000\000\181\189\186\253A\025\004\000\181\189\186\253A\000\000\016\181\189\186\253A\000\000\192\184\189\186\253A\000\000\000\150\190\186\253A\000\000\000A\200\186\253A\137\247\249\192\201\186\253A\255\255\255\204\210\186\253A\000\000\000\205\210\186\253A")) |}];
    ;;

    let%expect_test "With_utc_sexp.V2.Map" =
      test_stability (module struct
        include Specialize_to_int (Time.Stable.With_utc_sexp.V2.Map)
        include For_map
      end);
      [%expect {|
        (bin_shape_digest 31404094f08cdbe1f9fca07a1a1e5303)
        ((sexp ()) (bin_io "\000"))
        ((sexp (((1912-06-23 05:00:00.000000Z) 0)))
         (bin_io "\001\000\000\000\140\241\012\219\193\000"))
        ((sexp (((1912-06-23 05:00:00.000001Z) 1)))
         (bin_io "\001\252\255\255\139\241\012\219\193\001"))
        ((sexp (((1912-06-23 05:00:00.001000Z) 2)))
         (bin_io "\001\158\239\255\139\241\012\219\193\002"))
        ((sexp (((1912-06-23 05:00:01.000000Z) 3)))
         (bin_io "\001\000\000\192\139\241\012\219\193\003"))
        ((sexp (((1912-06-23 05:01:00.000000Z) 4)))
         (bin_io "\001\000\000\000}\241\012\219\193\004"))
        ((sexp (((1912-06-23 06:00:00.000000Z) 5)))
         (bin_io "\001\000\000\000\b\238\012\219\193\005"))
        ((sexp (((1912-06-23 17:00:00.000000Z) 6)))
         (bin_io "\001\000\000\000\\\199\012\219\193\006"))
        ((sexp (((1912-06-23 18:42:23.622933Z) 7)))
         (bin_io "\001\222!\024\\\193\012\219\193\007"))
        ((sexp (((1912-06-24 04:59:59.999999Z) 8)))
         (bin_io "\001\004\000\000,\157\012\219\193\b"))
        ((sexp (((1912-06-24 05:00:00.000000Z) 9)))
         (bin_io "\001\000\000\000,\157\012\219\193\t"))
        ((sexp (((1970-01-01 05:00:00.000000Z) 10)))
         (bin_io "\001\000\000\000\000\000\148\209@\n"))
        ((sexp (((1970-01-01 05:00:00.000001Z) 11)))
         (bin_io "\001\1901\004\000\000\148\209@\011"))
        ((sexp (((1970-01-01 05:00:00.001000Z) 12)))
         (bin_io "\001\211Mb\016\000\148\209@\012"))
        ((sexp (((1970-01-01 05:00:01.000000Z) 13)))
         (bin_io "\001\000\000\000\000@\148\209@\r"))
        ((sexp (((1970-01-01 05:01:00.000000Z) 14)))
         (bin_io "\001\000\000\000\000\000\163\209@\014"))
        ((sexp (((1970-01-01 06:00:00.000000Z) 15)))
         (bin_io "\001\000\000\000\000\000\024\213@\015"))
        ((sexp (((1970-01-01 17:00:00.000000Z) 16)))
         (bin_io "\001\000\000\000\000\000\226\237@\016"))
        ((sexp (((1970-01-01 18:42:23.622933Z) 17)))
         (bin_io "\001\234\151\136\247\249p\240@\017"))
        ((sexp (((1970-01-02 04:59:59.999999Z) 18)))
         (bin_io "\001\145\243\254\255\255|\249@\018"))
        ((sexp (((1970-01-02 05:00:00.000000Z) 19)))
         (bin_io "\001\000\000\000\000\000}\249@\019"))
        ((sexp (((1999-12-31 05:00:00.000000Z) 20)))
         (bin_io "\001\000\000\000(\0286\204A\020"))
        ((sexp (((1999-12-31 05:00:00.000001Z) 21)))
         (bin_io "\001\b\000\000(\0286\204A\021"))
        ((sexp (((1999-12-31 05:00:00.001000Z) 22)))
         (bin_io "\001\197 \000(\0286\204A\022"))
        ((sexp (((1999-12-31 05:00:01.000000Z) 23)))
         (bin_io "\001\000\000\128(\0286\204A\023"))
        ((sexp (((1999-12-31 05:01:00.000000Z) 24)))
         (bin_io "\001\000\000\000F\0286\204A\024"))
        ((sexp (((1999-12-31 06:00:00.000000Z) 25)))
         (bin_io "\001\000\000\0000#6\204A\025"))
        ((sexp (((1999-12-31 17:00:00.000000Z) 26)))
         (bin_io "\001\000\000\000\136p6\204A\026"))
        ((sexp (((1999-12-31 18:42:23.622933Z) 27)))
         (bin_io "\001E\188\207\135|6\204A\027"))
        ((sexp (((2000-01-01 04:59:59.999999Z) 28)))
         (bin_io "\001\248\255\255\231\1966\204A\028"))
        ((sexp (((2000-01-01 05:00:00.000000Z) 29)))
         (bin_io "\001\000\000\000\232\1966\204A\029"))
        ((sexp (((2000-01-01 05:00:00.000001Z) 30)))
         (bin_io "\001\b\000\000\232\1966\204A\030"))
        ((sexp (((2000-01-01 05:00:00.001000Z) 31)))
         (bin_io "\001\197 \000\232\1966\204A\031"))
        ((sexp (((2000-01-01 05:00:01.000000Z) 32)))
         (bin_io "\001\000\000\128\232\1966\204A "))
        ((sexp (((2000-01-01 05:01:00.000000Z) 33)))
         (bin_io "\001\000\000\000\006\1976\204A!"))
        ((sexp (((2000-01-01 06:00:00.000000Z) 34)))
         (bin_io "\001\000\000\000\240\2036\204A\""))
        ((sexp (((2000-01-01 17:00:00.000000Z) 35)))
         (bin_io "\001\000\000\000H\0257\204A#"))
        ((sexp (((2000-01-01 18:42:23.622933Z) 36)))
         (bin_io "\001E\188\207G%7\204A$"))
        ((sexp (((2000-01-02 04:59:59.999999Z) 37)))
         (bin_io "\001\248\255\255\167m7\204A%"))
        ((sexp (((2000-01-02 05:00:00.000000Z) 38)))
         (bin_io "\001\000\000\000\168m7\204A&"))
        ((sexp (((2013-10-07 04:00:00.000000Z) 39)))
         (bin_io "\001\000\000\000p\140\148\212A'"))
        ((sexp (((2013-10-07 04:00:00.000001Z) 40)))
         (bin_io "\001\004\000\000p\140\148\212A("))
        ((sexp (((2013-10-07 04:00:00.001000Z) 41)))
         (bin_io "\001b\016\000p\140\148\212A)"))
        ((sexp (((2013-10-07 04:00:01.000000Z) 42)))
         (bin_io "\001\000\000@p\140\148\212A*"))
        ((sexp (((2013-10-07 04:01:00.000000Z) 43)))
         (bin_io "\001\000\000\000\127\140\148\212A+"))
        ((sexp (((2013-10-07 05:00:00.000000Z) 44)))
         (bin_io "\001\000\000\000\244\143\148\212A,"))
        ((sexp (((2013-10-07 16:00:00.000000Z) 45)))
         (bin_io "\001\000\000\000\160\182\148\212A-"))
        ((sexp (((2013-10-07 17:42:23.622933Z) 46)))
         (bin_io "\001\"\222\231\159\188\148\212A."))
        ((sexp (((2013-10-08 03:59:59.999999Z) 47)))
         (bin_io "\001\252\255\255\207\224\148\212A/"))
        ((sexp (((2013-10-08 04:00:00.000000Z) 48)))
         (bin_io "\001\000\000\000\208\224\148\212A0"))
        ((sexp (((2222-11-22 05:00:00.000000Z) 49)))
         (bin_io "\001\000\000\000\181\189\186\253A1"))
        ((sexp (((2222-11-22 05:00:00.000001Z) 50)))
         (bin_io "\001\001\000\000\181\189\186\253A2"))
        ((sexp (((2222-11-22 05:00:00.001000Z) 51)))
         (bin_io "\001\025\004\000\181\189\186\253A3"))
        ((sexp (((2222-11-22 05:00:01.000000Z) 52)))
         (bin_io "\001\000\000\016\181\189\186\253A4"))
        ((sexp (((2222-11-22 05:01:00.000000Z) 53)))
         (bin_io "\001\000\000\192\184\189\186\253A5"))
        ((sexp (((2222-11-22 06:00:00.000000Z) 54)))
         (bin_io "\001\000\000\000\150\190\186\253A6"))
        ((sexp (((2222-11-22 17:00:00.000000Z) 55)))
         (bin_io "\001\000\000\000A\200\186\253A7"))
        ((sexp (((2222-11-22 18:42:23.622933Z) 56)))
         (bin_io "\001\137\247\249\192\201\186\253A8"))
        ((sexp (((2222-11-23 04:59:59.999999Z) 57)))
         (bin_io "\001\255\255\255\204\210\186\253A9"))
        ((sexp (((2222-11-23 05:00:00.000000Z) 58)))
         (bin_io "\001\000\000\000\205\210\186\253A:"))
        ((sexp (((1665-09-11 10:30:23.163040Z) 59)))
         (bin_io "\001\024\178\006\206\003\227\001\194;"))
        ((sexp (((1749-05-17 06:41:00.057413Z) 60)))
         (bin_io "\001\214\020?\176\186\239\249\193<"))
        ((sexp (((1799-10-01 21:31:59.622579Z) 61)))
         (bin_io "\001\235\t\006\187\165\003\244\193="))
        ((sexp (((1833-12-02 08:27:53.104713Z) 62)))
         (bin_io "\0010\166\220n\190\254\239\193>"))
        ((sexp (((1876-08-10 13:52:55.681209Z) 63)))
         (bin_io "\001\1383\ni9\245\229\193?"))
        ((sexp (((1876-08-30 01:57:33.672122Z) 64)))
         (bin_io "\001\250}J\158\002\242\229\193@"))
        ((sexp (((1896-03-25 14:54:28.564831Z) 65)))
         (bin_io "\001\232\236m\203!X\225\193A"))
        ((sexp (((1952-04-04 23:31:13.284826Z) 66)))
         (bin_io "\001\208\138[_\127\175\192\193B"))
        ((sexp (((2045-05-01 20:40:18.240950Z) 67)))
         (bin_io "\001\220\181G6O\182\225AC"))
        ((sexp (((2157-05-07 03:33:36.270771Z) 68)))
         (bin_io "\001\020U\004)9\006\246AD"))
        ((sexp (
           ((1665-09-11 10:30:23.163040Z) 59)
           ((1749-05-17 06:41:00.057413Z) 60)
           ((1799-10-01 21:31:59.622579Z) 61)
           ((1833-12-02 08:27:53.104713Z) 62)
           ((1876-08-10 13:52:55.681209Z) 63)
           ((1876-08-30 01:57:33.672122Z) 64)
           ((1896-03-25 14:54:28.564831Z) 65)
           ((1912-06-23 05:00:00.000000Z) 0)
           ((1912-06-23 05:00:00.000001Z) 1)
           ((1912-06-23 05:00:00.001000Z) 2)
           ((1912-06-23 05:00:01.000000Z) 3)
           ((1912-06-23 05:01:00.000000Z) 4)
           ((1912-06-23 06:00:00.000000Z) 5)
           ((1912-06-23 17:00:00.000000Z) 6)
           ((1912-06-23 18:42:23.622933Z) 7)
           ((1912-06-24 04:59:59.999999Z) 8)
           ((1912-06-24 05:00:00.000000Z) 9)
           ((1952-04-04 23:31:13.284826Z) 66)
           ((1970-01-01 05:00:00.000000Z) 10)
           ((1970-01-01 05:00:00.000001Z) 11)
           ((1970-01-01 05:00:00.001000Z) 12)
           ((1970-01-01 05:00:01.000000Z) 13)
           ((1970-01-01 05:01:00.000000Z) 14)
           ((1970-01-01 06:00:00.000000Z) 15)
           ((1970-01-01 17:00:00.000000Z) 16)
           ((1970-01-01 18:42:23.622933Z) 17)
           ((1970-01-02 04:59:59.999999Z) 18)
           ((1970-01-02 05:00:00.000000Z) 19)
           ((1999-12-31 05:00:00.000000Z) 20)
           ((1999-12-31 05:00:00.000001Z) 21)
           ((1999-12-31 05:00:00.001000Z) 22)
           ((1999-12-31 05:00:01.000000Z) 23)
           ((1999-12-31 05:01:00.000000Z) 24)
           ((1999-12-31 06:00:00.000000Z) 25)
           ((1999-12-31 17:00:00.000000Z) 26)
           ((1999-12-31 18:42:23.622933Z) 27)
           ((2000-01-01 04:59:59.999999Z) 28)
           ((2000-01-01 05:00:00.000000Z) 29)
           ((2000-01-01 05:00:00.000001Z) 30)
           ((2000-01-01 05:00:00.001000Z) 31)
           ((2000-01-01 05:00:01.000000Z) 32)
           ((2000-01-01 05:01:00.000000Z) 33)
           ((2000-01-01 06:00:00.000000Z) 34)
           ((2000-01-01 17:00:00.000000Z) 35)
           ((2000-01-01 18:42:23.622933Z) 36)
           ((2000-01-02 04:59:59.999999Z) 37)
           ((2000-01-02 05:00:00.000000Z) 38)
           ((2013-10-07 04:00:00.000000Z) 39)
           ((2013-10-07 04:00:00.000001Z) 40)
           ((2013-10-07 04:00:00.001000Z) 41)
           ((2013-10-07 04:00:01.000000Z) 42)
           ((2013-10-07 04:01:00.000000Z) 43)
           ((2013-10-07 05:00:00.000000Z) 44)
           ((2013-10-07 16:00:00.000000Z) 45)
           ((2013-10-07 17:42:23.622933Z) 46)
           ((2013-10-08 03:59:59.999999Z) 47)
           ((2013-10-08 04:00:00.000000Z) 48)
           ((2045-05-01 20:40:18.240950Z) 67)
           ((2157-05-07 03:33:36.270771Z) 68)
           ((2222-11-22 05:00:00.000000Z) 49)
           ((2222-11-22 05:00:00.000001Z) 50)
           ((2222-11-22 05:00:00.001000Z) 51)
           ((2222-11-22 05:00:01.000000Z) 52)
           ((2222-11-22 05:01:00.000000Z) 53)
           ((2222-11-22 06:00:00.000000Z) 54)
           ((2222-11-22 17:00:00.000000Z) 55)
           ((2222-11-22 18:42:23.622933Z) 56)
           ((2222-11-23 04:59:59.999999Z) 57)
           ((2222-11-23 05:00:00.000000Z) 58)))
         (bin_io
          "E\024\178\006\206\003\227\001\194;\214\020?\176\186\239\249\193<\235\t\006\187\165\003\244\193=0\166\220n\190\254\239\193>\1383\ni9\245\229\193?\250}J\158\002\242\229\193@\232\236m\203!X\225\193A\000\000\000\140\241\012\219\193\000\252\255\255\139\241\012\219\193\001\158\239\255\139\241\012\219\193\002\000\000\192\139\241\012\219\193\003\000\000\000}\241\012\219\193\004\000\000\000\b\238\012\219\193\005\000\000\000\\\199\012\219\193\006\222!\024\\\193\012\219\193\007\004\000\000,\157\012\219\193\b\000\000\000,\157\012\219\193\t\208\138[_\127\175\192\193B\000\000\000\000\000\148\209@\n\1901\004\000\000\148\209@\011\211Mb\016\000\148\209@\012\000\000\000\000@\148\209@\r\000\000\000\000\000\163\209@\014\000\000\000\000\000\024\213@\015\000\000\000\000\000\226\237@\016\234\151\136\247\249p\240@\017\145\243\254\255\255|\249@\018\000\000\000\000\000}\249@\019\000\000\000(\0286\204A\020\b\000\000(\0286\204A\021\197 \000(\0286\204A\022\000\000\128(\0286\204A\023\000\000\000F\0286\204A\024\000\000\0000#6\204A\025\000\000\000\136p6\204A\026E\188\207\135|6\204A\027\248\255\255\231\1966\204A\028\000\000\000\232\1966\204A\029\b\000\000\232\1966\204A\030\197 \000\232\1966\204A\031\000\000\128\232\1966\204A \000\000\000\006\1976\204A!\000\000\000\240\2036\204A\"\000\000\000H\0257\204A#E\188\207G%7\204A$\248\255\255\167m7\204A%\000\000\000\168m7\204A&\000\000\000p\140\148\212A'\004\000\000p\140\148\212A(b\016\000p\140\148\212A)\000\000@p\140\148\212A*\000\000\000\127\140\148\212A+\000\000\000\244\143\148\212A,\000\000\000\160\182\148\212A-\"\222\231\159\188\148\212A.\252\255\255\207\224\148\212A/\000\000\000\208\224\148\212A0\220\181G6O\182\225AC\020U\004)9\006\246AD\000\000\000\181\189\186\253A1\001\000\000\181\189\186\253A2\025\004\000\181\189\186\253A3\000\000\016\181\189\186\253A4\000\000\192\184\189\186\253A5\000\000\000\150\190\186\253A6\000\000\000A\200\186\253A7\137\247\249\192\201\186\253A8\255\255\255\204\210\186\253A9\000\000\000\205\210\186\253A:")) |}];
    ;;

    (* [With_t_of_sexp_abs] *)

    let%expect_test "With_t_of_sexp_abs.V1" =
      test_stability (module struct
        include Time.Stable.With_t_of_sexp_abs.V1
        include For_time
      end);
      [%expect {|
        (bin_shape_digest 1fd923acb2dd9c5d401ad5b08b1d40cd)
        ((sexp (1912-06-23 00:00:00.000000-05:00))
         (bin_io "\000\000\000\140\241\012\219\193"))
        ((sexp (1912-06-23 00:00:00.000001-05:00))
         (bin_io "\252\255\255\139\241\012\219\193"))
        ((sexp (1912-06-23 00:00:00.001000-05:00))
         (bin_io "\158\239\255\139\241\012\219\193"))
        ((sexp (1912-06-23 00:00:01.000000-05:00))
         (bin_io "\000\000\192\139\241\012\219\193"))
        ((sexp (1912-06-23 00:01:00.000000-05:00))
         (bin_io "\000\000\000}\241\012\219\193"))
        ((sexp (1912-06-23 01:00:00.000000-05:00))
         (bin_io "\000\000\000\b\238\012\219\193"))
        ((sexp (1912-06-23 12:00:00.000000-05:00))
         (bin_io "\000\000\000\\\199\012\219\193"))
        ((sexp (1912-06-23 13:42:23.622933-05:00))
         (bin_io "\222!\024\\\193\012\219\193"))
        ((sexp (1912-06-23 23:59:59.999999-05:00))
         (bin_io "\004\000\000,\157\012\219\193"))
        ((sexp (1912-06-24 00:00:00.000000-05:00))
         (bin_io "\000\000\000,\157\012\219\193"))
        ((sexp (1970-01-01 00:00:00.000000-05:00))
         (bin_io "\000\000\000\000\000\148\209@"))
        ((sexp (1970-01-01 00:00:00.000001-05:00))
         (bin_io "\1901\004\000\000\148\209@"))
        ((sexp (1970-01-01 00:00:00.001000-05:00)) (bin_io "\211Mb\016\000\148\209@"))
        ((sexp (1970-01-01 00:00:01.000000-05:00))
         (bin_io "\000\000\000\000@\148\209@"))
        ((sexp (1970-01-01 00:01:00.000000-05:00))
         (bin_io "\000\000\000\000\000\163\209@"))
        ((sexp (1970-01-01 01:00:00.000000-05:00))
         (bin_io "\000\000\000\000\000\024\213@"))
        ((sexp (1970-01-01 12:00:00.000000-05:00))
         (bin_io "\000\000\000\000\000\226\237@"))
        ((sexp (1970-01-01 13:42:23.622933-05:00))
         (bin_io "\234\151\136\247\249p\240@"))
        ((sexp (1970-01-01 23:59:59.999999-05:00))
         (bin_io "\145\243\254\255\255|\249@"))
        ((sexp (1970-01-02 00:00:00.000000-05:00))
         (bin_io "\000\000\000\000\000}\249@"))
        ((sexp (1999-12-31 00:00:00.000000-05:00)) (bin_io "\000\000\000(\0286\204A"))
        ((sexp (1999-12-31 00:00:00.000001-05:00)) (bin_io "\b\000\000(\0286\204A"))
        ((sexp (1999-12-31 00:00:00.001000-05:00)) (bin_io "\197 \000(\0286\204A"))
        ((sexp (1999-12-31 00:00:01.000000-05:00)) (bin_io "\000\000\128(\0286\204A"))
        ((sexp (1999-12-31 00:01:00.000000-05:00)) (bin_io "\000\000\000F\0286\204A"))
        ((sexp (1999-12-31 01:00:00.000000-05:00)) (bin_io "\000\000\0000#6\204A"))
        ((sexp (1999-12-31 12:00:00.000000-05:00)) (bin_io "\000\000\000\136p6\204A"))
        ((sexp (1999-12-31 13:42:23.622933-05:00)) (bin_io "E\188\207\135|6\204A"))
        ((sexp (1999-12-31 23:59:59.999999-05:00))
         (bin_io "\248\255\255\231\1966\204A"))
        ((sexp (2000-01-01 00:00:00.000000-05:00))
         (bin_io "\000\000\000\232\1966\204A"))
        ((sexp (2000-01-01 00:00:00.000001-05:00))
         (bin_io "\b\000\000\232\1966\204A"))
        ((sexp (2000-01-01 00:00:00.001000-05:00)) (bin_io "\197 \000\232\1966\204A"))
        ((sexp (2000-01-01 00:00:01.000000-05:00))
         (bin_io "\000\000\128\232\1966\204A"))
        ((sexp (2000-01-01 00:01:00.000000-05:00))
         (bin_io "\000\000\000\006\1976\204A"))
        ((sexp (2000-01-01 01:00:00.000000-05:00))
         (bin_io "\000\000\000\240\2036\204A"))
        ((sexp (2000-01-01 12:00:00.000000-05:00)) (bin_io "\000\000\000H\0257\204A"))
        ((sexp (2000-01-01 13:42:23.622933-05:00)) (bin_io "E\188\207G%7\204A"))
        ((sexp (2000-01-01 23:59:59.999999-05:00)) (bin_io "\248\255\255\167m7\204A"))
        ((sexp (2000-01-02 00:00:00.000000-05:00)) (bin_io "\000\000\000\168m7\204A"))
        ((sexp (2013-10-07 00:00:00.000000-04:00))
         (bin_io "\000\000\000p\140\148\212A"))
        ((sexp (2013-10-07 00:00:00.000001-04:00))
         (bin_io "\004\000\000p\140\148\212A"))
        ((sexp (2013-10-07 00:00:00.001000-04:00)) (bin_io "b\016\000p\140\148\212A"))
        ((sexp (2013-10-07 00:00:01.000000-04:00)) (bin_io "\000\000@p\140\148\212A"))
        ((sexp (2013-10-07 00:01:00.000000-04:00))
         (bin_io "\000\000\000\127\140\148\212A"))
        ((sexp (2013-10-07 01:00:00.000000-04:00))
         (bin_io "\000\000\000\244\143\148\212A"))
        ((sexp (2013-10-07 12:00:00.000000-04:00))
         (bin_io "\000\000\000\160\182\148\212A"))
        ((sexp (2013-10-07 13:42:23.622933-04:00))
         (bin_io "\"\222\231\159\188\148\212A"))
        ((sexp (2013-10-07 23:59:59.999999-04:00))
         (bin_io "\252\255\255\207\224\148\212A"))
        ((sexp (2013-10-08 00:00:00.000000-04:00))
         (bin_io "\000\000\000\208\224\148\212A"))
        ((sexp (2222-11-22 00:00:00.000000-05:00))
         (bin_io "\000\000\000\181\189\186\253A"))
        ((sexp (2222-11-22 00:00:00.000001-05:00))
         (bin_io "\001\000\000\181\189\186\253A"))
        ((sexp (2222-11-22 00:00:00.001000-05:00))
         (bin_io "\025\004\000\181\189\186\253A"))
        ((sexp (2222-11-22 00:00:01.000000-05:00))
         (bin_io "\000\000\016\181\189\186\253A"))
        ((sexp (2222-11-22 00:01:00.000000-05:00))
         (bin_io "\000\000\192\184\189\186\253A"))
        ((sexp (2222-11-22 01:00:00.000000-05:00))
         (bin_io "\000\000\000\150\190\186\253A"))
        ((sexp (2222-11-22 12:00:00.000000-05:00))
         (bin_io "\000\000\000A\200\186\253A"))
        ((sexp (2222-11-22 13:42:23.622933-05:00))
         (bin_io "\137\247\249\192\201\186\253A"))
        ((sexp (2222-11-22 23:59:59.999999-05:00))
         (bin_io "\255\255\255\204\210\186\253A"))
        ((sexp (2222-11-23 00:00:00.000000-05:00))
         (bin_io "\000\000\000\205\210\186\253A"))
        ((sexp (1665-09-11 05:34:21.163040-04:56:02))
         (bin_io "\024\178\006\206\003\227\001\194"))
        ((sexp (1749-05-17 01:44:58.057413-04:56:02))
         (bin_io "\214\020?\176\186\239\249\193"))
        ((sexp (1799-10-01 16:35:57.622579-04:56:02))
         (bin_io "\235\t\006\187\165\003\244\193"))
        ((sexp (1833-12-02 03:31:51.104713-04:56:02))
         (bin_io "0\166\220n\190\254\239\193"))
        ((sexp (1876-08-10 08:56:53.681209-04:56:02))
         (bin_io "\1383\ni9\245\229\193"))
        ((sexp (1876-08-29 21:01:31.672122-04:56:02))
         (bin_io "\250}J\158\002\242\229\193"))
        ((sexp (1896-03-25 09:54:28.564831-05:00)) (bin_io "\232\236m\203!X\225\193"))
        ((sexp (1952-04-04 18:31:13.284826-05:00))
         (bin_io "\208\138[_\127\175\192\193"))
        ((sexp (2045-05-01 15:40:18.240950-05:00)) (bin_io "\220\181G6O\182\225A"))
        ((sexp (2157-05-06 22:33:36.270771-05:00)) (bin_io "\020U\004)9\006\246A")) |}];
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
