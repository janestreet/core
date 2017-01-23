open Core.Std

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
      let open Quickcheck.Generator in
      List.gen' ~length Char.gen_digit
      >>| String.of_char_list

    let%test_unit "add colon" =
      let gen = gen_digit_string ~length:(`Between_inclusive (3, 4)) in
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
      let gen_prefix = gen_digit_string ~length:(`Between_inclusive (1, 2)) in
      let gen_suffix = gen_digit_string ~length:(`Exactly 2) in
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
    let local_zone_name = "Europe/London"
    let local           = Zone.find_exn local_zone_name

    let%test_unit _ =
      let t = of_string_abs ("2015-01-01 10:00:00 " ^ local_zone_name) in
      [%test_result: string] ~expect:"2015-01-01 10:00:00"
        (format ~zone:local t "%Y-%m-%d %H:%M:%S")

    let%test_unit _ =
      let t = of_string_abs ("2015-06-06 10:00:00 " ^ local_zone_name) in
      [%test_result: string] ~expect:"2015-06-06 17:00:00"
        (format ~zone:(Zone.find_exn "Asia/Hong_Kong") t "%Y-%m-%d %H:%M:%S")

    let%test_unit _ =
      let t = of_string_abs ("2015-01-01 10:00:00 " ^ local_zone_name) in
      [%test_result: string] ~expect:"2015-01-01 18:00:00"
        (format ~zone:(Zone.find_exn "Asia/Hong_Kong") t "%Y-%m-%d %H:%M:%S")
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
