open! Core

let%expect_test "[round_*_to_*] before and after the epoch" =
  let test string =
    let t = Time_ns.of_string_with_utc_offset string in
    let show name f =
      let rounded = f t in
      printf
        "%-18s %s  %s\n"
        name
        (Time_ns.to_string_utc rounded)
        (if Time_ns.equal rounded t
         then "="
         else if Time_ns.( < ) rounded t
         then "<"
         else ">")
    in
    printf "%-18s %s\n" "t" (Time_ns.to_string_utc t);
    show "round_down_to_sec" Time_ns.round_down_to_sec;
    show "round_up_to_sec" Time_ns.round_up_to_sec;
    show "round_down_to_ms" Time_ns.round_down_to_ms;
    show "round_up_to_ms" Time_ns.round_up_to_ms;
    show "round_down_to_us" Time_ns.round_down_to_us;
    show "round_up_to_us" Time_ns.round_up_to_us;
    show "round_down_to 1h" (Time_ns.round_down_to ~interval:Time_ns.Span.hour);
    show "round_up_to 1h" (Time_ns.round_up_to ~interval:Time_ns.Span.hour)
  in
  test "1950-06-15 12:34:56.789123456Z";
  [%expect
    {|
    t                  1950-06-15 12:34:56.789123456Z
    round_down_to_sec  1950-06-15 12:34:56.000000000Z  <
    round_up_to_sec    1950-06-15 12:34:57.000000000Z  >
    round_down_to_ms   1950-06-15 12:34:56.789000000Z  <
    round_up_to_ms     1950-06-15 12:34:56.790000000Z  >
    round_down_to_us   1950-06-15 12:34:56.789123000Z  <
    round_up_to_us     1950-06-15 12:34:56.789124000Z  >
    round_down_to 1h   1950-06-15 12:00:00.000000000Z  <
    round_up_to 1h     1950-06-15 13:00:00.000000000Z  >
    |}];
  test "1969-12-31 23:59:59.5Z";
  [%expect
    {|
    t                  1969-12-31 23:59:59.500000000Z
    round_down_to_sec  1969-12-31 23:59:59.000000000Z  <
    round_up_to_sec    1970-01-01 00:00:00.000000000Z  >
    round_down_to_ms   1969-12-31 23:59:59.500000000Z  =
    round_up_to_ms     1969-12-31 23:59:59.500000000Z  =
    round_down_to_us   1969-12-31 23:59:59.500000000Z  =
    round_up_to_us     1969-12-31 23:59:59.500000000Z  =
    round_down_to 1h   1969-12-31 23:00:00.000000000Z  <
    round_up_to 1h     1970-01-01 00:00:00.000000000Z  >
    |}];
  test "1990-06-15 12:34:56.789123456Z";
  [%expect
    {|
    t                  1990-06-15 12:34:56.789123456Z
    round_down_to_sec  1990-06-15 12:34:56.000000000Z  <
    round_up_to_sec    1990-06-15 12:34:57.000000000Z  >
    round_down_to_ms   1990-06-15 12:34:56.789000000Z  <
    round_up_to_ms     1990-06-15 12:34:56.790000000Z  >
    round_down_to_us   1990-06-15 12:34:56.789123000Z  <
    round_up_to_us     1990-06-15 12:34:56.789124000Z  >
    round_down_to 1h   1990-06-15 12:00:00.000000000Z  <
    round_up_to 1h     1990-06-15 13:00:00.000000000Z  >
    |}]
;;

let%test_unit "[round_down_to] and [round_up_to] are floor and ceiling" =
  let ns span = Time_ns.Span.to_int63_ns span in
  Quickcheck.test
    ~sexp_of:[%sexp_of: Time_ns.Span.t * Time_ns.Span.t]
    (Quickcheck.Generator.tuple2
       (Time_ns.Span.gen_incl
          (Time_ns.to_span_since_epoch Time_ns.min_value_for_1us_rounding)
          (Time_ns.to_span_since_epoch Time_ns.max_value_for_1us_rounding))
       (Quickcheck.Generator.union
          [ Quickcheck.Generator.of_list
              Time_ns.Span.[ nanosecond; microsecond; millisecond; second; hour; day ]
          ; Time_ns.Span.gen_incl Time_ns.Span.nanosecond Time_ns.Span.day
          ]))
    ~f:(fun (span, interval) ->
      let t = Time_ns.of_span_since_epoch span in
      let floor = Int63.(ns span /% ns interval * ns interval) in
      let ceil =
        if Int63.equal floor (ns span) then floor else Int63.(floor + ns interval)
      in
      let check f expect =
        [%test_result: Int63.t] (Time_ns.to_int63_ns_since_epoch (f t)) ~expect
      in
      check (Time_ns.round_down_to ~interval) floor;
      check (Time_ns.round_up_to ~interval) ceil;
      if Time_ns.Span.equal interval Time_ns.Span.microsecond
      then (
        check Time_ns.round_down_to_us floor;
        check Time_ns.round_up_to_us ceil);
      if Time_ns.Span.equal interval Time_ns.Span.millisecond
      then (
        check Time_ns.round_down_to_ms floor;
        check Time_ns.round_up_to_ms ceil);
      if Time_ns.Span.equal interval Time_ns.Span.second
      then (
        check Time_ns.round_down_to_sec floor;
        check Time_ns.round_up_to_sec ceil))
;;

let%expect_test "[round_*_to] still reject nonpositive intervals" =
  let test f =
    match f () with
    | (_ : Time_ns.t) -> print_endline "did not raise"
    | exception exn ->
      (match [%sexp (exn : exn)] with
       | List (Atom message :: _) -> print_endline message
       | sexp -> print_s sexp)
  in
  test (fun () -> Time_ns.round_down_to Time_ns.epoch ~interval:Time_ns.Span.zero);
  [%expect {| Time_ns.round_down_to got nonpositive interval |}];
  test (fun () ->
    Time_ns.round_up_to Time_ns.epoch ~interval:(Time_ns.Span.neg Time_ns.Span.second));
  [%expect {| Time_ns.round_up_to got nonpositive interval |}]
;;
