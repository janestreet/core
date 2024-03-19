open! Core
open Expect_test_helpers_core

type time_ns = Time_ns.t [@@deriving compare]

let sexp_of_time_ns = Time_ns.Alternate_sexp.sexp_of_t

let quickcheck_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%map ns_since_epoch =
    Int63.gen_incl
      (Time_ns.to_int63_ns_since_epoch Time_ns.min_value_for_1us_rounding)
      (Time_ns.to_int63_ns_since_epoch Time_ns.max_value_for_1us_rounding)
  in
  Time_ns.of_int63_ns_since_epoch ns_since_epoch
;;

let randomly_round quickcheck_generator =
  let open Quickcheck.Generator.Let_syntax in
  let%bind time_ns = quickcheck_generator in
  let%map unit =
    Quickcheck.Generator.of_list
      [ Time_ns.Span.second
      ; Time_ns.Span.of_int_ms 100
      ; Time_ns.Span.of_int_ms 10
      ; Time_ns.Span.millisecond
      ; Time_ns.Span.of_int_us 100
      ; Time_ns.Span.of_int_us 10
      ; Time_ns.Span.microsecond
      ; Time_ns.Span.of_int63_ns (Int63.of_int 100)
      ; Time_ns.Span.of_int63_ns (Int63.of_int 10)
      ; Time_ns.Span.nanosecond
      ]
  in
  let span_ns = Time_ns.to_span_since_epoch time_ns in
  let rounded_span_ns = Time_ns.Span.scale_int63 unit (Time_ns.Span.div span_ns unit) in
  Time_ns.of_span_since_epoch rounded_span_ns
;;

let quickcheck here quickcheck_generator f =
  require_does_not_raise here (fun () ->
    Quickcheck.test
      quickcheck_generator
      ~f
      ~sexp_of:[%sexp_of: time_ns]
      ~examples:
        [ Time_ns.min_value_for_1us_rounding
        ; Time_ns.epoch
        ; Time_ns.max_value_for_1us_rounding
        ])
;;

let%expect_test "Time_ns.Span.Stable.V1" =
  let module V = Time_ns.Span.Stable.V1 in
  let make int64 = V.of_int63_exn (Int63.of_int64_exn int64) in
  (* stable checks for values that round-trip *)
  print_and_check_stable_int63able_type
    [%here]
    (module V)
    [ make 0L
    ; make 1_000L
    ; make 1_000_000_000L
    ; make 1_234_560_000_000L
    ; make 71_623_008_000_000_000L
    ; make 80_000_006_400_000_000L
    ; make 1L
    ];
  [%expect
    {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp   0s)
     (bin_io "\000")
     (int63  0))
    ((sexp   0.001ms)
     (bin_io "\254\232\003")
     (int63  1_000))
    ((sexp   1s)
     (bin_io "\253\000\202\154;")
     (int63  1_000_000_000))
    ((sexp   20.576m)
     (bin_io "\252\000\160\130q\031\001\000\000")
     (int63  1_234_560_000_000))
    ((sexp   828.97d)
     (bin_io "\252\000\192\149\r\191t\254\000")
     (int63  71_623_008_000_000_000))
    ((sexp   925.926d)
     (bin_io "\252\000@\128\251\1487\028\001")
     (int63  80_000_006_400_000_000))
    ((sexp   1e-06ms)
     (bin_io "\001")
     (int63  1))
    |}];
  (* stable checks for values that do not precisely round-trip *)
  print_and_check_stable_int63able_type
    [%here]
    (module V)
    ~cr:Comment
    [ make 11_275_440_000L ];
  [%expect
    {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp   11.2754s)
     (bin_io "\252\128\143\017\160\002\000\000\000")
     (int63  11_275_440_000))
    (* require-failed: lib/core/test/test_time_ns.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       11.2754s)
      (sexp           11.2754s)
      (sexp_roundtrip 11.2754s))
    |}];
  (* make sure [of_int63_exn] allows all values *)
  show_raise ~hide_positions:true (fun () -> V.of_int63_exn (Int63.succ Int63.min_value));
  [%expect {| "did not raise" |}]
;;

let%test_module "Time_ns.Span.Stable.V2" =
  (module struct
    module V = Time_ns.Span.Stable.V2

    let span_gen =
      Quickcheck.Generator.map Int63.quickcheck_generator ~f:Time_ns.Span.of_int63_ns
    ;;

    let span_examples =
      let scales_of unit_of_time =
        match (unit_of_time : Unit_of_time.t) with
        | Nanosecond | Millisecond | Microsecond -> [ 0; 1; 10; 100; 999 ]
        | Second | Minute -> [ 0; 1; 10; 59 ]
        | Hour -> [ 0; 1; 10; 23 ]
        | Day -> [ 0; 1; 10; 100; 1_000; 10_000; 36_500 ]
      in
      let multiples_of unit_of_time =
        let span = Time_ns.Span.of_unit_of_time unit_of_time in
        List.map (scales_of unit_of_time) ~f:(fun scale ->
          Time_ns.Span.scale_int span scale)
      in
      List.fold Unit_of_time.all ~init:[ Time_ns.Span.zero ] ~f:(fun spans unit_of_time ->
        List.concat_map spans ~f:(fun span ->
          List.map (multiples_of unit_of_time) ~f:(fun addend ->
            Time_ns.Span.( + ) span addend)))
    ;;

    let%expect_test "round-trip" =
      Expect_test_helpers_core.quickcheck
        [%here]
        ~sexp_of:Time_ns.Span.sexp_of_t
        ~examples:span_examples
        span_gen
        ~f:(fun span ->
        let rt = V.t_of_sexp (V.sexp_of_t span) in
        require_equal [%here] (module Time_ns.Span) span rt;
        let rt = V.of_int63_exn (V.to_int63 span) in
        require_equal [%here] (module Time_ns.Span) span rt);
      [%expect {| |}]
    ;;

    let%expect_test "stability" =
      let make int64 = V.of_int63_exn (Int63.of_int64_exn int64) in
      print_and_check_stable_int63able_type
        [%here]
        (module V)
        [ make 0L
        ; make 1L
        ; make (-499L)
        ; make 500L
        ; make (-1_000L)
        ; make 987_654_321L
        ; make (-123_456_789_012L)
        ; make 52_200_010_101_101L
        ; make (-86_399_999_999_999L)
        ; make 86_400_000_000_000L
        ; make (-1_000_000_222_000_333L)
        ; make 80_000_006_400_000_000L
        ; make (-1_381_156_200_010_101_000L)
        ; make 4_110_307_199_999_999_000L
        ; make (Int63.to_int64 Int63.max_value)
        ; make (Int63.to_int64 Int63.min_value)
        ];
      [%expect
        {|
        (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
        ((sexp   0s)
         (bin_io "\000")
         (int63  0))
        ((sexp   1ns)
         (bin_io "\001")
         (int63  1))
        ((sexp   -499ns)
         (bin_io "\254\r\254")
         (int63  -499))
        ((sexp   500ns)
         (bin_io "\254\244\001")
         (int63  500))
        ((sexp   -1us)
         (bin_io "\254\024\252")
         (int63  -1_000))
        ((sexp   987.654321ms)
         (bin_io "\253\177h\222:")
         (int63  987_654_321))
        ((sexp   -2m3.456789012s)
         (bin_io "\252\236\229fA\227\255\255\255")
         (int63  -123_456_789_012))
        ((sexp   14h30m10.101101ms)
         (bin_io "\252m1\015\195y/\000\000")
         (int63  52_200_010_101_101))
        ((sexp   -23h59m59.999999999s)
         (bin_io "\252\001\000\177nk\177\255\255")
         (int63  -86_399_999_999_999))
        ((sexp   1d)
         (bin_io "\252\000\000O\145\148N\000\000")
         (int63  86_400_000_000_000))
        ((sexp   -11d13h46m40.222000333s)
         (bin_io "\2523\011\254M\129r\252\255")
         (int63  -1_000_000_222_000_333))
        ((sexp   925d22h13m26.4s)
         (bin_io "\252\000@\128\251\1487\028\001")
         (int63  80_000_006_400_000_000))
        ((sexp   -15985d14h30m10.101ms)
         (bin_io "\252\248\206\017\247\192%\213\236")
         (int63  -1_381_156_200_010_101_000))
        ((sexp   47572d23h59m59.999999s)
         (bin_io "\252\024\252\186\253\158\190\n9")
         (int63  4_110_307_199_999_999_000))
        ((sexp   53375d23h53m38.427387903s)
         (bin_io "\252\255\255\255\255\255\255\255?")
         (int63  4_611_686_018_427_387_903))
        ((sexp   -53375d23h53m38.427387904s)
         (bin_io "\252\000\000\000\000\000\000\000\192")
         (int63  -4_611_686_018_427_387_904))
        |}]
    ;;
  end)
;;

let%test_module "Time_ns.Alternate_sexp and Time_ns.Option.Alternate_sexp" =
  (module struct
    module Span = Time_ns.Span

    let epoch_date = Date.create_exn ~y:1970 ~m:Jan ~d:1

    let make ?(h = 0) ?(m = 0) ?(s = 0) ?(ms = 0) ?(us = 0) ?(ns = 0) date =
      let d = Date.diff (Date.of_string date) epoch_date in
      let spans =
        [ Span.scale_int Span.day d
        ; Span.scale_int Span.hour h
        ; Span.scale_int Span.minute m
        ; Span.scale_int Span.second s
        ; Span.scale_int Span.millisecond ms
        ; Span.scale_int Span.microsecond us
        ; Span.scale_int Span.nanosecond ns
        ]
      in
      let span = List.fold ~init:Span.zero ~f:Span.( + ) spans in
      Time_ns.of_span_since_epoch span
    ;;

    let%expect_test "sexp format" =
      let examples =
        [ Time_ns.min_value_for_1us_rounding
        ; Time_ns.max_value_for_1us_rounding
        ; Time_ns.epoch
        ; make "2001-01-01"
        ; make "2001-01-01" ~h:16 ~m:23 ~s:42
        ; make "2013-10-07" ~h:09 ~m:14 ~s:47 ~ms:999 ~us:749 ~ns:999
        ; make "2013-10-07" ~h:09 ~m:14 ~s:47 ~ms:999 ~us:750 ~ns:000
        ; make "2013-10-07" ~h:09 ~m:14 ~s:47 ~ms:999 ~us:750 ~ns:001
        ]
      in
      List.iter examples ~f:(fun time_ns ->
        print_s [%sexp (time_ns : Time_ns.Alternate_sexp.t)];
        let round_trip =
          Time_ns.Alternate_sexp.t_of_sexp (Time_ns.Alternate_sexp.sexp_of_t time_ns)
        in
        require
          [%here]
          (Time_ns.equal time_ns round_trip)
          ~if_false_then_print_s:
            (lazy
              [%message
                "Time_ns.Alternate_sexp round-trip failed"
                  (time_ns : time_ns)
                  (round_trip : time_ns)]));
      [%expect
        {|
        "1835-02-03 00:00:00Z"
        "2104-11-29 00:00:00Z"
        "1970-01-01 00:00:00Z"
        "2001-01-01 00:00:00Z"
        "2001-01-01 16:23:42Z"
        "2013-10-07 09:14:47.999749999Z"
        "2013-10-07 09:14:47.99975Z"
        "2013-10-07 09:14:47.999750001Z"
        |}];
      let option_examples =
        Time_ns.Option.none :: List.map examples ~f:Time_ns.Option.some
      in
      List.iter option_examples ~f:(fun time_ns_opt ->
        print_s [%sexp (time_ns_opt : Time_ns.Option.Alternate_sexp.t)];
        let round_trip =
          Time_ns.Option.Alternate_sexp.t_of_sexp
            (Time_ns.Option.Alternate_sexp.sexp_of_t time_ns_opt)
        in
        require
          [%here]
          (Time_ns.Option.equal time_ns_opt round_trip)
          ~if_false_then_print_s:
            (lazy
              [%message
                "Time_ns.Option.Alternate_sexp round-trip failed"
                  (time_ns_opt : Time_ns.Option.Alternate_sexp.t)
                  (round_trip : Time_ns.Option.Alternate_sexp.t)]));
      [%expect
        {|
        ()
        ("1835-02-03 00:00:00Z")
        ("2104-11-29 00:00:00Z")
        ("1970-01-01 00:00:00Z")
        ("2001-01-01 00:00:00Z")
        ("2001-01-01 16:23:42Z")
        ("2013-10-07 09:14:47.999749999Z")
        ("2013-10-07 09:14:47.99975Z")
        ("2013-10-07 09:14:47.999750001Z")
        |}]
    ;;

    let%expect_test "round-trip" =
      (* randomly round spans to make sure we round-trip at various precisions, since it
         affects number of decimal places *)
      quickcheck [%here] (randomly_round quickcheck_generator) (fun time_ns ->
        [%test_result: time_ns]
          (Time_ns.Alternate_sexp.t_of_sexp (Time_ns.Alternate_sexp.sexp_of_t time_ns))
          ~expect:time_ns);
      [%expect {| |}]
    ;;

    let%expect_test "validate sexp grammar" =
      require_ok
        [%here]
        (Sexp_grammar_validation.validate_grammar
           (module struct
             include Time_ns.Alternate_sexp

             let quickcheck_generator = quickcheck_generator
             let quickcheck_shrinker = Quickcheck.Shrinker.empty ()
           end));
      [%expect
        {|
        (Tagged
         ((key sexp_grammar.type_name)
          (value Core.Time_ns.Alternate_sexp.t)
          (grammar String)))
        |}]
    ;;
  end)
;;

let%test_module "Time_ns.Utc.to_date_and_span_since_start_of_day" =
  (module struct
    let%expect_test "span is non-negative and less than 1 day" =
      quickcheck [%here] quickcheck_generator (fun time_ns ->
        let date, span_since_start_of_day =
          Core.Time_ns.Utc.to_date_and_span_since_start_of_day time_ns
        in
        if Time_ns.Span.( < ) span_since_start_of_day Time_ns.Span.zero
           || Time_ns.Span.( >= ) span_since_start_of_day Time_ns.Span.day
        then
          raise_s
            [%message
              "span_since_start_of_day is out of bounds"
                (time_ns : time_ns)
                (date : Date.t)
                (span_since_start_of_day : Time_ns.Span.t)]);
      [%expect {| |}]
    ;;

    let%expect_test "round-trip" =
      quickcheck [%here] quickcheck_generator (fun time_ns ->
        let date, span_since_start_of_day =
          Core.Time_ns.Utc.to_date_and_span_since_start_of_day time_ns
        in
        let round_trip_time_ns =
          Time_ns.Utc.of_date_and_span_since_start_of_day date span_since_start_of_day
        in
        [%test_result: time_ns] round_trip_time_ns ~expect:time_ns);
      [%expect {| |}]
    ;;
  end)
;;

let%test_module "Time_ns.Span rounding" =
  (module struct
    open Time_ns.Span

    let%test_unit "Span.round_down" =
      [%test_result: t]
        (round_down ~to_multiple_of:nanosecond nanosecond)
        ~expect:nanosecond;
      [%test_result: t] (round_down ~to_multiple_of:second nanosecond) ~expect:zero;
      [%test_result: t] (round_down ~to_multiple_of:second second) ~expect:second;
      [%test_result: t] (round_down ~to_multiple_of:second hour) ~expect:hour;
      [%test_result: t] (round_down ~to_multiple_of:second (of_sec 1.6)) ~expect:second;
      [%test_result: t] (round_down ~to_multiple_of:minute (of_sec 30.)) ~expect:zero;
      [%test_result: t] (round_down ~to_multiple_of:hour (of_day 1.1)) ~expect:(of_hr 26.)
    ;;

    let%test_unit "Span.round_up" =
      [%test_result: t]
        (round_up ~to_multiple_of:nanosecond nanosecond)
        ~expect:nanosecond;
      [%test_result: t] (round_up ~to_multiple_of:second nanosecond) ~expect:second;
      [%test_result: t] (round_up ~to_multiple_of:second second) ~expect:second;
      [%test_result: t] (round_up ~to_multiple_of:second hour) ~expect:hour;
      [%test_result: t] (round_up ~to_multiple_of:second (of_sec 1.6)) ~expect:(of_sec 2.);
      [%test_result: t] (round_up ~to_multiple_of:minute (of_sec 30.)) ~expect:minute;
      [%test_result: t] (round_up ~to_multiple_of:hour (of_day 1.1)) ~expect:(of_hr 27.)
    ;;

    let%test_unit "Span.round_nearest" =
      [%test_result: t]
        (round_nearest ~to_multiple_of:nanosecond nanosecond)
        ~expect:nanosecond;
      [%test_result: t] (round_nearest ~to_multiple_of:second nanosecond) ~expect:zero;
      [%test_result: t] (round_nearest ~to_multiple_of:second second) ~expect:second;
      [%test_result: t] (round_nearest ~to_multiple_of:second hour) ~expect:hour;
      [%test_result: t]
        (round_nearest ~to_multiple_of:second (of_sec 1.6))
        ~expect:(of_sec 2.);
      [%test_result: t] (round_nearest ~to_multiple_of:minute (of_sec 30.)) ~expect:minute;
      [%test_result: t]
        (round_nearest ~to_multiple_of:hour (of_day 1.1))
        ~expect:(of_hr 26.)
    ;;

    let%expect_test "quickcheck rounding" =
      quickcheck_m
        [%here]
        (module struct
          type t = Time_ns.Span.t * Unit_of_time.t * Rounding_direction.t
          [@@deriving sexp_of]

          let quickcheck_shrinker = Quickcheck.Shrinker.empty ()

          let quickcheck_generator =
            let decade = Time_ns.Span.of_int_day 36525 in
            Quickcheck.Generator.tuple3
              (Time_ns.Span.gen_uniform_incl (Time_ns.Span.neg decade) decade)
              (Quickcheck.Generator.of_list Unit_of_time.all)
              (Quickcheck.Generator.of_list Rounding_direction.all)
          ;;
        end)
        ~f:(fun (span, unit_of_time, dir) ->
          let to_multiple_of = of_unit_of_time unit_of_time in
          let rounded = round span ~dir ~to_multiple_of in
          let expect =
            match dir with
            | Up -> round_up span ~to_multiple_of
            | Down -> round_down span ~to_multiple_of
            | Zero -> round_towards_zero span ~to_multiple_of
            | Nearest -> round_nearest span ~to_multiple_of
          in
          let remainder_ns =
            Int63.rem (to_int63_ns rounded) (to_int63_ns to_multiple_of)
          in
          let if_false_then_print_s =
            [%lazy_message
              "" (to_multiple_of : t) (rounded : t) (expect : t) (remainder_ns : Int63.t)]
          in
          require_equal
            [%here]
            (module Time_ns.Span)
            rounded
            expect
            ~if_false_then_print_s;
          require [%here] (abs (span - rounded) < to_multiple_of) ~if_false_then_print_s;
          require [%here] (Int63.equal remainder_ns Int63.zero) ~if_false_then_print_s;
          match dir with
          | Up -> require [%here] (rounded >= span) ~if_false_then_print_s
          | Down -> require [%here] (rounded <= span) ~if_false_then_print_s
          | Zero -> require [%here] (abs rounded <= abs span) ~if_false_then_print_s
          | Nearest -> ());
      [%expect {| |}]
    ;;
  end)
;;

let succ time_ns =
  time_ns
  |> Time_ns.to_int63_ns_since_epoch
  |> Int63.succ
  |> Time_ns.of_int63_ns_since_epoch
;;

let pred time_ns =
  time_ns
  |> Time_ns.to_int63_ns_since_epoch
  |> Int63.pred
  |> Time_ns.of_int63_ns_since_epoch
;;

let%expect_test "Stable.Alternate_sexp.V1 and Stable.Option.Alternate_sexp.V1" =
  let times =
    [ Time_ns.min_value_for_1us_rounding
    ; Time_ns.min_value_for_1us_rounding |> succ
    ; Time_ns.min_value_for_1us_rounding |> succ |> succ
    ; Time_ns.epoch |> pred
    ; Time_ns.epoch
    ; Time_ns.epoch |> succ
    ; Time_ns.of_int63_ns_since_epoch (Int63.of_int64_exn 999_999_999_999_999_999L)
    ; Time_ns.of_int63_ns_since_epoch (Int63.of_int64_exn 1_000_000_000_000_000_000L)
    ; Time_ns.of_int63_ns_since_epoch (Int63.of_int64_exn 1_000_000_000_000_000_001L)
    ; Time_ns.max_value_for_1us_rounding |> pred |> pred
    ; Time_ns.max_value_for_1us_rounding |> pred
    ; Time_ns.max_value_for_1us_rounding
    ]
  in
  print_and_check_stable_type [%here] (module Time_ns.Stable.Alternate_sexp.V1) times;
  [%expect
    {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp   "1835-02-03 00:00:00Z")
     (bin_io "\252\000\000\011\239\162\209\234\196"))
    ((sexp   "1835-02-03 00:00:00.000000001Z")
     (bin_io "\252\001\000\011\239\162\209\234\196"))
    ((sexp   "1835-02-03 00:00:00.000000002Z")
     (bin_io "\252\002\000\011\239\162\209\234\196"))
    ((sexp   "1969-12-31 23:59:59.999999999Z")
     (bin_io "\255\255"))
    ((sexp   "1970-01-01 00:00:00Z")
     (bin_io "\000"))
    ((sexp   "1970-01-01 00:00:00.000000001Z")
     (bin_io "\001"))
    ((sexp   "2001-09-09 01:46:39.999999999Z")
     (bin_io "\252\255\255c\167\179\182\224\r"))
    ((sexp   "2001-09-09 01:46:40Z")
     (bin_io "\252\000\000d\167\179\182\224\r"))
    ((sexp   "2001-09-09 01:46:40.000000001Z")
     (bin_io "\252\001\000d\167\179\182\224\r"))
    ((sexp   "2104-11-28 23:59:59.999999998Z")
     (bin_io "\252\254\255\244\016].\021;"))
    ((sexp   "2104-11-28 23:59:59.999999999Z")
     (bin_io "\252\255\255\244\016].\021;"))
    ((sexp   "2104-11-29 00:00:00Z")
     (bin_io "\252\000\000\245\016].\021;"))
    |}];
  let option_times = Time_ns.Option.none :: List.map times ~f:Time_ns.Option.some in
  print_and_check_stable_type
    [%here]
    (module Time_ns.Stable.Option.Alternate_sexp.V1)
    option_times;
  [%expect
    {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp ()) (bin_io "\252\000\000\000\000\000\000\000\192"))
    ((sexp ("1835-02-03 00:00:00Z"))
     (bin_io "\252\000\000\011\239\162\209\234\196"))
    ((sexp ("1835-02-03 00:00:00.000000001Z"))
     (bin_io "\252\001\000\011\239\162\209\234\196"))
    ((sexp ("1835-02-03 00:00:00.000000002Z"))
     (bin_io "\252\002\000\011\239\162\209\234\196"))
    ((sexp ("1969-12-31 23:59:59.999999999Z")) (bin_io "\255\255"))
    ((sexp ("1970-01-01 00:00:00Z")) (bin_io "\000"))
    ((sexp ("1970-01-01 00:00:00.000000001Z")) (bin_io "\001"))
    ((sexp ("2001-09-09 01:46:39.999999999Z"))
     (bin_io "\252\255\255c\167\179\182\224\r"))
    ((sexp ("2001-09-09 01:46:40Z")) (bin_io "\252\000\000d\167\179\182\224\r"))
    ((sexp ("2001-09-09 01:46:40.000000001Z"))
     (bin_io "\252\001\000d\167\179\182\224\r"))
    ((sexp ("2104-11-28 23:59:59.999999998Z"))
     (bin_io "\252\254\255\244\016].\021;"))
    ((sexp ("2104-11-28 23:59:59.999999999Z"))
     (bin_io "\252\255\255\244\016].\021;"))
    ((sexp ("2104-11-29 00:00:00Z")) (bin_io "\252\000\000\245\016].\021;"))
    |}]
;;

let%test_module "Ofday" =
  (module struct
    let%expect_test "of_string_iso8601_extended" =
      let success string =
        require_does_not_raise [%here] (fun () ->
          printf
            "%s <-- %s\n"
            (Time_ns.Ofday.to_string (Time_ns.Ofday.of_string_iso8601_extended string))
            string)
      in
      List.iter
        ~f:success
        [ (* normal times *)
          "12"
        ; "12:34"
        ; "12:34:56"
        ; "12:34:56.789"
        ; "12:34:56.789123"
        ; "12:34:56.789123456"
        ; (* lower boundary case *)
          "00"
        ; "00:00"
        ; "00:00:00"
        ; "00:00:00.000"
        ; "00:00:00.000000"
        ; "00:00:00.000000000"
        ; (* upper boundary case *)
          "23"
        ; "23:59"
        ; "23:59:59"
        ; "23:59:59.999"
        ; "23:59:59.999999"
        ; "23:59:59.999999999"
        ; (* midnight tomorrow *)
          "24"
        ; "24:00"
        ; "24:00:00"
        ; "24:00:00.000"
        ; "24:00:00.000000"
        ; "24:00:00.000000000"
        ; (* leap second *)
          "12:59:60"
        ; "12:59:60.789"
        ; "12:59:60.789123"
        ; "12:59:60.789123456"
        ];
      [%expect
        {|
        12:00:00.000000000 <-- 12
        12:34:00.000000000 <-- 12:34
        12:34:56.000000000 <-- 12:34:56
        12:34:56.789000000 <-- 12:34:56.789
        12:34:56.789123000 <-- 12:34:56.789123
        12:34:56.789123456 <-- 12:34:56.789123456
        00:00:00.000000000 <-- 00
        00:00:00.000000000 <-- 00:00
        00:00:00.000000000 <-- 00:00:00
        00:00:00.000000000 <-- 00:00:00.000
        00:00:00.000000000 <-- 00:00:00.000000
        00:00:00.000000000 <-- 00:00:00.000000000
        23:00:00.000000000 <-- 23
        23:59:00.000000000 <-- 23:59
        23:59:59.000000000 <-- 23:59:59
        23:59:59.999000000 <-- 23:59:59.999
        23:59:59.999999000 <-- 23:59:59.999999
        23:59:59.999999999 <-- 23:59:59.999999999
        24:00:00.000000000 <-- 24
        24:00:00.000000000 <-- 24:00
        24:00:00.000000000 <-- 24:00:00
        24:00:00.000000000 <-- 24:00:00.000
        24:00:00.000000000 <-- 24:00:00.000000
        24:00:00.000000000 <-- 24:00:00.000000000
        13:00:00.000000000 <-- 12:59:60
        13:00:00.000000000 <-- 12:59:60.789
        13:00:00.000000000 <-- 12:59:60.789123
        13:00:00.000000000 <-- 12:59:60.789123456
        |}];
      let failure string =
        match Time_ns.Ofday.of_string_iso8601_extended string with
        | exception exn -> print_endline (Exn.to_string exn)
        | ofday ->
          print_cr
            [%here]
            [%message "did not raise" (string : string) (ofday : Time_ns.Ofday.t)]
      in
      List.iter
        ~f:failure
        [ (* bad syntax *)
          ""
        ; "1"
        ; "123"
        ; ":"
        ; "12:"
        ; "1:23"
        ; "12:3"
        ; "12:345"
        ; "12:34:"
        ; "12:34:5"
        ; (* numerical bounds *)
          "25:00"
        ; "00:60"
        ; "00:59:61"
        ];
      [%expect
        {|
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" ""
          (Failure "len < 2"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" 1
          (Failure "len < 2"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" 123
          (Failure "2 < len < 5"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" :
          (Failure "len < 2"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" 12:
          (Failure "2 < len < 5"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" 1:23
          (Failure "Char.get_digit_exn ':': not a digit"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" 12:3
          (Failure "2 < len < 5"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" 12:345
          (Failure "5 < len < 8"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" 12:34:
          (Failure "5 < len < 8"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" 12:34:5
          (Failure "5 < len < 8"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" 25:00
          (Failure "hour > 24"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" 00:60
          (Failure "minute > 60"))
        ("Time_ns.Ofday.of_string_iso8601_extended: cannot parse string" 00:59:61
          (Failure "invalid second: 61"))
        |}]
    ;;

    let%expect_test "every" =
      let open Time_ns in
      let test ?(verbose = true) span start stop =
        let result = Ofday.every span ~start ~stop in
        if verbose then print_s [%sexp (result : Ofday.t list Or_error.t)];
        let crossed_bounds = Ofday.( > ) start stop in
        let non_positive_span = Span.( <= ) span Span.zero in
        let should_be_error = crossed_bounds || non_positive_span in
        match result with
        | Error _ ->
          require
            [%here]
            should_be_error
            ~if_false_then_print_s:
              (lazy
                [%message
                  "should have produced Ok"
                    (crossed_bounds : bool)
                    (non_positive_span : bool)])
        | Ok list ->
          require
            [%here]
            (not should_be_error)
            ~if_false_then_print_s:
              (lazy
                [%message
                  "should have produced Error"
                    (crossed_bounds : bool)
                    (non_positive_span : bool)]);
          require
            [%here]
            (List.is_sorted list ~compare:Ofday.compare)
            ~if_false_then_print_s:(lazy [%message "not sorted"]);
          require
            [%here]
            (List.for_all list ~f:(fun ofday ->
               Ofday.( >= ) ofday start && Ofday.( <= ) ofday stop))
            ~if_false_then_print_s:(lazy [%message "exceeds bounds"])
      in
      let hour = Span.hour in
      let ten_min = Span.of_min 10.0 in
      let sod = Ofday.start_of_day in
      let eod = Ofday.approximate_end_of_day in
      test hour eod sod;
      [%expect
        {|
        (Error (
          "[Time_ns.Ofday.every] called with [start] > [stop]"
          (start 23:59:59.999999999)
          (stop  00:00:00.000000000)))
        |}];
      test hour sod eod;
      [%expect
        {|
        (Ok (
          00:00:00.000000000
          01:00:00.000000000
          02:00:00.000000000
          03:00:00.000000000
          04:00:00.000000000
          05:00:00.000000000
          06:00:00.000000000
          07:00:00.000000000
          08:00:00.000000000
          09:00:00.000000000
          10:00:00.000000000
          11:00:00.000000000
          12:00:00.000000000
          13:00:00.000000000
          14:00:00.000000000
          15:00:00.000000000
          16:00:00.000000000
          17:00:00.000000000
          18:00:00.000000000
          19:00:00.000000000
          20:00:00.000000000
          21:00:00.000000000
          22:00:00.000000000
          23:00:00.000000000))
        |}];
      test ten_min sod (Ofday.of_string "00:20");
      [%expect {| (Ok (00:00:00.000000000 00:10:00.000000000 00:20:00.000000000)) |}];
      test ten_min sod (Ofday.of_string "00:25");
      [%expect {| (Ok (00:00:00.000000000 00:10:00.000000000 00:20:00.000000000)) |}];
      test hour sod (Ofday.of_string "00:25");
      [%expect {| (Ok (00:00:00.000000000)) |}];
      test hour sod sod;
      [%expect {| (Ok (00:00:00.000000000)) |}];
      test (Span.of_hr 25.) sod eod;
      [%expect {| (Ok (00:00:00.000000000)) |}];
      test Span.max_value_representable sod eod;
      [%expect {| (Ok (00:00:00.000000000)) |}];
      test Span.min_value_representable sod eod;
      [%expect
        {|
        (Error (
          "[Time_ns.Ofday.every] called with negative span"
          -53375d23h53m38.427387904s))
        |}];
      let span_gen =
        (* avoid intervals so small we generate, e.g., millions of values *)
        Quickcheck.Generator.filter Span.quickcheck_generator ~f:(fun span ->
          Span.( >= ) (Span.abs span) Span.second)
      in
      Expect_test_helpers_base.quickcheck
        [%here]
        [%quickcheck.generator: [%custom span_gen] * Ofday.t * Ofday.t]
        ~sexp_of:[%sexp_of: Span.t * Ofday.t * Ofday.t]
        ~f:(fun (span, start, stop) -> test ~verbose:false span start stop);
      [%expect {| |}]
    ;;
  end)
;;

module _ = struct
  open! Time_ns.Span

  let%test (_ [@tags "64-bits-only"]) =
    Int.( > ) (to_int_sec (of_int63_ns Int63.max_value)) 0
  ;;

  (* and doesn't raise *)

  let%test_module "overflow silently" =
    (module struct
      let doesn't_raise = Fn.non (Exn.does_raise :> _ -> _)

      let%test "+ range up" =
        doesn't_raise (fun () -> max_value_for_1us_rounding + nanosecond)
      ;;

      let%test "+ range down" =
        doesn't_raise (fun () -> min_value_for_1us_rounding + neg nanosecond)
      ;;

      let%test "+ overflow" =
        doesn't_raise (fun () -> max_value_for_1us_rounding + max_value_for_1us_rounding)
      ;;

      let%test "+ underflow" =
        doesn't_raise (fun () -> min_value_for_1us_rounding + min_value_for_1us_rounding)
      ;;

      let%test "- range down" =
        doesn't_raise (fun () -> min_value_for_1us_rounding - nanosecond)
      ;;

      let%test "- range up" =
        doesn't_raise (fun () -> max_value_for_1us_rounding - neg nanosecond)
      ;;

      let%test "- underflow" =
        doesn't_raise (fun () -> min_value_for_1us_rounding - max_value_for_1us_rounding)
      ;;

      let%test "- overflow" =
        doesn't_raise (fun () -> max_value_for_1us_rounding - min_value_for_1us_rounding)
      ;;

      let%test_module "intermediate ( * )" =
        (module struct
          let wrap_days =
            let margin_ns =
              Int63.( - ) (to_int63_ns min_value_for_1us_rounding) Int63.min_value
            in
            Int63.(
              (max_value / to_int63_ns day) + one + (margin_ns / to_int63_ns day) + one)
          ;;

          let%test_unit "wrap_days" =
            [%test_pred: Int63.t]
              (Int63.between
                 ~low:(to_int63_ns min_value_for_1us_rounding)
                 ~high:(Int63.neg (to_int63_ns nanosecond)))
              Int63.(wrap_days * to_int63_ns day)
          ;;

          let wrap_days_int = Int63.to_int_exn wrap_days
          let%test "scale_int63" = doesn't_raise (fun () -> scale_int63 day wrap_days)
          let%test "scale_int" = doesn't_raise (fun () -> scale_int day wrap_days_int)
          let%test "create" = doesn't_raise (fun () -> create () ~day:wrap_days_int)
        end)
      ;;
    end)
  ;;

  let%test_unit "int63-negative-division-by-positive-truncates-towards-zero" =
    let open Int63 in
    assert (of_int (-7) / of_int 4 = of_int (-1))
  ;;

  let%test_module _ =
    (module struct
      module Parts = Time_ns.Span.Parts

      let ( * ) = Int63.( * )
      let of_int = Int63.of_int
      let round_trip t = [%test_result: t] (Private.of_parts (to_parts t)) ~expect:t

      let eq t expect =
        [%test_result: t] t ~expect;
        [%test_result: Parts.t] (to_parts t) ~expect:(to_parts expect);
        round_trip t
      ;;

      let scale (i : int) t = of_int63_ns (of_int i * to_int63_ns t)
      let%test_unit _ = eq (create ~us:2 ()) (scale 2 microsecond)
      let%test_unit _ = eq (create ~min:3 ()) (scale 3 minute)
      let%test_unit _ = eq (create ~ms:4 ()) (scale 4 millisecond)
      let%test_unit _ = eq (create ~sec:5 ()) (scale 5 second)
      let%test_unit _ = eq (create ~hr:6 ()) (scale 6 hour)
      let%test_unit _ = eq (create ~day:7 ()) (scale 7 day)
      let%test_unit _ = eq (create ~us:8 ~sign:Neg ()) (scale (-8) microsecond)
      let%test_unit _ = eq (create ~ms:9 ~sign:Zero ()) (scale 9 millisecond)

      let%test_unit _ =
        eq
          (create ~us:3 ~ns:242 () |> to_sec |> of_sec_with_microsecond_precision)
          (scale 3 microsecond)
      ;;

      let%test_unit _ =
        for _ = 1 to 1_000_000 do
          let t =
            of_int63_ns
              (Int63.of_int64_exn (Random.int64 (Int63.to_int64 Int63.max_value)))
            + if Random.bool () then zero else min_value_for_1us_rounding
          in
          round_trip t
        done
      ;;

      let round_trip parts =
        [%test_result: Parts.t] (to_parts (Private.of_parts parts)) ~expect:parts
      ;;

      let%expect_test _ =
        print_s
          [%sexp
            (to_parts (create ~sign:Neg ~hr:2 ~min:3 ~sec:4 ~ms:5 ~us:6 ~ns:7 ())
              : Parts.t)];
        [%expect
          {|
          ((sign Neg)
           (hr   2)
           (min  3)
           (sec  4)
           (ms   5)
           (us   6)
           (ns   7))
          |}]
      ;;

      let%test_unit _ = round_trip (to_parts (create ~hr:25 ()))

      let%test_unit _ =
        let hr =
          match Word_size.word_size with
          | W32 -> Int.max_value
          | W64 -> Int64.to_int_exn 2217989799822798757L
        in
        round_trip (to_parts (create ~hr ()))
      ;;
    end)
  ;;

  let%test_unit "random smoke" =
    let state = Random.State.make [||] in
    for _ = 1 to 1000 do
      ignore (random ~state () : t)
    done
  ;;
end

open! Time_ns

let%test_unit "random smoke" =
  let state = Random.State.make [||] in
  for _ = 1 to 1000 do
    ignore (random ~state () : t)
  done
;;

let%test_module "next_multiple" =
  (module struct
    let test can_equal_after interval_ns after_ns =
      let base = epoch in
      let interval = Span.of_int63_ns (Int63.of_int interval_ns) in
      let after = of_int63_ns_since_epoch (Int63.of_int64_exn after_ns) in
      let result = next_multiple ~can_equal_after ~interval ~base ~after () in
      let lower_bound, upper_bound =
        let after_interval = add after interval in
        if can_equal_after
        then after, sub after_interval Span.nanosecond
        else add after Span.nanosecond, after_interval
      in
      if result < lower_bound || result > upper_bound
      then
        raise_s
          [%message
            "result out of bounds"
              (can_equal_after : bool)
              (interval : Span.t)
              (base : Alternate_sexp.t)
              (after : Alternate_sexp.t)
              (result : Alternate_sexp.t)
              (lower_bound : Alternate_sexp.t)
              (upper_bound : Alternate_sexp.t)]
    ;;

    (* The below tests all failed in a previous implementation of [next_multiple], due to
       the use of floating point division rather than integer division. *)
    let%test_unit _ = test true 71 1666750235549516973L
    let%test_unit _ = test true 4398 1232807081547132235L
    let%test_unit _ = test false 702561 1233263206897519979L
    let%test_unit _ = test true 65 1813146216102385742L
    let%test_unit _ = test false 3376 1430224273339105389L
    let%test_unit _ = test true 25 1289744875932860592L
    let%test_unit _ = test true 2640 1289026286379471964L
    let%test_unit _ = test true 7062861 1582734990009845838L
    let%test_unit _ = test false 26123810 1509919129138733390L
    let%test_unit _ = test false 1076 1514456253942665045L
    let%test_unit _ = test false 47873597 1567592770350241609L
    let%test_unit _ = test true 147 1794365064173405211L
    let%test_unit _ = test true 37416 1703355717287748172L
    let%test_unit _ = test false 11 1627963384978464309L
    let%test_unit _ = test true 362857 1477941666514490237L
    let%test_unit _ = test true 74 1835379421104268809L
    let%test_unit _ = test false 95 1518869409078948499L
    let%test_unit _ = test false 152 1774086601023993329L
    let%test_unit _ = test true 2963474 1177784542849146405L
    let%test_unit _ = test false 30 1322037015396216447L
    let%test_unit _ = test true 25 1686952462277171285L
    let%test_unit _ = test false 77747994 1232530693599997021L
    let%test_unit _ = test true 39 1418422346766901525L
    let%test_unit _ = test true 20 1164906391254697606L
    let%test_unit _ = test false 492686918 1350478871564364650L
    let%test_unit _ = test false 5626939 1254841457643911520L
    let%test_unit _ = test true 1189147 1566503665916540724L
    let%test_unit _ = test false 97968678 1202922821174442071L
    let%test_unit _ = test false 20 1241457243504201837L
    let%test_unit _ = test true 99 1063228554057138547L
    let%test_unit _ = test true 73 1127965283765790199L
    let%test_unit _ = test true 92513 1423525486630794877L
    let%test_unit _ = test true 208946207 1512896538257529954L
    let%test_unit _ = test true 558 1304902428047905868L
    let%test_unit _ = test true 27 1454760325484042946L
    let%test_unit _ = test true 9511417 1224625971869008380L
    let%test_unit _ = test true 1112121 1486628785456556405L
    let%test_unit _ = test true 36 1226843097592112449L
    let%test_unit _ = test true 60 1299700152825201828L
    let%test_unit _ = test true 114032 1507179377240642938L
    let%test_unit _ = test true 27905 1379112115218849615L
    let%test_unit _ = test true 368860702 1318925554630500136L
    let%test_unit _ = test true 1636 1670399627434728314L
    let%test_unit _ = test false 27 1735798120119522136L
    let%test_unit _ = test true 14 1880325938102084694L
    let%test_unit _ = test true 155 1488215974636830525L
    let%test_unit _ = test true 14319914 1298824542911254370L
    let%test_unit _ = test true 94 1961333441294309841L
    let%test_unit _ = test true 321 1191344461619096942L
    let%test_unit _ = test true 706626 1179098309603309142L
    let%test_unit _ = test true 5 1180517413083401326L
    let%test_unit _ = test false 30523434 1471069600394063742L
    let%test_unit _ = test false 106875447 1789919428848820069L
    let%test_unit _ = test true 28 1013606888178097611L
    let%test_unit _ = test true 5178 1168893256723816286L
    let%test_unit _ = test true 146907740 1402240657577530746L
    let%test_unit _ = test true 127125596 1332881548503325287L
    let%test_unit _ = test true 46691 1526532096462597222L
    let%test_unit _ = test true 1603 1745157292595832416L
    let%test_unit _ = test true 141650492 1779813912846436672L
    let%test_unit _ = test false 20 1916060142837991511L
    let%test_unit _ = test false 27 1366845916494697310L
    let%test_unit _ = test true 61 1572832513125636690L
    let%test_unit _ = test false 11254 1301465801253970270L
    let%test_unit _ = test true 2817556 1220217790200673585L
    let%test_unit _ = test true 46399240 1371834303096963699L
    let%test_unit _ = test true 10280275 1199022106578060117L
    let%test_unit _ = test true 163667 1277585249492511350L
    let%test_unit _ = test true 441771131 1865810978398941565L
    let%test_unit _ = test true 22561070 1535418639166874210L
    let%test_unit _ = test true 677456 1356038574036607058L
    let%test_unit _ = test true 109 1102385187927169659L
    let%test_unit _ = test true 169 1592923082707947954L
    let%test_unit _ = test false 2150725 1769663126416348286L
    let%test_unit _ = test true 159 1051696934142612937L
    let%test_unit _ = test true 29 1844613926625333568L
    let%test_unit _ = test true 30 1361000119652263049L
    let%test_unit _ = test false 21058 1323116357214603127L
    let%test_unit _ = test true 1163794 1221604356987291502L
    let%test_unit _ = test false 30 1040042732593079852L
    let%test_unit _ = test false 106 1997585750801910583L
    let%test_unit _ = test true 78 1292467707712256145L
    let%test_unit _ = test false 882992 1557796972319309155L
    let%test_unit _ = test false 1821 1973683565069601822L
    let%test_unit _ = test false 34661 1737515124214074993L
    let%test_unit _ = test true 91661 1525765679206225703L
    let%test_unit _ = test false 55 1287656410542943084L
    let%test_unit _ = test true 25 1144756873630117512L
    let%test_unit _ = test true 121625 1374589039260879728L
    let%test_unit _ = test false 55 1970197704905173942L
    let%test_unit _ = test true 17 1013158341065700634L
    let%test_unit _ = test true 5176 1352936504880492660L
    let%test_unit _ = test true 12 1955810895023292883L
    let%test_unit _ = test true 67034967 1556142079069258330L
    let%test_unit _ = test true 690258 1241013338154557567L
    let%test_unit _ = test false 5606142 1356689387566170970L
    let%test_unit _ = test true 548 1613807159903275820L
    let%test_unit _ = test true 13 1425941806049471918L
    let%test_unit _ = test false 155572024 1398827221896378979L
    let%test_unit _ = test true 938925403 1550277848520025471L
    let%test_unit _ = test false 13058335 1306567871862304618L
    let%test_unit _ = test true 2 1997152439817382933L
    let%test_unit _ = test true 131456077 1809241097498435420L
    let%test_unit _ = test true 5 1531223674910420761L
    let%test_unit _ = test false 1125 1175905228832358761L
    let%test_unit _ = test true 350 1573261556955534963L
    let%test_unit _ = test false 21 1529314545697532312L
    let%test_unit _ = test false 11816 1222083468556908088L
    let%test_unit _ = test true 86085 1436391155125371248L
    let%test_unit _ = test true 75063667 1395675403046737786L
    let%test_unit _ = test false 67 1765632860861960357L
    let%test_unit _ = test false 184086 1232986716459688821L
    let%test_unit _ = test true 53 1643034916467763402L
    let%test_unit _ = test true 164 1931973285029689763L
    let%test_unit _ = test true 10 1317304422397637720L
    let%test_unit _ = test true 12566 1421417764422298993L
    let%test_unit _ = test true 122903121 1389456412090860886L
    let%test_unit _ = test false 3831308 1617363073756443917L
    let%test_unit _ = test true 2274 1256309428080267889L
    let%test_unit _ = test true 69 1975893988922224788L
    let%test_unit _ = test true 460408083 1956390486383825465L
    let%test_unit _ = test true 20 1294502403828905377L
    let%test_unit _ = test true 75279 1210517500455430679L
    let%test_unit _ = test false 335 1184433858378833746L
    let%test_unit _ = test false 94523 1420732229891051641L
    let%test_unit _ = test false 16 1310464979299616987L
    let%test_unit _ = test true 5886 1602668327390189086L
    let%test_unit _ = test false 9584 1532134444641007990L
    let%test_unit _ = test true 17 1362463965931411147L
    let%test_unit _ = test false 2 1693027090042722358L
    let%test_unit _ = test false 228135731 1462077890315132778L
    let%test_unit _ = test false 11 1018644923234572949L
    let%test_unit _ = test false 132723 1582399817588675962L
    let%test_unit _ = test false 3667 1506604922540283994L
    let%test_unit _ = test true 265541944 1695560402922008138L
    let%test_unit _ = test true 310 1875190738574556027L
    let%test_unit _ = test true 8570918 1184809728498232683L
    let%test_unit _ = test false 16536379 1490415593503829866L
    let%test_unit _ = test false 32222516 1519021258420540539L
    let%test_unit _ = test true 152467451 1255624172539661165L
    let%test_unit _ = test true 13 1803425272409148050L
    let%test_unit _ = test true 26 1021777264383583552L
    let%test_unit _ = test true 11 1400486869768403422L
    let%test_unit _ = test true 229637 1410589173350489612L
    let%test_unit _ = test true 32 1960302290555348647L
    let%test_unit _ = test false 349881185 1831970413297175407L
    let%test_unit _ = test false 35457345 1967569813691929674L
    let%test_unit _ = test false 16 1556051447243676249L
    let%test_unit _ = test false 302933078 1816140399596962652L
    let%test_unit _ = test true 3609444 1802393395129668217L
  end)
;;

let%expect_test "times with implicit zones" =
  require_does_raise [%here] (fun () ->
    Time_ns.Stable.Alternate_sexp.V1.t_of_sexp (Sexp.Atom "2013-10-07 09:30:00"));
  [%expect
    {|
    (Of_sexp_error
      (Invalid_argument "String.chop_suffix_exn \"09:30:00\" \"Z\"")
      (invalid_sexp "2013-10-07 09:30:00"))
    |}];
  require_does_raise [%here] (fun () ->
    Time_ns.Alternate_sexp.t_of_sexp (Sexp.Atom "2013-10-07 09:30:00"));
  [%expect
    {|
    (Of_sexp_error
      (Invalid_argument "String.chop_suffix_exn \"09:30:00\" \"Z\"")
      (invalid_sexp "2013-10-07 09:30:00"))
    |}]
;;

let%expect_test "to_string" =
  print_endline
    (Time_ns.to_string_utc
       (Time_ns.of_int63_ns_since_epoch (Int63.of_int64_exn 1_234_567_890_123_456_789L)));
  [%expect {| 2009-02-13 23:31:30.123456789Z |}]
;;

let%expect_test "Ofday.to_microsecond_string" =
  let round ofday =
    ofday
    |> Time_ns.Ofday.to_span_since_start_of_day
    |> Time_ns.Span.to_int63_ns
    |> Int63.round_down ~to_multiple_of:(Int63.of_int 1000)
    |> Time_ns.Span.of_int63_ns
    |> Time_ns.Ofday.of_span_since_start_of_day_exn
  in
  [ Time_ns.Ofday.start_of_day
  ; Time_ns.Ofday.start_of_day |> Time_ns.Ofday.next |> Core.Option.value_exn
  ; Time_ns.Ofday.approximate_end_of_day
  ; Time_ns.Ofday.start_of_next_day
  ]
  |> List.iter ~f:(fun ofday ->
       let string = Time_ns.Ofday.to_microsecond_string ofday in
       print_endline string;
       require_equal
         [%here]
         (module Time_ns.Ofday)
         (Time_ns.Ofday.of_string string)
         (round ofday));
  [%expect
    {|
    00:00:00.000000
    00:00:00.000000
    23:59:59.999999
    24:00:00.000000
    |}]
;;

let%expect_test "to_sec_string[_with_zone]" =
  let time = Time_ns.add Time_ns.epoch (Time_ns.Span.of_int_ms 1234) in
  let zones =
    List.map [ -5; -1; 0; 1; 8 ] ~f:(fun offset ->
      Core_private.Time_zone.of_utc_offset ~hours:offset)
  in
  List.iter zones ~f:(fun zone -> to_sec_string time ~zone |> print_endline);
  [%expect
    {|
    1969-12-31 19:00:01
    1969-12-31 23:00:01
    1970-01-01 00:00:01
    1970-01-01 01:00:01
    1970-01-01 08:00:01
    |}];
  List.iter zones ~f:(fun zone -> to_sec_string_with_zone time ~zone |> print_endline);
  [%expect
    {|
    1969-12-31 19:00:01-05:00
    1969-12-31 23:00:01-01:00
    1970-01-01 00:00:01Z
    1970-01-01 01:00:01+01:00
    1970-01-01 08:00:01+08:00
    |}]
;;

let%expect_test "time zone offset parsing" =
  let test string =
    print_endline (Time_ns.to_string_utc (Time_ns.of_string_with_utc_offset string))
  in
  test "2000-01-01 12:34:56.789012345-00:00";
  test "2000-01-01 12:34:56.789012345-0:00";
  test "2000-01-01 12:34:56.789012345-00";
  test "2000-01-01 12:34:56.789012345-0";
  [%expect
    {|
    2000-01-01 12:34:56.789012345Z
    2000-01-01 12:34:56.789012345Z
    2000-01-01 12:34:56.789012345Z
    2000-01-01 12:34:56.789012345Z
    |}];
  test "2000-01-01 12:34:56.789012345-05:00";
  test "2000-01-01 12:34:56.789012345-5:00";
  test "2000-01-01 12:34:56.789012345-05";
  test "2000-01-01 12:34:56.789012345-5";
  [%expect
    {|
    2000-01-01 17:34:56.789012345Z
    2000-01-01 17:34:56.789012345Z
    2000-01-01 17:34:56.789012345Z
    2000-01-01 17:34:56.789012345Z
    |}];
  test "2000-01-01 12:34:56.789012345-23:00";
  test "2000-01-01 12:34:56.789012345-23";
  [%expect
    {|
    2000-01-02 11:34:56.789012345Z
    2000-01-02 11:34:56.789012345Z
    |}];
  test "2000-01-01 12:34:56.789012345-24:00";
  test "2000-01-01 12:34:56.789012345-24";
  [%expect
    {|
    2000-01-02 12:34:56.789012345Z
    2000-01-02 12:34:56.789012345Z
    |}]
;;

let%expect_test "time zone invalid offset parsing" =
  let test here string =
    require_does_raise here (fun () -> Time_ns.of_string_with_utc_offset string)
  in
  test [%here] "2000-01-01 12:34:56.789012345-0:";
  test [%here] "2000-01-01 12:34:56.789012345-00:";
  test [%here] "2000-01-01 12:34:56.789012345-0:0";
  test [%here] "2000-01-01 12:34:56.789012345-00:0";
  test [%here] "2000-01-01 12:34:56.789012345-:";
  test [%here] "2000-01-01 12:34:56.789012345-:00";
  test [%here] "2000-01-01 12:34:56.789012345-";
  (* yes this says Time.Ofday instead of Time_ns.Ofday, not fixing this (in
     Ofday_helpers.ml) unless someone cares *)
  [%expect
    {|
    (time_ns.ml.To_and_of_string.Time_ns_of_string
     "2000-01-01 12:34:56.789012345-0:"
     ("Time.Ofday: invalid string"
      0:
      "expected colon or am/pm suffix with optional space after minutes"))
    (time_ns.ml.To_and_of_string.Time_ns_of_string
     "2000-01-01 12:34:56.789012345-00:"
     ("Time.Ofday: invalid string"
      00:
      "expected colon or am/pm suffix with optional space after minutes"))
    (time_ns.ml.To_and_of_string.Time_ns_of_string
     "2000-01-01 12:34:56.789012345-0:0"
     ("Time.Ofday: invalid string"
      0:0
      "expected colon or am/pm suffix with optional space after minutes"))
    (time_ns.ml.To_and_of_string.Time_ns_of_string
     "2000-01-01 12:34:56.789012345-00:0"
     ("Time.Ofday: invalid string"
      00:0
      "expected colon or am/pm suffix with optional space after minutes"))
    (time_ns.ml.To_and_of_string.Time_ns_of_string
     "2000-01-01 12:34:56.789012345-:"
     (Invalid_argument "index out of bounds"))
    (time_ns.ml.To_and_of_string.Time_ns_of_string
     "2000-01-01 12:34:56.789012345-:00"
     (Failure "Char.get_digit_exn ':': not a digit"))
    (time_ns.ml.To_and_of_string.Time_ns_of_string
     "2000-01-01 12:34:56.789012345-"
     (Invalid_argument "index out of bounds"))
    |}];
  test [%here] "2000-01-01 12:34:56.789012-25:00";
  test [%here] "2000-01-01 12:34:56.789012-25";
  [%expect
    {|
    (time_ns.ml.To_and_of_string.Time_ns_of_string
     "2000-01-01 12:34:56.789012-25:00"
     ("Time.Ofday: invalid string" 25:00 "hours out of bounds"))
    (time_ns.ml.To_and_of_string.Time_ns_of_string
     "2000-01-01 12:34:56.789012-25"
     ("Time.Ofday: invalid string" 25:00 "hours out of bounds"))
    |}];
  test [%here] "2000-01-01 12:34:56.789012--1:00";
  test [%here] "2000-01-01 12:34:56.789012--1";
  [%expect
    {|
    (time_ns.ml.To_and_of_string.Time_ns_of_string
     "2000-01-01 12:34:56.789012--1:00"
     (Failure "Char.get_digit_exn '-': not a digit"))
    (time_ns.ml.To_and_of_string.Time_ns_of_string
     "2000-01-01 12:34:56.789012--1"
     (Invalid_argument "index out of bounds"))
    |}]
;;

let%expect_test "test human comparisons" =
  let test first_time last_time =
    let first_time = Time_ns.of_string_with_utc_offset first_time
    and last_time = Time_ns.of_string_with_utc_offset last_time in
    print_s
      [%sexp
        { is_later = (Time_ns.is_later first_time ~than:last_time : bool)
        ; is_earlier = (Time_ns.is_earlier first_time ~than:last_time : bool)
        ; first_time : Time_ns.Alternate_sexp.t
        ; last_time : Time_ns.Alternate_sexp.t
        }]
  in
  test "2000-01-01T00:00:00.000Z" "2019-01-01T12:00:00.000Z";
  test "2000-01-01T00:00:00.000Z" "2000-01-01T00:00:00.000Z";
  test "2019-01-01T12:00:00.000Z" "2000-01-01T00:00:00.000Z";
  (* cross the epoch *)
  test "1969-07-20T20:17:40.000Z" "2000-01-01T00:00:00.000Z";
  [%expect
    {|
    ((is_later   false)
     (is_earlier true)
     (first_time "2000-01-01 00:00:00Z")
     (last_time  "2019-01-01 12:00:00Z"))
    ((is_later   false)
     (is_earlier false)
     (first_time "2000-01-01 00:00:00Z")
     (last_time  "2000-01-01 00:00:00Z"))
    ((is_later   true)
     (is_earlier false)
     (first_time "2019-01-01 12:00:00Z")
     (last_time  "2000-01-01 00:00:00Z"))
    ((is_later   false)
     (is_earlier true)
     (first_time "1969-07-20 20:17:40Z")
     (last_time  "2000-01-01 00:00:00Z"))
    |}]
;;

let%expect_test "add_saturating" =
  let test x y =
    let sum = Time_ns.add x y in
    let saturating_sum = Time_ns.add_saturating x y in
    if Time_ns.equal sum saturating_sum
    then print_s [%message "correct" (sum : Time_ns.Alternate_sexp.t)]
    else (
      let message =
        if Time_ns.( < ) saturating_sum sum then "underflow" else "overflow"
      in
      print_s
        [%message
          message
            (sum : Time_ns.Alternate_sexp.t)
            (saturating_sum : Time_ns.Alternate_sexp.t)])
  in
  (* no overflow *)
  test (Time_ns.of_string_with_utc_offset "2018-01-01 00:00:00Z") Time_ns.Span.minute;
  [%expect {| (correct (sum "2018-01-01 00:01:00Z")) |}];
  (* some positive overflows *)
  test Time_ns.max_value_representable Time_ns.Span.minute;
  [%expect
    {|
    (overflow
      (sum            "1823-11-12 00:07:21.572612095Z")
      (saturating_sum "2116-02-20 23:53:38.427387903Z"))
    |}];
  test
    (Time_ns.of_string_with_utc_offset "2018-01-01 00:00:00Z")
    (Time_ns.Span.of_sec 4_000_000_000.);
  [%expect
    {|
    (overflow
      (sum            "1852-06-24 07:19:23.145224192Z")
      (saturating_sum "2116-02-20 23:53:38.427387903Z"))
    |}];
  (* and negative overflows *)
  test Time_ns.min_value_representable Time_ns.Span.(neg minute);
  [%expect
    {|
    (underflow
      (sum            "2116-02-20 23:52:38.427387904Z")
      (saturating_sum "1823-11-12 00:06:21.572612096Z"))
    |}];
  test
    (Time_ns.of_string_with_utc_offset "1918-01-01 00:00:00Z")
    (Time_ns.Span.of_sec (-4_000_000_000.));
  [%expect
    {|
    (underflow
      (sum            "2083-07-09 16:40:36.854775808Z")
      (saturating_sum "1823-11-12 00:06:21.572612096Z"))
    |}]
;;

let%expect_test "sub_saturating" =
  let test x y =
    let diff = Time_ns.sub x y in
    let saturating_diff = Time_ns.sub_saturating x y in
    if Time_ns.equal diff saturating_diff
    then print_s [%message "correct" (diff : Time_ns.Alternate_sexp.t)]
    else (
      let message =
        if Time_ns.( < ) saturating_diff diff then "underflow" else "overflow"
      in
      print_s
        [%message
          message
            (diff : Time_ns.Alternate_sexp.t)
            (saturating_diff : Time_ns.Alternate_sexp.t)])
  in
  (* no overflow *)
  test (Time_ns.of_string_with_utc_offset "2018-01-01 00:00:00Z") Time_ns.Span.minute;
  [%expect {| (correct (diff "2017-12-31 23:59:00Z")) |}];
  (* some negative overflows *)
  test Time_ns.min_value_representable Time_ns.Span.minute;
  [%expect
    {|
    (underflow
      (diff            "2116-02-20 23:52:38.427387904Z")
      (saturating_diff "1823-11-12 00:06:21.572612096Z"))
    |}];
  test
    (Time_ns.of_string_with_utc_offset "1918-01-01 00:00:00Z")
    (Time_ns.Span.of_sec 4_000_000_000.);
  [%expect
    {|
    (underflow
      (diff            "2083-07-09 16:40:36.854775808Z")
      (saturating_diff "1823-11-12 00:06:21.572612096Z"))
    |}];
  (* and positive overflows *)
  test Time_ns.max_value_representable Time_ns.Span.(neg minute);
  [%expect
    {|
    (overflow
      (diff            "1823-11-12 00:07:21.572612095Z")
      (saturating_diff "2116-02-20 23:53:38.427387903Z"))
    |}];
  test
    (Time_ns.of_string_with_utc_offset "2018-01-01 00:00:00Z")
    (Time_ns.Span.of_sec (-4_000_000_000.));
  [%expect
    {|
    (overflow
      (diff            "1852-06-24 07:19:23.145224192Z")
      (saturating_diff "2116-02-20 23:53:38.427387903Z"))
    |}]
;;

let%expect_test "[to_ofday] never returns 24:00" =
  quickcheck_m
    [%here]
    (module struct
      include struct
        type t = Time_ns.t [@@deriving quickcheck]
      end

      include Time_ns.Stable.Alternate_sexp.V1
    end)
    ~f:(fun time ->
      let ofday = Time_ns.to_ofday time ~zone:Time_float.Zone.utc in
      [%test_pred: Time_ns.Ofday.t]
        (fun ofday -> Time_ns.Ofday.( < ) ofday Time_ns.Ofday.start_of_next_day)
        ofday)
;;

let%expect_test "approximate conversions" =
  let open Time_ns.Span in
  let test to_precise to_approx =
    (* show that conversion can be imprecise *)
    quickcheck_m
      [%here]
      ~cr:Comment
      (module Time_ns.Span)
      ~f:(fun span ->
        let precise = to_precise span in
        let approx = to_approx span in
        if Float.( <> ) precise approx
        then (
          let diff = approx -. precise in
          print_cr
            [%here]
            ~cr:Comment
            [%message
              "result can be imprecise"
                (span : Time_ns.Span.t)
                (precise : float)
                (approx : float)
                (diff : float)]));
    (* check that imprecision is at most 1 ULP *)
    quickcheck_m
      [%here]
      (module Time_ns.Span)
      ~f:(fun span ->
        let precise = to_precise span in
        let approx = to_approx span in
        if Float.( > ) approx (Float.one_ulp `Up precise)
           || Float.( < ) approx (Float.one_ulp `Down precise)
        then (
          let diff = approx -. precise in
          print_cr
            [%here]
            [%message
              "imprecision can exceed 1 ULP"
                (span : Time_ns.Span.t)
                (precise : float)
                (approx : float)
                (diff : float)]))
  in
  test to_us to_us_approx;
  [%expect
    {|
    ("quickcheck: test failed" (input -225.617us))
    (* require-failed: lib/core/test/test_time_ns.ml:LINE:COL. *)
    ("result can be imprecise"
      (span    -225.617us)
      (precise -225.617)
      (approx  -225.61700000000002)
      (diff    -2.8421709430404007E-14))
    |}];
  test to_ms to_ms_approx;
  [%expect
    {|
    ("quickcheck: test failed" (input -627d5h39m8.208643064s))
    (* require-failed: lib/core/test/test_time_ns.ml:LINE:COL. *)
    ("result can be imprecise"
      (span    -627d5h39m8.208643064s)
      (precise -54193148208.643066)
      (approx  -54193148208.643059)
      (diff    7.62939453125E-06))
    |}];
  test to_sec to_sec_approx;
  [%expect
    {|
    ("quickcheck: test failed" (input -15.508265059s))
    (* require-failed: lib/core/test/test_time_ns.ml:LINE:COL. *)
    ("result can be imprecise"
      (span    -15.508265059s)
      (precise -15.508265059)
      (approx  -15.508265059000001)
      (diff    -1.7763568394002505E-15))
    |}];
  test to_min to_min_approx;
  [%expect
    {|
    ("quickcheck: test failed" (input 44.923us))
    (* require-failed: lib/core/test/test_time_ns.ml:LINE:COL. *)
    ("result can be imprecise"
      (span    44.923us)
      (precise 7.4871666666666662E-07)
      (approx  7.4871666666666673E-07)
      (diff    1.0587911840678754E-22))
    |}];
  test to_hr to_hr_approx;
  [%expect
    {|
    ("quickcheck: test failed" (input 76.753us))
    (* require-failed: lib/core/test/test_time_ns.ml:LINE:COL. *)
    ("result can be imprecise"
      (span    76.753us)
      (precise 2.1320277777777777E-08)
      (approx  2.132027777777778E-08)
      (diff    3.3087224502121107E-24))
    |}];
  test to_day to_day_approx;
  [%expect
    {|
    ("quickcheck: test failed" (input -225.617us))
    (* require-failed: lib/core/test/test_time_ns.ml:LINE:COL. *)
    ("result can be imprecise"
      (span    -225.617us)
      (precise -2.6113078703703705E-09)
      (approx  -2.61130787037037E-09)
      (diff    4.1359030627651384E-25))
    |}]
;;
