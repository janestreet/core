open! Core
open! Int.Replace_polymorphic_compare
open  Expect_test_helpers_kernel

let%test_unit "Time_ns.to_date_ofday" =
  assert (does_raise (fun () ->
    Time_ns.to_date_ofday Time_ns.max_value ~zone:Time.Zone.utc));
;;

let%test_module "Core_kernel.Time_ns.Utc.to_date_and_span_since_start_of_day" =
  (module struct
    type time_ns = Time_ns.t [@@deriving compare]
    let sexp_of_time_ns = Core_kernel.Time_ns.Alternate_sexp.sexp_of_t
    ;;

    (* move 1ms off min and max values because [Time_ns]'s boundary checking in functions
       that convert to/from float apparently has some fuzz issues. *)
    let safe_min_value = Time_ns.add Time_ns.min_value Time_ns.Span.microsecond
    let safe_max_value = Time_ns.sub Time_ns.max_value Time_ns.Span.microsecond
    ;;

    let gen =
      let open Quickcheck.Generator.Let_syntax in
      let%map ns_since_epoch =
        Int63.gen_incl
          (Time_ns.to_int63_ns_since_epoch safe_min_value)
          (Time_ns.to_int63_ns_since_epoch safe_max_value)
      in
      Time_ns.of_int63_ns_since_epoch ns_since_epoch
    ;;

    let test f =
      require_does_not_raise [%here] (fun () ->
        Quickcheck.test gen ~f
          ~sexp_of:[%sexp_of: time_ns]
          ~examples:[ safe_min_value; Time_ns.epoch; safe_max_value ])
    ;;

    let%expect_test "Utc.to_date_and_span_since_start_of_day vs Time_ns.to_date_ofday" =
      test (fun time_ns ->
        match Word_size.word_size with
        | W64 ->
          let kernel_date, kernel_span_since_start_of_day =
            Core_kernel.Time_ns.Utc.to_date_and_span_since_start_of_day time_ns
          in
          let kernel_ofday =
            Time_ns.Ofday.of_span_since_start_of_day_exn kernel_span_since_start_of_day
          in
          let core_date, core_ofday = Time_ns.to_date_ofday time_ns ~zone:Time.Zone.utc in
          [%test_result: Date.t * Time_ns.Ofday.t]
            (kernel_date, kernel_ofday)
            ~expect:(core_date, core_ofday)
        | W32 ->
          ());
      [%expect {| |}];
    ;;
  end)

let%expect_test "Time_ns.Span.Stable.V1" =
  let module V = Time_ns.Span.Stable.V1 in
  let make int64 = V.of_int63_exn (Int63.of_int64_exn int64) in
  (* stable checks for values that round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) [
    make                      0L;
    make                  1_000L;
    make          1_000_000_000L;
    make      1_234_560_000_000L;
    make 71_623_008_000_000_000L;
    make 80_000_006_400_000_000L;
  ];
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp   0s)
     (bin_io "\000")
     (int63  0))
    ((sexp   0.001ms)
     (bin_io "\254\232\003")
     (int63  1000))
    ((sexp   1s)
     (bin_io "\253\000\202\154;")
     (int63  1000000000))
    ((sexp   20.576m)
     (bin_io "\252\000\160\130q\031\001\000\000")
     (int63  1234560000000))
    ((sexp   828.97d)
     (bin_io "\252\000\192\149\r\191t\254\000")
     (int63  71623008000000000))
    ((sexp   925.926d)
     (bin_io "\252\000@\128\251\1487\028\001")
     (int63  80000006400000000)) |}];
  (* stable checks for values that do not precisely round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) ~cr:Comment [
    make              1L;
    make 11_275_440_000L;
  ];
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp   0s)
     (bin_io "\001")
     (int63  1))
    (* require-failed: lib/core/test/src/test_time_ns.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       0s)
      (sexp           0s)
      (sexp_roundtrip 0s))
    ((sexp   11.2754s)
     (bin_io "\252\128\143\017\160\002\000\000\000")
     (int63  11275440000))
    (* require-failed: lib/core/test/src/test_time_ns.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       11.2754s)
      (sexp           11.2754s)
      (sexp_roundtrip 11.2754s)) |}];
  (* make sure [of_int63_exn] checks range *)
  show_raise ~hide_positions:true (fun () ->
    V.of_int63_exn (Int63.succ Int63.min_value));
  [%expect {|
    (raised (
      "Span.t exceeds limits"
      (t         -53375.995583650321d)
      (min_value -49275d)
      (max_value 49275d))) |}];
;;

let%expect_test "Time_ns.Span.Option.Stable.V1" =
  let module V = Time_ns.Span.Option.Stable.V1 in
  let make int64 = V.of_int63_exn (Int63.of_int64_exn int64) in
  (* stable checks for values that round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) [
    make                           0L;
    make                       1_000L;
    make               1_000_000_000L;
    make           1_234_560_000_000L;
    make      71_623_008_000_000_000L;
    make      80_000_006_400_000_000L;
    make (-4_611_686_018_427_387_904L);
  ] ~hide_positions:true;
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp (0s))
     (bin_io "\000")
     (int63  0))
    ((sexp (0.001ms))
     (bin_io "\254\232\003")
     (int63  1000))
    ((sexp (1s))
     (bin_io "\253\000\202\154;")
     (int63  1000000000))
    ((sexp (20.576m))
     (bin_io "\252\000\160\130q\031\001\000\000")
     (int63  1234560000000))
    ((sexp (828.97d))
     (bin_io "\252\000\192\149\r\191t\254\000")
     (int63  71623008000000000))
    ((sexp (925.926d))
     (bin_io "\252\000@\128\251\1487\028\001")
     (int63  80000006400000000))
    ((sexp ())
     (bin_io "\252\000\000\000\000\000\000\000\192")
     (int63  -4611686018427387904)) |}];
  (* stable checks for values that do not precisely round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) ~cr:Comment [
    make              1L;
    make 11_275_440_000L;
  ];
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp (0s))
     (bin_io "\001")
     (int63  1))
    (* require-failed: lib/core/test/src/test_time_ns.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       (0s))
      (sexp           (0s))
      (sexp_roundtrip (0s)))
    ((sexp (11.2754s))
     (bin_io "\252\128\143\017\160\002\000\000\000")
     (int63  11275440000))
    (* require-failed: lib/core/test/src/test_time_ns.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       (11.2754s))
      (sexp           (11.2754s))
      (sexp_roundtrip (11.2754s))) |}];
  (* make sure [of_int63_exn] checks range *)
  show_raise ~hide_positions:true (fun () ->
    V.of_int63_exn (Int63.succ Int63.min_value));
  [%expect {|
    (raised (
      "Span.t exceeds limits"
      (t         -53375.995583650321d)
      (min_value -49275d)
      (max_value 49275d))) |}];
;;

let%expect_test "Time_ns.Stable.V1" =
  let module V = Time_ns.Stable.V1 in
  let make int64 = V.of_int63_exn (Int63.of_int64_exn int64) in
  (* stable checks for values that round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) [
    make                         0L;
    make                     1_000L;
    make         1_234_560_000_000L;
    make    80_000_006_400_000_000L;
    make 1_381_156_200_010_101_000L;
    make 4_110_307_199_999_999_000L;
  ];
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp (1969-12-31 19:00:00.000000-05:00))
     (bin_io "\000")
     (int63  0))
    ((sexp (1969-12-31 19:00:00.000001-05:00))
     (bin_io "\254\232\003")
     (int63  1000))
    ((sexp (1969-12-31 19:20:34.560000-05:00))
     (bin_io "\252\000\160\130q\031\001\000\000")
     (int63  1234560000000))
    ((sexp (1972-07-14 18:13:26.400000-04:00))
     (bin_io "\252\000@\128\251\1487\028\001")
     (int63  80000006400000000))
    ((sexp (2013-10-07 10:30:00.010101-04:00))
     (bin_io "\252\b1\238\b?\218*\019")
     (int63  1381156200010101000))
    ((sexp (2100-04-01 18:59:59.999999-05:00))
     (bin_io "\252\024\252\186\253\158\190\n9")
     (int63  4110307199999999000)) |}];
  (* stable checks for values that do not precisely round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) ~cr:Comment [
    make 1L;
  ];
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp (1969-12-31 19:00:00.000000-05:00))
     (bin_io "\001")
     (int63  1))
    (* require-failed: lib/core/test/src/test_time_ns.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       (1969-12-31 19:00:00.000000-05:00))
      (sexp           (1969-12-31 19:00:00.000000-05:00))
      (sexp_roundtrip (1969-12-31 19:00:00.000000-05:00))) |}];
  (* make sure [of_int63_exn] checks range *)
  show_raise ~hide_positions:true (fun () ->
    V.of_int63_exn (Int63.succ Int63.min_value));
  [%expect {|
    (raised (
      "Span.t exceeds limits"
      (t         -53375.995583650321d)
      (min_value -49275d)
      (max_value 49275d))) |}];
;;

let%expect_test "Time_ns.Option.Stable.V1" =
  let module V = Time_ns.Option.Stable.V1 in
  let make int64 = V.of_int63_exn (Int63.of_int64_exn int64) in
  (* stable checks for values that round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) [
    make                           0L;
    make                       1_000L;
    make           1_234_560_000_000L;
    make      80_000_006_400_000_000L;
    make   1_381_156_200_010_101_000L;
    make   4_110_307_199_999_999_000L;
    make (-4_611_686_018_427_387_904L);
  ];
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp ((1969-12-31 19:00:00.000000-05:00)))
     (bin_io "\000")
     (int63  0))
    ((sexp ((1969-12-31 19:00:00.000001-05:00)))
     (bin_io "\254\232\003")
     (int63  1000))
    ((sexp ((1969-12-31 19:20:34.560000-05:00)))
     (bin_io "\252\000\160\130q\031\001\000\000")
     (int63  1234560000000))
    ((sexp ((1972-07-14 18:13:26.400000-04:00)))
     (bin_io "\252\000@\128\251\1487\028\001")
     (int63  80000006400000000))
    ((sexp ((2013-10-07 10:30:00.010101-04:00)))
     (bin_io "\252\b1\238\b?\218*\019")
     (int63  1381156200010101000))
    ((sexp ((2100-04-01 18:59:59.999999-05:00)))
     (bin_io "\252\024\252\186\253\158\190\n9")
     (int63  4110307199999999000))
    ((sexp ())
     (bin_io "\252\000\000\000\000\000\000\000\192")
     (int63  -4611686018427387904)) |} ];
  (* stable checks for values that do not precisely round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) ~cr:Comment [
    make 1L;
  ];
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp ((1969-12-31 19:00:00.000000-05:00)))
     (bin_io "\001")
     (int63  1))
    (* require-failed: lib/core/test/src/test_time_ns.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       ((1969-12-31 19:00:00.000000-05:00)))
      (sexp           ((1969-12-31 19:00:00.000000-05:00)))
      (sexp_roundtrip ((1969-12-31 19:00:00.000000-05:00)))) |}];
  (* make sure [of_int63_exn] checks range *)
  show_raise ~hide_positions:true (fun () ->
    V.of_int63_exn (Int63.succ Int63.min_value));
  [%expect {|
    (raised (
      "Span.t exceeds limits"
      (t         -53375.995583650321d)
      (min_value -49275d)
      (max_value 49275d))) |}];
;;

let%test_module "Time_ns.Ofday.Stable.V1" =
  (module struct
    module V = Time_ns.Ofday.Stable.V1

    let%expect_test "stable conversions" =
      let make int64 = V.of_int63_exn (Int63.of_int64_exn int64) in
      (* stable checks for key values *)
      print_and_check_stable_int63able_type [%here] (module V) [
        make                  0L;
        make                  1L;
        make                499L;
        make                500L;
        make              1_000L;
        make    123_456_789_012L;
        make    987_654_321_000L;
        make  1_234_560_000_000L;
        make 52_200_010_101_000L;
        make 86_399_999_999_000L;
        make 86_399_999_999_999L;
        make 86_400_000_000_000L;
      ];
      [%expect {|
        (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
        ((sexp   00:00:00.000000000)
         (bin_io "\000")
         (int63  0))
        ((sexp   00:00:00.000000001)
         (bin_io "\001")
         (int63  1))
        ((sexp   00:00:00.000000499)
         (bin_io "\254\243\001")
         (int63  499))
        ((sexp   00:00:00.000000500)
         (bin_io "\254\244\001")
         (int63  500))
        ((sexp   00:00:00.000001000)
         (bin_io "\254\232\003")
         (int63  1000))
        ((sexp   00:02:03.456789012)
         (bin_io "\252\020\026\153\190\028\000\000\000")
         (int63  123456789012))
        ((sexp   00:16:27.654321000)
         (bin_io "\252h\243\200\244\229\000\000\000")
         (int63  987654321000))
        ((sexp   00:20:34.560000000)
         (bin_io "\252\000\160\130q\031\001\000\000")
         (int63  1234560000000))
        ((sexp   14:30:00.010101000)
         (bin_io "\252\b1\015\195y/\000\000")
         (int63  52200010101000))
        ((sexp   23:59:59.999999000)
         (bin_io "\252\024\252N\145\148N\000\000")
         (int63  86399999999000))
        ((sexp   23:59:59.999999999)
         (bin_io "\252\255\255N\145\148N\000\000")
         (int63  86399999999999))
        ((sexp   24:00:00.000000000)
         (bin_io "\252\000\000O\145\148N\000\000")
         (int63  86400000000000)) |}];
      (* make sure [of_int63_exn] checks range *)
      show_raise ~hide_positions:true (fun () ->
        V.of_int63_exn (Int63.succ Int63.min_value));
      [%expect {|
        (raised (
          "Span.t exceeds limits"
          (t         -53375.995583650321d)
          (min_value -49275d)
          (max_value 49275d))) |}];
      show_raise ~hide_positions:true (fun () ->
        V.of_int63_exn (Int63.pred Int63.max_value));
      [%expect {|
        (raised (
          "Span.t exceeds limits"
          (t         53375.995583650321d)
          (min_value -49275d)
          (max_value 49275d))) |}];
    ;;

    let%test_unit "roundtrip quickcheck" =
      let generator =
        Core_kernel.Int63.gen_incl
          (V.to_int63 Time_ns.Ofday.start_of_day)
          (V.to_int63 Time_ns.Ofday.start_of_next_day)
        |> Core_kernel.Quickcheck.Generator.map ~f:V.of_int63_exn
      in
      Core_kernel.Quickcheck.test generator
        ~sexp_of:V.sexp_of_t
        ~f:(fun ofday ->
          [%test_result: V.t] ~expect:ofday (V.of_int63_exn (V.to_int63  ofday));
          [%test_result: V.t] ~expect:ofday (V.t_of_sexp    (V.sexp_of_t ofday)))
    ;;
  end)

let%test_module "Time_ns.Ofday.Option.Stable.V1" =
  (module struct
    module V = Time_ns.Ofday.Option.Stable.V1

    let%expect_test "stable conversions" =
      let make int64 = V.of_int63_exn (Int63.of_int64_exn int64) in
      (* stable checks for key values *)
      print_and_check_stable_int63able_type [%here] (module V) [
        make                           0L;
        make                           1L;
        make                         499L;
        make                         500L;
        make                       1_000L;
        make             123_456_789_012L;
        make             987_654_321_000L;
        make           1_234_560_000_000L;
        make          52_200_010_101_000L;
        make          86_399_999_999_000L;
        make          86_399_999_999_999L;
        make          86_400_000_000_000L;
        make (-4_611_686_018_427_387_904L); (* None *)
      ];
      [%expect {|
        (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
        ((sexp (00:00:00.000000000))
         (bin_io "\000")
         (int63  0))
        ((sexp (00:00:00.000000001))
         (bin_io "\001")
         (int63  1))
        ((sexp (00:00:00.000000499))
         (bin_io "\254\243\001")
         (int63  499))
        ((sexp (00:00:00.000000500))
         (bin_io "\254\244\001")
         (int63  500))
        ((sexp (00:00:00.000001000))
         (bin_io "\254\232\003")
         (int63  1000))
        ((sexp (00:02:03.456789012))
         (bin_io "\252\020\026\153\190\028\000\000\000")
         (int63  123456789012))
        ((sexp (00:16:27.654321000))
         (bin_io "\252h\243\200\244\229\000\000\000")
         (int63  987654321000))
        ((sexp (00:20:34.560000000))
         (bin_io "\252\000\160\130q\031\001\000\000")
         (int63  1234560000000))
        ((sexp (14:30:00.010101000))
         (bin_io "\252\b1\015\195y/\000\000")
         (int63  52200010101000))
        ((sexp (23:59:59.999999000))
         (bin_io "\252\024\252N\145\148N\000\000")
         (int63  86399999999000))
        ((sexp (23:59:59.999999999))
         (bin_io "\252\255\255N\145\148N\000\000")
         (int63  86399999999999))
        ((sexp (24:00:00.000000000))
         (bin_io "\252\000\000O\145\148N\000\000")
         (int63  86400000000000))
        ((sexp ())
         (bin_io "\252\000\000\000\000\000\000\000\192")
         (int63  -4611686018427387904)) |}];
      (* make sure [of_int63_exn] checks range *)
      show_raise ~hide_positions:true (fun () ->
        V.of_int63_exn (Int63.succ Int63.min_value));
      [%expect {|
        (raised (
          "Span.t exceeds limits"
          (t         -53375.995583650321d)
          (min_value -49275d)
          (max_value 49275d))) |}];
      show_raise ~hide_positions:true (fun () ->
        V.of_int63_exn Int63.max_value);
      [%expect {|
        (raised (
          "Span.t exceeds limits"
          (t         53375.995583650321d)
          (min_value -49275d)
          (max_value 49275d))) |}];
    ;;

    let%test_unit "roundtrip quickcheck" =
      let generator =
        Core_kernel.Int63.gen_incl
          (V.to_int63 (Time_ns.Ofday.Option.some Time_ns.Ofday.start_of_day))
          (V.to_int63 (Time_ns.Ofday.Option.some Time_ns.Ofday.start_of_next_day))
        |> Core_kernel.Quickcheck.Generator.map ~f:V.of_int63_exn
      in
      Core_kernel.Quickcheck.test generator
        ~sexp_of:V.sexp_of_t
        ~f:(fun ofday_option ->
          [%test_result: V.t] ~expect:ofday_option
            (V.of_int63_exn (V.to_int63 ofday_option));
          [%test_result: V.t] ~expect:ofday_option
            (V.t_of_sexp (V.sexp_of_t ofday_option)))
    ;;
  end)

let%test_module "Time_ns.Span" =
  (module struct
    open Time_ns.Span

    let half_microsecond = Int63.of_int 500

    let nearest_microsecond t =
      Int63.((Time_ns.Span.to_int63_ns t + half_microsecond) /% of_int 1000)
    ;;

    let min_span_ns_as_span = to_span Time_ns.Span.min_value
    let max_span_ns_as_span = to_span Time_ns.Span.max_value

    let%test "to_span +/-140y raises" =
      List.for_all [ 1.; -1. ]
        ~f:(fun sign ->
          does_raise (fun () ->
            to_span (Time_ns.Span.of_day (140. *. 366. *. sign))))
    ;;

    let%test "of_span +/-140y raises" =
      List.for_all [ 1.; -1. ]
        ~f:(fun sign ->
          does_raise (fun () -> of_span (Time.Span.of_day (140. *. 366. *. sign))))
    ;;

    let%test_unit "Span.to_string_hum" =
      let open Time_ns.Span in
      [%test_result: string] (to_string_hum nanosecond) ~expect:"1ns";
      [%test_result: string] (to_string_hum day) ~expect:"1d";
      [%test_result: string]
        (to_string_hum ~decimals:6                      day)
        ~expect:"1d";
      [%test_result: string]
        (to_string_hum ~decimals:6 ~align_decimal:false day)
        ~expect:"1d";
      [%test_result: string]
        (to_string_hum ~decimals:6 ~align_decimal:true  day)
        ~expect:"1.000000d ";
      [%test_result: string]
        (to_string_hum ~decimals:6 ~align_decimal:true ~unit_of_time:Day
           (hour + minute))
        ~expect:"0.042361d "

    let a_few_more_or_less = [-3; -2; -1; 0; 1; 2; 3]

    let span_examples =
      let open Time.Span in
      [
        min_span_ns_as_span;
        zero;
        microsecond;
        millisecond;
        second;
        minute;
        hour;
        day;
        scale day 365.;
        max_span_ns_as_span;
      ]
      @ List.init 9 ~f:(fun _ ->
        of_us (Random.float (to_us max_span_ns_as_span)))

    let multiples_of_span span =
      List.map a_few_more_or_less ~f:(fun factor ->
        Time.Span.scale span (float factor))

    let within_a_few_microseconds_of_span span =
      List.map a_few_more_or_less ~f:(fun number_of_microseconds ->
        Time.Span.( + ) span
          (Time.Span.scale Time.Span.microsecond (float number_of_microseconds)))

    let nearest_microsecond_to_span span =
      Time.Span.of_us (Float.round_nearest (Time.Span.to_us span))

    let span_is_in_range span =
      Time.Span.( >= ) span min_span_ns_as_span &&
      Time.Span.( <= ) span max_span_ns_as_span

    let%expect_test "Time.Span.t -> Time_ns.Span.t round trip" =
      let open Time.Span in
      let spans =
        span_examples
        |> List.concat_map ~f:multiples_of_span
        |> List.concat_map ~f:within_a_few_microseconds_of_span
        |> List.map        ~f:nearest_microsecond_to_span
        |> List.filter     ~f:span_is_in_range
        |> List.dedup_and_sort ~compare:Time.Span.compare
      in
      List.iter spans ~f:(fun span ->
        let span_ns    = of_span span    in
        let round_trip = to_span span_ns in
        let precision = abs (round_trip - span) in
        require [%here] (precision <= microsecond)
          ~if_false_then_print_s:
            (lazy [%message
              "round-trip does not have microsecond precision"
                (span       : Time.Span.t)
                (span_ns    : Core_kernel.Time_ns.Span.Alternate_sexp.t)
                (round_trip : Time.Span.t)
                (precision  : Time.Span.t)]));
      [%expect {||}];
    ;;

    let span_ns_examples =
      let open Time_ns.Span in
      [
        min_value;
        zero;
        microsecond;
        millisecond;
        second;
        minute;
        hour;
        day;
        scale day 365.;
        max_value;
      ]
      @ List.init 9 ~f:(fun _ ->
        of_us (Random.float (to_us max_value)))

    let multiples_of_span_ns span_ns =
      List.filter_map a_few_more_or_less ~f:(fun factor ->
        Core.Option.try_with (fun () ->
          Time_ns.Span.scale span_ns (float factor)))

    let within_a_few_microseconds_of_span_ns span_ns =
      List.filter_map a_few_more_or_less ~f:(fun number_of_microseconds ->
        Core.Option.try_with (fun () ->
          Time_ns.Span.( + ) span_ns
            (Time_ns.Span.scale Time_ns.Span.microsecond (float number_of_microseconds))))

    let nearest_microsecond_to_span_ns span_ns =
      of_int63_ns (Int63.( * ) (nearest_microsecond span_ns) (Int63.of_int 1000))

    let span_ns_is_in_range span_ns =
      Time_ns.Span.( >= ) span_ns Time_ns.Span.min_value &&
      Time_ns.Span.( <= ) span_ns Time_ns.Span.max_value

    let%expect_test "Time_ns.Span.t -> Time.Span.t round trip" =
      let open Time_ns.Span in
      let span_nss =
        span_ns_examples
        |> List.concat_map ~f:multiples_of_span_ns
        |> List.concat_map ~f:within_a_few_microseconds_of_span_ns
        |> List.map        ~f:nearest_microsecond_to_span_ns
        |> List.filter     ~f:span_ns_is_in_range
        |> List.dedup_and_sort ~compare:Time_ns.Span.compare
      in
      List.iter span_nss ~f:(fun span_ns ->
        let span       = to_span span_ns in
        let round_trip = of_span span    in
        let precision = abs (round_trip - span_ns) in
        require [%here] (precision <= microsecond)
          ~if_false_then_print_s:
            (lazy [%message
              "round-trip does not have microsecond precision"
                (span_ns    : Core_kernel.Time_ns.Span.Alternate_sexp.t)
                (span       : Time.Span.t)
                (round_trip : Core_kernel.Time_ns.Span.Alternate_sexp.t)
                (precision  : Core_kernel.Time_ns.Span.Alternate_sexp.t)]));
      [%expect {||}];
    ;;

    let%test _ = Time.Span.is_positive (to_span max_value)  (* make sure no overflow *)
  end)

let%test_module "Time_ns.Span.Option" =
  (module struct
    open Time_ns.Span.Option

    let%test "none is not a valid span" =
      does_raise (fun () ->
        some (Time_ns.Span.of_int63_ns (Time_ns.Span.Option.Stable.V1.to_int63 none)))
  end)

let%test_module "Time_ns" =
  (module struct
    open Time_ns

    let min_time_value = to_time min_value
    let max_time_value = to_time max_value

    let%test_unit "Time.t -> Time_ns.t round trip" =
      let open Time in
      let time_to_float t = Time.to_span_since_epoch t |> Time.Span.to_sec in
      let sexp_of_t t = [%sexp_of: t * float] (t, time_to_float t) in (* more precise *)
      let us_since_epoch time = Time.(Span.to_us (diff time epoch)) in
      let min_us_since_epoch = us_since_epoch min_time_value in
      let max_us_since_epoch = us_since_epoch max_time_value in
      let time_of_us_since_epoch us_since_epoch =
        Time.(add epoch (Span.of_us (Float.round_nearest us_since_epoch)))
      in
      let times =                           (* touchstones *)
        [ min_time_value; Time.epoch; Time.now (); max_time_value ]
      in
      let times =                           (* a few units around *)
        List.concat_map times
          ~f:(fun time ->
            List.concat_map
              Time.Span.([ microsecond; millisecond; second; minute; hour; day;
                           scale day 365.
                         ])
              ~f:(fun unit ->
                List.map (List.map ~f:float (List.range (-3) 4))
                  ~f:(fun s -> Time.add time (Time.Span.scale unit s))))
      in
      let times =                           (* a few randoms *)
        times @
        List.init 9
          ~f:(fun _ ->
            Time.add Time.epoch
              (Time.Span.of_us
                 (min_us_since_epoch
                  +. Random.float (max_us_since_epoch -. min_us_since_epoch))))
      in
      let times =                           (* nearest microsecond *)
        List.map times
          ~f:(fun time ->
            time_of_us_since_epoch
              (Float.round_nearest Time.(Span.to_us (diff time epoch))))
      in
      let times =                           (* in range *)
        List.filter times
          ~f:(fun time -> Time.(time >= min_time_value && time <= max_time_value))
      in
      let is_64bit = match Word_size.word_size with
        | W64 -> true
        | W32 -> false
      in
      List.iter times
        ~f:(fun expect ->
          let time = to_time (of_time expect) in
          (* We don't have full microsecond precision at the far end of the range. *)
          if is_64bit && expect < Time.of_string "2107-01-01 00:00:00" then
            [%test_result: t] ~expect time
          else
            [%test_pred: t * t]
              (fun (a, b) -> Span.(abs (diff a b) <= microsecond))
              (expect, time))
    ;;

    let%test_unit "Time_ns.t -> Time.t round trip" =
      let open Core_kernel.Time_ns.Alternate_sexp in
      let ts =                              (* touchstones *)
        [ min_value; epoch; now (); max_value ]
      in
      (* Some tweaks will be out of range, which will raise exceptions. *)
      let filter_map list ~f =
        List.filter_map list ~f:(fun x -> Core.Option.try_with (fun () -> f x))
      in
      let ts =                              (* a few units around *)
        List.concat_map ts
          ~f:(fun time ->
            List.concat_map
              Span.([ microsecond; millisecond; second; minute; hour; day;
                      scale day 365.
                    ])
              ~f:(fun unit ->
                filter_map (List.map ~f:float (List.range (-3) 4))
                  ~f:(fun s -> add time (Span.scale unit s))))
      in
      let ts =                              (* a few randoms *)
        ts @ List.init 9 ~f:(fun _ -> random ())
      in
      let ts =                              (* nearest microsecond since epoch *)
        List.map ts
          ~f:(fun time ->
            Time_ns.of_int63_ns_since_epoch
              (let open Int63 in
               (Time_ns.to_int63_ns_since_epoch time + of_int 500)
               /% of_int 1000
               * of_int 1000))
      in
      let ts =                              (* in range *)
        List.filter ts ~f:(fun t -> t >= min_value && t <= max_value)
      in
      List.iter ts ~f:(fun expect -> [%test_result: t] ~expect (of_time (to_time expect)))
    ;;

    let%test _ = epoch = of_span_since_epoch Span.zero

    let%test_unit "round trip from [Time.t] to [t] and back" =
      let time_of_float f = Time.of_span_since_epoch (Time.Span.of_sec f) in
      let times = List.map ~f:time_of_float [ 0.0; 1.0; 1.123456789 ] in
      List.iter times ~f:(fun time ->
        let res = to_time (of_time time) in
        [%test_result: Time.t] ~equal:Time.(=.) ~expect:time res
      )

    let%test_unit "round trip from [t] to [Time.t] and back" =
      List.iter Span.([ zero; second; scale day 365. ]) ~f:(fun since_epoch ->
        let t = of_span_since_epoch since_epoch in
        let res = of_time (to_time t) in
        (* Allow up to 100ns discrepancy in a year due to float precision issues. *)
        let discrepancy = diff res t in
        if Span.(abs discrepancy > of_ns 100.) then
          failwiths "Failed on span since epoch"
            (`since_epoch since_epoch, t, `res res, `discrepancy discrepancy)
            [%sexp_of: [ `since_epoch of Span.t ]
                       * t * [ `res of t ]
                       * [ `discrepancy of Span.t ]])

    let%test_unit _ =
      let span = Span.create ~hr:8 ~min:27 ~sec:14 ~ms:359 () in
      let ofday = Ofday.of_span_since_start_of_day_exn span in
      let expected = "08:27:14.359" in
      let ms_str = Ofday.to_millisecond_string ofday    in
      if String.(<>) ms_str expected then
        failwithf "Failed on Ofday.to_millisecond_string Got (%s) expected (%s)"
          ms_str expected ()

    let check ofday =
      try
        assert Ofday.(ofday >= start_of_day && ofday < start_of_next_day);
        [%test_result: Ofday.t] ~expect:ofday
          (Ofday.of_string (Ofday.to_string ofday));
        [%test_result: Ofday.t] ~expect:ofday
          (Ofday.t_of_sexp (Ofday.sexp_of_t ofday));
        let of_ofday = Ofday.of_ofday (Ofday.to_ofday ofday) in
        let diff = Span.abs (Ofday.diff ofday of_ofday) in
        if Span.( >= ) diff Span.microsecond then
          raise_s [%message
            "of_ofday / to_ofday round-trip failed"
              (ofday    : Ofday.t)
              (of_ofday : Ofday.t)]
      with raised ->
        failwiths "check ofday"
          (Or_error.try_with (fun () -> [%sexp_of: Ofday.t] ofday),
           Span.to_int63_ns (Time_ns.Ofday.to_span_since_start_of_day ofday),
           raised)
          [%sexp_of: Sexp.t Or_error.t * Int63.t * exn]

    let%test_unit _ =
      (* Ensure that midnight_cache doesn't interfere with converting times that are much
         earlier or later than each other. *)
      check (to_ofday ~zone:(force Time_ns.Zone.local) epoch);
      check (to_ofday ~zone:(force Time_ns.Zone.local) (now ()));
      check (to_ofday ~zone:(force Time_ns.Zone.local) epoch)

    (* Reproduce a failure of the prior test before taking DST into account. *)
    let%test_unit "to_ofday around fall 2015 DST transition" =
      List.iter
        ~f:(fun (time_ns, expect) ->
          let zone = Time.Zone.find_exn "US/Eastern" in
          (* First make sure Time.to_ofday behaves as expected with these inputs. *)
          let time_ofday = Time.to_ofday (to_time time_ns) ~zone in
          if Time.Ofday.(<>) time_ofday (Time.Ofday.of_string expect) then
            failwiths "Time.to_ofday"
              [%sexp (time_ns    : t),
                     (time_ofday : Time.Ofday.t),
                     (expect     : string)]
              Fn.id;
          (* Then make sure we do the same, correct thing. *)
          let ofday = to_ofday time_ns ~zone in
          check ofday;
          if Ofday.(<>) ofday (Ofday.of_string expect) then
            failwiths "to_ofday"
              [%sexp (time_ns : t),
                     (ofday   : Ofday.t),
                     (expect  : string)]
              Fn.id)
        ([ epoch, "19:00:00"
         ; of_string_abs "2015-11-02 23:59:59 US/Eastern", "23:59:59"
         ; epoch, "19:00:00"
         (* [of_string] chooses the second occurrence of a repeated wall clock time in a
            DST (Daylight Saving Time) transition. *)
         ; add (of_string "2015-11-01 01:59:59 US/Eastern") Span.second, "02:00:00"
         ]
         (* We can denote specific linear times during the repeated wall-clock hour
            relative to a time before the ambiguity. *)
         @ List.map
             ~f:(fun (span, ofday) ->
               add (of_string "2015-11-01 00:59:59 US/Eastern") span, ofday)
             [ Span.second,          "01:00:00"
             ; Span.(second + hour), "01:00:00"
             ]
         @ [ add (of_string "2015-03-08 01:59:59 US/Eastern") Span.second, "03:00:00"
           ; epoch, "19:00:00"
           ])

    let random_nativeint_range =
      match Word_size.word_size with
      | W64 -> fun () -> random ()
      | W32 ->
        (* In 32 bits, some functions in [Time] don't work on all the float values, but
           only the part that fits in a native int. *)
        let in_ns = Int63.of_float 1e9 in
        let max_time_ns = Int63.(of_nativeint_exn Nativeint.max_value * in_ns) in
        let min_time_ns = Int63.(of_nativeint_exn Nativeint.min_value * in_ns) in
        let range = Int63.(one + max_time_ns - min_time_ns) in
        fun () ->
          let r = Time_ns.to_int63_ns_since_epoch (random ()) in
          Time_ns.of_int63_ns_since_epoch
            Int63.(((r - min_time_ns) % range) + min_time_ns)
    ;;

    let%test_unit "to_ofday random" =
      List.iter !Time.Zone.likely_machine_zones ~f:(fun zone ->
        let zone = Time.Zone.find_exn zone in
        for _ = 0 to 1_000 do check (to_ofday (random_nativeint_range ()) ~zone) done)

    let%test_unit "to_ofday ~zone:(force Time_ns.Zone.local) random" =
      for _ = 0 to 1_000 do check (to_ofday ~zone:(force Time_ns.Zone.local) (random_nativeint_range ())) done

    let%expect_test "[to_date ofday] - [of_date_ofday] roundtrip" =
      let times =
        (* midnight EDT on 2016-11-01 +/- 1ns and 2ns *)
        [ 1477972799999999998L
        ; 1477972799999999999L
        ; 1477972800000000000L
        ; 1477972800000000001L
        ; 1477972800000000002L
        (* two timestamps on 2016-11-01 in the middle of the day, 1ns apart *)
        ; 1478011075019386869L
        ; 1478011075019386670L
        (* two timestamps on 2016-11-06 (Sunday), when DST ends at 2am. When DST ends, time
           jumps from 2:00am back to 1:00am. Hence there are two 1:00ams, the later occurs 1hr
           later in linear time. This test starts with the initial 1:00am (as inputs to
           [to_date] and [to_ofday]) and then gets reinterpreted as the later 1:00am by
           [of_date_ofday] *)
        ; 1478408399999999999L (* just before 1am *)
        ; 1478408400000000000L (* 1ns later, at 1am (for the first time),
                                  NOTE: we're off by 1h when we try the round-trip, see
                                  [3600000000000] in the expect_test below.  *)
        ; 1478412000000000000L (* 1h later, which is 1am again (this time we round-trip) *)
        ; 1478412000000000001L (* another 1ns later *)
        ]
      in
      let zone = Time.Zone.of_string "America/New_York" in
      List.iter times ~f:(fun ns ->
        let t = Int63.of_int64_exn ns |> Time_ns.of_int63_ns_since_epoch in
        let date = to_date t ~zone in
        let ofday = to_ofday t ~zone in
        let t' = of_date_ofday ~zone date ofday in
        printf !"%{Date} %{Ofday} %{Int64} %{Int64}\n"
          date
          ofday
          ns
          (Int64.(-) (to_int63_ns_since_epoch t' |> Int63.to_int64) ns));
      [%expect {|
    2016-10-31 23:59:59.999999998 1477972799999999998 0
    2016-10-31 23:59:59.999999999 1477972799999999999 0
    2016-11-01 00:00:00.000000000 1477972800000000000 0
    2016-11-01 00:00:00.000000001 1477972800000000001 0
    2016-11-01 00:00:00.000000002 1477972800000000002 0
    2016-11-01 10:37:55.019386869 1478011075019386869 0
    2016-11-01 10:37:55.019386670 1478011075019386670 0
    2016-11-06 00:59:59.999999999 1478408399999999999 0
    2016-11-06 01:00:00.000000000 1478408400000000000 3600000000000
    2016-11-06 01:00:00.000000000 1478412000000000000 0
    2016-11-06 01:00:00.000000001 1478412000000000001 0 |}]
    ;;

    let%expect_test "in tests, [to_string] uses NYC's time zone" =
      printf "%s" (to_string epoch);
      [%expect {| 1969-12-31 19:00:00.000000-05:00 |}];
    ;;

    let%expect_test "in tests, [sexp_of_t] uses NYC's time zone" =
      printf !"%{Sexp}" [%sexp (epoch : t)];
      [%expect {| (1969-12-31 19:00:00.000000-05:00) |}];
    ;;
  end)

let%expect_test "end-of-day constants" =
  let zones = List.map !Time_ns.Zone.likely_machine_zones ~f:Time_ns.Zone.find_exn in
  let test_round_trip zone date ofday ~expect =
    require_equal [%here] (module Date)
      (Time_ns.of_date_ofday ~zone date ofday |> Time_ns.to_date ~zone)
      expect
      ~message:(Time_ns.Zone.name zone)
  in
  let test date_string =
    let date = Date.of_string date_string in
    List.iter zones ~f:(fun zone ->
      test_round_trip zone date Time_ns.Ofday.approximate_end_of_day
        ~expect:date;
      test_round_trip zone date Time_ns.Ofday.start_of_next_day
        ~expect:(Date.add_days date 1));
  in
  test "1970-01-01";
  test "2013-10-07";
  test "2099-12-31";
  test "2101-04-01";
  [%expect {||}];
;;

let%test_module "Time_ns.Option" =
  (module struct
    open Time_ns.Option

    let%test_module "round trip" =
      (module struct
        let roundtrip t = (value_exn (some t))
        let%test_unit "epoch" =
          [%test_result: Time_ns.t] (roundtrip Time_ns.epoch) ~expect:Time_ns.epoch
        let%test_unit "now" =
          let t = Time_ns.now () in [%test_result: Time_ns.t] (roundtrip t) ~expect:t
      end)

    let%test _ = is_error (Result.try_with (fun () -> value_exn none))
  end)

let%expect_test _ =
  print_and_check_container_sexps [%here] (module Time_ns) [
    Time_ns.epoch;
    Time_ns.of_string "1955-11-12 18:38:00-08:00";
    Time_ns.of_string "1985-10-26 21:00:00-08:00";
    Time_ns.of_string "2015-10-21 19:28:00-08:00";
  ];
  [%expect {|
    (Set (
      (1955-11-12 21:38:00.000000-05:00)
      (1969-12-31 19:00:00.000000-05:00)
      (1985-10-27 01:00:00.000000-04:00)
      (2015-10-21 23:28:00.000000-04:00)))
    (Map (
      ((1955-11-12 21:38:00.000000-05:00) 1)
      ((1969-12-31 19:00:00.000000-05:00) 0)
      ((1985-10-27 01:00:00.000000-04:00) 2)
      ((2015-10-21 23:28:00.000000-04:00) 3)))
    (Hash_set (
      (1955-11-12 21:38:00.000000-05:00)
      (1969-12-31 19:00:00.000000-05:00)
      (1985-10-27 01:00:00.000000-04:00)
      (2015-10-21 23:28:00.000000-04:00)))
    (Table (
      ((1955-11-12 21:38:00.000000-05:00) 1)
      ((1969-12-31 19:00:00.000000-05:00) 0)
      ((1985-10-27 01:00:00.000000-04:00) 2)
      ((2015-10-21 23:28:00.000000-04:00) 3))) |}];
;;

let%expect_test _ =
  print_and_check_container_sexps [%here] (module Time_ns.Option) [
    Time_ns.Option.none;
    Time_ns.Option.some (Time_ns.epoch);
    Time_ns.Option.some (Time_ns.of_string "1955-11-12 18:38:00-08:00");
    Time_ns.Option.some (Time_ns.of_string "1985-10-26 21:00:00-08:00");
    Time_ns.Option.some (Time_ns.of_string "2015-10-21 19:28:00-08:00");
  ];
  [%expect {|
    (Set (
      ()
      ((1955-11-12 21:38:00.000000-05:00))
      ((1969-12-31 19:00:00.000000-05:00))
      ((1985-10-27 01:00:00.000000-04:00))
      ((2015-10-21 23:28:00.000000-04:00))))
    (Map (
      (() 0)
      (((1955-11-12 21:38:00.000000-05:00)) 2)
      (((1969-12-31 19:00:00.000000-05:00)) 1)
      (((1985-10-27 01:00:00.000000-04:00)) 3)
      (((2015-10-21 23:28:00.000000-04:00)) 4)))
    (Hash_set (
      ()
      ((1955-11-12 21:38:00.000000-05:00))
      ((1969-12-31 19:00:00.000000-05:00))
      ((1985-10-27 01:00:00.000000-04:00))
      ((2015-10-21 23:28:00.000000-04:00))))
    (Table (
      (() 0)
      (((1955-11-12 21:38:00.000000-05:00)) 2)
      (((1969-12-31 19:00:00.000000-05:00)) 1)
      (((1985-10-27 01:00:00.000000-04:00)) 3)
      (((2015-10-21 23:28:00.000000-04:00)) 4))) |}];
;;

let%expect_test _ =
  print_and_check_container_sexps [%here] (module Time_ns.Span) [
    Time_ns.Span.zero;
    Time_ns.Span.of_string "101.5ms";
    Time_ns.Span.of_string "3.125s";
    Time_ns.Span.of_string "252d";
  ];
  [%expect {|
    (Set (0s 101.5ms 3.125s 252d))
    (Map (
      (0s      0)
      (101.5ms 1)
      (3.125s  2)
      (252d    3)))
    (Hash_set (0s 101.5ms 3.125s 252d))
    (Table (
      (0s      0)
      (101.5ms 1)
      (3.125s  2)
      (252d    3))) |}]
;;

let%expect_test _ =
  print_and_check_container_sexps [%here] (module Time_ns.Span.Option) [
    Time_ns.Span.Option.none;
    Time_ns.Span.Option.some (Time_ns.Span.zero);
    Time_ns.Span.Option.some (Time_ns.Span.of_string "101.5ms");
    Time_ns.Span.Option.some (Time_ns.Span.of_string "3.125s");
    Time_ns.Span.Option.some (Time_ns.Span.of_string "252d");
  ];
  [%expect {|
    (Set (
      ()
      (0s)
      (101.5ms)
      (3.125s)
      (252d)))
    (Map (
      (() 0)
      ((0s)      1)
      ((101.5ms) 2)
      ((3.125s)  3)
      ((252d)    4)))
    (Hash_set (
      ()
      (0s)
      (101.5ms)
      (3.125s)
      (252d)))
    (Table (
      (() 0)
      ((0s)      1)
      ((101.5ms) 2)
      ((3.125s)  3)
      ((252d)    4))) |}];
;;

let%expect_test _ =
  print_and_check_container_sexps [%here] (module Time_ns.Ofday) [
    Time_ns.Ofday.start_of_day;
    Time_ns.Ofday.of_string "18:38:00";
    Time_ns.Ofday.of_string "21:00:00";
    Time_ns.Ofday.of_string "19:28:00";
  ];
  [%expect {|
    (Set (
      00:00:00.000000000 18:38:00.000000000 19:28:00.000000000 21:00:00.000000000))
    (Map (
      (00:00:00.000000000 0)
      (18:38:00.000000000 1)
      (19:28:00.000000000 3)
      (21:00:00.000000000 2)))
    (Hash_set (
      00:00:00.000000000 18:38:00.000000000 19:28:00.000000000 21:00:00.000000000))
    (Table (
      (00:00:00.000000000 0)
      (18:38:00.000000000 1)
      (19:28:00.000000000 3)
      (21:00:00.000000000 2))) |}];
;;

let%expect_test _ =
  print_and_check_container_sexps [%here] (module Time_ns.Ofday.Option) [
    Time_ns.Ofday.Option.none;
    Time_ns.Ofday.Option.some (Time_ns.Ofday.start_of_day);
    Time_ns.Ofday.Option.some (Time_ns.Ofday.of_string "18:38:00");
    Time_ns.Ofday.Option.some (Time_ns.Ofday.of_string "21:00:00");
    Time_ns.Ofday.Option.some (Time_ns.Ofday.of_string "19:28:00");
  ];
  [%expect {|
    (Set (
      ()
      (00:00:00.000000000)
      (18:38:00.000000000)
      (19:28:00.000000000)
      (21:00:00.000000000)))
    (Map (
      (() 0)
      ((00:00:00.000000000) 1)
      ((18:38:00.000000000) 2)
      ((19:28:00.000000000) 4)
      ((21:00:00.000000000) 3)))
    (Hash_set (
      ()
      (00:00:00.000000000)
      (18:38:00.000000000)
      (19:28:00.000000000)
      (21:00:00.000000000)))
    (Table (
      (() 0)
      ((00:00:00.000000000) 1)
      ((18:38:00.000000000) 2)
      ((19:28:00.000000000) 4)
      ((21:00:00.000000000) 3))) |}];
;;

let%expect_test "time ns zone offset parsing" =
  let to_string t = Time_ns.to_string_abs ~zone:Time_ns.Zone.utc t in
  let test string =
    print_endline (to_string (Time_ns.of_string string));
  in
  test "2000-01-01 12:34:56.789012-00:00";
  test "2000-01-01 12:34:56.789012-0:00";
  test "2000-01-01 12:34:56.789012-00";
  test "2000-01-01 12:34:56.789012-0";
  [%expect {|
    2000-01-01 12:34:56.789012Z
    2000-01-01 12:34:56.789012Z
    2000-01-01 12:34:56.789012Z
    2000-01-01 12:34:56.789012Z |}];
  test "2000-01-01 12:34:56.789012-05:00";
  test "2000-01-01 12:34:56.789012-5:00";
  test "2000-01-01 12:34:56.789012-05";
  test "2000-01-01 12:34:56.789012-5";
  [%expect {|
    2000-01-01 17:34:56.789012Z
    2000-01-01 17:34:56.789012Z
    2000-01-01 17:34:56.789012Z
    2000-01-01 17:34:56.789012Z |}];
  test "2000-01-01 12:34:56.789012-23:00";
  test "2000-01-01 12:34:56.789012-23";
  [%expect {|
    2000-01-02 11:34:56.789012Z
    2000-01-02 11:34:56.789012Z |}];
  test "2000-01-01 12:34:56.789012-24:00";
  test "2000-01-01 12:34:56.789012-24";
  [%expect {|
    2000-01-02 12:34:56.789012Z
    2000-01-02 12:34:56.789012Z |}];
;;

let%expect_test "time ns zone invalid offset parsing" =
  let test here string =
    require_does_raise here (fun () ->
      Time_ns.of_string string)
  in
  test [%here] "2000-01-01 12:34:56.789012-0:";
  test [%here] "2000-01-01 12:34:56.789012-00:";
  test [%here] "2000-01-01 12:34:56.789012-0:0";
  test [%here] "2000-01-01 12:34:56.789012-00:0";
  test [%here] "2000-01-01 12:34:56.789012-:";
  test [%here] "2000-01-01 12:34:56.789012-:00";
  test [%here] "2000-01-01 12:34:56.789012-";
  [%expect {|
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-0:"
     ("Time.Ofday: invalid string"
      0:
      "expected colon or am/pm suffix with optional space after minutes"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-00:"
     ("Time.Ofday: invalid string"
      00:
      "expected colon or am/pm suffix with optional space after minutes"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-0:0"
     ("Time.Ofday: invalid string"
      0:0
      "expected colon or am/pm suffix with optional space after minutes"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-00:0"
     ("Time.Ofday: invalid string"
      00:0
      "expected colon or am/pm suffix with optional space after minutes"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-:"
     (Invalid_argument "index out of bounds"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-:00"
     (Failure "Char.get_digit_exn ':': not a digit"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-"
     (Invalid_argument "index out of bounds")) |}];
  test [%here] "2000-01-01 12:34:56.789012-25:00";
  test [%here] "2000-01-01 12:34:56.789012-25";
  [%expect {|
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-25:00"
     ("Time.Ofday: invalid string" 25:00 "hours out of bounds"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012-25"
     ("Time.Ofday: invalid string" 25:00 "hours out of bounds")) |}];
  test [%here] "2000-01-01 12:34:56.789012--1:00";
  test [%here] "2000-01-01 12:34:56.789012--1";
  [%expect {|
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012--1:00"
     (Failure "Char.get_digit_exn '-': not a digit"))
    (time.ml.Make.Time_of_string
     "2000-01-01 12:34:56.789012--1"
     (Invalid_argument "index out of bounds")) |}];
;;
