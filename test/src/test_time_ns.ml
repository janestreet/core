open! Core
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
  ] ~hide_positions:true;
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
  ] ~hide_positions:true;
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
  ] ~hide_positions:true;
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
  ] ~hide_positions:true;
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

let%expect_test "Time_ns.Ofday.Stable.V1" =
  let module V = Time_ns.Ofday.Stable.V1 in
  let make int64 = V.of_int63_exn (Int63.of_int64_exn int64) in
  (* stable checks for values that round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) [
    make                  0L;
    make              1_000L;
    make  1_234_560_000_000L;
    make 52_200_010_101_000L;
    make 86_399_999_999_000L;
  ];
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp   00:00:00.000000)
     (bin_io "\000")
     (int63  0))
    ((sexp   00:00:00.000001)
     (bin_io "\254\232\003")
     (int63  1000))
    ((sexp   00:20:34.560000)
     (bin_io "\252\000\160\130q\031\001\000\000")
     (int63  1234560000000))
    ((sexp   14:30:00.010101)
     (bin_io "\252\b1\015\195y/\000\000")
     (int63  52200010101000))
    ((sexp   23:59:59.999999)
     (bin_io "\252\024\252N\145\148N\000\000")
     (int63  86399999999000)) |}];
  (* stable checks for values that do not precisely round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) ~cr:Comment [
    make 1L;
  ] ~hide_positions:true;
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp   00:00:00.000000)
     (bin_io "\001")
     (int63  1))
    (* require-failed: lib/core/test/src/test_time_ns.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       00:00:00.000000)
      (sexp           00:00:00.000000)
      (sexp_roundtrip 00:00:00.000000)) |}];
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

let%expect_test "Time_ns.Ofday.Option.Stable.V1" =
  let module V = Time_ns.Ofday.Option.Stable.V1 in
  let make int64 = V.of_int63_exn (Int63.of_int64_exn int64) in
  (* stable checks for values that round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) [
    make                           0L;
    make                       1_000L;
    make             987_654_321_000L;
    make          52_200_010_101_000L;
    make          86_399_999_999_000L;
    make (-4_611_686_018_427_387_904L);
  ];
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp (00:00:00.000000))
     (bin_io "\000")
     (int63  0))
    ((sexp (00:00:00.000001))
     (bin_io "\254\232\003")
     (int63  1000))
    ((sexp (00:16:27.654321))
     (bin_io "\252h\243\200\244\229\000\000\000")
     (int63  987654321000))
    ((sexp (14:30:00.010101))
     (bin_io "\252\b1\015\195y/\000\000")
     (int63  52200010101000))
    ((sexp (23:59:59.999999))
     (bin_io "\252\024\252N\145\148N\000\000")
     (int63  86399999999000))
    ((sexp ())
     (bin_io "\252\000\000\000\000\000\000\000\192")
     (int63  -4611686018427387904)) |}];
  (* stable checks for values that do not precisely round-trip *)
  print_and_check_stable_int63able_type [%here] (module V) ~cr:Comment [
    make 1L;
  ] ~hide_positions:true;
  [%expect {|
    (bin_shape_digest 2b528f4b22f08e28876ffe0239315ac2)
    ((sexp (00:00:00.000000))
     (bin_io "\001")
     (int63  1))
    (* require-failed: lib/core/test/src/test_time_ns.ml:LINE:COL. *)
    ("sexp serialization failed to round-trip"
      (original       (00:00:00.000000))
      (sexp           (00:00:00.000000))
      (sexp_roundtrip (00:00:00.000000))) |}];
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

let%test_module "Time_ns.Span" =
  (module struct
    open Time_ns.Span

    let half_microsecond = Int63.of_int 500

    let nearest_microsecond t =
      Int63.((Time_ns.Span.to_int63_ns t + half_microsecond) /% of_int 1000)
    ;;

    let min_kspan_value = to_span Time_ns.Span.min_value
    let max_kspan_value = to_span Time_ns.Span.max_value

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

    let%test_unit "Time.Span.t -> Time_ns.Span.t round trip with microsecond precision" =
      let open Time.Span in
      let time_spans =                                (* touchstones *)
        min_kspan_value :: max_kspan_value :: Time.(diff (now ()) epoch)
        :: Time.Span.([ zero; microsecond; millisecond; second; minute; hour; day;
                        scale day 365.
                      ])
      in
      let time_spans =                    (* a few randoms *)
        time_spans
        @ List.init 9 ~f:(fun _ -> Time.Span.(of_us (Random.float (to_us max_kspan_value))))
      in
      let time_spans =                    (* a few multiples *)
        List.concat_map time_spans
          ~f:(fun time_span ->
            List.map (List.range (-3) 4)
              ~f:(fun s -> Time.Span.scale time_span (float s)))
      in
      let time_spans =                    (* a few microseconds around *)
        List.concat_map time_spans
          ~f:(fun time_span ->
            List.map (List.range (-3) 4)
              ~f:(fun s -> Time.Span.(time_span + scale microsecond (float s))))
      in
      let time_spans =                    (* nearest microsecond *)
        List.map time_spans
          ~f:(fun s -> Time.Span.(of_us (Float.round_nearest (to_us s))))
      in
      let time_spans =                    (* in range *)
        List.filter time_spans
          ~f:(fun s -> Time.Span.(s >= min_kspan_value && s <= max_kspan_value))
      in
      List.iter time_spans
        ~f:(fun expect ->
          let kspan = to_span (of_span expect) in
          [%test_pred: Time.Span.t * Time.Span.t] (fun (a,b) -> abs (a - b) <= microsecond)
            (expect, kspan))
    ;;

    let%test_unit "Time_ns.Span.t -> Time.Span.t round trip" =
      let open Time_ns.Span in
      (* The default sexp is not precise enough. *)
      let sexp_of_t kspan = Sexp.Atom (Int63.to_string (to_int63_ns kspan) ^ "ns") in
      let kspans =                        (* touchstones *)
        min_value :: max_value :: Time_ns.(diff (now ()) epoch)
        :: [ zero; microsecond; millisecond; second; minute; hour; day;
             scale day 365.
           ]
      in
      let kspans =                        (* a few randoms *)
        kspans @ List.init 9 ~f:(fun _ -> of_us (Random.float (to_us max_value)))
      in
      (* Some tweaks will be out of range, which will raise exceptions. *)
      let filter_map list ~f =
        List.filter_map list ~f:(fun x -> Core.Option.try_with (fun () -> f x))
      in
      let kspans =                        (* a few multiples *)
        List.concat_map kspans
          ~f:(fun kspan ->
            filter_map (List.range (-3) 4) ~f:(fun s -> scale kspan (float s)))
      in
      let kspans =                        (* a few microseconds around *)
        List.concat_map kspans
          ~f:(fun kspan ->
            filter_map (List.range (-3) 4)
              ~f:(fun s -> kspan + scale microsecond (float s)))
      in
      let kspans =                        (* nearest microsecond *)
        List.map kspans
          ~f:(fun s -> of_int63_ns Int63.(nearest_microsecond s * of_int 1000))
      in
      let kspans =                        (* in range *)
        List.filter kspans ~f:(fun s -> s >= min_value && s <= max_value)
      in
      List.iter kspans
        ~f:(fun expect ->
          let kspan = of_span (to_span expect) in
          [%test_pred: t * t] (fun (a, b) -> abs (a - b) <= microsecond)
            (expect, kspan))
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
        assert Ofday.(ofday >= start_of_day && ofday < end_of_day);
        (* The sexp is more similar than the string.  The string includes different
           numbers of trailing zeros, which are trimmed in the sexp. *)
        [%test_result: Sexp.t] (Ofday.sexp_of_t ofday)
          ~expect:(Time.Ofday.sexp_of_t (Ofday.to_ofday ofday))
      with raised ->
        failwiths "check ofday"
          (Or_error.try_with (fun () -> [%sexp_of: Ofday.t] ofday),
           Span.to_int63_ns (Time_ns.Ofday.to_span_since_start_of_day ofday),
           raised)
          [%sexp_of: Sexp.t Or_error.t * Int63.t * exn]

    let%test_unit _ =
      (* Ensure that midnight_cache doesn't interfere with converting times that are much
         earlier or later than each other. *)
      check (Ofday.of_local_time epoch);
      check (Ofday.of_local_time (now ()));
      check (Ofday.of_local_time epoch)

    (* Reproduce a failure of the prior test before taking DST into account. *)
    let%test_unit "Ofday.of_local_time around fall 2015 DST transition" =
      List.iter
        ~f:(fun (time_ns, expect) ->
          let zone = Time.Zone.find_exn "US/Eastern" in
          (* First make sure Time.Ofday.of_time behaves as expected with these inputs. *)
          let time_ofday = Time.to_ofday (to_time time_ns) ~zone in
          if Time.Ofday.(<>) time_ofday (Time.Ofday.of_string expect) then
            failwiths "Time.Ofday.of_time"
              [%sexp (time_ns    : t),
                     (time_ofday : Time.Ofday.t),
                     (expect     : string)]
              Fn.id;
          (* Then make sure we do the same, correct thing. *)
          let ofday = Ofday.of_time time_ns ~zone in
          check ofday;
          if Ofday.(<>) ofday (Ofday.of_string expect) then
            failwiths "Ofday.of_time"
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

    let%test_unit "Ofday.of_time random" =
      List.iter !Time.Zone.likely_machine_zones ~f:(fun zone ->
        let zone = Time.Zone.find_exn zone in
        for _ = 0 to 1_000 do check (Ofday.of_time (random_nativeint_range ()) ~zone) done)

    let%test_unit "Ofday.of_local_time random" =
      for _ = 0 to 1_000 do check (Ofday.of_local_time (random_nativeint_range ())) done

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
