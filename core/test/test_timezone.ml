open! Core
open! Import

let with_the_one_and_only f =
  let open Timezone.Private.Zone_cache in
  Capsule.Mutex.with_lock The_one_and_only.mutex ~f:(fun password ->
    Capsule.Data.get The_one_and_only.capsule ~password ~f)
;;

(* [init] is a noop in javascript, so this is expected to not work. It will only fail
   with [Private.Zone_cache.find], which only checks the cache and shouldn't be used
   directly anyway. *)
let%expect_test (_ [@tags "no-js"]) =
  let open Timezone.Private.Zone_cache in
  init ();
  let result = Option.is_some (find "America/New_York") in
  require result;
  (* keep this test from contaminating tests later in the file *)
  clear ()
;;

let%expect_test (_ [@tags "js-only"]) =
  let public_find = Timezone.find in
  let open Timezone.Private.Zone_cache in
  (* This test is a dual to the one above that shows that [init] is a noop and how the
     behavior differs between native and web contexts. *)
  init ();
  require_none [%sexp_of: Timezone.t] (find "America/New_York");
  let is_empty =
    with_the_one_and_only (fun the_one_and_only ->
      Hashtbl.is_empty the_one_and_only.table)
  in
  let new_york = public_find "America/New_York" in
  let { portended = new_york, full } =
    with_the_one_and_only (fun the_one_and_only ->
      let full = the_one_and_only.full in
      { portended = new_york, full })
  in
  require is_empty;
  require_some new_york;
  require (not full);
  (* keep this test from contaminating tests later in the file *)
  clear ()
;;

let%expect_test "Zone.V1" =
  print_and_check_stable_type
    ~cr:Comment
    (module Timezone.Stable.V1)
    (List.concat
       [ List.map ~f:Timezone.find_exn [ "nyc"; "ldn"; "hkg"; "tyo"; "chi"; "UTC"; "GMT" ]
       ; List.map
           ~f:(fun hours -> Time_float.Zone.of_utc_offset ~hours)
           [ 0; 1; -1; 24; -24 ]
       ; (* these time zones only round-trip via [Stable.Full_data] *)
         [ Time_float.Zone.of_utc_offset_explicit_name ~name:"my zone" ~hours:1
         ; Time_float.Zone.of_utc_offset_in_seconds_round_down
             (Time_float.Span.of_string "-3h2m1s")
         ; Time_float.Zone.add_offset_in_seconds_round_down
             (Timezone.find_exn "hkg")
             ~name:"hkg+1"
             ~span:Time_float.Span.hour
         ]
       ]);
  [%expect
    {|
    (bin_shape_digest d9a8da25d5656b016fb4dbdc2e4197fb)
    ((sexp   America/New_York)
     (bin_io "\016America/New_York"))
    ((sexp   Europe/London)
     (bin_io "\rEurope/London"))
    ((sexp   Asia/Hong_Kong)
     (bin_io "\014Asia/Hong_Kong"))
    ((sexp   Asia/Tokyo)
     (bin_io "\nAsia/Tokyo"))
    ((sexp   America/Chicago)
     (bin_io "\015America/Chicago"))
    ((sexp   UTC)
     (bin_io "\003UTC"))
    ((sexp   GMT)
     (bin_io "\003GMT"))
    ((sexp   UTC)
     (bin_io "\003UTC"))
    ((sexp   UTC+1)
     (bin_io "\005UTC+1"))
    ((sexp   UTC-1)
     (bin_io "\005UTC-1"))
    ((sexp   UTC+24)
     (bin_io "\006UTC+24"))
    ((sexp   UTC-24)
     (bin_io "\006UTC-24"))
    (* require-failed: lib/core/test/test_timezone.ml:LINE:COL. *)
    ("unexpectedly raised" (
      Of_sexp_error
      "Timezone.t_of_sexp: (\"unknown zone\" (zone \"my zone\"))"
      (invalid_sexp "my zone")))
    (* require-failed: lib/core/test/test_timezone.ml:LINE:COL. *)
    ("unexpectedly raised" (
      Of_sexp_error
      "Timezone.t_of_sexp: (Failure \"Int.of_string: \\\"3:02:01\\\"\")"
      (invalid_sexp UTC-3:02:01)))
    (* require-failed: lib/core/test/test_timezone.ml:LINE:COL. *)
    ("unexpectedly raised" (
      Of_sexp_error
      "Timezone.t_of_sexp: (\"unknown zone\" (zone hkg+1))"
      (invalid_sexp hkg+1)))
    |}];
  require_does_not_raise (fun () ->
    ignore (Timezone.Stable.V1.t_of_sexp (Atom "Local") : Timezone.t))
;;

module Common_dates = struct
  let mkt ?(year = 2013) month day hr min =
    let ofday_mins = (hr * 60) + min in
    let ofday =
      Time_float.Span.of_sec (Float.of_int (ofday_mins * 60))
      |> Time_float.Ofday.of_span_since_start_of_day_exn
    in
    let date = Date.create_exn ~y:year ~m:month ~d:day in
    Time_float.of_date_ofday date ofday ~zone:Timezone.utc
  ;;

  let bst_start = mkt ~year:2013 Mar 31 01 00, Time_float.Span.hour
  let bst_end = mkt ~year:2013 Oct 27 01 00, Time_float.Span.(neg hour)
  let bst_start_2014 = mkt ~year:2014 Mar 30 01 00, Time_float.Span.hour
end

module%test [@name "differences in abbreviations between native and js"] _ = struct
  open Common_dates

  let print_abbrev ~zone ~time =
    let time_in_seconds =
      time
      |> Time_float.to_span_since_epoch
      |> Time_float.Span.to_int63_seconds_round_down_exn
      |> Timezone.Time_in_seconds.Span.of_int63_seconds
      |> Timezone.Time_in_seconds.of_span_since_epoch
    in
    let index = Timezone.index zone time_in_seconds in
    print_endline (Timezone.index_abbreviation_exn zone index)
  ;;

  let%expect_test ("bst-start native" [@tags "no-js"]) =
    print_abbrev ~zone:(Timezone.find_exn "Europe/London") ~time:(fst bst_start);
    [%expect {| BST |}]
  ;;

  let%expect_test ("bst-start javascript" [@tags "js-only"]) =
    print_abbrev ~zone:(Timezone.find_exn "Europe/London") ~time:(fst bst_start);
    [%expect {| |}]
  ;;

  let%expect_test ("bst-end native" [@tags "no-js"]) =
    print_abbrev ~zone:(Timezone.find_exn "Europe/London") ~time:(fst bst_end);
    [%expect {| GMT |}]
  ;;

  let%expect_test ("bst-end javascript" [@tags "js-only"]) =
    print_abbrev ~zone:(Timezone.find_exn "Europe/London") ~time:(fst bst_end);
    [%expect {| |}]
  ;;
end

module%test [@name "next_clock_shift, prev_clock_shift"] _ = struct
  open Common_dates

  let%expect_test "UTC" =
    Time_float.Zone.next_clock_shift Timezone.utc ~strictly_after:(mkt Jan 01 12 00)
    |> require_none [%sexp_of: Time_float.t * Time_float.Span.t];
    Time_float.Zone.prev_clock_shift Timezone.utc ~at_or_before:(mkt Jan 01 12 00)
    |> require_none [%sexp_of: Time_float.t * Time_float.Span.t]
  ;;

  let expect_next strictly_after next =
    [%test_result: (Time_float.t * Time_float.Span.t) option]
      ~expect:(Some next)
      (Time_float.Zone.next_clock_shift
         (Timezone.find_exn "Europe/London")
         ~strictly_after)
  ;;

  let expect_prev at_or_before prev =
    [%test_result: (Time_float.t * Time_float.Span.t) option]
      ~expect:(Some prev)
      (Time_float.Zone.prev_clock_shift (Timezone.find_exn "Europe/London") ~at_or_before)
  ;;

  let expect_between time prev next =
    expect_prev time prev;
    expect_next time next
  ;;

  let%expect_test "outside BST" = expect_next (mkt Jan 01 12 00) bst_start
  let%expect_test "just before BST start" = expect_next (mkt Mar 31 00 59) bst_start
  let%expect_test "on BST start time" = expect_next (mkt Mar 31 01 00) bst_end

  let%expect_test "just after BST start" =
    expect_between (mkt Mar 31 01 01) bst_start bst_end
  ;;

  let%expect_test "inside BST" = expect_between (mkt Jun 01 12 00) bst_start bst_end

  let%expect_test "just before BST end" =
    expect_between (mkt Oct 27 00 59) bst_start bst_end
  ;;

  let%expect_test "BST end time" =
    expect_between (mkt Oct 27 01 00) bst_end bst_start_2014
  ;;

  let%expect_test "just after BST end" =
    expect_between (mkt Oct 27 01 01) bst_end bst_start_2014
  ;;
end

module%test [@name "clock shift stuff"] _ = struct
  module Test (Time : sig
      type t [@@deriving compare]

      module Span : sig
        type t [@@deriving compare, sexp_of]

        val of_int_sec : int -> t
        val hour : t
      end

      module Ofday : sig
        type t [@@deriving compare]

        val ( ^: ) : int -> int -> t
        val to_string_trimmed : t -> string
        val of_span_since_start_of_day_exn : Span.t -> t
        val diff : t -> t -> Span.t
        val sub : t -> Span.t -> t option
      end

      val add : t -> Span.t -> t
      val to_string_trimmed : t -> zone:Timezone.t -> string

      val of_date_ofday
        :  ?prefer:Timezone.Earlier_or_later.t
        -> zone:Timezone.t
        -> Date.t
        -> Ofday.t
        -> t

      val of_date_ofday_precise
        :  Date.t
        -> Ofday.t
        -> zone:Timezone.t
        -> [ `Once of t | `Twice of t * t | `Never of t ]

      val to_date_ofday_precise
        :  t
        -> zone:Timezone.t
        -> Date.t
           * Ofday.t
           * [ `Only | `Also_at of t | `Also_skipped of Date.t * Ofday.t ]
    end) =
  struct
    module Time = struct
      include Time

      module Ofday = struct
        include Ofday

        let sexp_of_t t = to_string_trimmed t |> sexp_of_string
      end

      let sexp_of_t t = to_string_trimmed t ~zone:Timezone.utc ^ " UTC" |> sexp_of_string
    end

    type to_date_ofday_ambiguity =
      [ `Only
      | `Also_at of Time.t
      | `Also_skipped of Date.t * Time.Ofday.t
      ]
    [@@deriving compare, sexp_of]

    type of_date_ofday_result =
      [ `Once of Time.t
      | `Twice of Time.t * Time.t
      | `Never of Time.t
      ]
    [@@deriving compare, sexp_of]

    let zone = Timezone.find_exn "Europe/London"

    let mkt ?(y = 2013) month day hr min =
      let ofday =
        Time.Span.of_int_sec (60 * ((hr * 60) + min))
        |> Time.Ofday.of_span_since_start_of_day_exn
      in
      let date = Date.create_exn ~y ~m:month ~d:day in
      Time.of_date_ofday date ofday ~zone:Timezone.utc
    ;;

    let simple_case ?(zone = zone) date ofday time =
      [%test_result: of_date_ofday_result]
        ~expect:(`Once time)
        (Time.of_date_ofday_precise ~zone date ofday);
      [%test_result: Date.t * Time.Ofday.t * to_date_ofday_ambiguity]
        ~expect:(date, ofday, `Only)
        (Time.to_date_ofday_precise ~zone time)
    ;;

    let skipped_this_time ?(zone = zone) date ofday skipped_at =
      [%test_result: of_date_ofday_result]
        ~expect:(`Never skipped_at)
        (Time.of_date_ofday_precise ~zone date ofday);
      let time = Time.of_date_ofday ~zone date ofday in
      let d, o, a = Time.to_date_ofday_precise ~zone time in
      [%test_result: Date.t] ~expect:date d;
      let diff = Time.Ofday.diff o ofday in
      [%test_result: Time.Span.t] ~expect:Time.Span.hour diff;
      [%test_result: to_date_ofday_ambiguity] ~expect:(`Also_skipped (date, ofday)) a
    ;;

    let skipped_prev_time date ofday time =
      [%test_result: of_date_ofday_result]
        ~expect:(`Once time)
        (Time.of_date_ofday_precise ~zone date ofday);
      let d, o, a = Time.to_date_ofday_precise ~zone time in
      [%test_result: Date.t] ~expect:date d;
      [%test_result: Time.Ofday.t] ~expect:ofday o;
      [%test_result: to_date_ofday_ambiguity]
        ~expect:(`Also_skipped (date, Option.value_exn (Time.Ofday.sub o Time.Span.hour)))
        a
    ;;

    let repeated_time ?(zone = zone) date ofday ~first =
      let second = Time.add first Time.Span.hour in
      [%test_result: of_date_ofday_result]
        ~expect:(`Twice (first, second))
        (Time.of_date_ofday_precise ~zone date ofday);
      [%test_result: Date.t * Time.Ofday.t * to_date_ofday_ambiguity]
        ~expect:(date, ofday, `Also_at second)
        (Time.to_date_ofday_precise ~zone first);
      [%test_result: Date.t * Time.Ofday.t * to_date_ofday_ambiguity]
        ~expect:(date, ofday, `Also_at first)
        (Time.to_date_ofday_precise ~zone second)
    ;;

    let ( ^: ) = Time.Ofday.( ^: )
    let outside_bst = Date.of_string "2013-01-01"
    let inside_bst = Date.of_string "2013-06-01"

    let%expect_test "of_date_ofday_precise, outside BST" =
      simple_case outside_bst (12 ^: 00) (mkt Jan 01 12 00)
    ;;

    let%expect_test "of_date_ofday_precise, inside BST" =
      simple_case inside_bst (12 ^: 00) (mkt Jun 01 11 00)
    ;;

    let bst_start = Date.of_string "2013-03-31"
    let bst_end = Date.of_string "2013-10-27"

    let%expect_test "of_date_ofday_precise, just before skipped hour" =
      simple_case bst_start (00 ^: 59) (mkt Mar 31 00 59)
    ;;

    let%expect_test "of_date_ofday_precise, start of skipped hour" =
      skipped_this_time bst_start (01 ^: 00) (mkt Mar 31 01 00)
    ;;

    let%expect_test "of_date_ofday_precise, during skipped hour" =
      skipped_this_time bst_start (01 ^: 30) (mkt Mar 31 01 00)
    ;;

    let%expect_test "of_date_ofday_precise, end of skipped hour" =
      skipped_prev_time bst_start (02 ^: 00) (mkt Mar 31 01 00)
    ;;

    let%expect_test "of_date_ofday_precise, just after skipped hour" =
      skipped_prev_time bst_start (02 ^: 01) (mkt Mar 31 01 01)
    ;;

    let%expect_test "of_date_ofday_precise, later after skipped hour" =
      simple_case bst_start (03 ^: 00) (mkt Mar 31 02 00)
    ;;

    let%expect_test "of_date_ofday_precise, just before repeated hour" =
      simple_case bst_end (00 ^: 59) (mkt Oct 26 23 59)
    ;;

    let%expect_test "of_date_ofday_precise, start of repeated hour" =
      repeated_time bst_end (01 ^: 00) ~first:(mkt Oct 27 00 00)
    ;;

    let%expect_test "of_date_ofday_precise, during repeated hour" =
      repeated_time bst_end (01 ^: 30) ~first:(mkt Oct 27 00 30)
    ;;

    let%expect_test "of_date_ofday_precise, end of repeated hour" =
      simple_case bst_end (02 ^: 00) (mkt Oct 27 02 00)
    ;;

    let%expect_test "of_date_ofday_precise, after repeated hour" =
      simple_case bst_end (02 ^: 01) (mkt Oct 27 02 01)
    ;;

    let%expect_test "of_date_ofday_precise, time zone with no transitions" =
      simple_case
        (Date.of_string "2013-01-01")
        (12 ^: 00)
        (mkt Jan 01 04 00)
        ~zone:(Timezone.of_utc_offset ~hours:8)
    ;;

    let%expect_test "of_date_ofday_precise, time zone with no recent transitions" =
      (* The Hong Kong time zone observed daylight savings from 1941 to 1979, but not
         since, so the zone arithmetic for recent dates hits boundary cases. *)
      simple_case
        (Date.of_string "2013-01-01")
        (12 ^: 00)
        (mkt Jan 01 04 00)
        ~zone:(Timezone.find_exn "Asia/Hong_Kong")
    ;;

    let%expect_test "of_date_ofday_precise, time zone with shift from 00:00 to 01:00" =
      simple_case
        (Date.of_string "2012-03-30")
        (00 ^: 01)
        (mkt ~y:2012 Mar 29 16 01)
        ~zone:(Timezone.find_exn "Asia/Hong_Kong")
    ;;

    let%expect_test "of_date_ofday_precise, time zone with shift from 00:00 to 23:00" =
      repeated_time
        (Date.of_string "2013-12-19")
        (23 ^: 01)
        ~first:(mkt Dec 19 20 01)
        ~zone:(Timezone.find_exn "Asia/Amman")
    ;;

    let%expect_test "of_date_ofday_precise, time zone with shift from 00:00 to 01:00" =
      let test_date = Date.of_string "2012-03-30" in
      let test_time_ofday = 00 ^: 01 in
      skipped_this_time
        test_date
        test_time_ofday
        (mkt ~y:2012 Mar 29 22 00)
        ~zone:(Timezone.find_exn "Asia/Amman")
    ;;

    let%expect_test "of_date_ofday_precise, time zone with shift from 01:00 to 00:00" =
      let test_date = Date.of_string "2021-10-29" in
      let test_time_ofday = 00 ^: 01 in
      repeated_time
        test_date
        test_time_ofday
        ~first:(mkt ~y:2021 Oct 28 21 01)
        ~zone:(Timezone.find_exn "Asia/Amman")
    ;;
  end

  module _ = Test (Time_float)
  module _ = Test (Time_ns)
end

let%expect_test "grammar" =
  Sexp_grammar_validation.validate_grammar
    (module struct
      include Timezone

      let quickcheck_generator =
        Timezone.init ();
        Quickcheck.Generator.of_list (Timezone.initialized_zones () |> List.map ~f:snd)
      ;;

      let quickcheck_shrinker = [%quickcheck.shrinker: _]
    end)
  |> Expect_test_helpers_core.require_ok;
  [%expect
    {| (Tagged ((key sexp_grammar.type_name) (value Timezone.t) (grammar String))) |}]
;;

let%expect_test "UTC survives round trip" =
  let test ?cr zone =
    require_does_not_raise ?cr (fun () ->
      print_s [%sexp (Time_float.utc_offset Time_float.epoch ~zone : Time_float.Span.t)])
  in
  test Time_float.Zone.utc;
  [%expect {| 0s |}];
  let zone =
    Time_float.Zone.utc
    |> Binable.to_string (module Time_float.Stable.Zone.Full_data.V1)
    |> Binable.of_string (module Time_float.Stable.Zone.Full_data.V1)
  in
  test zone;
  [%expect {| 0s |}]
;;

let%expect_test "time zone construction" =
  let utc time = [%sexp (Time_float.to_string_abs time ~zone:Timezone.utc : string)] in
  let transitions_of zone =
    let start = Time_float.of_string "2024-01-01 00:00:00Z" in
    let until = Time_float.of_string "2025-01-01 00:00:00Z" in
    let[@tail_mod_cons] rec loop index =
      match Timezone.index_has_next_clock_shift zone index with
      | false -> []
      | true ->
        let time = Time_float.Zone.index_next_clock_shift_time_exn zone index in
        (match Time_float.( < ) time until with
         | false -> []
         | true -> time :: loop (Timezone.Index.next index))
    in
    List.concat [ [ start ]; loop (Time_float.Zone.index zone start); [ until ] ]
  in
  let test maybe_zone =
    let initial_zone = Option.value maybe_zone ~default:Timezone.utc in
    let offset ?name span_string =
      let span = Time_float.Span.of_string span_string in
      match maybe_zone with
      | None -> Time_float.Zone.of_utc_offset_in_seconds_round_down ?name span
      | Some zone ->
        Time_float.Zone.add_offset_in_seconds_round_down
          zone
          ~name:(Option.value name ~default:(Timezone.name zone ^ span_string))
          ~span
    in
    let zones =
      [ initial_zone
      ; offset "+0s"
      ; offset "+1h"
      ; offset "-1h"
      ; offset "+30s"
      ; offset "-30s"
      ; offset "+30.1s"
      ; offset "-30.1s"
      ; offset "+30m" ~name:"one half hour"
      ]
    in
    let initial_transitions = transitions_of initial_zone in
    List.iter zones ~f:(fun zone ->
      let transitions = transitions_of zone in
      require
        ([%equal: Time_float.t list] transitions initial_transitions)
        ~if_false_then_print_s:
          [%lazy_message
            "UTC times at which DST transitions occur are inconsistent"
              (zone : Timezone.t)
              ~original:(List.map ~f:utc initial_transitions : Sexp.t list)
              ~modified:(List.map ~f:utc transitions : Sexp.t list)]);
    let zones_with_utc =
      if Option.is_none maybe_zone then zones else Timezone.utc :: zones
    in
    List.iter initial_transitions ~f:(fun time ->
      List.map zones_with_utc ~f:(fun zone ->
        [%sexp
          { zone : Timezone.t; time = (Time_float.to_string_abs time ~zone : string) }])
      |> Expectable.print)
  in
  (* We see UTC twice because [of_utc_offset_in_seconds_round_down Span.zero = UTC]. *)
  test None;
  [%expect
    {|
    ┌───────────────┬─────────────────────────────────────┐
    │ zone          │ time                                │
    ├───────────────┼─────────────────────────────────────┤
    │ UTC           │ 2024-01-01 00:00:00.000000Z         │
    │ UTC           │ 2024-01-01 00:00:00.000000Z         │
    │ UTC+1         │ 2024-01-01 01:00:00.000000+01:00    │
    │ UTC-1         │ 2023-12-31 23:00:00.000000-01:00    │
    │ UTC+0:00:30   │ 2024-01-01 00:00:30.000000+00:00:30 │
    │ UTC-0:00:30   │ 2023-12-31 23:59:30.000000-00:00:30 │
    │ UTC+0:00:30   │ 2024-01-01 00:00:30.000000+00:00:30 │
    │ UTC-0:00:31   │ 2023-12-31 23:59:29.000000-00:00:31 │
    │ one half hour │ 2024-01-01 00:30:00.000000+00:30    │
    └───────────────┴─────────────────────────────────────┘

    ┌───────────────┬─────────────────────────────────────┐
    │ zone          │ time                                │
    ├───────────────┼─────────────────────────────────────┤
    │ UTC           │ 2025-01-01 00:00:00.000000Z         │
    │ UTC           │ 2025-01-01 00:00:00.000000Z         │
    │ UTC+1         │ 2025-01-01 01:00:00.000000+01:00    │
    │ UTC-1         │ 2024-12-31 23:00:00.000000-01:00    │
    │ UTC+0:00:30   │ 2025-01-01 00:00:30.000000+00:00:30 │
    │ UTC-0:00:30   │ 2024-12-31 23:59:30.000000-00:00:30 │
    │ UTC+0:00:30   │ 2025-01-01 00:00:30.000000+00:00:30 │
    │ UTC-0:00:31   │ 2024-12-31 23:59:29.000000-00:00:31 │
    │ one half hour │ 2025-01-01 00:30:00.000000+00:30    │
    └───────────────┴─────────────────────────────────────┘
    |}];
  test (Some (Timezone.find_exn "Asia/Hong_Kong"));
  [%expect
    {|
    ┌──────────────────────┬─────────────────────────────────────┐
    │ zone                 │ time                                │
    ├──────────────────────┼─────────────────────────────────────┤
    │ UTC                  │ 2024-01-01 00:00:00.000000Z         │
    │ Asia/Hong_Kong       │ 2024-01-01 08:00:00.000000+08:00    │
    │ Asia/Hong_Kong+0s    │ 2024-01-01 08:00:00.000000+08:00    │
    │ Asia/Hong_Kong+1h    │ 2024-01-01 09:00:00.000000+09:00    │
    │ Asia/Hong_Kong-1h    │ 2024-01-01 07:00:00.000000+07:00    │
    │ Asia/Hong_Kong+30s   │ 2024-01-01 08:00:30.000000+08:00:30 │
    │ Asia/Hong_Kong-30s   │ 2024-01-01 07:59:30.000000+07:59:30 │
    │ Asia/Hong_Kong+30.1s │ 2024-01-01 08:00:30.000000+08:00:30 │
    │ Asia/Hong_Kong-30.1s │ 2024-01-01 07:59:29.000000+07:59:29 │
    │ one half hour        │ 2024-01-01 08:30:00.000000+08:30    │
    └──────────────────────┴─────────────────────────────────────┘

    ┌──────────────────────┬─────────────────────────────────────┐
    │ zone                 │ time                                │
    ├──────────────────────┼─────────────────────────────────────┤
    │ UTC                  │ 2025-01-01 00:00:00.000000Z         │
    │ Asia/Hong_Kong       │ 2025-01-01 08:00:00.000000+08:00    │
    │ Asia/Hong_Kong+0s    │ 2025-01-01 08:00:00.000000+08:00    │
    │ Asia/Hong_Kong+1h    │ 2025-01-01 09:00:00.000000+09:00    │
    │ Asia/Hong_Kong-1h    │ 2025-01-01 07:00:00.000000+07:00    │
    │ Asia/Hong_Kong+30s   │ 2025-01-01 08:00:30.000000+08:00:30 │
    │ Asia/Hong_Kong-30s   │ 2025-01-01 07:59:30.000000+07:59:30 │
    │ Asia/Hong_Kong+30.1s │ 2025-01-01 08:00:30.000000+08:00:30 │
    │ Asia/Hong_Kong-30.1s │ 2025-01-01 07:59:29.000000+07:59:29 │
    │ one half hour        │ 2025-01-01 08:30:00.000000+08:30    │
    └──────────────────────┴─────────────────────────────────────┘
    |}];
  test (Some (Timezone.find_exn "Europe/London"));
  [%expect
    {|
    ┌─────────────────────┬─────────────────────────────────────┐
    │ zone                │ time                                │
    ├─────────────────────┼─────────────────────────────────────┤
    │ UTC                 │ 2024-01-01 00:00:00.000000Z         │
    │ Europe/London       │ 2024-01-01 00:00:00.000000Z         │
    │ Europe/London+0s    │ 2024-01-01 00:00:00.000000Z         │
    │ Europe/London+1h    │ 2024-01-01 01:00:00.000000+01:00    │
    │ Europe/London-1h    │ 2023-12-31 23:00:00.000000-01:00    │
    │ Europe/London+30s   │ 2024-01-01 00:00:30.000000+00:00:30 │
    │ Europe/London-30s   │ 2023-12-31 23:59:30.000000-00:00:30 │
    │ Europe/London+30.1s │ 2024-01-01 00:00:30.000000+00:00:30 │
    │ Europe/London-30.1s │ 2023-12-31 23:59:29.000000-00:00:31 │
    │ one half hour       │ 2024-01-01 00:30:00.000000+00:30    │
    └─────────────────────┴─────────────────────────────────────┘

    ┌─────────────────────┬─────────────────────────────────────┐
    │ zone                │ time                                │
    ├─────────────────────┼─────────────────────────────────────┤
    │ UTC                 │ 2024-03-31 01:00:00.000000Z         │
    │ Europe/London       │ 2024-03-31 02:00:00.000000+01:00    │
    │ Europe/London+0s    │ 2024-03-31 02:00:00.000000+01:00    │
    │ Europe/London+1h    │ 2024-03-31 03:00:00.000000+02:00    │
    │ Europe/London-1h    │ 2024-03-31 01:00:00.000000Z         │
    │ Europe/London+30s   │ 2024-03-31 02:00:30.000000+01:00:30 │
    │ Europe/London-30s   │ 2024-03-31 01:59:30.000000+00:59:30 │
    │ Europe/London+30.1s │ 2024-03-31 02:00:30.000000+01:00:30 │
    │ Europe/London-30.1s │ 2024-03-31 01:59:29.000000+00:59:29 │
    │ one half hour       │ 2024-03-31 02:30:00.000000+01:30    │
    └─────────────────────┴─────────────────────────────────────┘

    ┌─────────────────────┬─────────────────────────────────────┐
    │ zone                │ time                                │
    ├─────────────────────┼─────────────────────────────────────┤
    │ UTC                 │ 2024-10-27 01:00:00.000000Z         │
    │ Europe/London       │ 2024-10-27 01:00:00.000000Z         │
    │ Europe/London+0s    │ 2024-10-27 01:00:00.000000Z         │
    │ Europe/London+1h    │ 2024-10-27 02:00:00.000000+01:00    │
    │ Europe/London-1h    │ 2024-10-27 00:00:00.000000-01:00    │
    │ Europe/London+30s   │ 2024-10-27 01:00:30.000000+00:00:30 │
    │ Europe/London-30s   │ 2024-10-27 00:59:30.000000-00:00:30 │
    │ Europe/London+30.1s │ 2024-10-27 01:00:30.000000+00:00:30 │
    │ Europe/London-30.1s │ 2024-10-27 00:59:29.000000-00:00:31 │
    │ one half hour       │ 2024-10-27 01:30:00.000000+00:30    │
    └─────────────────────┴─────────────────────────────────────┘

    ┌─────────────────────┬─────────────────────────────────────┐
    │ zone                │ time                                │
    ├─────────────────────┼─────────────────────────────────────┤
    │ UTC                 │ 2025-01-01 00:00:00.000000Z         │
    │ Europe/London       │ 2025-01-01 00:00:00.000000Z         │
    │ Europe/London+0s    │ 2025-01-01 00:00:00.000000Z         │
    │ Europe/London+1h    │ 2025-01-01 01:00:00.000000+01:00    │
    │ Europe/London-1h    │ 2024-12-31 23:00:00.000000-01:00    │
    │ Europe/London+30s   │ 2025-01-01 00:00:30.000000+00:00:30 │
    │ Europe/London-30s   │ 2024-12-31 23:59:30.000000-00:00:30 │
    │ Europe/London+30.1s │ 2025-01-01 00:00:30.000000+00:00:30 │
    │ Europe/London-30.1s │ 2024-12-31 23:59:29.000000-00:00:31 │
    │ one half hour       │ 2025-01-01 00:30:00.000000+00:30    │
    └─────────────────────┴─────────────────────────────────────┘
    |}];
  test (Some (Timezone.find_exn "America/New_York"));
  [%expect
    {|
    ┌────────────────────────┬─────────────────────────────────────┐
    │ zone                   │ time                                │
    ├────────────────────────┼─────────────────────────────────────┤
    │ UTC                    │ 2024-01-01 00:00:00.000000Z         │
    │ America/New_York       │ 2023-12-31 19:00:00.000000-05:00    │
    │ America/New_York+0s    │ 2023-12-31 19:00:00.000000-05:00    │
    │ America/New_York+1h    │ 2023-12-31 20:00:00.000000-04:00    │
    │ America/New_York-1h    │ 2023-12-31 18:00:00.000000-06:00    │
    │ America/New_York+30s   │ 2023-12-31 19:00:30.000000-04:59:30 │
    │ America/New_York-30s   │ 2023-12-31 18:59:30.000000-05:00:30 │
    │ America/New_York+30.1s │ 2023-12-31 19:00:30.000000-04:59:30 │
    │ America/New_York-30.1s │ 2023-12-31 18:59:29.000000-05:00:31 │
    │ one half hour          │ 2023-12-31 19:30:00.000000-04:30    │
    └────────────────────────┴─────────────────────────────────────┘

    ┌────────────────────────┬─────────────────────────────────────┐
    │ zone                   │ time                                │
    ├────────────────────────┼─────────────────────────────────────┤
    │ UTC                    │ 2024-03-10 07:00:00.000000Z         │
    │ America/New_York       │ 2024-03-10 03:00:00.000000-04:00    │
    │ America/New_York+0s    │ 2024-03-10 03:00:00.000000-04:00    │
    │ America/New_York+1h    │ 2024-03-10 04:00:00.000000-03:00    │
    │ America/New_York-1h    │ 2024-03-10 02:00:00.000000-05:00    │
    │ America/New_York+30s   │ 2024-03-10 03:00:30.000000-03:59:30 │
    │ America/New_York-30s   │ 2024-03-10 02:59:30.000000-04:00:30 │
    │ America/New_York+30.1s │ 2024-03-10 03:00:30.000000-03:59:30 │
    │ America/New_York-30.1s │ 2024-03-10 02:59:29.000000-04:00:31 │
    │ one half hour          │ 2024-03-10 03:30:00.000000-03:30    │
    └────────────────────────┴─────────────────────────────────────┘

    ┌────────────────────────┬─────────────────────────────────────┐
    │ zone                   │ time                                │
    ├────────────────────────┼─────────────────────────────────────┤
    │ UTC                    │ 2024-11-03 06:00:00.000000Z         │
    │ America/New_York       │ 2024-11-03 01:00:00.000000-05:00    │
    │ America/New_York+0s    │ 2024-11-03 01:00:00.000000-05:00    │
    │ America/New_York+1h    │ 2024-11-03 02:00:00.000000-04:00    │
    │ America/New_York-1h    │ 2024-11-03 00:00:00.000000-06:00    │
    │ America/New_York+30s   │ 2024-11-03 01:00:30.000000-04:59:30 │
    │ America/New_York-30s   │ 2024-11-03 00:59:30.000000-05:00:30 │
    │ America/New_York+30.1s │ 2024-11-03 01:00:30.000000-04:59:30 │
    │ America/New_York-30.1s │ 2024-11-03 00:59:29.000000-05:00:31 │
    │ one half hour          │ 2024-11-03 01:30:00.000000-04:30    │
    └────────────────────────┴─────────────────────────────────────┘

    ┌────────────────────────┬─────────────────────────────────────┐
    │ zone                   │ time                                │
    ├────────────────────────┼─────────────────────────────────────┤
    │ UTC                    │ 2025-01-01 00:00:00.000000Z         │
    │ America/New_York       │ 2024-12-31 19:00:00.000000-05:00    │
    │ America/New_York+0s    │ 2024-12-31 19:00:00.000000-05:00    │
    │ America/New_York+1h    │ 2024-12-31 20:00:00.000000-04:00    │
    │ America/New_York-1h    │ 2024-12-31 18:00:00.000000-06:00    │
    │ America/New_York+30s   │ 2024-12-31 19:00:30.000000-04:59:30 │
    │ America/New_York-30s   │ 2024-12-31 18:59:30.000000-05:00:30 │
    │ America/New_York+30.1s │ 2024-12-31 19:00:30.000000-04:59:30 │
    │ America/New_York-30.1s │ 2024-12-31 18:59:29.000000-05:00:31 │
    │ one half hour          │ 2024-12-31 19:30:00.000000-04:30    │
    └────────────────────────┴─────────────────────────────────────┘
    |}]
;;
