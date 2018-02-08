open Core

let%test_module "Zone.V1" = (module Stable_unit_test.Make (struct
    include Time.Stable.Zone.V1

    let equal z1 z2 = Time.Zone.name z1 = Time.Zone.name z2

    let tests =
      let zone = Time.Zone.find_exn in
      [ zone "nyc", "America/New_York", "\016America/New_York";
        zone "ldn", "Europe/London",    "\013Europe/London";
        zone "hkg", "Asia/Hong_Kong",   "\014Asia/Hong_Kong";
      ]
    ;;

    let%test_unit "special form [Local]" =
      ignore (t_of_sexp (Sexp.of_string "Local"))
    ;;
  end))

let%test_module "Zone.V1" = (module Stable_unit_test.Make (struct
    include Core.Core_stable.Time.Zone.V1

    let equal z1 z2 = Time.Zone.name z1 = Time.Zone.name z2

    let tests =
      let zone = Time.Zone.find_exn in
      [ zone "nyc", "America/New_York", "\016America/New_York";
        zone "ldn", "Europe/London",    "\013Europe/London";
        zone "hkg", "Asia/Hong_Kong",   "\014Asia/Hong_Kong";
      ]
    ;;

    let%test_unit "special form [Local]" =
      ignore (t_of_sexp (Sexp.of_string "Local"))
    ;;
  end))

let%test_module "next_clock_shift, prev_clock_shift" =
  (module struct
    module Span = Time.Span

    let mkt ?(year=2013) month day hr min =
      let ofday_mins = (hr * 60) + min in
      let ofday      =
        Span.of_sec (Float.of_int (ofday_mins * 60))
        |> Time.Ofday.of_span_since_start_of_day
      in
      let date = Date.create_exn ~y:year ~m:month ~d:day in
      Time.of_date_ofday date ofday ~zone:Time.Zone.utc

    let%test "UTC" =
      Option.is_none (Time.Zone.next_clock_shift Time.Zone.utc ~strictly_after: (mkt Jan 01 12 00)) &&
      Option.is_none (Time.Zone.prev_clock_shift Time.Zone.utc ~at_or_before:   (mkt Jan 01 12 00))

    let expect_next strictly_after next =
      [%test_result: (Time.t * Span.t) option]
        ~expect:(Some next)
        (Time.Zone.next_clock_shift (Time.Zone.find_exn "Europe/London") ~strictly_after)

    let expect_prev at_or_before prev =
      [%test_result: (Time.t * Span.t) option]
        ~expect:(Some prev)
        (Time.Zone.prev_clock_shift (Time.Zone.find_exn "Europe/London") ~at_or_before)

    let expect_between time prev next =
      expect_prev time prev;
      expect_next time next

    let bst_start      = mkt ~year:2013 Mar 31  01 00, Span.hour
    let bst_end        = mkt ~year:2013 Oct 27  01 00, Span.(neg hour)
    let bst_start_2014 = mkt ~year:2014 Mar 30  01 00, Span.hour

    let%test_unit "outside BST" =
      expect_next (mkt Jan 01  12 00) bst_start

    let%test_unit "just before BST start" =
      expect_next (mkt Mar 31  00 59) bst_start

    let%test_unit "on BST start time" =
      expect_next (mkt Mar 31  01 00) bst_end

    let%test_unit "just after BST start" =
      expect_between (mkt Mar 31  01 01) bst_start bst_end

    let%test_unit "inside BST" =
      expect_between (mkt Jun 01  12 00) bst_start bst_end

    let%test_unit "just before BST end" =
      expect_between (mkt Oct 27  00 59) bst_start bst_end

    let%test_unit "BST end time" =
      expect_between (mkt Oct 27  01 00) bst_end bst_start_2014

    let%test_unit "just after BST end" =
      expect_between (mkt Oct 27  01 01) bst_end bst_start_2014
  end)

let%test_module "clock shift stuff" =
  (module struct
    (* Some stuff to make [%test_result: t] failures look nicer. Notice that a bug in
       [to_date_ofday] could cause this thing to lie. *)
    type time = Time.t [@@deriving compare]

    let sexp_of_time t =
      let d, o = Time.to_date_ofday t ~zone:Time.Zone.utc in
      [%sexp_of:  Date.t * Time.Ofday.t * Time.Zone.t] (d, o, Time.Zone.utc)

    type to_date_ofday_ambiguity =
      [ `Only
      | `Also_at of time
      | `Also_skipped of (Date.t * Time.Ofday.t)
      ] [@@deriving compare, sexp_of]

    type of_date_ofday_result =
      [ `Once of time
      | `Twice of time * time
      | `Never of time
      ] [@@deriving compare, sexp_of]

    let zone = Time.Zone.find_exn "Europe/London"

    let mkt month day hr min =
      let ofday =
        Time.Span.of_sec (60. *. ((Float.of_int hr *. 60.) +. Float.of_int min))
        |> Time.Ofday.of_span_since_start_of_day
      in
      let date = Date.create_exn ~y:2013 ~m:month ~d:day in
      Time.of_date_ofday date ofday ~zone:Time.Zone.utc
    ;;

    let simple_case ?(zone = zone) date ofday time =
      [%test_result: of_date_ofday_result]
        ~expect:(`Once time) (Time.of_date_ofday_precise ~zone date ofday);
      [%test_result: Date.t * Time.Ofday.t * to_date_ofday_ambiguity]
        ~expect:(date, ofday, `Only) (Time.to_date_ofday_precise ~zone time)

    let skipped_this_time date ofday skipped_at =
      [%test_result: of_date_ofday_result]
        ~expect:(`Never skipped_at) (Time.of_date_ofday_precise ~zone date ofday);
      let time = Time.of_date_ofday ~zone date ofday in
      let d, o, a = Time.to_date_ofday_precise ~zone time in
      [%test_result: Date.t] ~expect:date d;
      let diff = Time.Ofday.diff o ofday in
      [%test_result: Time.Span.t] ~expect:Time.Span.hour diff;
      [%test_result: to_date_ofday_ambiguity] ~expect:(`Also_skipped (date, ofday)) a

    let skipped_prev_time date ofday time =
      [%test_result: of_date_ofday_result]
        ~expect:(`Once time) (Time.of_date_ofday_precise ~zone date ofday);
      let d, o, a = Time.to_date_ofday_precise ~zone time in
      [%test_result: Date.t] ~expect:date d;
      [%test_result: Time.Ofday.t] ~expect:ofday o;
      [%test_result: to_date_ofday_ambiguity]
        ~expect:(`Also_skipped (date, Option.value_exn (Time.Ofday.sub o Time.Span.hour))) a

    let repeated_time date ofday ~first =
      let second = Time.add first Time.Span.hour in
      [%test_result: of_date_ofday_result]
        ~expect:(`Twice (first, second)) (Time.of_date_ofday_precise ~zone date ofday);
      [%test_result: Date.t * Time.Ofday.t * to_date_ofday_ambiguity]
        ~expect:(date, ofday, `Also_at second) (Time.to_date_ofday_precise ~zone first);
      [%test_result: Date.t * Time.Ofday.t * to_date_ofday_ambiguity]
        ~expect:(date, ofday, `Also_at first) (Time.to_date_ofday_precise ~zone second)

    let (^:) hr min = Time.Ofday.create ~hr ~min ()

    let outside_bst = Date.of_string "2013-01-01";;
    let inside_bst  = Date.of_string "2013-06-01";;

    let%test_unit "of_date_ofday_precise, outside BST" =
      simple_case outside_bst (12^:00) (mkt Jan 01  12 00)

    let%test_unit "of_date_ofday_precise, inside BST" =
      simple_case inside_bst (12^:00) (mkt Jun 01  11 00)

    let bst_start   = Date.of_string "2013-03-31";;
    let bst_end     = Date.of_string "2013-10-27";;

    let%test_unit "of_date_ofday_precise, just before skipped hour" =
      simple_case bst_start (00^:59) (mkt Mar 31  00 59)

    let%test_unit "of_date_ofday_precise, start of skipped hour" =
      skipped_this_time bst_start (01^:00) (mkt Mar 31  01 00)

    let%test_unit "of_date_ofday_precise, during skipped hour" =
      skipped_this_time bst_start (01^:30) (mkt Mar 31  01 00)

    let%test_unit "of_date_ofday_precise, end of skipped hour" =
      skipped_prev_time bst_start (02^:00) (mkt Mar 31  01 00)

    let%test_unit "of_date_ofday_precise, just after skipped hour" =
      skipped_prev_time bst_start (02^:01) (mkt Mar 31  01 01)

    let%test_unit "of_date_ofday_precise, later after skipped hour" =
      simple_case bst_start (03^:00) (mkt Mar 31  02 00)

    let%test_unit "of_date_ofday_precise, just before repeated hour" =
      simple_case bst_end (00^:59) (mkt Oct 26  23 59)

    let%test_unit "of_date_ofday_precise, start of repeated hour" =
      repeated_time bst_end (01^:00) ~first:(mkt Oct 27  00 00)

    let%test_unit "of_date_ofday_precise, during repeated hour" =
      repeated_time bst_end (01^:30) ~first:(mkt Oct 27  00 30)

    let%test_unit "of_date_ofday_precise, end of repeated hour" =
      simple_case bst_end (02^:00) (mkt Oct 27  02 00)

    let%test_unit "of_date_ofday_precise, after repeated hour" =
      simple_case bst_end (02^:01) (mkt Oct 27  02 01)

    let%test_unit "of_date_ofday_precise, time zone with no transitions" =
      simple_case (Date.of_string "2013-01-01") (12^:00) (mkt Jan 01 04 00)
        ~zone:(Time.Zone.of_utc_offset ~hours:8)

    let%test_unit "of_date_ofday_precise, time zone with no recent transitions" =
      (* The Hong Kong time zone observed daylight savings from 1941 to 1979, but not
         since, so the zone arithmetic for recent dates hits boundary cases. *)
      simple_case (Date.of_string "2013-01-01") (12^:00) (mkt Jan 01 04 00)
        ~zone:(Time.Zone.find_exn "Asia/Hong_Kong")
  end)
