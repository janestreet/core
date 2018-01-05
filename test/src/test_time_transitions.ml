(* This module tests the conversions between times and dates/ofdays. The tests are chosen
   to stress the various cache layers added to [Time], [Time_ns], and [Zone] to make sure
   the behavior is correct and consistent during cache hits and cache misses of various
   sorts. *)
open! Core
open  Expect_test_helpers_kernel

module Examples = struct

  (* a set of time zone transitions to keep in mind when reading below *)
  let%expect_test "sample transitions" =
    let zone = Time.Zone.find_exn "America/New_York" in
    let time =
      Time.of_date_ofday ~zone (Date.of_string "2013-10-07") (Time.Ofday.of_string "09:30")
    in
    let prev_clock_shift = Time.Zone.prev_clock_shift zone ~at_or_before:time in
    print_s [%sexp (prev_clock_shift : (Time.t * Time.Span.t) option)];
    [%expect {| (((2013-03-10 03:00:00.000000-04:00) 1h)) |}];
    let next_clock_shift = Time.Zone.next_clock_shift zone ~strictly_after:time in
    print_s [%sexp (next_clock_shift : (Time.t * Time.Span.t) option)];
    [%expect {| (((2013-11-03 01:00:00.000000-05:00) -1h)) |}];
  ;;


  (* Most of our examples are of times that occur exactly once, not during a DST
     transition: *)

  let same_day = [
    "America/New_York", "2013-10-07", "01:00", `Once ("-04:00");
    "America/New_York", "2013-10-07", "02:00", `Once ("-04:00");
    "America/New_York", "2013-10-07", "03:00", `Once ("-04:00");
    "America/New_York", "2013-10-07", "04:00", `Once ("-04:00");
  ]

  let consecutive_days = [
    "America/New_York", "2013-10-05", "09:30", `Once ("-04:00");
    "America/New_York", "2013-10-06", "09:30", `Once ("-04:00");
    "America/New_York", "2013-10-07", "09:30", `Once ("-04:00");
    "America/New_York", "2013-10-08", "09:30", `Once ("-04:00");
  ]

  let non_consecutive_days = [
    "America/New_York", "2013-06-01", "09:30", `Once ("-04:00");
    "America/New_York", "2013-07-01", "09:30", `Once ("-04:00");
    "America/New_York", "2013-08-01", "09:30", `Once ("-04:00");
    "America/New_York", "2013-09-01", "09:30", `Once ("-04:00");
  ]

  let same_day_across_transition = [
    "America/New_York", "2013-03-10", "00:30", `Once ("-05:00");
    "America/New_York", "2013-03-10", "23:30", `Once ("-04:00");
  ]

  let consecutive_days_across_transition = [
    "America/New_York", "2013-03-09", "09:30", `Once ("-05:00");
    "America/New_York", "2013-03-10", "09:30", `Once ("-04:00");
  ]

  let consecutive_transitions = [
    "America/New_York", "2012-12-11", "09:30", `Once ("-05:00");
    "America/New_York", "2013-10-07", "09:30", `Once ("-04:00");
    "America/New_York", "2013-12-11", "09:30", `Once ("-05:00");
    "America/New_York", "2014-10-07", "09:30", `Once ("-04:00");
  ]

  let non_consecutive_transitions = [
    "America/New_York", "2012-10-07", "09:30", `Once ("-04:00");
    "America/New_York", "2013-10-07", "09:30", `Once ("-04:00");
    "America/New_York", "2014-10-07", "09:30", `Once ("-04:00");
    "America/New_York", "2015-10-07", "09:30", `Once ("-04:00");
  ]

  let different_zones = [
    "Europe/London"   , "2011-10-07", "09:30", `Once ("+01:00");
    "America/New_York", "2012-10-07", "09:30", `Once ("-04:00");
  ]

  (* During transitions forward that skip hours on the clock, we annotate [`Never] and
     include the GMT offset of the synthetic time [of_date_ofday] produces.

     After transitions forward that skip hours on the clock, for the following same amount
     of time as the transition, we annotate [`Echo] and include the skipped ofday that
     gets mapped to the given time.

     During transitions backward that repeat hours on the clock, we annotate [`Twice] with
     both GMT offsets for which the date and ofday occur, in the chronological order that
     they occur. *)
  let during_transitions = [
    "America/New_York", "2013-03-10", "02:00", `Never (`Synthetic "-05:00");
    "America/New_York", "2013-03-10", "02:30", `Never (`Synthetic "-05:00");
    "America/New_York", "2013-03-10", "03:00", `Echo  ("-04:00", `Of "02:00");
    "America/New_York", "2013-03-10", "03:30", `Echo  ("-04:00", `Of "02:30");
    "America/New_York", "2013-11-03", "01:00", `Twice ("-04:00", "-05:00");
    "America/New_York", "2013-11-03", "01:30", `Twice ("-04:00", "-05:00");
    "America/New_York", "2013-11-03", "02:00", `Once  ("-05:00");
  ]
end

module type Time = sig
  module Zone : sig
    type t

    val find_exn : string -> t
  end

  module Ofday : sig
    type t

    val of_string : string -> t
  end

  type t

  val of_string : string -> t
end

let run (type zone) (type ofday) (type time) time_m title f examples =
  let (module Time : Time
        with type Zone.t  = zone
         and type Ofday.t = ofday
         and type t       = time)
    = time_m
  in
  List.iter examples ~f:(fun (zone, date, ofday, occurrences) ->
    let time offset =
      Time.of_string (sprintf "%s %s%s" date ofday offset)
    in
    let times =
      match occurrences with
      | `Never (`Synthetic offset) -> `Never (`Synthetic (time offset))
      | `Once  offset              -> `Once  (time offset)
      | `Twice (offset1, offset2)  ->
        `Twice (time offset1, time offset2)
      | `Echo (offset, `Of ofday2) ->
        `Echo (time offset, `Of (Time.Ofday.of_string ofday2))
    in
    let zone  = Time.Zone.find_exn   zone  in
    let date  = Date.of_string       date  in
    let ofday = Time.Ofday.of_string ofday in
    f ~title ~zone ~date ~ofday ~times)

let test time_m f =
  let go title examples =
    require_does_not_raise [%here] (fun () ->
      run time_m title f  examples;
      run time_m title f (examples |> List.rev))
  in
  let open Examples in
  go "same_day"                           same_day;
  go "consecutive_days"                   consecutive_days;
  go "non_consecutive_days"               non_consecutive_days;
  go "same_day_across_transition"         same_day_across_transition;
  go "consecutive_days_across_transition" consecutive_days_across_transition;
  go "consecutive_transitions"            consecutive_transitions;
  go "non_consecutive_transitions"        non_consecutive_transitions;
  go "different_zones"                    different_zones;
  go "during_transitions"                 during_transitions;
;;

module Date_and_ofday = struct
  type t = Date.t * Time.Ofday.t
  [@@deriving compare, sexp_of]
  let equal = [%compare.equal: t]
end

module Date_and_ofday_ns = struct
  type t = Date.t * Time_ns.Ofday.t
  [@@deriving compare, sexp_of]
  let equal = [%compare.equal: t]
end

module Time_precise = struct
  type t = [
    | `Never of Time.t
    | `Once  of Time.t
    | `Twice of Time.t * Time.t
  ] [@@deriving compare, sexp_of]
  let equal = [%compare.equal: t]
end

module Date_ofday_precise = struct
  type t =
    Date.t       *
    Time.Ofday.t *
    [
      | `Only
      | `Also_at      of Time.t
      | `Also_skipped of Date.t * Time.Ofday.t
    ]
  [@@deriving compare, sexp_of]
  let equal = [%compare.equal: t]
end

let%expect_test "Time.of_date_ofday" =
  test (module Time) (fun ~title ~zone ~date ~ofday ~times ->
    match times with
    | `Once time | `Echo (time, _) | `Never (`Synthetic time) | `Twice (_, time) ->
      require_equal [%here] ~message:title (module Time)
        (Time.of_date_ofday ~zone date ofday)
        time);
  [%expect {| |}]

let%expect_test "Time_ns.of_date_ofday" =
  test (module Time_ns) (fun ~title ~zone ~date ~ofday ~times ->
    match times with
    | `Once time | `Echo (time, _) | `Never (`Synthetic time) | `Twice (_, time) ->
      require_equal [%here] ~message:title (module Time_ns)
        (Time_ns.of_date_ofday ~zone date ofday)
        time);
  [%expect {| |}]

let%expect_test "Time.to_date" =
  test (module Time) (fun ~title ~zone ~date ~ofday:_ ~times ->
    let f time =
      require_equal [%here] ~message:title (module Date)
        (Time.to_date ~zone time)
        date
    in
    match times with
    | `Never (`Synthetic time) -> f time
    | `Once  time              -> f time
    | `Echo  (time, _)         -> f time
    | `Twice (time1, time2) -> f time1; f time2);
  [%expect {| |}]

let%expect_test "Time_ns.to_date" =
  test (module Time_ns) (fun ~title ~zone ~date ~ofday:_ ~times ->
    let f time =
      require_equal [%here] ~message:title (module Date)
        (Time_ns.to_date ~zone time)
        date
    in
    match times with
    | `Never (`Synthetic time) -> f time
    | `Once  time              -> f time
    | `Echo  (time, _)         -> f time
    | `Twice (time1, time2) -> f time1; f time2);
  [%expect {| |}]

let%expect_test "Time.to_ofday" =
  test (module Time) (fun ~title ~zone ~date:_ ~ofday ~times ->
    let f time =
      require_equal [%here] ~message:title (module Time.Ofday)
        (Time.to_ofday ~zone time)
        ofday
    in
    match times with
    | `Once  time                        -> f time
    | `Echo  (time, _)                   -> f time
    | `Twice (time1, time2)              -> f time1; f time2
    | `Never (`Synthetic synthetic_time) ->
      let synthetic_ofday = Time.to_ofday ~zone synthetic_time in
      require [%here] (not (Time.Ofday.equal ofday synthetic_ofday))
        ~if_false_then_print_s:
          (lazy [%message
            "impossible time occurred"
              (synthetic_time  : Time.t)
              (synthetic_ofday : Time.Ofday.t)])
  );
  [%expect {| |}]

let%expect_test "Time_ns.to_ofday" =
  test (module Time_ns) (fun ~title ~zone ~date:_ ~ofday ~times ->
    let f time =
      require_equal [%here] ~message:title (module Time_ns.Ofday)
        (Time_ns.to_ofday ~zone time)
        ofday
    in
    match times with
    | `Once  time                        -> f time
    | `Echo  (time, _)                   -> f time
    | `Twice (time1, time2)              -> f time1; f time2
    | `Never (`Synthetic synthetic_time) ->
      let synthetic_ofday = Time_ns.to_ofday ~zone synthetic_time in
      require [%here] (not (Time_ns.Ofday.equal ofday synthetic_ofday))
        ~if_false_then_print_s:
          (lazy [%message
            "impossible time occurred"
              (synthetic_time  : Time_ns.t)
              (synthetic_ofday : Time_ns.Ofday.t)])
  );
  [%expect {| |}]

let%expect_test "Time.to_date_ofday" =
  test (module Time) (fun ~title ~zone ~date:_ ~ofday:_ ~times ->
    let f time =
      require_equal [%here] ~message:title (module Date_and_ofday)
        (Time.to_date_ofday ~zone time)
        (Time.to_date ~zone time, Time.to_ofday ~zone time)
    in
    match times with
    | `Never (`Synthetic time) -> f time
    | `Once  time              -> f time
    | `Echo  (time, _)         -> f time
    | `Twice (time1, time2)    -> f time1; f time2);
  [%expect {| |}]

let%expect_test "Time_ns.to_date_ofday" =
  test (module Time_ns) (fun ~title ~zone ~date:_ ~ofday:_ ~times ->
    let f time =
      require_equal [%here] ~message:title (module Date_and_ofday_ns)
        (Time_ns.to_date_ofday ~zone time)
        (Time_ns.to_date ~zone time, Time_ns.to_ofday ~zone time)
    in
    match times with
    | `Never (`Synthetic time) -> f time
    | `Once  time              -> f time
    | `Echo  (time, _)         -> f time
    | `Twice (time1, time2)    -> f time1; f time2);
  [%expect {| |}]

let%expect_test "Time.of_date_ofday_precise" =
  test (module Time) (fun ~title ~zone ~date ~ofday ~times ->
    require_equal [%here] ~message:title (module Time_precise)
      (Time.of_date_ofday_precise ~zone date ofday)
      (match times with
       | (`Once _ | `Twice _) as times -> times
       | `Echo (time, _)               -> `Once time
       | `Never (`Synthetic time)      ->
         match Time.Zone.prev_clock_shift zone ~at_or_before:time with
         | None -> raise_s [%message "skipped time has no prior transition"]
         | Some (transition, _) -> `Never transition));
  [%expect {| |}]

let%expect_test "Time.to_date_ofday_precise" =
  test (module Time) (fun ~title ~zone ~date ~ofday ~times ->
    (match times with
     | `Once time ->
       require_equal [%here] ~message:title (module Date_ofday_precise)
         (Time.to_date_ofday_precise ~zone time)
         (date, ofday, `Only)
     | `Echo (time, `Of other_ofday) ->
       require_equal [%here] ~message:title (module Date_ofday_precise)
         (Time.to_date_ofday_precise ~zone time)
         (date, ofday, `Also_skipped (date, other_ofday))
     | `Never (`Synthetic time) ->
       require_equal [%here] ~message:title (module Date_ofday_precise)
         (Time.to_date_ofday_precise ~zone time)
         (Time.to_date  ~zone time,
          Time.to_ofday ~zone time,
          `Also_skipped (date, ofday))
     | `Twice (time1, time2) ->
       require_equal [%here] ~message:title (module Date_ofday_precise)
         (Time.to_date_ofday_precise ~zone time1)
         (date, ofday, `Also_at time2);
       require_equal [%here] ~message:title (module Date_ofday_precise)
         (Time.to_date_ofday_precise ~zone time2)
         (date, ofday, `Also_at time1)));
  [%expect {| |}]
