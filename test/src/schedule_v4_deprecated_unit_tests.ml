(* The module these tests cover is deprecated.  Please direct new work to Schedule_v5, and
   please ensure that any applicable test changes are reflected in both modules and
   tests. *)
open! Core

let%test_module "Schedule" =
  (module struct
    open Schedule_v4_deprecated

    let next_representable_time t =
      Time.to_span_since_epoch t
      |> Time.Span.to_sec
      |> Float.one_ulp `Up
      |> Time.Span.of_sec
      |> Time.of_span_since_epoch
    ;;

    let prev_representable_time t =
      Time.to_span_since_epoch t
      |> Time.Span.to_sec
      |> Float.one_ulp `Down
      |> Time.Span.of_sec
      |> Time.of_span_since_epoch
    ;;

    let zone  = Time.Zone.utc
    let date  = Date.create_exn
    let time  = Time.of_date_ofday ~zone
    let ofday = Time.Ofday.create

    module Testing = struct
      type t =
        | Time of Time.t
        | Next_time of Time.t
        | Prev_time of Time.t
      [@@deriving sexp_of]
    end

    (* The following schedules are tested to ensure that they behave as expected for
       includes.  The triples should contain (schedule, included times, excluded times).
       Prev_time and next time, cause it to use the previous and next representable times
       respectively.

       When you add branches to this, please make sure you test the following times:

       - the ulp before the schedule starts
       - the start of the schedule
       - the end of the schedule
       - the ulp after the schedule ends
    *)

    let unzoned_includes_schedules =
      let open Testing in
      [ Secs [ 30 ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:20 ~min:04 ~sec:30 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:20 ~min:05 ~sec:30 ~ms:0123 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~sec:31 ()))]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:20 ~min:05 ~sec:31 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:20 ~min:05 ~sec:30 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:00 ~min:00 ~sec:29 ())) ]
      ; Mins [ 5 ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:20 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:00 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:00 ~min:05 ~sec:04 ~ms:023 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:00 ~min:06 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:20 ~min:10 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:00 ~min:06 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:00 ~min:05 ~sec:00 ())) ]
      ; Hours [ 3; 18 ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:03 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:18 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:03 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:04 ~min:00 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:20 ~min:10 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:00 ~min:06 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:03 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:04 ~min:00 ~sec:00 ())) ]
      ; Weekdays [ Mon; Wed ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:03 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:19) (ofday ~hr:18 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:19) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:19) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:18) (ofday ~hr:20 ~min:10 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:20) (ofday ~hr:00 ~min:06 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Nov ~d:20) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:18) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      ; Days [ 1; 30 ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:01) (ofday ~hr:03 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:30) (ofday ~hr:18 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:01) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:01) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:02) (ofday ~hr:20 ~min:10 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Feb ~d:28) (ofday ~hr:00 ~min:06 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Nov ~d:02) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Oct ~d:31) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      ; Weeks [ 1 ]
      , [ Time (time (date ~y:2015 ~m:Jan ~d:01) (ofday ~hr:12 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Dec ~d:29) (ofday ~hr:12 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Dec ~d:29) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2015 ~m:Jan ~d:04) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      , [ Time (time (date ~y:2015 ~m:Jan ~d:06) (ofday ~hr:12 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:10) (ofday ~hr:12 ~min:00 ~sec:00 ()))
        ; Next_time (time (date ~y:2015 ~m:Jan ~d:05) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Dec ~d:28) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      ; Weeks [ 1; 7 ]
      , [ Time (time (date ~y:2015 ~m:Feb ~d:10) (ofday ~hr:12 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Dec ~d:30) (ofday ~hr:12 ~min:00 ~sec:00 ())) ]
      , [ Time (time (date ~y:2015 ~m:Feb ~d:26) (ofday ~hr:12 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Dec ~d:27) (ofday ~hr:12 ~min:00 ~sec:00 ())) ]
      ; Months [ Nov; Mar ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:03 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Mar ~d:19) (ofday ~hr:18 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Mar ~d:01) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Mar ~d:31) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Oct ~d:18) (ofday ~hr:20 ~min:10 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Apr ~d:20) (ofday ~hr:00 ~min:06 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Apr ~d:01) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Feb ~d:28) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      ; On [ date ~y:2014 ~m:Nov ~d:17; date ~y:2014 ~m:Jan ~d:1 ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:03 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:18 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Oct ~d:18) (ofday ~hr:20 ~min:10 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Apr ~d:20) (ofday ~hr:00 ~min:06 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Nov ~d:18) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:16) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      ; Before (Inclusive, ( date ~y:2014 ~m:Nov ~d:17, Time.Ofday.create () ))
      , [ Time (time (date ~y:2014 ~m:Nov ~d:16) (ofday ~hr:03 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:18 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:00 ~min:00 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:18) (ofday ~hr:20 ~min:10 ~sec:00 ()))
        ; Time (time (date ~y:2015 ~m:Apr ~d:20) (ofday ~hr:00 ~min:06 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:00 ~min:00 ~sec:00 ())) ]
      ; After (Exclusive, ( date ~y:2014 ~m:Nov ~d:18, Time.Ofday.create () ))
      , [ Time (time (date ~y:2014 ~m:Nov ~d:18) (ofday ~hr:20 ~min:10 ~sec:00 ()))
        ; Time (time (date ~y:2015 ~m:Apr ~d:20) (ofday ~hr:00 ~min:06 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:18) (ofday ~hr:00 ~min:00 ~sec:01 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:16) (ofday ~hr:03 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:18 ~min:05 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      ; Between ((Inclusive, Time.Ofday.create ~hr:9 ()), (Exclusive, Time.Ofday.create ~hr:17 ()))
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:09 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:09 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:13 ~min:05 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:17 ~min:00 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:03 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:17 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:18 ~min:05 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:09 ~min:00 ~sec:00 ())) ]
      ; Between ((Exclusive, Time.Ofday.create ~hr:9 ()), (Inclusive, Time.Ofday.create ~hr:9 ()))
      , []
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:09 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:9 ~min:00 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:9 ~min:00 ~sec:00 ())) ]
      ; Between ((Inclusive, Time.Ofday.create ~hr:9 ()), (Exclusive, Time.Ofday.create ~hr:9 ()))
      , []
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:09 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:9 ~min:00 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:9 ~min:00 ~sec:00 ())) ]
      ; Between ((Inclusive, Time.Ofday.create ~hr:9 ()), (Inclusive, Time.Ofday.create ~hr:9 ()))
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:09 ~min:00 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:03 ~min:05 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:9 ~min:00 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:9 ~min:00 ~sec:00 ())) ]
      ; Not (Between ((Inclusive, Time.Ofday.create ~hr:9 ()), (Exclusive, Time.Ofday.create ~hr:17 ())))
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:03 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:17 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:18 ~min:05 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:09 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:09 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:13 ~min:05 ~sec:00 ())) ]
      ; Or [At [ Time.Ofday.create ~hr:23 () ]; Not (At [ Time.Ofday.create ~hr:23 () ]) ]
      , [ Time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:23 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:23 ~min:00 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:23 ~min:00 ~sec:00 ())) ]
      , []
      ; At [ Time.Ofday.create ~hr:23 () ]
      , [ Time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:23 ~min:00 ~sec:00 ()))]
      , [ Prev_time (time (date ~y:2014 ~m:Dec ~d:16) (ofday ~hr:23 ~min:00 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:23 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Dec ~d:14) (ofday ~hr:23 ~min:00 ~sec:00 ~ms:1 ())) ]
      ; Not (At [ Time.Ofday.create ~hr:23 () ])
      , [ Prev_time (time (date ~y:2014 ~m:Dec ~d:16) (ofday ~hr:23 ~min:00 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:23 ~min:00 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Dec ~d:14) (ofday ~hr:23 ~min:00 ~sec:00 ~ms:1 ())) ]
      , [ Time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:23 ~min:00 ~sec:00 ()))]
      ; At [ Time.Ofday.create ~hr:23 ~min:14 () ]
      , [ Time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:23 ~min:14 ~sec:00 ())) ]
      , [ Prev_time (time (date ~y:2014 ~m:Dec ~d:16) (ofday ~hr:23 ~min:14 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Dec ~d:14) (ofday ~hr:23 ~min:14 ~sec:00 ())) ]
      ; And
          [ Mins [ 15 ]
          ; Hours [ 7; 8 ]
          ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:07 ~min:15 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jul ~d:14) (ofday ~hr:8 ~min:15 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:09 ~min:15 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jul ~d:14) (ofday ~hr:6 ~min:15 ~sec:00 ())) ]
      ; And []
      , [ Time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:23 ~min:15 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:15) (ofday ~hr:23 ~min:14 ~sec:00 ())) ]
      , []
      ; And
          [ On [ date ~y:2014 ~m:Dec ~d:15 ] ]
      , [ Time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:23 ~min:14 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      , [ Next_time (time (date ~y:2014 ~m:Dec ~d:16) (ofday ~hr:00 ~min:00 ~sec:00 ()))
        ; Prev_time (time (date ~y:2014 ~m:Dec ~d:14) (ofday ~hr:24 ~min:00 ~sec:00 ())) ]
      ; And
          [ On [ date ~y:2014 ~m:Dec ~d:15 ]
          ; At [ Time.Ofday.create ~hr:23 ~min:14 () ] ]
      , [ Time (time (date ~y:2014 ~m:Dec ~d:15) (ofday ~hr:23 ~min:14 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Dec ~d:16) (ofday ~hr:23 ~min:14 ~sec:00 ~ms:001 ()))
        ; Prev_time (time (date ~y:2014 ~m:Dec ~d:14) (ofday ~hr:23 ~min:14 ~sec:00 ()))
        ; Next_time (time (date ~y:2014 ~m:Dec ~d:14) (ofday ~hr:23 ~min:14 ~sec:00 ())) ]
      ; Or []
      , []
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:07 ~min:15 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jul ~d:14) (ofday ~hr:8 ~min:15 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:07 ~min:04 ~sec:00 ())) ]
      ; Or
          [ Mins [ 15 ]
          ; Hours [ 7; 8 ]
          ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:07 ~min:15 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jul ~d:14) (ofday ~hr:8 ~min:15 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:07 ~min:04 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:09 ~min:13 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jul ~d:14) (ofday ~hr:6 ~min:25 ~sec:00 ())) ]
      ; Shift (Time.Span.of_sec 0., Mins [ 5 ])
      , [ Time (time (date ~y:2015 ~m:Jan ~d:16) (ofday ~hr:07 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2015 ~m:Jan ~d:01) (ofday ~hr:18 ~min:05 ~sec:00 ())) ]
      , [ Time (time (date ~y:2015 ~m:Jan ~d:16) (ofday ~hr:07 ~min:06 ~sec:00 ()))
        ; Time (time (date ~y:2015 ~m:Jan ~d:01) (ofday ~hr:18 ~min:04 ~sec:00 ())) ]
      ; Shift (Time.Span.of_sec 7., Mins [ 5 ])
      , [ Time (time (date ~y:2015 ~m:Jan ~d:16) (ofday ~hr:07 ~min:05 ~sec:07 ()))
        ; Time (time (date ~y:2015 ~m:Jan ~d:01) (ofday ~hr:18 ~min:05 ~sec:07 ()))
        ; Prev_time (time (date ~y:2015 ~m:Jan ~d:01) (ofday ~hr:18 ~min:06 ~sec:07 ())) ]
      , [ Time (time (date ~y:2015 ~m:Jan ~d:16) (ofday ~hr:07 ~min:06 ~sec:07 ()))
        ; Prev_time (time (date ~y:2015 ~m:Jan ~d:16) (ofday ~hr:07 ~min:05 ~sec:07 ())) ]
      ; Shift (Time.Span.of_sec 7.3, Mins [ 5 ])
      , [ Time (time (date ~y:2015 ~m:Jan ~d:16) (ofday ~hr:07 ~min:05 ~sec:07 ~ms:300 ()))
        ; Prev_time (time (date ~y:2015 ~m:Jan ~d:01) (ofday ~hr:18 ~min:06 ~sec:07 ~ms:300 ())) ]
      , [ Time (time (date ~y:2015 ~m:Jan ~d:16) (ofday ~hr:07 ~min:06 ~sec:07 ~ms:300 ()))
        ; Prev_time (time (date ~y:2015 ~m:Jan ~d:01) (ofday ~hr:18 ~min:05 ~sec:07 ())) ]
      ; Shift (Time.Span.of_min 1., Mins [ 5 ])
      , [ Time (time (date ~y:2015 ~m:Jan ~d:16) (ofday ~hr:07 ~min:06 ~sec:00 ()))
        ; Time (time (date ~y:2015 ~m:Jan ~d:01) (ofday ~hr:18 ~min:06 ~sec:00 ())) ]
      , [ Time (time (date ~y:2015 ~m:Jan ~d:16) (ofday ~hr:07 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2015 ~m:Jan ~d:01) (ofday ~hr:18 ~min:05 ~sec:00 ())) ]
      ; Shift (Time.Span.of_min (-1.), Mins [ 5 ])
      , [ Time (time (date ~y:2015 ~m:Jan ~d:16) (ofday ~hr:07 ~min:04 ~sec:00 ()))
        ; Time (time (date ~y:2015 ~m:Jan ~d:01) (ofday ~hr:18 ~min:04 ~sec:00 ())) ]
      , [ Time (time (date ~y:2015 ~m:Jan ~d:16) (ofday ~hr:07 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2015 ~m:Jan ~d:01) (ofday ~hr:18 ~min:05 ~sec:00 ())) ]
      ; If_then_else
          (On [ date ~y:2014 ~m:Nov ~d:17 ]
          , Between ((Inclusive, Time.Ofday.create ~hr:9 ()), (Inclusive, Time.Ofday.create ~hr:12 ()))
          , Between ((Inclusive, Time.Ofday.create ~hr:9 ()), (Inclusive, Time.Ofday.create ~hr:17 ())))
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:09 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:13 ~min:05 ~sec:00 ())) ]
      , [ Time (time (date ~y:2014 ~m:Nov ~d:17) (ofday ~hr:08 ~min:05 ~sec:00 ()))
        ; Time (time (date ~y:2014 ~m:Jan ~d:01) (ofday ~hr:18 ~min:05 ~sec:00 ())) ]
      ]
    ;;

    let%test_unit "Test that includes does the right thing for every branch in a simple case" =
      let get_time (t : Testing.t) =
        match t with
        | Time      time -> time
        | Next_time time -> next_representable_time time
        | Prev_time time -> prev_representable_time time
      in
      let zoned_includes_schedules =
        List.map unzoned_includes_schedules ~f:(fun (schedule, matches, fails) ->
          In_zone (zone, schedule), matches, fails)
      in
      List.iteri zoned_includes_schedules
        ~f:(fun i (schedule, matches, fails) ->
          let not_schedule          = Not schedule in
          let schedule_with_tag     = Tag (i, schedule) in
          let not_schedule_with_tag = Tag (i, not_schedule) in
          List.iter matches ~f:(fun t ->
            let time = get_time t in
            try
              assert (includes schedule time);
              assert (not (includes not_schedule time));
              begin match tags schedule_with_tag time with
              | `Included [ tag ] -> assert (Int.(=) tag i)
              | `Included _
              | `Not_included     -> assert false
              end
            with
            | exn ->
              failwithf !"%s failed inclusion tests on %s : %{Exn}"
                (Time.to_string_abs ~zone time)
                (to_string_zoned schedule ~string_of_tag:Int.to_string)
                exn
                ());
          List.iter fails ~f:(fun t ->
            let time = get_time t in
            try
              assert (not (includes schedule time));
              assert (includes not_schedule time);
              begin match tags not_schedule_with_tag time with
              | `Included [ tag ] -> assert (Int.(=) tag i)
              | `Included _
              | `Not_included     -> assert false
              end
            with
            | exn ->
              failwithf !"%s failed non-inclusion test (%s) on %s : %{Exn}"
                (Time.to_string_abs ~zone time)
                (Sexp.to_string_hum (Testing.sexp_of_t t))
                (to_string_zoned schedule ~string_of_tag:Int.to_string)
                exn
                ()))
    ;;

    let%test_unit "test to_sequence_start_of_day" =
      let enter = time (date ~y:2014 ~m:Nov ~d:17) Time.Ofday.start_of_day in
      let leave = next_representable_time enter in
      let expected =
        [ `Enter (enter, [])
        ; `Leave leave
        ]
      in
      let schedule =
        In_zone (zone , (At [ Time.Ofday.start_of_day ] ))
      in
      let start_time = Time.of_string "2014-11-16 15:54:00Z" in
      let ( `Started_in_range (_, sequence)
          | `Started_out_of_range sequence ) =
        to_endless_sequence schedule ~start_time ~emit:Transitions
      in
      [%test_result: [ [ `Start | `Stop ] Event.transition | Event.no_change ] list]
        ~expect:expected
        (Sequence.to_list (Sequence.take sequence 2))
    ;;

    let%test_unit "Time.Ofday.start_of_day is handled correctly on date" =
      let time1 =
        Time.of_date_ofday ~zone (date ~y:2015 ~m:Jan ~d:7)
          Time.Ofday.start_of_day
      in
      let time2 =
        Time.of_date_ofday ~zone (date ~y:2015 ~m:Jan ~d:7)
          Time.Ofday.start_of_day
        |> next_representable_time
      in
      let schedule =
        In_zone (zone,
                 And [ On [ date ~y:2015 ~m:Jan ~d:7 ]
                     ; At [ Time.Ofday.start_of_day ]])
      in
      assert (includes schedule time1);
      assert (not (includes schedule time2))
    ;;

    let%test_unit "Time.Ofday.start_of_day is handled reasonably" =
      let time =
        Time.of_date_ofday ~zone (date ~y:2015 ~m:Jan ~d:7)
          Time.Ofday.start_of_day
      in
      let schedule = In_zone (zone, At [ Time.Ofday.start_of_day ]) in
      assert (includes schedule time)
    ;;

    let%test_unit "Shifting to end of day is start of day" =
      let start_of_day =
        Time.of_date_ofday ~zone (date ~y:2015 ~m:Jan ~d:8)
          Time.Ofday.start_of_day
      in
      let schedule = In_zone (zone, Shift (Time.Span.of_hr 1., At [ Time.Ofday.create ~hr:23 () ])) in
      assert (includes schedule start_of_day)
    ;;

    let%test_unit "Not start_of_day is handled reasonalby" =
      let start_of_day =
        Time.of_date_ofday ~zone (date ~y:2015 ~m:Jan ~d:7)
          Time.Ofday.start_of_day
      in
      let schedule = In_zone (zone, Not (At [ Time.Ofday.start_of_day ])) in
      assert (not (includes schedule start_of_day))
    ;;

    let next_enter_between_seq sched ~start_time ~stop_time =
      let seq = to_endless_sequence sched ~start_time ~emit:Transitions in
      let seq =
        match seq with
        | `Started_in_range (_, _) -> assert false
        | `Started_out_of_range seq -> seq
      in
      Sequence.take_while seq ~f:(fun event ->
        let time = Event.to_time event in
        Time.(<) time stop_time)
      |> Sequence.filter_map ~f:(function
        | `Enter (t, _) -> Some t
        | `Leave _
        | `No_change_until_at_least _ -> None)
      |> Sequence.hd
    ;;

    let test_speed schedule start_time distance expected_time =
      let stop_time  = Time.add start_time distance in
      let start_times = Unix.times () in
      assert (includes schedule expected_time);
      assert (Time.(>) expected_time start_time && Time.(<) expected_time stop_time);
      match next_enter_between_seq schedule ~start_time ~stop_time with
      | Some t ->
        assert (Time.(=) t expected_time);
        let max_span   = Time.Span.of_sec 5. in
        let stop_times = Unix.times () in
        let total_time = Time.Span.of_sec (stop_times.tms_utime -. start_times.tms_utime) in
        if (Time.Span.(>) total_time max_span)
        then failwithf "scanning a sparse schedule took %s, \
                        which is longer than the limit of %s"
               (Time.Span.to_string total_time)
               (Time.Span.to_string max_span)
               ()
      | None   -> assert false
    ;;

    let test_all_starting_second_offsets ~base_time ~expected ~sched =
      let stop_time = Time.add expected (Time.Span.of_min 5.) in
      for offset = 0 to 60 do
        let start_time = Time.add base_time (Time.Span.of_sec (Float.of_int offset)) in
        let next = next_enter_between_seq sched ~start_time ~stop_time in
        [%test_eq: Time.t option] (Some expected) next;
      done;
    ;;

    let%test_unit "we correctly round start time for \
                   a minute-spanned calculation like At with 0 secs times of day" =
      let today     = Date.today ~zone in
      let base_time = Time.of_date_ofday today (Time.Ofday.create ()) ~zone in
      let ofday     = Time.Ofday.create ~hr:11 () in
      let expected  = Time.of_date_ofday ~zone today ofday in
      let sched     = In_zone (zone, At [ofday] ) in
      test_all_starting_second_offsets ~base_time ~expected ~sched
    ;;

    let%test_unit "we correctly round start time for \
                   a minute-spanned calculation like hours" =
      let today     = Date.today ~zone in
      let base_time = Time.of_date_ofday today (Time.Ofday.create ()) ~zone in
      let expected  = Time.add base_time (Time.Span.of_hr 1.) in
      let sched     = In_zone (zone, Hours [1] ) in
      test_all_starting_second_offsets ~base_time ~expected ~sched
    ;;

    let%test_unit "speed test of an almost empty schedule over a year" =
      let start_time = Time.of_string "2014-01-01 00:00:00Z" in
      let distance = Time.Span.of_day 365. in
      let schedule =
        In_zone (zone
                , And
                    [ On [ date ~y:2014 ~m:Dec ~d:15 ]
                    ; At [ Time.Ofday.create ~hr:23 ~min:14 () ] ])
      in
      let expected_time = Time.of_string "2014-12-15 23:14:00Z" in
      test_speed schedule start_time distance expected_time
    ;;

    let%test_unit "speed test of an almost empty second set over a month" =
      let start_time = Time.of_string "2014-11-16 00:00:00Z" in
      let distance = Time.Span.of_day 30. in
      let schedule =
        In_zone (zone
                , And
                    [ On [ date ~y:2014 ~m:Dec ~d:15 ]
                    ; At [ Time.Ofday.create ~hr:23 ~min:14 ~sec:23 () ] ])
      in
      let expected_time = Time.of_string "2014-12-15 23:14:23Z" in
      test_speed schedule start_time distance expected_time
    ;;

    let test_speed_to_sequence schedule start_time =
      let distance_in_days = 365. in
      let stop_time        = Time.add start_time (Time.Span.of_day distance_in_days) in
      let start_times      = Unix.times () in
      let sequence =
        match to_endless_sequence schedule ~start_time ~emit:Transitions with
        | `Started_in_range (_tags, sequence) -> sequence
        | `Started_out_of_range sequence -> sequence
      in
      Sequence.take_while sequence ~f:(fun event ->
        let time =
          match event with
          | `Enter (time, _) -> time
          | `Leave time -> time
          | `No_change_until_at_least (_, time) -> time
        in
        Time.(>) stop_time time)
      |> Sequence.iter ~f:(fun event -> ignore event);
      let stop_times       = Unix.times () in
      let max_span_per_day = Time.Span.of_ms 1. in
      let total_time       = stop_times.tms_utime -. start_times.tms_utime in
      let time_per_day     = Time.Span.of_sec (total_time /. distance_in_days) in
      if (Time.Span.(>) time_per_day max_span_per_day)
      then failwithf "scanning a sparse schedule took %s per day, \
                      which is longer than the limit of %s per day"
             (Time.Span.to_string time_per_day)
             (Time.Span.to_string max_span_per_day)
             ()
    ;;

    let%test_unit "Ensure that to_sequence is fast for a schedule that only returns includes once a year"  =
      let start_time = Time.of_string "2014-01-01 00:00:00Z" in
      let schedule =
        In_zone (zone
                , And
                    [ On [ date ~y:2014 ~m:Dec ~d:15 ]
                    ; At [ Time.Ofday.create ~hr:23 ~min:14 () ] ])
      in
      test_speed_to_sequence schedule start_time
    ;;

    let%test_unit "Show that schedules are fast for schedules that only happen once a year specified in minutes" =
      (* Regression test for a common case in Grass and perhaps Appd. *)
      let span_of_computation_time_in_ms () =
        (* We have to use [Unix.times], not [Time.now] so this doesn't fail when the test
           box is under heavy load. *)
        let { Unix.tms_stime; tms_utime; tms_cstime; tms_cutime } = Unix.times () in
        tms_stime +. tms_utime +. tms_cstime +. tms_cutime
      in
      let less_than_10ms f =
        let before_f = span_of_computation_time_in_ms () in
        f ();
        let after_f = span_of_computation_time_in_ms () in
        assert (after_f -. before_f < 10.)
      in
      let start_time = Time.of_string "2014-01-02 00:00:00Z" in
      let schedule =
        In_zone (zone
                , And
                    [ Mins [1]
                    ; Hours [1]
                    ; Days [1]
                    ; Months [Month.Jan]])
      in
      less_than_10ms (fun () -> test_speed_to_sequence schedule start_time);
    ;;

    let%test_unit "Ensure that to_sequence is fast for a schedule that occurs every day" =
      let start_time = Time.of_string "2014-01-01 00:00:00Z" in
      let on_date ~y ~m ~d = On [ date ~y ~m ~d ] in
      let between ~start ~end_ =
        Between ( (Inclusive_exclusive.Inclusive, Time.Ofday.create ~hr:start ())
                , (Inclusive_exclusive.Exclusive, Time.Ofday.create ~hr:end_ ()))
      in
      let schedule =
        In_zone (zone
                , If_then_else (on_date ~y:2015 ~m:Nov ~d:28
                               , between ~start:9 ~end_:16
                               , between ~start:9 ~end_:13))
      in
      test_speed_to_sequence schedule start_time
    ;;

    let%test_unit "speed test repeated Or" =
      let start_time = Time.of_string "2014-01-01 00:00:00Z" in
      let on_date ~y ~m ~d = On [ date ~y ~m ~d ] in
      let between ~start ~end_ =
        Between ( (Inclusive_exclusive.Inclusive, Time.Ofday.create ~hr:start ())
                , (Inclusive_exclusive.Exclusive, Time.Ofday.create ~hr:end_ ()))
      in
      let weekdays = Weekdays Day_of_week.weekdays in
      let s =
        If_then_else (And [ on_date ~y:2015 ~m:Nov ~d:28
                          ; weekdays ]
                     , between ~start:9 ~end_:16
                     , between ~start:9 ~end_:13)
      in
      let schedule =
        In_zone (zone
                , Or [s; s; s; s])
      in
      test_speed_to_sequence schedule start_time
    ;;

    let%test_unit "test if_then_else in sequence to ensure that the guard clause is honored" =
      let enter ~d ~hr =
        Time.of_date_ofday
          ~zone
          (date ~y:2014 ~m:Nov ~d)
          (Time.Ofday.create ~hr ~min:0 ~sec:0 ())
      in
      let leave ~d ~hr = enter ~d ~hr in
      let schedule =
        In_zone
          (zone
          , If_then_else ( Hours [ 9 ]
                         , Always
                         , Hours [ 11 ] ))
      in
      let expected =
        [ `Enter (enter ~d:15 ~hr:9, [])
        ; `Leave (leave ~d:15 ~hr:10)
        ; `Enter (enter ~d:15 ~hr:11, [])
        ; `Leave (leave ~d:15 ~hr:12)
        ; `Enter (enter ~d:16 ~hr:9, [])
        ; `Leave (leave ~d:16 ~hr:10)
        ; `Enter (enter ~d:16 ~hr:11, [])
        ; `Leave (leave ~d:16 ~hr:12)
        ]
      in
      let start_time = Time.of_string "2014-11-15 8:55:00Z" in
      let ( `Started_in_range (_, sequence)
          | `Started_out_of_range sequence ) =
        to_endless_sequence schedule ~start_time ~emit:Transitions
      in
      [%test_result: [ [ `Start | `Stop ] Event.transition | Event.no_change ] list]
        ~expect:expected
        (Sequence.to_list (Sequence.take sequence 8))
    ;;

    let%test_unit "test to_tag_sequence for or's that change within a schedule" =
      let time ~d ~hr ~m =
        Time.of_date_ofday
          ~zone
          (date ~y:2015 ~m:Jun ~d)
          (Time.Ofday.create ~hr ~min:m ~sec:0 ())
      in
      let expected =
        [ `Enter (time ~d:7 ~hr:22 ~m:5, [ 3 ])
        ; `Leave (time ~d:7 ~hr:22 ~m:6)
        ; `Enter (time ~d:7 ~hr:22 ~m:7, [ 3 ])
        ; `Leave (time ~d:7 ~hr:22 ~m:8)
        ; `Enter (time ~d:7 ~hr:23 ~m:5, [ 3 ])
        ; `Leave (time ~d:7 ~hr:23 ~m:6)
        ; `Enter (time ~d:7 ~hr:23 ~m:7, [ 3 ])
        ; `Leave (time ~d:7 ~hr:23 ~m:8)
        ; `Enter (time ~d:8 ~hr:0 ~m:0, [ 1 ])
        ; `Change_tags (time ~d:8 ~hr:0 ~m:5, [ 3; 1 ])
        ; `Change_tags (time ~d:8 ~hr:0 ~m:6, [ 1 ])
        ; `Change_tags (time ~d:8 ~hr:0 ~m:7, [ 3; 1 ])
        ; `Change_tags (time ~d:8 ~hr:0 ~m:8, [ 1 ])
        ; `Change_tags (time ~d:8 ~hr:1 ~m:0, [ 2; 1 ])
        ; `Change_tags (time ~d:8 ~hr:1 ~m:5, [ 3; 2; 1 ])
        ; `Change_tags (time ~d:8 ~hr:1 ~m:6, [ 2; 1 ])
        ; `Change_tags (time ~d:8 ~hr:1 ~m:7, [ 3; 2; 1 ])
        ; `Change_tags (time ~d:8 ~hr:1 ~m:8, [ 2; 1 ])
        ; `Change_tags (time ~d:8 ~hr:2 ~m:0, [ 1 ])
        ; `Change_tags (time ~d:8 ~hr:2 ~m:5, [ 3; 1 ])
        ; `Change_tags (time ~d:8 ~hr:2 ~m:6, [ 1 ])
        ; `Change_tags (time ~d:8 ~hr:2 ~m:7, [ 3; 1 ])
        ; `Change_tags (time ~d:8 ~hr:2 ~m:8, [ 1 ])
        ]
      in
      let schedule =
        In_zone
          (zone
          , Or
              [ Tag (1, Weekdays [ Day_of_week.Mon ])
              ; Tag (2, Hours [ 1 ])
              ; Tag (3, Mins [ 5; 7 ])
              ])
      in
      let start_time = Time.of_string "2015-06-07 22:00:00Z" in
      let ( `Started_in_range (_, sequence)
          | `Started_out_of_range sequence ) =
        to_endless_sequence schedule ~start_time ~emit:(Transitions_and_tag_changes Int.equal)
      in
      [%test_result: [ int Event.transition | Event.no_change | int Event.tag_change ] list]
        ~expect:expected
        (Sequence.to_list (Sequence.take sequence 23))
    ;;

    let%test_unit "test to_tag_sequence for not ands that change within a schedule" =
      let time ~d ~hr ~m =
        Time.of_date_ofday
          ~zone
          (date ~y:2015 ~m:Jun ~d)
          (Time.Ofday.create ~hr ~min:m ~sec:0 ())
      in
      let expected =
        [ `Enter (time ~d:7 ~hr:22 ~m:5, [ 3 ])
        ; `Leave (time ~d:7 ~hr:22 ~m:6)
        ; `Enter (time ~d:7 ~hr:22 ~m:7, [ 3 ])
        ; `Leave (time ~d:7 ~hr:22 ~m:8)
        ; `Enter (time ~d:7 ~hr:23 ~m:5, [ 3 ])
        ; `Leave (time ~d:7 ~hr:23 ~m:6)
        ; `Enter (time ~d:7 ~hr:23 ~m:7, [ 3 ])
        ; `Leave (time ~d:7 ~hr:23 ~m:8)
        ; `Enter (time ~d:8 ~hr:0 ~m:0, [ 1 ])
        ; `Change_tags (time ~d:8 ~hr:0 ~m:5, [ 3; 1 ])
        ; `Change_tags (time ~d:8 ~hr:0 ~m:6, [ 1 ])
        ; `Change_tags (time ~d:8 ~hr:0 ~m:7, [ 3; 1 ])
        ; `Change_tags (time ~d:8 ~hr:0 ~m:8, [ 1 ])
        ; `Change_tags (time ~d:8 ~hr:1 ~m:0, [ 2; 1 ])
        ; `Change_tags (time ~d:8 ~hr:1 ~m:5, [ 3; 2; 1 ])
        ; `Change_tags (time ~d:8 ~hr:1 ~m:6, [ 2; 1 ])
        ; `Change_tags (time ~d:8 ~hr:1 ~m:7, [ 3; 2; 1 ])
        ; `Change_tags (time ~d:8 ~hr:1 ~m:8, [ 2; 1 ])
        ; `Change_tags (time ~d:8 ~hr:2 ~m:0, [ 1 ])
        ; `Change_tags (time ~d:8 ~hr:2 ~m:5, [ 3; 1 ])
        ; `Change_tags (time ~d:8 ~hr:2 ~m:6, [ 1 ])
        ; `Change_tags (time ~d:8 ~hr:2 ~m:7, [ 3; 1 ])
        ; `Change_tags (time ~d:8 ~hr:2 ~m:8, [ 1 ])
        ]
      in
      let schedule =
        In_zone
          (zone
          , Not (And
                   [ Not (Tag (1, Weekdays [ Day_of_week.Mon ]))
                   ; Not (Tag (2, Hours [ 1 ]))
                   ; Not (Tag (3, Mins [ 5; 7 ]))
                   ]))
      in
      let start_time = Time.of_string "2015-06-07 22:00:00Z" in
      let ( `Started_in_range (_, sequence)
          | `Started_out_of_range sequence ) =
        to_endless_sequence schedule ~start_time ~emit:(Transitions_and_tag_changes Int.equal)
      in
      [%test_result: [ int Event.transition | Event.no_change | int Event.tag_change ] list]
        ~expect:expected
        (Sequence.to_list (Sequence.take sequence 23))
    ;;

    let%test_unit "test to_sequence" =
      let (!!) s = Time.of_string s in
      let enter ~d ~hr =
        Time.of_date_ofday
          ~zone
          (date ~y:2014 ~m:Nov ~d)
          (Time.Ofday.create ~hr ~min:0 ~sec:0 ())
      in
      let leave ~d ~hr = next_representable_time (enter ~d ~hr) in
      let expected =
        [ `No_change_until_at_least (`Out_of_range, !!"2014-11-16 15:54:00Z")
        ; `Enter (enter ~d:17 ~hr:9, [ `Start ])
        ; `Leave (leave ~d:17 ~hr:9)
        ; `Enter (enter ~d:17 ~hr:16, [ `Stop ])
        ; `Leave (leave ~d:17 ~hr:16)
        ; `Enter (enter ~d:18 ~hr:9, [ `Start ])
        ; `Leave (leave ~d:18 ~hr:9)
        ; `Enter (enter ~d:18 ~hr:16, [ `Stop ])
        ; `Leave (leave ~d:18 ~hr:16)
        ; `Enter (enter ~d:19 ~hr:9, [ `Start ])
        ; `Leave (leave ~d:19 ~hr:9)
        ; `Enter (enter ~d:19 ~hr:16, [ `Stop ])
        ; `Leave (leave ~d:19 ~hr:16)
        ; `Enter (enter ~d:20 ~hr:9, [ `Start ])
        ; `Leave (leave ~d:20 ~hr:9)
        ; `Enter (enter ~d:20 ~hr:16, [ `Stop ])
        ; `Leave (leave ~d:20 ~hr:16)
        ; `Enter (enter ~d:21 ~hr:9, [ `Start ])
        ; `Leave (leave ~d:21 ~hr:9)
        ; `Enter (enter ~d:21 ~hr:16, [ `Stop ])
        ]
      in
      let schedule =
        In_zone
          (zone
          , Or
              [ Tag
                  (`Start
                  , And
                      [ Weekdays Day_of_week.weekdays
                      ; At [ Time.Ofday.create ~hr:9 () ] ])
              ; Tag
                  (`Stop
                  , And
                      [ Weekdays Day_of_week.weekdays
                      ; At [ Time.Ofday.create ~hr:16 () ] ])
              ])
      in
      let start_time = Time.of_string "2014-11-15 15:54:00Z" in
      let ( `Started_in_range (_, sequence)
          | `Started_out_of_range sequence ) =
        to_endless_sequence schedule ~start_time ~emit:Transitions
      in
      [%test_result: [ [ `Start | `Stop ] Event.transition | Event.no_change ] list]
        ~expect:expected
        (Sequence.to_list (Sequence.take sequence 20))
    ;;

    module Tag = struct
      module T = struct
        type t =
          [ `Open | `Close | `Continuous ]
        [@@deriving compare, sexp]
      end
      include T
      include Comparable.Make(T)
    end

    let%test_unit "test to_tag_sequence" =
      let (!!) s = Time.of_string s in
      let enter ~d ~hr =
        Time.of_date_ofday
          ~zone
          (date ~y:2014 ~m:Nov ~d)
          (Time.Ofday.create ~hr ~min:0 ~sec:0 ())
      in
      let leave ~d ~hr = next_representable_time (enter ~d ~hr) in
      let (expected : [ Event.no_change
                      | Tag.t Event.transition
                      | Tag.t Event.tag_change
                      ] list)
        =
        [ `No_change_until_at_least (`Out_of_range, !!"2014-11-16 15:54:00Z")
        ; `Enter (enter ~d:17 ~hr:9, [ `Open; `Continuous ])
        ; `Change_tags (leave ~d:17 ~hr:9, [ `Continuous ])
        ; `Change_tags (enter ~d:17 ~hr:16, [ `Close ])
        ; `Leave (leave ~d:17 ~hr:16)
        ; `Enter (enter ~d:18 ~hr:9, [ `Open; `Continuous ])
        ; `Change_tags (leave ~d:18 ~hr:9, [ `Continuous ])
        ; `Change_tags (enter ~d:18 ~hr:16, [ `Close ])
        ; `Leave (leave ~d:18 ~hr:16)
        ; `Enter (enter ~d:19 ~hr:9, [ `Open; `Continuous ])
        ; `Change_tags (leave ~d:19 ~hr:9, [ `Continuous ])
        ; `Change_tags (enter ~d:19 ~hr:16, [ `Close ])
        ; `Leave (leave ~d:19 ~hr:16)
        ; `Enter (enter ~d:20 ~hr:9, [ `Open; `Continuous ])
        ; `Change_tags (leave ~d:20 ~hr:9, [ `Continuous ])
        ; `Change_tags (enter ~d:20 ~hr:16, [ `Close ])
        ; `Leave (leave ~d:20 ~hr:16)
        ; `Enter (enter ~d:21 ~hr:9, [ `Open; `Continuous ])
        ; `Change_tags (leave ~d:21 ~hr:9, [ `Continuous ])
        ; `Change_tags (enter ~d:21 ~hr:16, [ `Close ])
        ; `Leave (leave ~d:21 ~hr:16)
        ]
      in
      let weekdays = Weekdays [ Mon; Tue; Wed; Thu; Fri ] in
      let schedule =
        In_zone
          (zone
          , Or
              [ Tag
                  (`Continuous
                  , And
                      [ weekdays
                      ; Between ( (Inclusive_exclusive.Inclusive, Time.Ofday.create ~hr:9 ())
                                , (Inclusive_exclusive.Exclusive, Time.Ofday.create ~hr:16 ()))
                      ])
              ; Tag
                  (`Open
                  , And
                      [ weekdays
                      ; At [ Time.Ofday.create ~hr:9 () ] ])
              ; Tag
                  (`Close
                  , And
                      [ weekdays
                      ; At [ Time.Ofday.create ~hr:16 () ] ])
              ])
      in
      let start_time = Time.of_string "2014-11-15 15:54:00Z" in
      let ( `Started_in_range (_, sequence)
          | `Started_out_of_range sequence ) =
        to_endless_sequence schedule
          ~start_time
          ~emit:(Transitions_and_tag_changes (fun t1 t2 -> Tag.compare t1 t2 = 0))
      in
      assert (Set.equal
                (all_tags schedule ~tag_comparator:Tag.comparator)
                (Tag.Set.of_list [`Open; `Continuous; `Close]));
      [%test_result: [ Event.no_change
                     | Tag.t Event.transition
                     | Tag.t Event.tag_change
                     ] list]
        ~expect:expected
        (Sequence.to_list (Sequence.take sequence 21))
    ;;

    let%test_unit "test tagging over our office on-call schedule"  =
      let tot_zone = Time.Zone.find_exn "America/New_York" in
      let ldn_zone = Time.Zone.find_exn "Europe/London" in
      let hkg_zone = Time.Zone.find_exn "Asia/Hong_Kong" in
      let weekdays = Weekdays Day_of_week.weekdays in
      let working_hours =
        Between ((Inclusive, Time.Ofday.create ~hr:8 ()), (Inclusive, Time.Ofday.create ~hr:18 ()))
      in
      let working_schedule = And [ weekdays; working_hours ] in
      let offices =
        [ "tot", tot_zone
        ; "hkg", hkg_zone
        ; "ldn", ldn_zone ]
      in
      let schedule =
        Or
          (List.map offices ~f:(fun (office, zone) ->
             In_zone (zone, Tag (office, working_schedule))))
      in
      let expectations =
        [ tot_zone, "2014-11-15", "13:00", []
        ; tot_zone, "2014-11-14", "16:00", [ "tot" ]
        ; tot_zone, "2014-11-14", "11:00", [ "ldn"; "tot" ]
        ; tot_zone, "2014-11-13", "20:00", [ "hkg" ]
        ; tot_zone, "2014-11-14", "20:00", [ ]
        ; tot_zone, "2014-11-14", "05:00", [ "hkg"; "ldn" ]
        ]
      in
      List.iter expectations
        ~f:(fun (zone, date_s, ofday_s, expected_tags) ->
          let time =
            Time.of_date_ofday ~zone (Date.of_string date_s)
              (Time.Ofday.of_string ofday_s)
          in
          let tags =
            match tags schedule time with
            | `Not_included  -> []
            | `Included tags -> tags
          in
          [%test_result: string list]
            (List.sort ~compare:String.compare tags)
            ~expect:expected_tags)
    ;;

    let sorted_tags schedule time =
      match tags schedule time with
      | `Not_included  -> []
      | `Included tags -> List.sort ~compare:String.compare tags
    ;;

    let%test_unit "Test tags is correct in a simple example"=
      let schedule =
        In_zone
          (zone
          , Tag
              ( "a"
              , Or
                  [ Tag ("b", Weekdays [])
                  ; Tag ("c", Weekdays Day_of_week.all)
                  ; Tag ("d", Not (Weekdays []))
                  ]))
      in
      [%test_result: string list]
        ~expect:[ "a"; "c"; "d" ]
        (sorted_tags schedule (Time.now ()))
    ;;

    let%test_unit "Test tags is correct for nested tags"=
      let schedule =
        In_zone (zone, Tag ("a", Tag ("b", Tag ("c", Weekdays Day_of_week.all))))
      in
      [%test_result: string list]
        ~expect:[ "a"; "b"; "c" ]
        (sorted_tags schedule (Time.now ()))
    ;;

    let%test_unit "Test tags is correct under not"=
      let schedule = In_zone (zone, Not (Tag ("a", Weekdays Day_of_week.all))) in
      [%test_result: string list]
        ~expect:[]
        (sorted_tags schedule (Time.now ()))
    ;;

    let%test_unit "Test tag is correct under double negation"=
      let schedule = In_zone (zone, Not (Not (Tag ("a", Weekdays Day_of_week.all)))) in
      [%test_result: string list]
        ~expect:[ "a" ]
        (sorted_tags schedule (Time.now ()))
    ;;

    let%test_unit "Test tag is correct under demorgan's law"=
      let schedule =
        In_zone (zone,
                 Not (And
                        [ Not (Tag ("a", Weekdays Day_of_week.all))
                        ; Tag ("b", Weekdays Day_of_week.all)
                        ]))
      in
      [%test_result: string list]
        ~expect:[ "a" ]
        (sorted_tags schedule (Time.now ()))
    ;;

    let%test_unit "Test fold tags is correct for always" =
      let schedule = Always in
      [%test_result: unit option]
        ~expect:(Some ())
        (fold_tags ~init:() ~f:(fun _ _ -> ()) schedule Time.epoch)
    ;;

    let%test_unit "Test fold tags is correct for never" =
      let schedule = Never in
      [%test_result: unit option]
        ~expect:None
        (fold_tags ~init:() ~f:(fun _ _ -> ()) schedule Time.epoch)
    ;;

    let is_correct_transition ~here test_output reference =
      [%test_eq: Time.t option]
        ~here
        test_output
        reference
    ;;

    let schedule =
      At [ Time.Ofday.create ~hr:9 ~min:0 ~sec:0 ()
         ; Time.Ofday.create ~hr:9 ~min:0 ~sec:2 ()
         ; Time.Ofday.create ~hr:9 ~min:0 ~sec:4 ()
         ]

    let schedule = In_zone (zone, schedule)
    let at_9 =
      Time.of_date_ofday
        ~zone
        (date ~y:2014 ~m:Nov ~d:17)
        (Time.Ofday.create ~hr:9 ~min:0 ~sec:0 ())
    let leave_9 = next_representable_time at_9

    let at_9_0_2 =
      Time.of_date_ofday
        ~zone
        (date ~y:2014 ~m:Nov ~d:17)
        (Time.Ofday.create ~hr:9 ~min:0 ~sec:2 ())
    let leave_9_0_2 = next_representable_time at_9_0_2

    let%test_unit "call next_enter before start of schedule" =
      let output =
        next_enter_between schedule
          (prev_representable_time at_9)
          (Time.add leave_9 (Time.Span.of_sec 1.))
      in
      is_correct_transition ~here:[[%here]] output (Some at_9)
    ;;

    let%test_unit "call next_enter at start of schedule" =
      let output =
        next_enter_between schedule
          at_9
          (Time.add leave_9 (Time.Span.of_sec 1.))
      in
      is_correct_transition ~here:[[%here]] output (Some at_9)
    ;;

    let%test_unit "call next_enter after start of schedule" =
      let output =
        next_enter_between schedule
          (next_representable_time at_9)
          leave_9
      in
      is_correct_transition ~here:[[%here]] output None
    ;;

    let%test_unit "call next_enter ends before start of schedule" =
      let output =
        next_enter_between schedule
          (Time.sub (prev_representable_time at_9) (Time.Span.of_sec 1.))
          (prev_representable_time at_9)
      in
      is_correct_transition ~here:[[%here]] output None
    ;;

    let%test_unit "call next_enter ends at start of schedule" =
      let output =
        next_enter_between schedule
          (prev_representable_time at_9)
          at_9
      in
      is_correct_transition ~here:[[%here]] output (Some at_9)
    ;;

    let%test_unit "call next_enter after start of schedule" =
      let output =
        next_enter_between schedule
          (prev_representable_time at_9)
          leave_9
      in
      is_correct_transition ~here:[[%here]] output (Some at_9)
    ;;

    let%test_unit "call next_enter after start_of_schedule" =
      let output =
        next_enter_between schedule
          (next_representable_time at_9)
          leave_9_0_2
      in
      is_correct_transition ~here:[[%here]] output (Some at_9_0_2)
    ;;

    let%test_unit "call_next_leave before leave" =
      let output =
        next_leave_between schedule
          (prev_representable_time leave_9)
          (Time.add leave_9 (Time.Span.of_sec 1.))
      in
      assert (not (includes schedule leave_9));
      is_correct_transition ~here:[[%here]] output (Some leave_9)
    ;;

    let%test_unit "call_next_leave at leave" =
      let output =
        next_leave_between schedule
          leave_9
          (Time.add leave_9 (Time.Span.of_sec 1.))
      in
      is_correct_transition ~here:[[%here]] output (Some leave_9)
    ;;

    let%test_unit "call_next_leave after leave" =
      let output =
        next_leave_between schedule
          (next_representable_time leave_9)
          (Time.add leave_9 (Time.Span.of_sec 0.5))
      in
      is_correct_transition ~here:[[%here]] output None
    ;;

    let%test_unit "call_next_leave ends before leave" =
      let output =
        next_leave_between schedule
          at_9
          (prev_representable_time leave_9)
      in
      assert (not (includes schedule leave_9));
      is_correct_transition ~here:[[%here]] output None
    ;;

    let%test_unit "call_next_leave ends at leave" =
      let output =
        next_leave_between schedule
          (prev_representable_time leave_9)
          leave_9
      in
      is_correct_transition ~here:[[%here]] output (Some leave_9)
    ;;

    let%test_unit "call_next_leave after leave" =
      let output =
        next_leave_between schedule
          (prev_representable_time leave_9)
          (Time.add leave_9 (Time.Span.of_sec 0.5))
      in
      is_correct_transition ~here:[[%here]] output (Some leave_9)
    ;;

    let schedule =
      At [ Time.Ofday.create ~hr:9 ~min:0 ()
         ; Time.Ofday.create ~hr:9 ~min:2 ()
         ]

    let schedule = In_zone (zone, schedule)

    let%test_unit "next_enter_between_minutes_on_hr_boundary_1" =
      let output =
        next_enter_between schedule
          (Time.of_string "2014-11-17 08:59:00Z")
          (Time.of_string "2014-11-17 09:00:00Z")
      in
      is_correct_transition ~here:[[%here]] output (Some at_9)
    ;;

    let%test_unit "next_leave_between_minutes_on_hr_boundary_2" =
      let output =
        next_leave_between schedule
          (Time.of_string "2014-11-17 09:00:00.0000001Z")
          (Time.of_string "2014-11-17 09:00:01Z")
      in
      is_correct_transition ~here:[[%here]] output (Some leave_9)
    ;;

    let%test_unit "next_leave_between_minutes_on_hr_boundary_3" =
      let output =
        next_leave_between schedule
          (Time.of_string "2014-11-17 09:00:00Z")
          (Time.of_string "2014-11-17 09:01:00Z")
      in
      is_correct_transition ~here:[[%here]] output (Some leave_9)
    ;;

    let%test_unit "next_enter_between_minutes_on_hr_boundary_4" =
      let output =
        next_enter_between schedule
          (Time.of_string "2014-11-17 08:59:00Z")
          (Time.of_string "2014-11-17 09:01:00Z")
      in
      is_correct_transition ~here:[[%here]] output (Some at_9)
    ;;

    let%test_unit "Regression test: this does not crash" =
      let time = Time.of_string "2016-07-27 00:00:00.000000Z" in
      let _ : Time.t option =
        next_enter_between (In_zone (Time.Zone.utc, Hours [])) time
          (Time.add time (Time.Span.of_day 1.))
      in
      ()
    ;;

    (* note that next_enter_between is inclusive on both ends *)
    let%test_unit "Test 24:00:00 handling for Hours [0]" =
      let time = Time.of_string "2016-07-26 24:00:00.000000Z" in
      let next =
        next_enter_between
          (In_zone (Time.Zone.utc, Hours [0]))
          time
          (Time.add time (Time.Span.of_hr 5.))
      in
      match next with
      | None    -> failwith "no next time found starting with hour 0"
      | Some t' -> assert (Time.(=) time t')
    ;;

    (* Test start of day *)
    let schedule = At [ Time.Ofday.start_of_day ]
    let schedule = In_zone (zone, schedule)
    let date ~d = date ~y:2014 ~m:Nov ~d
    let start_of_day ~d = time (date ~d) Time.Ofday.start_of_day

    let%test_unit "next_enter_between starting from start_of_day" =
      let output =
        next_enter_between schedule
          (time (date ~d:17) Time.Ofday.start_of_day)
          (time (date ~d:17) (ofday ~hr:1 ()))
      in
      is_correct_transition ~here:[[%here]] output (Some (start_of_day ~d:17))
    ;;

    let%test_unit "next_enter_between ending at start_of_day" =
      let output =
        next_enter_between schedule
          (time (date ~d:16) (ofday ~hr:23 ()))
          (time (date ~d:17) Time.Ofday.start_of_day)
      in
      is_correct_transition ~here:[[%here]] output (Some (start_of_day ~d:17))
    ;;

    let%test_unit "next_enter_between starting at start_of_day ending at start_of_day" =
      let output =
        next_enter_between schedule
          (time (date ~d:17) Time.Ofday.start_of_day)
          (time (date ~d:17) Time.Ofday.start_of_day)
      in
      is_correct_transition ~here:[[%here]] output (Some (start_of_day ~d:17))
    ;;

    let%test_unit "exclusive start time in between is honored" =
      Time.set_sexp_zone Time.Zone.utc;
      let schedule =
        (
          In_zone ((Time.Zone.find_exn "Europe/London")
                  , Between ((Exclusive , Time.Ofday.create ~hr:11 ())
                            , (Inclusive, Time.Ofday.create ~hr:13 ()))))
      in
      let s =
        begin
          match to_endless_sequence schedule
                  ~start_time:(Time.of_string "2016-01-05 00:00:00Z")
                  ~emit:Transitions
          with
          | `Started_in_range (_, q) -> q
          | `Started_out_of_range q -> q
        end
        |> Fn.flip Sequence.take 5
        |> Sequence.to_list
      in
      [%test_eq: [ Event.no_change | unit Event.transition ] list]
        [ `Enter (next_representable_time (Time.of_string "2016-01-05 11:00:00Z"), [])
        ; `Leave (next_representable_time (Time.of_string "2016-01-05 13:00:00.000000Z"))
        ; `Enter (next_representable_time (Time.of_string "2016-01-06 11:00:00Z"), [])
        ; `Leave (next_representable_time (Time.of_string "2016-01-06 13:00:00.000000Z"))
        ; `Enter (next_representable_time (Time.of_string "2016-01-07 11:00:00Z"), []) ]
        s
    ;;

    let%test_module "Schedule.Stable.V4" = (module Core_kernel.Stable_unit_test.Make (struct
        type t = unit Stable.V4.t [@@deriving bin_io, compare, sexp]

        let equal = [%compare.equal: t]

        let tests =
          [ Always
          , "Always"
          , "\006"
          ; Never
          , "Never"
          , "\007" ]
          @
          ([ Always
           , "(In_zone UTC Always)"
           , "\000\003UTC\017"
           ; Never
           , "(In_zone UTC Never)"
           , "\000\003UTC\018"
           ; Secs [ 30 ]
           , "(In_zone UTC (Secs (30)))"
           , "\000\003UTC\007\001\030"
           ; Mins [ 5 ]
           , "(In_zone UTC (Mins (5)))"
           , "\000\003UTC\b\001\005"
           ; Hours [ 3; 18 ]
           , "(In_zone UTC (Hours (3 18)))"
           , "\000\003UTC\t\002\003\018"
           ; Weekdays [ Mon; Wed ]
           , "(In_zone UTC (Weekdays (MON WED)))"
           , "\000\003UTC\n\002\001\003"
           ; Days [ 1; 30 ]
           , "(In_zone UTC (Days (1 30)))"
           , "\000\003UTC\011\002\001\030"
           ; Weeks [ 1 ]
           , "(In_zone UTC (Weeks (1)))"
           , "\000\003UTC\012\001\001"
           ; Weeks [ 1; 7 ]
           , "(In_zone UTC (Weeks (1 7)))"
           , "\000\003UTC\012\002\001\007"
           ; Months [ Nov; Mar ]
           , "(In_zone UTC (Months (Nov Mar)))"
           , "\000\003UTC\r\002\n\002"
           ; On [ Date.create_exn ~y:2014 ~m:Nov ~d:17; Date.create_exn ~y:2014 ~m:Jan ~d:1 ]
           , "(In_zone UTC (On (2014-11-17 2014-01-01)))"
           , "\000\003UTC\014\002\254\222\007\n\017\254\222\007\000\001"
           ; Before (Inclusive,
                     ( Date.create_exn ~y:2014 ~m:Nov ~d:17, Time.Ofday.create ()))
           , "(In_zone UTC (Before Inclusive (2014-11-17 00:00:00.000000)))"
           , "\000\003UTC\015\000\254\222\007\n\017\000\000\000\000\000\000\000\000"
           ; After (Exclusive, ( Date.create_exn ~y:2014 ~m:Nov ~d:18, Time.Ofday.create () ))
           , "(In_zone UTC (After Exclusive (2014-11-18 00:00:00.000000)))"
           , "\000\003UTC\016\001\254\222\007\n\018\000\000\000\000\000\000\000\000"
           ; Between ((Inclusive, Time.Ofday.create ~hr:9 ()), (Exclusive, Time.Ofday.create ~hr:17 ()))
           , "(In_zone UTC (Between (Inclusive 09:00:00.000000) (Exclusive 17:00:00.000000)))"
           , "\000\003UTC\005\000\000\000\000\000\000\164\223@\001\000\000\000\000\000\226\237@"
           ; And [ Mins [ 15 ] ; Hours [ 7; 8 ] ]
           , "(In_zone UTC (And ((Mins (15)) (Hours (7 8)))))"
           , "\000\003UTC\001\002\b\001\015\t\002\007\b"
           ; And
               [ On [ Date.create_exn ~y:2014 ~m:Dec ~d:15 ]
               ; At [ Time.Ofday.create ~hr:23 ~min:14 () ] ]
           , "(In_zone UTC (And ((On (2014-12-15)) (At (23:14:00.000000)))))"
           , "\000\003UTC\001\002\014\001\254\222\007\011\015\006\001\000\000\000\000\128k\244@"
           ; Or [ Mins [ 15 ] ; Hours [ 7; 8 ] ]
           , "(In_zone UTC (Or ((Mins (15)) (Hours (7 8)))))"
           , "\000\003UTC\002\002\b\001\015\t\002\007\b"
           ; Shift (Time.Span.of_sec 0., Mins [ 5 ])
           , "(In_zone UTC (Shift 0s (Mins (5))))"
           , "\000\003UTC\004\000\000\000\000\000\000\000\000\b\001\005"
           ; Not Always
           , "(In_zone UTC (Not Always))"
           , "\000\003UTC\003\017"
           ; If_then_else (Always, Never, Never)
           , "(In_zone UTC (If_then_else Always Never Never))"
           , "\000\003UTC\019\017\018\018"
           ]
           |> List.map ~f:(fun (t, sexp, binprot) -> In_zone (Time.Zone.utc, t), sexp, binprot))
        ;;

        let%test_unit _ =
          ignore (t_of_sexp (Sexp.of_string "(In_zone Local Always)"))
        ;;
      end))

    let extract_first_element_of_endless_sequence ~schedule ~start_time ~emit =
      match to_endless_sequence schedule ~start_time ~emit with
      | `Started_in_range (_, seq)
      | `Started_out_of_range seq -> Sequence.hd_exn seq
    ;;

    let schedule_do_not_spin_forever ~start_time schedule =
      let one_day_later = Time.add start_time (Time.Span.of_hr 24.) in
      let tags =
        extract_first_element_of_endless_sequence ~schedule ~start_time
          ~emit:(Transitions_and_tag_changes (=))
      in
      let no_change = (`No_change_until_at_least (`In_range, one_day_later)) in
      match tags with
      | #Event.tag_change ->
        extract_first_element_of_endless_sequence ~schedule ~start_time ~emit:Transitions
        |> [%test_eq: [ unit Event.transition | Event.no_change ]] no_change;
      | #Event.transition
      | #Event.no_change -> assert false
    ;;

    let%test_unit "Schedules with tag transitions but no Enter/Leave do not spin forever." =
      let start_time = Time.of_string "2016-05-06 00:00Z" in
      let zone = Time.Zone.find_exn "America/New_York" in
      schedule_do_not_spin_forever ~start_time
        (In_zone (zone, Or [ Always; Tag ((), Hours [19] )]));
      let initial_bug_report =
        In_zone (zone
                , Or
                    [ After (Exclusive, (Date.of_string "2016-05-05", Time.Ofday.of_string "1:00"))
                    ; Tag ( ()
                          , Not (Between ( (Inclusive, Time.Ofday.of_string "1:05")
                                         , (Inclusive, Time.Ofday.of_string "23:00")))) ])
      in
      schedule_do_not_spin_forever ~start_time initial_bug_report
    ;;

    let%test_unit "Schedules using Sec and Min are inclusive when used with next_enter_between" =
      let start_time = Time.of_string "2016-07-27 00:00:00.000000+00:00" in
      let test_inclusivity sched =
        let first_enter_found =
          next_enter_between
            sched
            start_time
            (Time.add start_time (Time.Span.of_day 365.))
          |> Option.value_exn
        in
        let second_enter_found =
          next_enter_between
            sched
            first_enter_found
            (Time.add first_enter_found (Time.Span.of_day 365.))
          |> Option.value_exn
        in
        if Time.(<>) first_enter_found second_enter_found
        then raise_s
               [%message
                 "next_enter_between should always be inclusive in the first bound"
                   (first_enter_found : Time.t)
                   (second_enter_found : Time.t) ]
      in
      let sec_sched = In_zone (Time.Zone.utc, Secs [15]) in
      let min_sched = In_zone (Time.Zone.utc, Mins [15]) in
      test_inclusivity sec_sched;
      test_inclusivity min_sched;
    ;;

    let%expect_test "DST fallback bugs" =
      let schedule =
        Schedule_v4_deprecated.In_zone
          ( force Time.Zone.local
          , At [ Time.Ofday.create ~hr:1 ~min:30 () ]
          )
      in
      let print_next start_time =
        Schedule_v4_deprecated.next_enter_between schedule
          start_time
          (Time.of_string "2018-11-06 00:00:00")
        |> Option.map ~f:Time.to_string
        |> [%sexp_of: string option]
        |> print_s
      in

      (* Just demonstrating the relative ordering of two times, to help understand the
         rest of the test. *)
      begin
        Time.compare
          (Time.of_string "2018-11-04 01:30:00-04:00")
          (Time.of_string "2018-11-04 01:30:00-05:00")
        |> [%sexp_of: int]
        |> print_s;
        [%expect {| -1 |}]
      end;

      (* Start time as explicit offset from UTC *)
      begin
        (* The documentation claims that this should be in schedule twice, but this result
           says the first occurrence is the second of the two. *)
        print_next (Time.of_string "2018-11-04 00:59:00-04:00");
        [%expect {| ("2018-11-04 01:30:00.000000-05:00") |}];

        (* Moving the start time forward made the next event earlier, which is clearly a
           bug. *)
        print_next (Time.of_string "2018-11-04 01:29:00-04:00");
        [%expect {| ("2018-11-04 01:30:00.000000-04:00") |}];

        (* This looks correct. *)
        print_next (Time.of_string "2018-11-04 01:31:00-04:00");
        [%expect {| ("2018-11-04 01:30:00.000000-05:00") |}];

        (* This looks correct. *)
        print_next (Time.of_string "2018-11-04 01:31:00-05:00");
        [%expect {| ("2018-11-05 01:30:00.000000-05:00") |}];
      end;

      (* Start time in NYC time zone *)
      begin
        (* The documentation claims that this should be in schedule twice, but this result
           says the first occurrence is the second of the two. *)
        print_next
          (Time.of_date_ofday (Date.of_string "2018-11-04")
             (Time.Ofday.create ~hr:0 ~min:59 ()) ~zone:(force Time.Zone.local));
        [%expect {| ("2018-11-04 01:30:00.000000-05:00") |}];

        (* Whether this is correct depends on whether [Time.of_date_ofday] is generating
           the first or second occurrence of the given ofday, which I'm not sure of. If
           it's the second, which seems likely, this is correct. *)
        print_next
          (Time.of_date_ofday (Date.of_string "2018-11-04")
             (Time.Ofday.create ~hr:1 ~min:29 ()) ~zone:(force Time.Zone.local));
        [%expect {| ("2018-11-04 01:30:00.000000-05:00") |}];

        (* Whether this is correct depends on whether [Time.of_date_ofday] is generating
           the first or second occurrence of the given ofday, which I'm not sure of. If
           it's the second, which seems likely, this is correct. *)
        print_next
          (Time.of_date_ofday (Date.of_string "2018-11-04")
             (Time.Ofday.create ~hr:1 ~min:31 ()) ~zone:(force Time.Zone.local));
        [%expect {| ("2018-11-05 01:30:00.000000-05:00") |}];
      end
    ;;
  end)
