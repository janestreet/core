open Core_kernel.Std

module Date = Date0
module Unix = Core_unix

module Stable = struct
  module V1 = struct
    (* IF THIS REPRESENTATION EVER CHANGES, ENSURE THAT EITHER
       (1) all values serialize the same way in both representations, or
       (2) you add a new Time version to stable.ml *)
    include Time_internal

    let to_epoch t = T.to_float t
    let of_epoch t = T.of_float t

    module Epoch_cache = struct
      type t = {
        zone      : Zone.t;
        day_start : float;
        day_end   : float;
        date      : Date.t
      } [@@deriving sexp]
    end

    let upper_bound_native_int = Core_kernel.Float0.upper_bound_for_int Nativeint.num_bits
     (* shifted epoch for the time zone for conversion *)
    let date_ofday_of_epoch_internal zone time =
      let parts  = Float.modf time in
      let sec    = Float.Parts.integral parts in
      let subsec = Float.Parts.fractional parts in
      let sec,subsec =
        if subsec < 0. then (sec -. 1., 1. +. subsec)
        else (sec, subsec)
      in
      if Float.(abs sec > upper_bound_native_int)
      then raise (Invalid_argument "Time.date_ofday_of_epoch");
      let tm      = Unix.gmtime sec in
      let date    = Date.of_tm tm in
      let ofday_span =
        Float.of_int
          (tm.Unix.tm_hour * 60 * 60
           + tm.Unix.tm_min * 60
           + tm.Unix.tm_sec)
        +. (Float.abs subsec)
      in
      let ofday     = Ofday.of_span_since_start_of_day (Span.of_sec ofday_span) in
      let day_start = time -. ofday_span in
      let day_end   = day_start +. (24. *. 60. *. 60.) in
      let cache     = {Epoch_cache. zone; day_start; day_end; date } in
      (cache, (date, ofday))
    ;;

    (* A thin caching layer over the actual date_ofday_of_epoch
       (date_ofday_of_epoch_internal just above) used only to gain some speed when we
       translate the same time/date over and over again *)
    let date_ofday_of_epoch =
      let cache = ref (fst (date_ofday_of_epoch_internal Zone.utc (to_epoch (T.now ())))) in
      (fun zone unshifted ->
        let time = Zone.shift_epoch_time zone `UTC unshifted in
        let {Epoch_cache.zone = z; day_start = s; day_end = e; date = date} = !cache in
        if phys_equal zone z && time >= s && time < e then (
          (date, Ofday.of_span_since_start_of_day (Span.of_sec (time -. s))))
        else begin
          let (new_cache,r) = date_ofday_of_epoch_internal zone time in
          cache := new_cache;
          r
        end)
    ;;

    let to_date_ofday time ~zone =
      try
        date_ofday_of_epoch zone (to_epoch time)
      with
      | Invalid_argument _
      | Unix.Unix_error(_, "gmtime", _)
        -> raise (Invalid_argument "Time.to_date_ofday")
    ;;

    (* The correctness of this algorithm (interface, even) depends on the fact that
       timezone shifts aren't too close together (as in, it can't simultaneously be the
       case that a timezone shift of X hours occurred less than X hours ago, *and*
       a timezone shift of Y hours will occur in less than Y hours' time) *)
    let to_date_ofday_precise time ~zone =
      let date, ofday       = to_date_ofday time ~zone in
      let clock_shift_after = Zone.next_clock_shift zone ~after:time in
      let clock_shift_before_or_at =
        let after_time_but_not_after_next =
          match clock_shift_after with
          | None                 -> T.add time Span.second
          | Some (next_start, _) -> next_start
        in
        Zone.prev_clock_shift zone ~before:after_time_but_not_after_next
      in
      let also_skipped_earlier amount =
        (* Using [date] and [Option.value_exn] here is OK on the assumption that clock
           shifts can't cross date boundaries. This is true in all cases I've ever heard
           of (and [of_date_ofday_precise] would need revisiting if it turned out to be
           false) *)
        `Also_skipped
          ( date
          , Option.value_exn
              ~error:(Error.create "Time.to_date_ofday_precise"
                (T.to_float time, zone) [%sexp_of: float * Zone.t])
              (Ofday.sub ofday amount)
          )
      in
      let ambiguity =
        (* Edge cases: the instant of transition belongs to the new zone regime. So if the
           clock moved by an hour exactly one hour ago, there's no ambiguity, because the
           hour-ago time belongs to the same regime as you, and conversely, if the clock
           will move by an hour in an hours' time, there *is* ambiguity. Hence [>.] for
           the first case and [<=.] for the second. *)
        match clock_shift_before_or_at, clock_shift_after with
        | Some (start, amount), _ when T.(>.) (T.add start (Span.abs amount)) time ->
          (* clock shifted recently *)
          if Span.(amount > zero) then
            (* clock shifted forward recently: we skipped a time *)
            also_skipped_earlier amount
          else begin
            (* clock shifted back recently: this date/ofday already happened *)
            assert Span.(amount < zero);
            `Also_at (T.sub time (Span.abs amount))
          end
        | _, Some (start, amount) when T.(<=.) (T.sub start (Span.abs amount)) time ->
          (* clock is about to shift *)
          if Span.(amount > zero) then
            (* clock about to shift forward: no effect *)
            `Only
          else begin
            (* clock about to shift back: this date/ofday will be repeated *)
            assert Span.(amount < zero);
            `Also_at (T.add time (Span.abs amount))
          end
        | _ -> `Only
      in
      date, ofday, ambiguity
    ;;

    let of_date_ofday date ofday ~zone =
      let time =
        let epoch =
          utc_mktime ~year:(Date.year date) ~month:(Month.to_int (Date.month date))
            ~day:(Date.day date) ~ofday_sec:(Span.to_sec (Ofday.to_span_since_start_of_day ofday))
        in
        Zone.shift_epoch_time zone `Local epoch
      in
      T.of_float time
    ;;

    let of_date_ofday_precise date ofday ~zone =
      (* We assume that there will be only one zone shift within a given local day.  *)
      let start_of_day = of_date_ofday ~zone date Ofday.start_of_day in
      let proposed_time = T.add start_of_day (Ofday.to_span_since_start_of_day ofday) in
      match Zone.next_clock_shift zone ~after:start_of_day with
      | None -> `Once proposed_time
      | Some (shift_start, shift_amount) ->
        let shift_backwards = Span.(shift_amount < zero) in
        (* start and end of the "problematic region" *)
        let s,e =
          if shift_backwards
          then T.add shift_start shift_amount, shift_start
          else shift_start, T.add shift_start shift_amount
        in
        if T.(proposed_time < s) then
          `Once proposed_time
        else if T.(s <= proposed_time && proposed_time < e) then begin
          if shift_backwards
          then `Twice (proposed_time, T.sub proposed_time shift_amount)
          else `Never shift_start
        end else
          `Once (T.sub proposed_time shift_amount)
    ;;

    let of_tm tm ~zone =
      (* Explicitly ignoring isdst, wday, yday (they are redundant with the other fields
         and the [zone] argument) *)
      let
        { Core_unix.tm_year; tm_mon; tm_mday; tm_hour; tm_min; tm_sec
        ; tm_isdst = _; tm_wday = _; tm_yday = _ } = tm
      in
      let date =
        Date.create_exn
          ~y:(tm_year + 1900)
          ~m:(Month.of_int_exn (tm_mon + 1))
          ~d:tm_mday
      in
      let ofday = Ofday.create ~hr:tm_hour ~min:tm_min ~sec:tm_sec () in
      of_date_ofday ~zone date ofday
    ;;

    let%test_module "clock shift stuff" = (module struct
      (* Some stuff to make [%test_result: t] failures look nicer. Notice that a bug in
         [to_date_ofday] could cause this thing to lie. *)
      type time = T.t [@@deriving compare]
      let sexp_of_time t =
        let d, o = to_date_ofday t ~zone:Zone.utc in
        [%sexp_of:  Date.t * Ofday.t * Zone.t] (d, o, Zone.utc)

      type to_date_ofday_ambiguity =
        [ `Only
        | `Also_at of time
        | `Also_skipped of (Date.t * Ofday.t)
        ] [@@deriving compare, sexp_of]

      type of_date_ofday_result =
        [ `Once of time
        | `Twice of time * time
        | `Never of time
        ] [@@deriving compare, sexp_of]

      let zone = Zone.find_exn "Europe/London"

      let mkt month day hr min =
        let ofday_sec = 60. *. ((Float.of_int hr *. 60.) +. Float.of_int min) in
        T.of_float (utc_mktime ~year:2013 ~month ~day ~ofday_sec)

      let simple_case date ofday time =
        [%test_result: of_date_ofday_result]
          ~expect:(`Once time) (of_date_ofday_precise ~zone date ofday);
        [%test_result: Date.t * Ofday.t * to_date_ofday_ambiguity]
          ~expect:(date, ofday, `Only) (to_date_ofday_precise ~zone time)

      let skipped_this_time date ofday skipped_at =
        [%test_result: of_date_ofday_result]
          ~expect:(`Never skipped_at) (of_date_ofday_precise ~zone date ofday);
        let time = of_date_ofday ~zone date ofday in
        let d, o, a = to_date_ofday_precise ~zone time in
        [%test_result: Date.t] ~expect:date d;
        let diff = Ofday.diff o ofday in
        [%test_result: Span.t] ~expect:Span.hour diff;
        [%test_result: to_date_ofday_ambiguity] ~expect:(`Also_skipped (date, ofday)) a

      let skipped_prev_time date ofday time =
        [%test_result: of_date_ofday_result]
          ~expect:(`Once time) (of_date_ofday_precise ~zone date ofday);
        let d, o, a = to_date_ofday_precise ~zone time in
        [%test_result: Date.t] ~expect:date d;
        [%test_result: Ofday.t] ~expect:ofday o;
        [%test_result: to_date_ofday_ambiguity]
          ~expect:(`Also_skipped (date, Option.value_exn (Ofday.sub o Span.hour))) a

      let repeated_time date ofday ~first =
        let second = T.add first Span.hour in
        [%test_result: of_date_ofday_result]
          ~expect:(`Twice (first, second)) (of_date_ofday_precise ~zone date ofday);
        [%test_result: Date.t * Ofday.t * to_date_ofday_ambiguity]
          ~expect:(date, ofday, `Also_at second) (to_date_ofday_precise ~zone first);
        [%test_result: Date.t * Ofday.t * to_date_ofday_ambiguity]
          ~expect:(date, ofday, `Also_at first) (to_date_ofday_precise ~zone second)

      let (^:) hr min = Ofday.create ~hr ~min ()

      let outside_bst = Date.of_string "2013-01-01";;
      let inside_bst  = Date.of_string "2013-06-01";;

      let%test_unit "of_date_ofday_precise, outside BST" =
        simple_case outside_bst (12^:00) (mkt 01 01  12 00)

      let%test_unit "of_date_ofday_precise, inside BST" =
        simple_case inside_bst (12^:00) (mkt 06 01  11 00)

      let bst_start   = Date.of_string "2013-03-31";;
      let bst_end     = Date.of_string "2013-10-27";;

      let%test_unit "of_date_ofday_precise, just before skipped hour" =
        simple_case bst_start (00^:59) (mkt 03 31  00 59)

      let%test_unit "of_date_ofday_precise, start of skipped hour" =
        skipped_this_time bst_start (01^:00) (mkt 03 31  01 00)

      let%test_unit "of_date_ofday_precise, during skipped hour" =
        skipped_this_time bst_start (01^:30) (mkt 03 31  01 00)

      let%test_unit "of_date_ofday_precise, end of skipped hour" =
        skipped_prev_time bst_start (02^:00) (mkt 03 31  01 00)

      let%test_unit "of_date_ofday_precise, just after skipped hour" =
        skipped_prev_time bst_start (02^:01) (mkt 03 31  01 01)

      let%test_unit "of_date_ofday_precise, later after skipped hour" =
        simple_case bst_start (03^:00) (mkt 03 31  02 00)

      let%test_unit "of_date_ofday_precise, just before repeated hour" =
        simple_case bst_end (00^:59) (mkt 10 26  23 59)

      let%test_unit "of_date_ofday_precise, start of repeated hour" =
        repeated_time bst_end (01^:00) ~first:(mkt 10 27  00 00)

      let%test_unit "of_date_ofday_precise, during repeated hour" =
        repeated_time bst_end (01^:30) ~first:(mkt 10 27  00 30)

      let%test_unit "of_date_ofday_precise, end of repeated hour" =
        simple_case bst_end (02^:00) (mkt 10 27  02 00)

      let%test_unit "of_date_ofday_precise, after repeated hour" =
        simple_case bst_end (02^:01) (mkt 10 27  02 01)
    end)

    let to_date t ~zone  = fst (to_date_ofday t ~zone)
    let to_ofday t ~zone = snd (to_date_ofday t ~zone)

    let convert ~from_tz ~to_tz date ofday =
      let start_time = T.to_float (of_date_ofday ~zone:from_tz date ofday) in
      date_ofday_of_epoch to_tz start_time

    let utc_offset t ~zone =
      let epoch     = to_epoch t in
      let utc_epoch = Zone.shift_epoch_time zone `UTC epoch in
      Span.of_sec (utc_epoch -. epoch)
    ;;

    let offset_string time ~zone =
      let utc_offset   = utc_offset time ~zone in
      let is_utc       = Span.(=) utc_offset Span.zero in
      if is_utc
      then "Z"
      else
        String.concat
          [ (if Span.(<) utc_offset Span.zero then "-" else "+");
            Ofday.to_string_trimmed
              (Ofday.of_span_since_start_of_day (Span.abs utc_offset));
          ]
    ;;

    let to_string_abs_parts time ~zone =
      let date, ofday   = to_date_ofday time ~zone in
      let offset_string = offset_string time ~zone in
      [ Date.to_string date;
        String.concat ~sep:"" [ Ofday.to_string ofday; offset_string ]
      ]
    ;;

    let to_string_abs_trimmed time ~zone =
      let date, ofday = to_date_ofday time ~zone in
      let offset_string = offset_string time ~zone in
      String.concat ~sep:" "
        [ (Date.to_string date)
        ; (Ofday.to_string_trimmed ofday) ^ offset_string
        ]
    ;;

    let to_string_abs time ~zone =
      String.concat ~sep:" " (to_string_abs_parts ~zone time)
    ;;

    let to_string_iso8601_basic time ~zone =
      String.concat ~sep:"T" (to_string_abs_parts ~zone time)

    let%expect_test "[to_string_iso8601] in in zulu" =
      printf !"%s" (to_string_iso8601_basic ~zone:Zone.utc (T.of_float 12.345678));
      [%expect {| 1970-01-01T00:00:12.345678Z |}];
    ;;

    let%expect_test "[to_string_iso8601] in in local/nyc" =
      printf !"%s" (to_string_iso8601_basic ~zone:Zone.local (T.of_float 12.345678));
      [%expect {| 1969-12-31T19:00:12.345678-05:00 |}];
    ;;

    let to_string_trimmed t ~zone =
      let date, sec = to_date_ofday ~zone t in
      (Date.to_string date) ^ " " ^ (Ofday.to_string_trimmed sec)
    ;;

    let to_sec_string t ~zone =
      let date, sec = to_date_ofday ~zone t in
      (Date.to_string date) ^ " " ^ (Ofday.to_sec_string sec)
    ;;

    let to_filename_string t ~zone =
      let date, ofday = to_date_ofday ~zone t in
      (Date.to_string date) ^ "_" ^
        (String.tr ~target:':' ~replacement:'-' (Ofday.to_string ofday))
    ;;

    let to_string_fix_proto utc t =
      let zone =
        match utc with
        | `Utc -> Zone.utc
        | `Local -> Zone.local
      in
      let date, sec = to_date_ofday t ~zone in
      (Date.to_string_iso8601_basic date) ^ "-" ^ (Ofday.to_millisec_string sec)
    ;;

    let of_string_fix_proto utc str =
      try
        let expect_length = 21 in  (* = 8 + 1 + 12 *)
        let expect_dash = 8 in
        if str.[expect_dash] <> '-' then
          failwithf "no dash in position %d" expect_dash ();
        let zone =
          match utc with
          | `Utc -> Zone.utc
          | `Local -> Zone.local
        in
        if Int.(>) (String.length str) expect_length then
          failwithf "input too long" ();
        of_date_ofday ~zone
          (Date.of_string_iso8601_basic str ~pos:0)
          (Ofday.of_string_iso8601_extended str ~pos:(expect_dash + 1))
      with exn ->
        invalid_argf "Time.of_string_fix_proto %s: %s" str (Exn.to_string exn) ()
    ;;

    let of_filename_string s ~zone =
      try
        match String.lsplit2 s ~on:'_' with
        | None -> failwith "no space in filename string"
        | Some (date, ofday) ->
          let date = Date.of_string date in
          let ofday = String.tr ~target:'-' ~replacement:':' ofday in
          let ofday = Ofday.of_string ofday in
          of_date_ofday date ofday ~zone
      with
      | exn ->
        invalid_argf "Time.of_filename_string (%s): %s" s (Exn.to_string exn) ()
    ;;

    let format t s ~zone =
      let local = Zone.local in
      let epoch_time =
        to_epoch t
        |> Zone.shift_epoch_time local `Local
        |> Zone.shift_epoch_time zone `UTC
      in
      Unix.strftime (Unix.localtime epoch_time) s
    ;;

    let parse s ~fmt ~zone =
      Unix.strptime ~fmt s
      |> of_tm ~zone
    ;;

    let pause_for span =
      let time_remaining =
        (* If too large a float is passed in (Span.max_value for instance) then
           nanosleep will return immediately, leading to an infinite and expensive
           select loop.  This is handled by pausing for no longer than 100 days.
        *)
        let span = Span.min span (Span.scale Span.day 100.) in
        Unix.nanosleep (Span.to_sec span)
      in
      if time_remaining > 0.0
      then `Remaining (Span.of_sec time_remaining)
      else `Ok
    ;;

    (** Pause and don't allow events to interrupt. *)
    let rec pause span =
      match pause_for span with
      | `Remaining span -> pause span
      | `Ok -> ()
    ;;

    (** Pause but allow events to interrupt. *)
    let interruptible_pause = pause_for

    let rec pause_forever () =
      pause (Span.of_day 1.0);
      pause_forever ()
    ;;

    let occurrence before_or_after t ~ofday ~zone =
      let first_guess_date = to_date t ~zone in
      let first_guess      = of_date_ofday ~zone first_guess_date ofday in
      let cmp, increment =
        match before_or_after with
        | `Last_before_or_at -> T.(<=), (-1)
        | `First_after_or_at -> T.(>=), 1
      in
      if cmp first_guess t
      then first_guess
      else of_date_ofday ~zone (Date.add_days first_guess_date increment) ofday
    ;;

    let epoch = T.of_float 0.0

    (* There are a number of things that would be shadowed by this include because of the
       scope of Constrained_float.  These need to be defined below.  It's a an unfortunate
       situation because we would like to say include T, without shadowing. *)
    include T

    let to_string t = to_string_abs t ~zone:Zone.local

    let ensure_colon_in_offset offset =
      if Char.(=) offset.[1] ':'
         || Char.(=) offset.[2] ':'
      then offset
      else begin
        let offset_length = String.length offset in
        if Int.(<) offset_length 3 || Int.(>) offset_length 4
        then failwithf "invalid offset %s" offset ()
        else String.concat
               [ String.slice offset 0 (offset_length - 2)
               ; ":"
               ; String.slice offset (offset_length - 2) offset_length ]
      end
    ;;

    let%test_module "ensure_colon_in_offset" = (module struct
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

    exception Time_of_string of string * Exn.t [@@deriving sexp]
    exception Time_string_not_absolute of string [@@deriving sexp]
    let of_string_gen ~if_no_timezone s =
      try
        let date,ofday,tz =
          match String.split s ~on:' ' with
          | [day; month; year; ofday] ->
            (String.concat [day; " "; month; " "; year], ofday, None)
          | [date; ofday; tz] -> (date, ofday, Some tz)
          | [date; ofday]     -> (date, ofday, None)
          | [s]              ->
            begin match String.rsplit2 ~on:'T' s with
            | Some (date, ofday) -> (date, ofday, None)
            | None -> failwith "no spaces or T found"
            end
          | _ -> failwith "too many spaces"
        in
        let ofday_to_sec od = Span.to_sec (Ofday.to_span_since_start_of_day od) in
        let ofday,utc_offset =
          match tz with
          | Some _ -> ofday, None
          | None   ->
            if Char.(=) ofday.[String.length ofday - 1] 'Z'
            then (String.sub ofday ~pos:0 ~len:(String.length ofday - 1), Some 0.)
            else begin
              match String.lsplit2 ~on:'+' ofday with
              | Some (l, r) ->
                (l, Some (ofday_to_sec (Ofday.of_string (ensure_colon_in_offset r))))
              | None ->
                begin match String.lsplit2 ~on:'-' ofday with
                | Some (l, r) ->
                  ( l
                  , Some ((-1.)
                          *. (ofday_to_sec (Ofday.of_string (ensure_colon_in_offset r)))))
                | None       -> ofday, None
                end
            end
        in
        let date  = Date.of_string date in
        let ofday = Ofday.of_string ofday in
        match tz with
        | Some tz -> of_date_ofday ~zone:(Zone.find_exn tz) date ofday
        | None ->
          match utc_offset with
          | None            ->
            begin match if_no_timezone with
            | `Fail -> raise (Time_string_not_absolute s);
            | `Use_this_one zone -> of_date_ofday ~zone date ofday
            end
          | Some utc_offset ->
            of_float (to_float (of_date_ofday ~zone:Zone.utc date ofday) -. utc_offset)
      with
      | e -> raise (Time_of_string (s,e))
    ;;

    let of_string_abs s = of_string_gen ~if_no_timezone:`Fail s
    let of_string s     = of_string_gen ~if_no_timezone:(`Use_this_one Zone.local) s

    let sexp_zone = ref Zone.local
    let get_sexp_zone () = !sexp_zone
    let set_sexp_zone zone = sexp_zone := zone

    let t_of_sexp_gen ~if_no_timezone sexp =
      try
        match sexp with
        | Sexp.List [Sexp.Atom date; Sexp.Atom ofday; Sexp.Atom tz] ->
          of_date_ofday ~zone:(Zone.find_exn tz)
            (Date.of_string date) (Ofday.of_string ofday)
        (* This is actually where the output of [sexp_of_t] is handled, since that's e.g.
           (2015-07-06 09:09:44.787988+01:00). *)
        | Sexp.List [Sexp.Atom date; Sexp.Atom ofday_and_possibly_zone] ->
          of_string_gen ~if_no_timezone (date ^ " " ^ ofday_and_possibly_zone)
        | Sexp.Atom datetime ->
          of_string_gen ~if_no_timezone datetime
        | _ -> of_sexp_error "Time.t_of_sexp" sexp
      with
      | Of_sexp_error _ as e -> raise e
      | e -> of_sexp_error (sprintf "Time.t_of_sexp: %s" (Exn.to_string e)) sexp
    ;;

    let t_of_sexp sexp =
      t_of_sexp_gen sexp ~if_no_timezone:(`Use_this_one !sexp_zone)
    let t_of_sexp_abs sexp =
      t_of_sexp_gen sexp ~if_no_timezone:`Fail

    let sexp_of_t_abs ~zone t =
      Sexp.List (List.map (to_string_abs_parts ~zone t) ~f:(fun s -> Sexp.Atom s))
    ;;

    let sexp_of_t t = sexp_of_t_abs ~zone:!sexp_zone t

    let%test_unit _ =
      let unzoned_sexp = Sexp.of_string "(2015-07-03 16:27:00)" in
      set_sexp_zone Zone.utc;
      let in_utc = t_of_sexp unzoned_sexp in
      set_sexp_zone (Zone.of_utc_offset ~hours:8);
      let in_plus8 = t_of_sexp unzoned_sexp in
      set_sexp_zone Zone.local;
      [%test_result: Span.t]
        ~expect:(Span.of_hr 8.)
        (diff in_utc in_plus8)

    module C = struct
      type t = T.t [@@deriving bin_io]

      let compare = compare

      type comparator_witness = T.comparator_witness

      let comparator = T.comparator

      (* In 108.06a and earlier, times in sexps of Maps and Sets were raw floats.  From
         108.07 through 109.13, the output format remained raw as before, but both the raw
         and pretty format were accepted as input.  From 109.14 on, the output format was
         changed from raw to pretty, while continuing to accept both formats.  Once we
         believe most programs are beyond 109.14, we will switch the input format to no
         longer accept raw. *)
      let sexp_of_t = sexp_of_t

      let t_of_sexp sexp =
        match Option.try_with (fun () -> T.of_float (Float.t_of_sexp sexp)) with
        | Some t -> t
        | None -> t_of_sexp sexp
      ;;
    end

    let is_earlier t1 ~than:t2 = t1 <. t2
    let is_later   t1 ~than:t2 = t1 >. t2

    module Map = Map.Make_binable_using_comparator (C)
    module Set = Set.Make_binable_using_comparator (C)

    let%test _ =
      Set.equal (Set.of_list [epoch])
        (Set.t_of_sexp (Sexp.List [Float.sexp_of_t (to_float epoch)]))
    ;;

    include Pretty_printer.Register (struct
        type nonrec t = t
        let to_string = to_string
        let module_name = "Core.Std.Time"
      end)

    let of_localized_string ~zone str =
      try
        match String.lsplit2 str ~on:' ' with
        | None -> invalid_arg (sprintf "no space in date_ofday string: %s" str)
        | Some (date,time) ->
          let date  = Date.of_string date in
          let ofday = Ofday.of_string time in
          of_date_ofday ~zone date ofday
      with e ->
        Exn.reraise e "Time.of_localstring"
    ;;

    let next_multiple ?(can_equal_after = false) ~base ~after ~interval () =
      if Span.(<=) interval Span.zero
      then failwiths "Time.next_multiple got nonpositive interval" interval
             [%sexp_of: Span.t];
      let base_to_after = diff after base in
      if Span.(<) base_to_after Span.zero
      then base (* [after < base], choose [k = 0]. *)
      else begin
        let next =
          add base
            (Span.scale interval
               (Float.round ~dir:`Down (Span.(//) base_to_after interval)))
        in
        if next > after || (can_equal_after && next = after)
        then next
        else add next interval
      end
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
            (Date.create_exn ~y:1970 ~m:Core_kernel.Month.Jan ~d:1)
            (Ofday.create ())

        let%test_unit _ =
          [%test_result: t] ~expect:unix_epoch_t
            (of_tm ~zone:Zone.local (Core_unix.localtime 0.))

        let%test_unit _ =
          [%test_result: t] ~expect:unix_epoch_t
            (of_tm ~zone:Zone.utc (Core_unix.gmtime 0.))
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
            (Date.create_exn ~y:1970 ~m:Core_kernel.Month.Jan ~d:1)
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
  end

  let%test_module "Time.V1" = (module struct
    let%test_module "Time.V1 functor application" = (module Core_kernel.Stable_unit_test.Make (struct
      include V1
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
        ] @ if Int.(Core_sys.c_int_size () < 64) then [] else [
          time ~y:2222 ~m:Month.Nov ~d:22 (Ofday.create ~hr:17 ~min:17 ~sec:17 ()),
          "(2222-11-22 17:17:17.000000-05:00)",
          "\000\000\208\230\204\186\253\065";
        ]
    end))

    (* test that t_of_sexp accepts sexps qualified with time zones in two formats *)
    let%test_unit _ =
      ignore (V1.t_of_sexp (Sexp.of_string "(2012-04-09 12:00:00.000000-04:00:00)"))

    let%test_unit _ =
      ignore
        (V1.t_of_sexp (Sexp.of_string "(2012-04-09 12:00:00.000000 America/New_York)"))
  end)

  module With_utc_sexp = struct
    module V1 = struct
      include V1

      let sexp_of_t t = sexp_of_t_abs t ~zone:Zone.utc
    end
  end

  module With_t_of_sexp_abs = struct
    module V1 = struct
      include V1
      let t_of_sexp = t_of_sexp_abs
    end
  end
end

include Stable.V1

let%test_module "Time robustly compare" = (module struct
  let%test _ = of_float 0.0 =. of_float 0.000_000_99
  let%test _ = of_float 0.0 <. of_float 0.000_001_1

  let%test_unit _ =
    for i = 0 to 100 do
      let time = of_float (Float.of_int i /. 17.) in
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
