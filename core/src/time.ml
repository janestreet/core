(* See time_float.ml for the primary instantiation of this functor that is visible outside
   of Core as Time (see core.ml). *)
open! Import
open Std_internal
open! Int.Replace_polymorphic_compare
include Time_intf

module Make (Time0 : Time0_intf.S) = struct
  module Time0 = Time0
  include Time0

  let epoch = of_span_since_epoch Span.zero
  let is_earlier t1 ~than:t2 = t1 <. t2
  let is_later t1 ~than:t2 = t1 >. t2

  module Zone : sig
    include Time_intf.Zone with module Time := Time0
  end = struct
    include Zone

    let of_span_in_seconds span_in_seconds =
      (* NB. no actual rounding or exns can occur here *)
      Time_in_seconds.Span.to_int63_seconds_round_down_exn span_in_seconds
      |> Time0.Span.of_int63_seconds
    ;;

    let of_time_in_seconds time_in_seconds =
      Time_in_seconds.to_span_since_epoch time_in_seconds
      (* NB. no actual rounding or exns can occur here *)
      |> Time_in_seconds.Span.to_int63_seconds_round_down_exn
      |> Time0.Span.of_int63_seconds
      |> Time0.of_span_since_epoch
    ;;

    let to_time_in_seconds_round_down_exn time =
      Time0.to_span_since_epoch time
      |> Time0.Span.to_int63_seconds_round_down_exn
      |> Time_in_seconds.Span.of_int63_seconds
      |> Time_in_seconds.of_span_since_epoch
    ;;

    let to_date_and_ofday_in_seconds_round_down_exn relative =
      Time0.Date_and_ofday.to_synthetic_span_since_epoch relative
      |> Time0.Span.to_int63_seconds_round_down_exn
      |> Time_in_seconds.Span.of_int63_seconds
      |> Time_in_seconds.Date_and_ofday.of_synthetic_span_since_epoch
    ;;

    let index t time = index t (to_time_in_seconds_round_down_exn time)

    let index_of_date_and_ofday t relative =
      index_of_date_and_ofday t (to_date_and_ofday_in_seconds_round_down_exn relative)
    ;;

    let index_offset_from_utc_exn t index =
      of_span_in_seconds (index_offset_from_utc_exn t index)
    ;;

    let index_prev_clock_shift_time_exn t index =
      of_time_in_seconds (index_prev_clock_shift_time_exn t index)
    ;;

    let index_next_clock_shift_time_exn t index =
      of_time_in_seconds (index_next_clock_shift_time_exn t index)
    ;;

    let index_prev_clock_shift_amount_exn t index =
      of_span_in_seconds (index_prev_clock_shift_amount_exn t index)
    ;;

    let index_next_clock_shift_amount_exn t index =
      of_span_in_seconds (index_next_clock_shift_amount_exn t index)
    ;;

    let abbreviation t time =
      (* no exn because [index] always returns a valid index *)
      index_abbreviation_exn t (index t time)
    ;;

    let index_prev_clock_shift t index =
      match index_has_prev_clock_shift t index with
      | false -> None
      | true ->
        Some
          ( index_prev_clock_shift_time_exn t index
          , index_prev_clock_shift_amount_exn t index )
    ;;

    let index_next_clock_shift t index = index_prev_clock_shift t (Index.next index)
    let prev_clock_shift t ~at_or_before:time = index_prev_clock_shift t (index t time)
    let next_clock_shift t ~strictly_after:time = index_next_clock_shift t (index t time)

    let date_and_ofday_of_absolute_time t time =
      let index = index t time in
      (* no exn because [index] always returns a valid index *)
      let offset_from_utc = index_offset_from_utc_exn t index in
      Time0.Date_and_ofday.of_absolute time ~offset_from_utc
    ;;

    let absolute_time_of_date_and_ofday t relative =
      let index = index_of_date_and_ofday t relative in
      (* no exn because [index_of_date_and_ofday] always returns a valid index *)
      let offset_from_utc = index_offset_from_utc_exn t index in
      Time0.Date_and_ofday.to_absolute relative ~offset_from_utc
    ;;
  end

  let abs_diff t1 t2 = Span.abs (diff t1 t2)

  let of_date_ofday ~zone date ofday =
    let relative = Date_and_ofday.of_date_ofday date ofday in
    Zone.absolute_time_of_date_and_ofday zone relative
  ;;

  let of_date_ofday_precise date ofday ~zone =
    (* We assume that there will be only one zone shift within a given local day.  *)
    let start_of_day = of_date_ofday ~zone date Ofday.start_of_day in
    let proposed_time = add start_of_day (Ofday.to_span_since_start_of_day ofday) in
    match Zone.next_clock_shift zone ~strictly_after:start_of_day with
    | None -> `Once proposed_time
    | Some (shift_start, shift_amount) ->
      let shift_backwards = Span.(shift_amount < zero) in
      (* start and end of the "problematic region" *)
      let s, e =
        if shift_backwards
        then add shift_start shift_amount, shift_start
        else shift_start, add shift_start shift_amount
      in
      if proposed_time < s
      then `Once proposed_time
      else if s <= proposed_time && proposed_time < e
      then
        if shift_backwards
        then `Twice (proposed_time, sub proposed_time shift_amount)
        else `Never shift_start
      else `Once (sub proposed_time shift_amount)
  ;;

  module Date_cache = struct
    type t =
      { mutable zone : Zone.t
      ; mutable cache_start_incl : Time0.t
      ; mutable cache_until_excl : Time0.t
      ; mutable effective_day_start : Time0.t
      ; mutable date : Date0.t
      }
  end

  let date_cache : Date_cache.t =
    { zone = Zone.utc
    ; cache_start_incl = epoch
    ; cache_until_excl = epoch
    ; effective_day_start = epoch
    ; date = Date0.unix_epoch
    }
  ;;

  let reset_date_cache () =
    date_cache.zone <- Zone.utc;
    date_cache.cache_start_incl <- epoch;
    date_cache.cache_until_excl <- epoch;
    date_cache.effective_day_start <- epoch;
    date_cache.date <- Date0.unix_epoch
  ;;

  let is_in_cache time ~zone =
    phys_equal zone date_cache.zone
    && Time0.( >= ) time date_cache.cache_start_incl
    && Time0.( < ) time date_cache.cache_until_excl
  ;;

  let set_date_cache time ~zone =
    match is_in_cache time ~zone with
    | true -> ()
    | false ->
      let index = Zone.index zone time in
      (* no exn because [Zone.index] always returns a valid index *)
      let offset_from_utc = Zone.index_offset_from_utc_exn zone index in
      let rel = Date_and_ofday.of_absolute time ~offset_from_utc in
      let date = Date_and_ofday.to_date rel in
      let span = Date_and_ofday.to_ofday rel |> Ofday.to_span_since_start_of_day in
      let effective_day_start =
        Time0.sub (Date_and_ofday.to_absolute rel ~offset_from_utc) span
      in
      let effective_day_until = Time0.add effective_day_start Span.day in
      let cache_start_incl =
        match Zone.index_has_prev_clock_shift zone index with
        | false -> effective_day_start
        | true ->
          effective_day_start
          |> Time0.max (Zone.index_prev_clock_shift_time_exn zone index)
      in
      let cache_until_excl =
        match Zone.index_has_next_clock_shift zone index with
        | false -> effective_day_until
        | true ->
          effective_day_until
          |> Time0.min (Zone.index_next_clock_shift_time_exn zone index)
      in
      date_cache.zone <- zone;
      date_cache.cache_start_incl <- cache_start_incl;
      date_cache.cache_until_excl <- cache_until_excl;
      date_cache.effective_day_start <- effective_day_start;
      date_cache.date <- date
  ;;

  let to_date time ~zone =
    set_date_cache time ~zone;
    date_cache.date
  ;;

  let end_of_day = Ofday.prev Ofday.start_of_next_day |> Option.value_exn ~here:[%here]

  let to_ofday time ~zone =
    set_date_cache time ~zone;
    let of_day =
      Time0.diff time date_cache.effective_day_start
      |> Ofday.of_span_since_start_of_day_exn
    in
    if Ofday.equal of_day Ofday.start_of_next_day then end_of_day else of_day
  ;;

  let to_date_ofday time ~zone = to_date time ~zone, to_ofday time ~zone

  (* The correctness of this algorithm (interface, even) depends on the fact that
     timezone shifts aren't too close together (as in, it can't simultaneously be the
     case that a timezone shift of X hours occurred less than X hours ago, *and*
     a timezone shift of Y hours will occur in less than Y hours' time) *)
  let to_date_ofday_precise time ~zone =
    let date, ofday = to_date_ofday time ~zone in
    let clock_shift_after = Zone.next_clock_shift zone ~strictly_after:time in
    let clock_shift_before_or_at = Zone.prev_clock_shift zone ~at_or_before:time in
    let also_skipped_earlier amount =
      (* Using [date] and raising on [None] here is OK on the assumption that clock
         shifts can't cross date boundaries. This is true in all cases I've ever heard
         of (and [of_date_ofday_precise] would need revisiting if it turned out to be
         false) *)
      match Ofday.sub ofday amount with
      | Some ofday -> `Also_skipped (date, ofday)
      | None ->
        raise_s
          [%message
            "Time.to_date_ofday_precise"
              ~span_since_epoch:(to_span_since_epoch time : Span.t)
              (zone : Zone.t)]
    in
    let ambiguity =
      (* Edge cases: the instant of transition belongs to the new zone regime. So if the
         clock moved by an hour exactly one hour ago, there's no ambiguity, because the
         hour-ago time belongs to the same regime as you, and conversely, if the clock
         will move by an hour in an hours' time, there *is* ambiguity. Hence [>.] for
         the first case and [<=.] for the second. *)
      match clock_shift_before_or_at, clock_shift_after with
      | Some (start, amount), _ when add start (Span.abs amount) >. time ->
        (* clock shifted recently *)
        if Span.(amount > zero)
        then
          (* clock shifted forward recently: we skipped a time *)
          also_skipped_earlier amount
        else (
          (* clock shifted back recently: this date/ofday already happened *)
          assert (Span.(amount < zero));
          `Also_at (sub time (Span.abs amount)))
      | _, Some (start, amount) when sub start (Span.abs amount) <=. time ->
        (* clock is about to shift *)
        if Span.(amount > zero)
        then (* clock about to shift forward: no effect *)
          `Only
        else (
          (* clock about to shift back: this date/ofday will be repeated *)
          assert (Span.(amount < zero));
          `Also_at (add time (Span.abs amount)))
      | _ -> `Only
    in
    date, ofday, ambiguity
  ;;

  let convert ~from_tz ~to_tz date ofday =
    let start_time = of_date_ofday ~zone:from_tz date ofday in
    to_date_ofday ~zone:to_tz start_time
  ;;

  let utc_offset t ~zone =
    let utc_epoch = Zone.date_and_ofday_of_absolute_time zone t in
    Span.( - )
      (Date_and_ofday.to_synthetic_span_since_epoch utc_epoch)
      (to_span_since_epoch t)
  ;;

  let offset_string time ~zone =
    let utc_offset = utc_offset time ~zone in
    let is_utc = Span.( = ) utc_offset Span.zero in
    if is_utc
    then "Z"
    else
      String.concat
        [ (if Span.( < ) utc_offset Span.zero then "-" else "+")
        ; Ofday.to_string_trimmed
            (Ofday.of_span_since_start_of_day_exn (Span.abs utc_offset))
        ]
  ;;

  let to_string_abs_parts time ~zone =
    let date, ofday = to_date_ofday time ~zone in
    let offset_string = offset_string time ~zone in
    [ Date0.to_string date
    ; String.concat ~sep:"" [ Ofday.to_string ofday; offset_string ]
    ]
  ;;

  let to_string_abs_trimmed time ~zone =
    let date, ofday = to_date_ofday time ~zone in
    let offset_string = offset_string time ~zone in
    String.concat
      ~sep:" "
      [ Date0.to_string date; Ofday.to_string_trimmed ofday ^ offset_string ]
  ;;

  let to_string_abs time ~zone = String.concat ~sep:" " (to_string_abs_parts ~zone time)
  let to_string_utc t = to_string_abs t ~zone:Zone.utc
  let to_string = to_string_utc

  let to_string_iso8601_basic time ~zone =
    String.concat ~sep:"T" (to_string_abs_parts ~zone time)
  ;;

  let to_string_trimmed t ~zone =
    let date, sec = to_date_ofday ~zone t in
    Date0.to_string date ^ " " ^ Ofday.to_string_trimmed sec
  ;;

  let to_sec_string t ~zone =
    let date, sec = to_date_ofday ~zone t in
    Date0.to_string date ^ " " ^ Ofday.to_sec_string sec
  ;;

  let to_sec_string_with_zone t ~zone = to_sec_string t ~zone ^ offset_string t ~zone

  let to_filename_string t ~zone =
    let date, ofday = to_date_ofday ~zone t in
    Date0.to_string date
    ^ "_"
    ^ String.tr ~target:':' ~replacement:'-' (Ofday.to_string ofday)
  ;;

  let of_filename_string s ~zone =
    try
      match String.lsplit2 s ~on:'_' with
      | None -> failwith "no space in filename string"
      | Some (date, ofday) ->
        let date = Date0.of_string date in
        let ofday = String.tr ~target:'-' ~replacement:':' ofday in
        let ofday = Ofday.of_string ofday in
        of_date_ofday date ofday ~zone
    with
    | exn -> invalid_argf "Time.of_filename_string (%s): %s" s (Exn.to_string exn) ()
  ;;

  let of_localized_string ~zone str =
    try
      match String.lsplit2 str ~on:' ' with
      | None -> invalid_arg (sprintf "no space in date_ofday string: %s" str)
      | Some (date, time) ->
        let date = Date0.of_string date in
        let ofday = Ofday.of_string time in
        of_date_ofday ~zone date ofday
    with
    | e -> Exn.reraise e "Time.of_localized_string"
  ;;

  let occurrence before_or_after t ~ofday ~zone =
    let first_guess_date = to_date t ~zone in
    let first_guess = of_date_ofday ~zone first_guess_date ofday in
    let cmp, increment =
      match before_or_after with
      | `Last_before_or_at -> ( <= ), -1
      | `First_after_or_at -> ( >= ), 1
    in
    if cmp first_guess t
    then first_guess
    else of_date_ofday ~zone (Date0.add_days first_guess_date increment) ofday
  ;;

  let ensure_colon_in_offset offset =
    let offset_length = String.length offset in
    if Int.( <= ) offset_length 2
       && Char.is_digit offset.[0]
       && Char.is_digit offset.[offset_length - 1]
    then offset ^ ":00"
    else if Char.( = ) offset.[1] ':' || Char.( = ) offset.[2] ':'
    then offset
    else if Int.( < ) offset_length 3 || Int.( > ) offset_length 4
    then failwithf "invalid offset %s" offset ()
    else
      String.concat
        [ String.slice offset 0 (offset_length - 2)
        ; ":"
        ; String.slice offset (offset_length - 2) offset_length
        ]
  ;;

  exception Time_of_string of string * Exn.t [@@deriving sexp]

  let of_string_gen ~default_zone ~find_zone s =
    try
      let date, ofday, tz =
        match String.split s ~on:' ' with
        | [ day; month; year; ofday ] ->
          String.concat [ day; " "; month; " "; year ], ofday, None
        | [ date; ofday; tz ] -> date, ofday, Some tz
        | [ date; ofday ] -> date, ofday, None
        | [ s ] ->
          (match String.rsplit2 ~on:'T' s with
           | Some (date, ofday) -> date, ofday, None
           | None -> failwith "no spaces or T found")
        | _ -> failwith "too many spaces"
      in
      let ofday_to_sec od = Span.to_sec (Ofday.to_span_since_start_of_day od) in
      let ofday, utc_offset =
        match tz with
        | Some _ -> ofday, None
        | None ->
          if Char.( = ) ofday.[String.length ofday - 1] 'Z'
          then String.sub ofday ~pos:0 ~len:(String.length ofday - 1), Some 0.
          else (
            match String.lsplit2 ~on:'+' ofday with
            | Some (l, r) ->
              l, Some (ofday_to_sec (Ofday.of_string (ensure_colon_in_offset r)))
            | None ->
              (match String.lsplit2 ~on:'-' ofday with
               | Some (l, r) ->
                 l, Some (-1. *. ofday_to_sec (Ofday.of_string (ensure_colon_in_offset r)))
               | None -> ofday, None))
      in
      let date = Date0.of_string date in
      let ofday = Ofday.of_string ofday in
      match tz with
      | Some tz -> of_date_ofday ~zone:(find_zone tz) date ofday
      | None ->
        (match utc_offset with
         | None ->
           let zone = default_zone () in
           of_date_ofday ~zone date ofday
         | Some utc_offset ->
           let utc_t = of_date_ofday ~zone:Zone.utc date ofday in
           sub utc_t (Span.of_sec utc_offset))
    with
    | e -> raise (Time_of_string (s, e))
  ;;

  let of_string_with_utc_offset s =
    let default_zone () = raise_s [%message "time has no time zone or UTC offset" s] in
    let find_zone zone_name =
      failwithf "unable to lookup Zone %s.  Try using Core.Time.of_string" zone_name ()
    in
    of_string_gen ~default_zone ~find_zone s
  ;;

  let of_string = of_string_with_utc_offset

  let quickcheck_shrinker =
    Quickcheck.Shrinker.map
      Span.quickcheck_shrinker
      ~f:of_span_since_epoch
      ~f_inverse:to_span_since_epoch
  ;;

  let quickcheck_observer =
    Quickcheck.Observer.unmap Span.quickcheck_observer ~f:to_span_since_epoch
  ;;

  let quickcheck_generator =
    Quickcheck.Generator.map Span.quickcheck_generator ~f:of_span_since_epoch
  ;;

  let gen_incl lo hi =
    Span.gen_incl (to_span_since_epoch lo) (to_span_since_epoch hi)
    |> Quickcheck.Generator.map ~f:of_span_since_epoch
  ;;

  let gen_uniform_incl lo hi =
    Span.gen_uniform_incl (to_span_since_epoch lo) (to_span_since_epoch hi)
    |> Quickcheck.Generator.map ~f:of_span_since_epoch
  ;;
end
