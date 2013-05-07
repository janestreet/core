open Std_internal
module Unix = Core_unix

module Stable = struct
  module V1 = struct
    (* this is a recreation of the algorithm used internally by the linux kernel
       (supposedly invented by Gauss).  In this case it is used to produce the number
       of seconds since 1970-01-01 00:00:00 using epoch time semantics (86,400 seconds
       per day) *)
    let utc_mktime ~year ~month ~day ~hour ~min ~sec ~ms ~us =
      (* move February to the conceptual end of the ordering - 1..12 -> 11,12,1..10 -
         because it carries the leap day.  The months are 0 indexed for this calculation,
         so 1 is February. *)
      let shuffle_year_month year month =
        let month = month - 2 in
        if month <= 0 then (year - 1, month + 12) else (year,month)
      in
      let hour       = Float.of_int hour in
      let min        = Float.of_int min in
      let sec        = Float.of_int sec in
      let year,month = shuffle_year_month year month in
      let days       = year / 4 - year / 100 + year / 400 + 367 * month / 12 + day in
      let days       = Float.of_int days +. 365. *. Float.of_int year -. 719499. in
      let hours      = 24. *. days +. hour in
      let mins       = 60. *. hours +. min in
      60. *. mins +. sec +. (Float.of_int ms /. 1000.)
      +. (Float.of_int us /. 1000. /. 1000.)
    ;;

    (* IF THIS REPRESENTATION EVER CHANGES, ENSURE THAT EITHER
       (1) all values serialize the same way in both representations, or
       (2) you add a new Time version to stable.ml *)
    include Time_internal

    let to_epoch t = T.to_float t

    module Epoch_cache = struct
      type t = {
        zone      : Zone.t;
        day_start : float;
        day_end   : float;
        date      : Date.t
      } with sexp
    end

    let of_epoch_internal zone time (* shifted epoch for the time zone for conversion *) =
      let parts  = Float.modf time in
      let sec    = Float.Parts.integral parts in
      let subsec = Float.Parts.fractional parts in
      let sec,subsec =
        if subsec < 0. then (sec -. 1., 1. +. subsec)
        else (sec, subsec)
      in
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

    (* A thin caching layer over the actual of_epoch (of_epoch_internal just above) used
       only to gain some speed when we translate the same time/date over and over again *)
    let of_epoch =
      let cache = ref (fst (of_epoch_internal Zone.utc (to_epoch (T.now ())))) in
      (fun zone unshifted ->
        let time = Zone.shift_epoch_time zone `UTC unshifted in
        let {Epoch_cache.zone = z; day_start = s; day_end = e; date = date} = !cache in
        if phys_equal zone z && time >= s && time < e then (
          (date, Ofday.of_span_since_start_of_day (Span.of_sec (time -. s))))
        else begin
          let (new_cache,r) = of_epoch_internal zone time in
          cache := new_cache;
          r
        end)
    ;;

    let to_date_ofday time zone =
      try
        of_epoch zone (to_epoch time)
      with
      | Unix.Unix_error(_, "gmtime", _) -> raise (Invalid_argument "Time.to_date_ofday")
    ;;

    let of_date_ofday zone date ofday =
      let module P = Span.Parts in
      let parts = Span.to_parts (Ofday.to_span_since_start_of_day ofday) in
      let time =
        let epoch =
          utc_mktime ~year:date.Date.y ~month:(Month.to_int date.Date.m)
            ~day:date.Date.d ~hour:parts.P.hr ~min:parts.P.min ~sec:parts.P.sec
            ~ms:parts.P.ms ~us:parts.P.us
        in
        Zone.shift_epoch_time zone `Local epoch
      in
      T.of_float time
    ;;

    let to_local_date_ofday t          = to_date_ofday t (Zone.machine_zone ())
    let of_local_date_ofday date ofday = of_date_ofday (Zone.machine_zone ()) date ofday
    let to_date t zone                 = fst (to_date_ofday t zone)
    let to_ofday t zone                = snd (to_date_ofday t zone)
    let to_local_date t                = fst (to_local_date_ofday t)
    let to_local_ofday t               = snd (to_local_date_ofday t)

    let convert ~from_tz ~to_tz date ofday =
      let start_time = T.to_float (of_date_ofday from_tz date ofday) in
      of_epoch to_tz start_time

    let utc_offset ?(zone=Zone.machine_zone ()) t =
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

    let to_string_abs_parts ?(zone=Zone.machine_zone ()) time =
      let date, ofday   = to_date_ofday time zone in
      let offset_string = offset_string time ~zone in
      [ Date.to_string date;
        String.concat ~sep:"" [ Ofday.to_string ofday; offset_string ]
      ]
    ;;

    let to_string_abs ?zone time =
      String.concat ~sep:" " (to_string_abs_parts ?zone time)
    ;;

    let to_string_trimmed t =
      let date, sec = to_local_date_ofday t in
      (Date.to_string date) ^ " " ^ (Ofday.to_string_trimmed sec)
    ;;

    let to_sec_string t =
      let date, sec = to_local_date_ofday t in
      (Date.to_string date) ^ " " ^ (Ofday.to_sec_string sec)
    ;;

    let to_filename_string t =
      let date, ofday = to_local_date_ofday t in
      (Date.to_string date) ^ "_" ^
        (String.tr ~target:':' ~replacement:'-' (Ofday.to_string ofday))
    ;;

    let to_string_fix_proto utc t =
      let date, sec =
        match utc with
        | `Utc -> to_date_ofday t Zone.utc
        | `Local -> to_local_date_ofday t
      in
      (Date.to_string_iso8601_basic date) ^ "-" ^ (Ofday.to_millisec_string sec)
    ;;

    let of_string_fix_proto utc str =
      try
        let expect_length = 21 in  (* = 8 + 1 + 12 *)
        let expect_dash = 8 in
        if str.[expect_dash] <> '-' then
          failwithf "no dash in position %d" expect_dash ();
        let of_date_ofday =
          match utc with
          | `Utc -> of_date_ofday Zone.utc
          | `Local -> of_local_date_ofday
        in
        if Int.(>) (String.length str) expect_length then
          failwithf "input too long" ();
        of_date_ofday
          (Date.of_string_iso8601_basic str ~pos:0)
          (Ofday.of_string_iso8601_extended str ~pos:(expect_dash + 1))
      with exn ->
        invalid_argf "Time.of_string_fix_proto %s: %s" str (Exn.to_string exn) ()
    ;;

    let of_filename_string s =
      try
        match String.lsplit2 s ~on:'_' with
        | None -> failwith "no space in filename string"
        | Some (date, ofday) ->
          let date = Date.of_string date in
          let ofday = String.tr ~target:'-' ~replacement:':' ofday in
          let ofday = Ofday.of_string ofday in
          of_local_date_ofday date ofday
      with
      | exn ->
        invalid_argf "Time.of_filename_string (%s): %s" s (Exn.to_string exn) ()
    ;;

    let format t s = Unix.strftime (to_tm t) s

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
      let first_guess_date = to_date t zone in
      let first_guess      = of_date_ofday zone first_guess_date ofday in
      let cmp, increment =
        match before_or_after with
        | `Last_before_or_at -> T.(<=), (-1)
        | `First_after_or_at -> T.(>=), 1
      in
      if cmp first_guess t
      then first_guess
      else of_date_ofday zone (Date.add_days first_guess_date increment) ofday
    ;;

    let epoch = T.of_float 0.0

    (* There are a number of things that would be shadowed by this include because of the
       scope of Constrained_float.  These need to be defined below.  It's a an unfortunate
       situation because we would like to say include T, without shadowing. *)
    include T

    let to_string_deprecated t =
      let date, sec = to_local_date_ofday t in
      String.concat [Date.to_string date; " "; Ofday.to_string sec]
    ;;

    let to_string t = to_string_abs t

    exception Time_of_string of string * Exn.t with sexp
    exception Time_string_not_absolute of string with sexp
    let of_string_gen ~require_absolute s =
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
            if Char.(=) ofday.[String.length ofday - 1] 'Z' then
              (String.sub ofday ~pos:0 ~len:(String.length ofday - 1)), Some 0.
            else begin
              match String.lsplit2 ~on:'+' ofday with
              | Some (l,r) ->
                assert (Char.(=) r.[1] ':' || Char.(=) r.[2] ':');
                l, Some (ofday_to_sec (Ofday.of_string r))
              | None ->
                match String.lsplit2 ~on:'-' ofday with
                | Some (l,r) ->
                  assert (Char.(=) r.[1] ':' || Char.(=) r.[2] ':');
                  l, Some ((-1.) *. (ofday_to_sec (Ofday.of_string r)))
                | None       -> ofday, None
            end
        in
        let date  = Date.of_string date in
        let ofday = Ofday.of_string ofday in
        match tz with
        | Some tz -> of_date_ofday (Zone.find_exn tz) date ofday
        | None ->
          match utc_offset with
          | None            ->
            if require_absolute then raise (Time_string_not_absolute s);
            of_local_date_ofday date ofday
          | Some utc_offset ->
            of_float (to_float (of_date_ofday Zone.utc date ofday) -. utc_offset)
      with
      | e -> raise (Time_of_string (s,e))
    ;;

    let of_string_abs s = of_string_gen ~require_absolute:true s
    let of_string s     = of_string_gen ~require_absolute:false s

    let t_of_sexp_gen sexp of_string =
      try
        match sexp with
        | Sexp.List [Sexp.Atom date; Sexp.Atom ofday; Sexp.Atom tz] ->
          of_date_ofday (Zone.find_exn tz) (Date.of_string date) (Ofday.of_string ofday)
        | Sexp.List [Sexp.Atom date; Sexp.Atom ofday] ->
          of_string (date ^ " " ^ ofday)
        | Sexp.Atom datetime ->
          of_string datetime
        | _ -> of_sexp_error "Time.t_of_sexp" sexp
      with
      | Of_sexp_error _ as e -> raise e
      | e -> of_sexp_error (sprintf "Time.t_of_sexp: %s" (Exn.to_string e)) sexp
    ;;

    let t_of_sexp     sexp = t_of_sexp_gen sexp of_string
    let t_of_sexp_abs sexp = t_of_sexp_gen sexp of_string_abs

    let sexp_of_t_with_zone ?zone t =
      Sexp.List (List.map (to_string_abs_parts ?zone t) ~f:(fun s -> Sexp.Atom s))
    ;;

    let sexp_of_t t = sexp_of_t_with_zone t

    module C = struct
      type t = T.t with bin_io

      let compare = compare

      type comparator = T.comparator

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

    module Map = Core_map.Make_binable_using_comparator (C)
    module Set = Core_set.Make_binable_using_comparator (C)

    TEST =
      Set.equal (Set.of_list [epoch])
        (Set.t_of_sexp (Sexp.List [Float.sexp_of_t (to_float epoch)]))
    ;;

    include Pretty_printer.Register (struct
      type nonrec t = t
      let to_string = to_string
      let module_name = "Core.Std.Time"
    end)

    let to_localized_string time zone =
      let date,ofday = to_date_ofday time zone in
      String.concat [Date.to_string date; " "; Ofday.to_string ofday]
    ;;

    let of_localized_string zone str =
      try
        match String.lsplit2 str ~on:' ' with
        | None -> invalid_arg (sprintf "no space in date_ofday string: %s" str)
        | Some (date,time) ->
          let date  = Date.of_string date in
          let ofday = Ofday.of_string time in
          of_date_ofday zone date ofday
      with e ->
        Exn.reraise e "Time.of_localstring"
    ;;

    let next_multiple ~base ~after ~interval =
      if Span.(<=) interval Span.zero then
        failwiths "Time.next_multiple got nonpositive interval" interval <:sexp_of< Span.t >>;
      let base_to_after = diff after base in
      if Span.(<) base_to_after Span.zero
      then base (* [after < base], choose [k = 0]. *)
      else begin
        let next =
          add base
            (Span.scale interval
               (Float.round ~dir:`Down (Span.(//) base_to_after interval)))
        in
        if next > after
        then next
        else add next interval
      end
    ;;

    TEST_UNIT =
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
            let actual_next_multiple = next_multiple ~base ~after ~interval in
            let expected_next_multiple = expected_next_multiple ~base ~after ~interval in
            let relativize time = diff time base in
            let times_are_close t1 t2 = Float.(<) (Float.abs (Span.to_us (diff t1 t2))) 1. in
            if not (times_are_close actual_next_multiple expected_next_multiple) then
              failwiths "Time.next_multiple" (since_base, interval,
                                              relativize expected_next_multiple,
                                              relativize actual_next_multiple)
                (<:sexp_of< float * Span.t * Span.t * Span.t >>))
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
  end

  TEST_MODULE "Time.V1" = struct
    TEST_MODULE "Time.V1 functor application" = Stable_unit_test.Make (struct
      include V1
      let zone = Zone.find_office `nyc
      let sexp_of_t t = sexp_of_t_with_zone ~zone t

      let tests =
        let time ~y ~m ~d ofday =
          of_date_ofday zone (Date.create_exn ~y ~m ~d) ofday
        in
        [ time ~y:2012 ~m:Month.Apr ~d:9 (Ofday.create ~hr:12 ()),
          "(2012-04-09 12:00:00.000000-04:00)",
          "\000\000\000\224\193\224\211\065";
          time ~y:1985 ~m:Month.Jun ~d:5 (Ofday.create ~hr:5 ~min:25 ()),
          "(1985-06-05 05:25:00.000000-04:00)",
          "\000\000\000\108\039\004\189\065";
        ] @ if Int.(Sys.c_int_size () < 64) then [] else [
          time ~y:2222 ~m:Month.Nov ~d:22 (Ofday.create ~hr:17 ~min:17 ~sec:17 ()),
          "(2222-11-22 17:17:17.000000-05:00)",
          "\000\000\208\230\204\186\253\065";
        ]
    end)

    (* test that t_of_sexp accepts sexps qualified with time zones in two formats *)
    TEST_UNIT =
      ignore (V1.t_of_sexp (Sexp.of_string "(2012-04-09 12:00:00.000000-04:00:00)"))

    TEST_UNIT =
      ignore
        (V1.t_of_sexp (Sexp.of_string "(2012-04-09 12:00:00.000000 America/New_York)"))
  end
end

include Stable.V1

TEST_MODULE "Time robustly compare" = struct
  TEST = of_float 0.0 =. of_float 0.000_000_99
  TEST = of_float 0.0 <. of_float 0.000_001_1

  TEST_UNIT =
    for i = 0 to 100 do
      let time = of_float (Float.of_int i /. 17.) in
      assert ((=.) time (sexp_of_t time |! t_of_sexp))
    done
end
