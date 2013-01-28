open Common
open Std_internal
module Unix = Core_unix

(* this is a recreation of the algorithm used internally by the linux kernel
  (supposedly invented by Gauss).  In this case it is used to produce the number
  of seconds since 1970-01-01 00:00:00 using epoch time semantics (86,400 seconds
  per day *)
let utc_mktime ~year ~month ~day ~hour ~min ~sec ~ms ~us =
  (* move February to the conceptual end of the ordering - 1..12 -> 11,12,1..10 -
    because it carries the leap day.  The months are 0 indexed for this calculation,
    so 1 is February. *)
  let shuffle_year_month year month =
    let month = month - 2 in
    if month <= 0 then (year - 1, month + 12) else (year,month)
  in
  let hour       = float_of_int hour in
  let min        = float_of_int min in
  let sec        = float_of_int sec in
  let year,month = shuffle_year_month year month in
  let days       = year / 4 - year / 100 + year / 400 + 367 * month / 12 + day in
  let days       = float_of_int days +. 365. *. float_of_int year -. 719499. in
  let hours      = 24. *. days +. hour in
  let mins       = 60. *. hours +. min in
  60. *. mins +. sec +. (float_of_int ms /. 1000.) +. (float_of_int us /. 1000. /. 1000.)
;;

include Time_internal

let string_and_sexp_format : [
  | `Old
  | `Force_old
  | `Write_new_read_both
  | `Write_new_read_only_new
] ref = ref `Old

let current_string_and_sexp_format () =
  match !string_and_sexp_format with
  | `Old | `Force_old -> `Old
  | _ as format -> format
;;

let modify_string_and_sexp_format =
  let string_and_sexp_format_mutex = Mutex.create () in
  (fun f ->
    Mutex.lock string_and_sexp_format_mutex;
    try
      string_and_sexp_format := f !string_and_sexp_format;
      Mutex.unlock string_and_sexp_format_mutex
    with
    | e ->
      Mutex.unlock string_and_sexp_format_mutex;
      raise e)
;;

let write_new_string_and_sexp_formats__read_both () =
  modify_string_and_sexp_format (function
    | `Force_old ->
      failwith "write_new_string_and_sexp_formats__read_both called after \
        forbid_new_string_and_sexp_formats"
    | _ -> `Write_new_read_both)
;;

let write_new_string_and_sexp_formats__read_only_new () =
  modify_string_and_sexp_format (function
    | `Force_old ->
      failwith "write_new_string_and_sexp_formats__read_only_new called after \
        forbid_new_string_and_sexp_formats"
    | _ -> `Write_new_read_only_new)
;;

let forbid_new_string_and_sexp_formats () =
  modify_string_and_sexp_format (function
    | `Old | `Force_old -> `Force_old
    | _ ->
      failwith "use_new_string_and_sexp_formats called before \
        forbid_new_string_and_sexp_formats"
  )
;;

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

(* A thin caching layer over the actual of_epoch (of_epoch_internal just above) used only
   to gain some speed when we translate the same time/date over and over again *)
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

let to_string_abs ?(zone=Zone.machine_zone ()) time =
  let date, ofday  = to_date_ofday time zone in
  let utc_offset   = utc_offset time ~zone in
  let is_utc       = Span.(=) utc_offset Span.zero in
  String.concat ~sep:"" (
       Date.to_string date
    :: " "
    :: Ofday.to_string ofday
    :: (if is_utc then ["Z"]
      else [
        (if Span.(<) utc_offset Span.zero then "-" else "+");
        Ofday.to_string_trimmed (Ofday.of_span_since_start_of_day (Span.abs utc_offset))
      ]))
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

let ofday_occurrence t zone ofday before_or_after =
  let first_guess =
    of_date_ofday zone (fst (to_date_ofday t zone)) ofday
  in
  match before_or_after with
  | `right_before ->
      if T.(<) first_guess t
        then first_guess
        else T.sub first_guess Span.day
  | `right_after ->
      if T.(>) first_guess t
        then first_guess
        else T.add first_guess Span.day
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

let to_string t =
  match !string_and_sexp_format with
  | `Write_new_read_both
  | `Write_new_read_only_new -> to_string_abs t
  | `Old | `Force_old -> to_string_deprecated t
;;

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
            l, Some (Ofday.to_sec (Ofday.of_string r))
          | None ->
            match String.lsplit2 ~on:'-' ofday with
            | Some (l,r) ->
              assert (Char.(=) r.[1] ':' || Char.(=) r.[2] ':');
              l, Some ((-1.) *. (Ofday.to_sec (Ofday.of_string r)))
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
let of_string s =
  let require_absolute =
    match !string_and_sexp_format with
    | `Write_new_read_only_new                 -> true
    | `Old | `Force_old | `Write_new_read_both -> false
  in
  of_string_gen s ~require_absolute
;;

let t_of_sexp sexp = match sexp with
  | Sexp.List [Sexp.Atom date; Sexp.Atom ofday; (Sexp.Atom "UTC" | Sexp.Atom "utc")] ->
    begin try
      of_date_ofday Zone.utc (Date.of_string date) (Ofday.of_string ofday)
    with
    | e -> of_sexp_error (sprintf "Time.t_of_sexp: %s" (Exn.to_string e)) sexp
    end
  | Sexp.List [Sexp.Atom date; Sexp.Atom ofday] ->
      begin try
        of_string (date ^ " " ^ ofday)
      with
      | e -> of_sexp_error (sprintf "Time.t_of_sexp (2 atoms): %s" (Exn.to_string e)) sexp
      end
  | Sexp.Atom datetime ->
      begin try
        of_string datetime
      with
      | e -> of_sexp_error (sprintf "Time.t_of_sexp: %s" (Exn.to_string e)) sexp
      end
  | _ -> of_sexp_error "Time.t_of_sexp" sexp
;;

let sexp_of_t t =
  match String.lsplit2 (to_string t) ~on:' ' with
  | Some (date,ofday) ->
      Sexp.List [Sexp.Atom date; Sexp.Atom ofday]
  | None ->
      raise (Bug "Time.sexp_of_t: unexpected None")
;;

let pp ppf t = Format.fprintf ppf "%s" (to_string t)
let () = Pretty_printer.register "Core.Time.pp"

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
