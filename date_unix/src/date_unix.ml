open! Core
open! Date_unix_intf
module Time = Time_unix

let of_tm (tm : Unix.tm) =
  Date.create_exn
    ~y:(tm.tm_year + 1900)
    ~m:(Month.of_int_exn (tm.tm_mon + 1))
    ~d:tm.tm_mday
;;

let format date pat =
  (* as long as you don't use anything silly like %z, the zone here is irrelevant, since
     we use the same zone for constructing a time and formatting it *)
  let zone = force Time.Zone.local in
  let time = Time.of_date_ofday ~zone date Time.Ofday.start_of_day in
  Time.format time pat ~zone
;;

let parse ?allow_trailing_input ~fmt s =
  Unix.strptime ?allow_trailing_input ~fmt s |> of_tm
;;
