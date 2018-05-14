open! Import
open! Import_time

include Date

let of_tm (tm : Core_unix.tm) =
  create_exn
    ~y:(tm.tm_year + 1900)
    ~m:(Month.of_int_exn (tm.tm_mon + 1))
    ~d:tm.tm_mday
;;

let format date pat =
  (* as long as you don't use anything silly like %z, the zone here is irrelevant, since
     we use the same zone for constructing a time and formatting it *)
  let zone = (force Time.Zone.local) in
  let time = Time.of_date_ofday ~zone date Time.Ofday.start_of_day in
  Time.format time pat ~zone
;;

let parse ~fmt s =
  Core_unix.strptime ~fmt s
  |> of_tm
;;
