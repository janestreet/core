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

let%test_unit "parse" =
  [%test_result: t]
    ~expect:(create_exn ~y:1970 ~m:Jan ~d:1)
    (parse ~fmt:"%a, %d %b %Y" "Thu, 1 Jan 1970");
  [%test_result: t]
    ~expect:(create_exn ~y:2016 ~m:Apr ~d:19)
    (parse ~fmt:"%a, %d %b %Y %H:%M:%S %z" "Tue, 19 Apr 2016 07:34:04 +0800")
;;

let%test_module "week_number" =
  (module struct
    let%test_unit _ =
      let start_date = create_exn ~y:2000 ~m:Jan ~d:1 in
      let stop_date  = create_exn ~y:2020 ~m:Dec ~d:31 in
      let rec loop acc d =
        if (>) d stop_date
        then Result.ok_unit :: acc
        else begin
          let format_str = format d "%V" in
          let week_number_str = Printf.sprintf "%02i" (week_number d) in
          let result =
            if String.(<>) format_str week_number_str
            then
              Or_error.errorf
                "week_number for %s (%s) doesn't match output of (format \"%%V\") (%s)"
                (to_string d) week_number_str format_str
            else Result.ok_unit
          in
          loop (result :: acc) (add_days d 1)
        end
      in
      loop [] start_date
      |> Or_error.combine_errors_unit
      |> function
      | Result.Ok ()   -> ()
      | Result.Error e -> Error.raise e
    ;;
  end)
