include Date0

let of_time time ~zone = Time.to_date ~zone time

let today ~zone = of_time (Time.now ()) ~zone

let format date pat =
  let time = Time.of_date_ofday ~zone:Zone.local date Ofday.start_of_day in
  Time.format time pat
;;

TEST_MODULE "week_number" = struct
  TEST_UNIT =
    let module Error = Core_kernel.Error in
    let module Result = Core_kernel.Result in
    let module Or_error = Core_kernel.Or_error in
    let start_date = create_exn ~y:2000 ~m:Jan ~d:1 in
    let stop_date  = create_exn ~y:2020 ~m:Dec ~d:31 in
    let rec loop acc d =
      if (>) d stop_date
      then Result.ok_unit :: acc
      else begin
        let format_str = format d "%V" in
        let week_number_str = Printf.sprintf "%02i" (week_number d) in
        let result =
          if Core_kernel.Core_string.(<>) format_str week_number_str
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
end
