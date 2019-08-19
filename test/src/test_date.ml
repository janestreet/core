open! Core
open! Import
open! Date

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
      let stop_date = create_exn ~y:2020 ~m:Dec ~d:31 in
      let rec loop acc d =
        if d > stop_date
        then Ok () :: acc
        else (
          let format_str = format d "%V" in
          let week_number_str = Printf.sprintf "%02i" (week_number d) in
          let result =
            if String.( <> ) format_str week_number_str
            then
              Or_error.errorf
                "week_number for %s (%s) doesn't match output of (format \"%%V\") (%s)"
                (to_string d)
                week_number_str
                format_str
            else Ok ()
          in
          loop (result :: acc) (add_days d 1))
      in
      loop [] start_date
      |> Or_error.combine_errors_unit
      |> function
      | Result.Ok () -> ()
      | Result.Error e -> Error.raise e
    ;;
  end)
;;
