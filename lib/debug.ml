open Core_kernel.Std

include Core_kernel.Debug

let should_print_backtrace = ref false

let am_internal here message =
  (* In this function we use [Printf.eprintf] rather than [Debug.eprintf], because the
     former doesn't flush, while the latter does.  We'd rather flush once at the end,
     rather than three times. *)
  Printf.eprintf "%s:\n" (Source_code_position.to_string here);
  if !should_print_backtrace then
    Option.iter (Backtrace.get_opt ()) ~f:(fun backtrace ->
      Printf.eprintf "%s\n"
        (backtrace |> <:sexp_of< Backtrace.t >> |> Sexp.to_string_hum));
  begin match message with
  | None -> ()
  | Some message -> Printf.eprintf "%s\n" message;
  end;
  Printf.eprintf "%!";
;;

let am here = am_internal here None

let amf here fmt = ksprintf (fun string -> am_internal here (Some string)) fmt

let ams here message a sexp_of_a =
  am_internal here (Some ((message, a)
                          |> <:sexp_of< string * a >>
                          |> Sexp.to_string_hum))
;;
