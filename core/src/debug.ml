open! Import
module List = Base.List
module String = Base.String

let eprint message = Printf.eprintf "%s\n%!" message
let eprint_s sexp = eprint (Sexp.to_string_hum sexp)
let eprints message a sexp_of_a = eprint_s ([%sexp_of: string * a] (message, a))
let eprintf format = Printf.ksprintf eprint format
let failwiths = Error.failwiths

module Make () = struct
  let check_invariant = ref true
  let show_messages = ref true

  let debug invariant ~module_name name ts arg sexp_of_arg sexp_of_result f =
    if !show_messages
    then eprints (String.concat ~sep:"" [ module_name; "."; name ]) arg sexp_of_arg;
    if !check_invariant
    then (
      try List.iter ts ~f:invariant with
      | exn ->
        failwiths
          ~here:[%here]
          "invariant pre-condition failed"
          (name, exn)
          [%sexp_of: string * exn]);
    let result_or_exn = Result.try_with f in
    if !check_invariant
    then (
      try List.iter ts ~f:invariant with
      | exn ->
        failwiths
          ~here:[%here]
          "invariant post-condition failed"
          (name, exn)
          [%sexp_of: string * exn]);
    if !show_messages
    then
      eprints
        (String.concat ~sep:"" [ module_name; "."; name; "-result" ])
        result_or_exn
        [%sexp_of: (result, exn) Result.t];
    Result.ok_exn result_or_exn
  ;;
end

let should_print_backtrace = ref false

let am_internal here message =
  (* In this function we use [Printf.eprintf] rather than [Debug.eprintf], because the
     former doesn't flush, while the latter does.  We'd rather flush once at the end,
     rather than three times. *)
  Printf.eprintf "%s:\n" (Source_code_position.to_string here);
  if !should_print_backtrace
  then
    Printf.eprintf
      "%s\n"
      (Backtrace.get () |> [%sexp_of: Backtrace.t] |> Sexp.to_string_hum);
  (match message with
   | None -> ()
   | Some message -> Printf.eprintf "%s\n" message);
  Printf.eprintf "%!"
;;

let am here = am_internal here None
let amf here fmt = Printf.ksprintf (fun string -> am_internal here (Some string)) fmt

let ams here message a sexp_of_a =
  am_internal here (Some ((message, a) |> [%sexp_of: string * a] |> Sexp.to_string_hum))
;;
