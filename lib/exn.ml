module Sexp = Sexplib.Sexp
module Conv = Sexplib.Conv
open Sexplib.Std
open Bin_prot.Std

let sexp_of_exn = Conv.sexp_of_exn
let sexp_of_exn_opt = Conv.sexp_of_exn_opt

type t = exn with sexp_of

exception Finally of t * t with sexp
exception Reraised of string * t with sexp

let reraise exc str =
  raise (Reraised (str, exc))

let reraisef exc format =
  Printf.ksprintf (fun str () -> reraise exc str) format

let () =
  StdLabels.List.iter
    ~f:(fun (exc, handler) ->
      Conv.Exn_converter.add_auto ~finalise:false exc handler)
    [
      (
        Bin_prot.Common.Read_exc (Not_found, 0),
        (function
        | Bin_prot.Common.Read_exc (exc, pos) ->
            Sexp.List [
              Sexp.Atom "Bin_prot.Common.Read_exc";
              sexp_of_exn exc;
              Conv.sexp_of_int pos;
            ]
        | _ -> assert false)
      );(
        Bin_prot.Common.Read_error (Bin_prot.Common.ReadError.Neg_int8, 0),
        (function
        | Bin_prot.Common.Read_error (err, pos) ->
            let str_err = Bin_prot.Common.ReadError.to_string err in
            Sexp.List [
              Sexp.Atom "Bin_prot.Common.Read_error";
              Sexp.Atom str_err;
              Conv.sexp_of_int pos;
            ]
        | _ -> assert false)
      );(
        Bin_prot.Unsafe_read_c.Error Bin_prot.Common.ReadError.Neg_int8,
        (function
        | Bin_prot.Unsafe_read_c.Error err ->
            let str_err = Bin_prot.Common.ReadError.to_string err in
            Sexp.List [ Sexp.Atom "Bin_prot.Common.Read_error";
                              Sexp.Atom str_err ]
        | _ -> assert false)
      )
    ]

let to_string exc = Sexp.to_string_hum ~indent:2 (sexp_of_exn exc)

let sexp_of_t = sexp_of_exn

let protectx ~f x ~(finally : _ -> unit) =
  let res =
    try f x
    with exn ->
      (try finally x with final_exn -> raise (Finally (exn, final_exn)));
      raise exn
  in
  finally x;
  res

let protect ~f ~finally = protectx ~f () ~finally

let pp ppf t =
  match sexp_of_exn_opt t with
  | Some sexp -> Sexp.pp_hum ppf sexp
  | None -> Format.fprintf ppf "%s" (Printexc.to_string t)

let backtrace = Printexc.get_backtrace

let handle_uncaught_with_exit_function ~exit f =
  try f ()
  with exc ->
    let bt = backtrace () in
    Format.eprintf "@[<2>Uncaught exception:@\n@\n@[%a@]@]@.%!" pp exc;
    if Printexc.backtrace_status () then prerr_string bt;
    exit 1

let handle_uncaught ~exit:must_exit f =
  handle_uncaught_with_exit_function f ~exit:(
    if must_exit
    then exit
    else ignore
  )

let reraise_uncaught str func =
  try func () with
  | exn -> raise (Reraised (str, exn))

let () = Pretty_printer.register "Core.Exn.pp"

let () =
  Printexc.register_printer (fun exc ->
    Option.map (sexp_of_exn_opt exc) ~f:(fun sexp ->
      Sexp.to_string_hum ~indent:2 sexp))
