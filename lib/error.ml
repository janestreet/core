(* This module is trying to minimize dependencies on modules in Core, so as to allow Error
   (and Or_error) to be used in various places.  Please avoid adding new dependencies. *)

open Sexplib.Std
open Bin_prot.Std

module Conv = Sexplib.Conv

module List = Core_list

module Sexp = struct
  include Sexplib.Sexp
  include (struct
    type t = Sexplib.Sexp.t = Atom of string | List of t list with bin_io
  end : Binable.S with type t := t)
end

let concat ?(sep="") l = String.concat sep l

type sexp = Sexp.t = Atom of string | List of sexp list (* constructor import *)

module Message = struct
  type t =
  | Could_not_construct of Sexp.t
  | String of string
  | Sexp of Sexp.t
  | Tag_sexp of string * Sexp.t
  | Tag_t of string * t
  | Tag_arg of string * Sexp.t * t
  | Of_list of int option * t list
  | With_backtrace of t * string (* backtrace *)
  with bin_io, sexp

  let rec to_strings_hum t ac =
    (* We use [Sexp.to_string_mach], despite the fact that we are implementing
       [to_strings_hum], because we want the error to fit on a single line, and once
       we've had to resort to sexps, the message is going to start not looking so
       pretty anyway. *)
    match t with
    | Could_not_construct sexp ->
      "could not construct error: " :: Sexp.to_string_mach sexp :: ac
    | String string -> string :: ac
    | Sexp sexp -> Sexp.to_string_mach sexp :: ac
    | Tag_sexp (tag, sexp) -> tag :: ": " :: Sexp.to_string_mach sexp :: ac
    | Tag_t (tag, t) -> tag :: ": " :: to_strings_hum t ac
    | Tag_arg (tag, sexp, t) ->
      tag :: ": " :: Sexp.to_string_mach sexp :: ": " :: to_strings_hum t ac
    | With_backtrace (t, backtrace) ->
      to_strings_hum t ("\nBacktrace:\n" :: backtrace :: ac)
    | Of_list (trunc_after, ts) ->
      let ts =
        match trunc_after with
        | None -> ts
        | Some max ->
          let n = List.length ts in
          if n <= max then
            ts
          else
            List.take ts max @ [ String (Printf.sprintf "and %d more errors" (n - max)) ]
      in
      List.fold (List.rev ts) ~init:ac ~f:(fun ac t ->
        to_strings_hum t (if List.is_empty ac then ac else ("; " :: ac)))
  ;;

  let to_string_hum t = concat ~sep:"" (to_strings_hum t [])

  let rec to_sexps_hum t ac =
    match t with
    | Could_not_construct _ as t -> sexp_of_t t :: ac
    | String string -> Atom string :: ac
    | Sexp sexp -> sexp :: ac
    | Tag_sexp (tag, sexp) -> List [ Atom tag; sexp ] :: ac
    | Tag_t (tag, t) -> List (Atom tag :: to_sexps_hum t []) :: ac
    | Tag_arg (tag, sexp, t) -> List (Atom tag :: sexp :: to_sexps_hum t []) :: ac
    | With_backtrace (t, backtrace) ->
      Sexp.List [ to_sexp_hum t; Sexp.Atom backtrace ] :: ac
    | Of_list (_, ts) ->
      List.fold (List.rev ts) ~init:ac ~f:(fun ac t -> to_sexps_hum t ac)
  and to_sexp_hum t =
    match to_sexps_hum t [] with
    | [sexp] -> sexp
    | sexps -> Sexp.List sexps
  ;;
end

open Message

type t = Message.t Lazy.t

type error_ = t

(* We use [protect] to guard against exceptions raised by user-supplied functons, so
   that failure to produce an error message doesn't interfere with other error messages. *)
let protect f = try f () with exn -> Message.Could_not_construct (Exn.sexp_of_t exn)

let to_message t = protect (fun () -> Lazy.force t)

let of_message message = Lazy.lazy_from_val message

let sexp_of_t t = Message.sexp_of_t (to_message t)

let t_of_sexp sexp = of_message (Message.t_of_sexp sexp)

let to_string_hum t = Message.to_string_hum (to_message t)

let to_sexp_hum t = Message.to_sexp_hum (to_message t)

include Bin_prot.Utils.Make_binable (struct
  module Binable = Message
  type t = error_
  let to_binable = to_message
  let of_binable = of_message
end)

let of_lazy l = lazy (protect (fun () -> String (Lazy.force l)))

let of_string error = Lazy.lazy_from_val (String error)

let of_thunk f = lazy (protect (fun () -> String (f ())))

let create tag x sexp_of_x = lazy (protect (fun () -> Tag_sexp (tag, sexp_of_x x)))

let tag t tag = lazy (Tag_t (tag, to_message t))

let tag_arg t tag x sexp_of_x =
  lazy (protect (fun () -> Tag_arg (tag, sexp_of_x x, to_message t)))
;;

let of_list ?trunc_after ts = lazy (Of_list (?trunc_after, List.map ts ~f:to_message))

exception Error of t

let () =
  let format =
    if (try Some (Sys.getenv "CORE_ERROR_FORMAT") with _ -> None) = Some "sexp" then
      `Sexp
    else
      `String
  in
  Sexplib.Conv.Exn_converter.add_auto (Error (of_string "<template>"))
    (function
    | Error t ->
      begin match format with
      | `String -> Sexp.Atom (to_string_hum t)
      | `Sexp -> to_sexp_hum t
      end
    | _ -> assert false)
;;

let to_exn t = Error t

let of_exn ?backtrace exn =
  let backtrace =
    match backtrace with
    | None -> None
    | Some `Get -> Some (Printexc.get_backtrace ())
    | Some (`This s) -> Some s
  in
  match exn, backtrace with
  | Error t, None           -> t
  | Error t, Some backtrace -> lazy (With_backtrace (to_message t, backtrace))
  | _      , None           -> lazy (Sexp (Exn.sexp_of_t exn))
  | _      , Some backtrace -> lazy (With_backtrace (Sexp (Exn.sexp_of_t exn), backtrace))
;;

let raise t = raise (Error t)

let failwiths message a sexp_of_a = raise (create message a sexp_of_a)

let pp ppf t = Format.fprintf ppf "\"%s\"" (to_string_hum t)
let () = Pretty_printer.register "Core.Error.pp"

TEST_MODULE "error" = struct
  TEST = to_string_hum (tag (of_string "b") "a") = "a: b"
  TEST = to_string_hum (of_list (List.map ~f:of_string [ "a"; "b"; "c" ])) = "a; b; c"

  let round t =
    let sexp = sexp_of_t t in
    sexp = sexp_of_t (t_of_sexp sexp)
  ;;

  TEST = round (of_string "hello")
  TEST = round (of_thunk (fun () -> "hello"))
  TEST = round (create "tag" 13 <:sexp_of< int >>)
  TEST = round (tag (of_string "hello") "tag")
  TEST = round (tag_arg (of_string "hello") "tag" 13 <:sexp_of< int >>)
  TEST = round (of_list [ of_string "hello"; of_string "goodbye" ])
end

(* yminsky: benchmarks

   open Core.Std
   module Bench = Core_extended.Bench

   let () =
   Bench.bench ~print:true (fun () ->
   let x = 33 in
   ignore (sprintf "%d" x)) ()
   |! Bench.print_costs

   let () =
   Bench.bench ~print:true (fun () ->
   let x = 33 in
   let closure = (fun () -> sprintf "%d" x) in
   ignore (if 3 = 4 then closure () else "")) ()
   |! Bench.print_costs

   Here are the bench results themselves:

   calculating cost of timing measurement: 260 ns
   calculating minimal measurable interval: 1000 ns
   determining number of runs per sample: 1048576
   stabilizing GC: done
   calculating the cost of a full major sweep: 1431398 ns
   running samples (estimated time 59 sec)
   ....................................................................................................
   mean run time + mean gc time: 568 ns
   warning: max run time is more than 5% away from mean
   calculating cost of timing measurement: 258 ns
   calculating minimal measurable interval: 1000 ns
   determining number of runs per sample: 134217728
   stabilizing GC: done
   calculating the cost of a full major sweep: 1484784 ns
   running samples (estimated time 75 sec)
   ....................................................................................................
   mean run time + mean gc time: 5 ns
   warning: max run time is more than 5% away from mean

*)
