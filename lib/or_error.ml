open Sexplib
open Sexplib.Conv
open Result.Export

type 'a t = ('a, Error.t) Result.t with sexp, bin_io

let try_with ?(backtrace = false) f =
  try Ok (f ())
  with exn -> Error (Error.of_exn exn ?backtrace:(if backtrace then Some `Get else None))
;;

let try_with_bind ?(backtrace = false) f =
  try f ()
  with exn -> Error (Error.of_exn exn ?backtrace:(if backtrace then Some `Get else None))
;;

let ok_exn = function
  | Ok x -> x
  | Error err -> Error.raise err
;;

let of_exn ?backtrace exn = Error (Error.of_exn ?backtrace exn)

let error message a sexp_of_a = Error (Error.create message a sexp_of_a)

let error_string message = error message () <:sexp_of< unit >>

let unimplemented s = error "unimplemented" s <:sexp_of< string >>

include (Result : Monad.S2 with type ('a, 'b) t := ('a, 'b) Result.t)
