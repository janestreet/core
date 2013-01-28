open Sexplib.Conv
open Result.Export

module List = Core_list

type 'a t = ('a, Error.t) Result.t with sexp, bin_io

include (Result : Monad.S2 with type ('a, 'b) t := ('a, 'b) Result.t)

let try_with ?(backtrace = false) f =
  let backtrace = (backtrace :> bool) in
  try Ok (f ())
  with exn -> Error (Error.of_exn exn ?backtrace:(if backtrace then Some `Get else None))
;;

let try_with_join ?backtrace f = join (try_with ?backtrace f)

let ok_exn = function
  | Ok x -> x
  | Error err -> Error.raise err
;;

let of_exn ?backtrace exn = Error (Error.of_exn ?backtrace exn)

let of_exn_result = function
  | Ok _ as z -> z
  | Error exn -> of_exn exn
;;

let error message a sexp_of_a = Error (Error.create message a sexp_of_a)

let error_string message = Error (Error.of_string message)

let unimplemented s = error "unimplemented" s <:sexp_of< string >>

let all_ok l =
  let ok, errs = List.partition_map l ~f:Result.ok_fst in
  match errs with
  | [] -> Ok ok
  | _ -> Error (Error.of_list errs)
;;

TEST_UNIT =
  for i = 0 to 10; do
    assert (all_ok (List.init i ~f:(fun _ -> Ok ()))
            = Ok (List.init i ~f:(fun _ -> ())));
  done
TEST = Result.is_error (all_ok [ error_string "" ])
TEST = Result.is_error (all_ok [ Ok (); error_string "" ])

