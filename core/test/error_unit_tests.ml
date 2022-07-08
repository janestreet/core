open! Core

let%test_unit "[raise_s sexp] raises an exn whose [sexp_of_t] is [sexp]" =
  let sexp = [%sexp "foo"] in
  match Nothing.unreachable_code (Error.raise_s sexp) with
  | exception exn -> assert (phys_equal [%sexp (exn : exn)] sexp)
  | _ -> failwith "impossible"
;;
