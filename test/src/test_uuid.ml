open! Core
open! Import
open! Uuid

let%expect_test "[Unstable.t_of_sexp] validates its input"=
  require_does_raise [%here] (fun () ->
    [%of_sexp: Unstable.t] [%sexp "not a uuid"]);
  [%expect {|
    (Sexplib.Conv.Of_sexp_error
      (Failure "not a uuid: not a valid UUID")
      "not a uuid") |}]
;;

let%expect_test "[Stable.V1.t_of_sexp] does not validate its input" =
  require_does_not_raise [%here] (fun () ->
    ignore ([%of_sexp: Stable.V1.t] [%sexp "not a uuid"] : t));
  [%expect {| |}]
;;

let%expect_test "[Unstable.sexp_of_t] on a valid input" =
  require_does_not_raise [%here] (fun () ->
    ignore ([%of_sexp: Unstable.t] [%sexp (create () : t)] : t));
  [%expect {| |}]
;;

let%expect_test "[Stable.V1.sexp_of_t] on a valid input" =
  require_does_not_raise [%here] (fun () ->
    ignore ([%of_sexp: Stable.V1.t] [%sexp (create () : t)] : t));
  [%expect {| |}]
;;
