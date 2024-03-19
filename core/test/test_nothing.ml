open! Core
open! Import

let%expect_test "t_of_sexp" =
  require_does_raise [%here] (fun () -> Nothing.t_of_sexp (Sexp.List []));
  [%expect
    {|
    (Of_sexp_error
     "Base.Nothing.t_of_sexp: trying to convert an empty type"
     (invalid_sexp ()))
    |}]
;;

let%test_module "Stable.V1" =
  (module struct
    module Nothing = Nothing.Stable.V1

    let%expect_test "t_of_sexp" =
      require_does_raise [%here] (fun () -> Nothing.t_of_sexp (Sexp.List []));
      [%expect
        {|
        (Of_sexp_error
         "lib/core/src/nothing.ml.Stable.V1.t_of_sexp: trying to convert an empty type"
         (invalid_sexp ()))
        |}]
    ;;

    let%expect_test _ =
      print_endline [%bin_and_sexp_digest: Nothing.t];
      [%expect {| 4c263abcdcee45b5eb6ba02681664ded |}]
    ;;
  end)
;;
