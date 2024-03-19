open! Core

let%test_unit "sexp_of_int respects sexp_of_int_style" =
  let r = Int_conversions.sexp_of_int_style in
  let old = !r in
  r := `Underscores;
  [%test_result: Sexp.t] (1234 |> [%sexp_of: int]) ~expect:(Atom "1_234");
  r := `No_underscores;
  [%test_result: Sexp.t] (1234 |> [%sexp_of: int]) ~expect:(Atom "1234");
  r := old
;;

let%test_unit "print_s is provided by Core" = assert (phys_equal print_s Core.print_s)

let%expect_test "print_s default" =
  print_s [%sexp (List.init 30 ~f:Fn.id : int list)];
  [%expect
    {|
    (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
     29)
    |}]
;;

let%expect_test "print_s mach" =
  print_s ~mach:() [%sexp (List.init 30 ~f:Fn.id : int list)];
  [%expect
    {| (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29) |}]
;;
