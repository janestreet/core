open! Core
open! Import
open! Sexp

module _ = struct
  open! With_text

  let sexp_of_il = sexp_of_list sexp_of_int
  let il_of_sexp = list_of_sexp int_of_sexp
  let il_of_text text = Or_error.ok_exn (of_text il_of_sexp text)
  let il_of_value il = of_value sexp_of_il il

  module IL = struct
    type t = int list [@@deriving compare, sexp_of]

    let equal = [%compare.equal: t]
  end

  let t = il_of_value [ 3; 4 ]

  let%expect_test _ =
    print_s [%sexp (text t : string)];
    [%expect {| "(3 4)" |}]
  ;;

  let t' = il_of_text (text t)

  let%expect_test _ =
    require_equal [%here] (module IL) (value t') [ 3; 4 ];
    [%expect {| |}]
  ;;

  let%expect_test _ =
    print_s [%sexp (t : il t)];
    [%expect {| "(3 4)" |}]
  ;;

  let%expect_test _ =
    require_equal
      [%here]
      (module IL)
      (value (t_of_sexp il_of_sexp (Atom "(3 4)")))
      [ 3; 4 ];
    [%expect {| |}]
  ;;

  let%expect_test _ =
    require_equal
      [%here]
      (module IL)
      [ 8; 9 ]
      (value (il_of_text ";this is a comment\n (8; foo\n 9)   \n "));
    [%expect {| |}]
  ;;

  let%expect_test _ =
    require_does_raise [%here] (fun () -> il_of_text "(1 2 bla)");
    [%expect
      {| (Of_sexp_error :1:5 "int_of_sexp: (Failure int_of_string)" (invalid_sexp bla)) |}]
  ;;

  let%expect_test _ =
    require_does_raise [%here] (fun () ->
      t_of_sexp il_of_sexp (Sexp.of_string "\"(1 2 bla)\""));
    [%expect
      {| (Of_sexp_error :1:5 "int_of_sexp: (Failure int_of_string)" (invalid_sexp bla)) |}]
  ;;
end

let%test_module _ =
  (module struct
    type t1 = Sexp.t [@@deriving bin_io]
    type t2 = Sexp.Stable.V1.t [@@deriving bin_io]
    type t3 = Core_stable.Sexp.V1.t [@@deriving bin_io]

    let%expect_test "Sexp.t, Sexp.Stable.V1.t and Core_stable.Sexp.V1.t bin_digests match"
      =
      print_endline [%bin_digest: t1];
      print_endline [%bin_digest: t2];
      print_endline [%bin_digest: t3];
      [%expect
        {|
        832b40ae394f2851da8ba67b3339b429
        832b40ae394f2851da8ba67b3339b429
        832b40ae394f2851da8ba67b3339b429
        |}]
    ;;
  end)
;;

let%test_module "of_sexp_allow_extra_fields_recursively" =
  (module struct
    module V = struct
      type v1 =
        { a : string
        ; b : int
        ; suffix : string
        }
      [@@deriving sexp]

      type v2 =
        { a : string
        ; b : int
        }
      [@@deriving sexp]

      type t = v2 [@@deriving sexp_of]

      let t_of_sexp sexp : t =
        try v2_of_sexp sexp with
        | e ->
          (match v1_of_sexp sexp with
           | { a; b; suffix } -> { a = a ^ suffix; b }
           | exception _ -> raise e)
      ;;
    end

    type t = { v : V.t } [@@deriving sexp]

    let%expect_test "affect sexp converter globally" =
      let sexp = Sexp.of_string {|((v ((a a)(b 0)(suffix "-suffix"))))|} in
      let t = t_of_sexp sexp in
      print_s (sexp_of_t t);
      [%expect
        {|
        ((
          v (
            (a a-suffix)
            (b 0))))
        |}];
      let t = Sexp.of_sexp_allow_extra_fields_recursively t_of_sexp sexp in
      print_s (sexp_of_t t);
      [%expect
        {|
        ((
          v (
            (a a)
            (b 0))))
        |}]
    ;;
  end)
;;
