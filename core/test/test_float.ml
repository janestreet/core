open! Core
open Poly
open! Import

let%expect_test "[Pervasives.float_of_string] supports underscores" =
  print_endline (Stdlib.string_of_float (Stdlib.float_of_string "1_234.567_8"));
  [%expect {| 1234.5678 |}]
;;

let%expect_test "[Sexp.of_float_style] is respected by the various names for [float]" =
  let f = 1234.5678 in
  let print () =
    print_s [%sexp (f : float)];
    print_s [%sexp (f : Float.t)];
    print_s [%sexp (f : Core.Core_stable.float)]
  in
  print ();
  [%expect {|
    1234.5678
    1234.5678
    1234.5678
    |}];
  Ref.set_temporarily Sexp.of_float_style `Underscores ~f:print;
  [%expect {|
    1_234.5678
    1_234.5678
    1_234.5678
    |}]
;;

let%expect_test "[Sexp.of_float_style = `Underscores]" =
  let check f =
    let sexp style =
      Ref.set_temporarily Sexp.of_float_style style ~f:(fun () -> [%sexp (f : float)])
    in
    print_s [%sexp (sexp `No_underscores : Sexp.t), (sexp `Underscores : Sexp.t)];
    if not (Float.is_nan f)
    then require [%here] (Float.equal f (sexp `Underscores |> [%of_sexp: Float.t]))
  in
  List.iter
    [ 0.
    ; Float.min_positive_subnormal_value
    ; Float.min_positive_normal_value
    ; 1E-7
    ; 1.
    ; 12.
    ; 123.
    ; 1234.
    ; 12345.
    ; 1234E100
    ; Float.max_value
    ; Float.nan
    ]
    ~f:(fun f ->
    check f;
    check (-.f));
  Expect_test_patterns.require_match
    [%here]
    {|
    (0 0)
    (-0 -0)
    (4.94065645841247E-324 4.94065645841247E-324)
    (-4.94065645841247E-324 -4.94065645841247E-324)
    (2.2250738585072014E-308 2.2250738585072014E-308)
    (-2.2250738585072014E-308 -2.2250738585072014E-308)
    (1E-07 1E-07)
    (-1E-07 -1E-07)
    (1 1)
    (-1 -1)
    (12 12)
    (-12 -12)
    (123 123)
    (-123 -123)
    (1234 1_234)
    (-1234 -1_234)
    (12345 12_345)
    (-12345 -12_345)
    (1.234E+103 1.234E+103)
    (-1.234E+103 -1.234E+103)
    (INF INF)
    (-INF -INF)
    (NAN NAN)
    ({-,}NAN {-,}NAN) (glob) |}
;;

let%expect_test "Terse.sexp_of_t" =
  let test number =
    let string = Float.Terse.to_string number in
    require_equal
      [%here]
      (module String)
      string
      (Sexp.to_string (Float.Terse.sexp_of_t number));
    print_endline string
  in
  test 0.0123456789;
  [%expect {| 0.012345679 |}];
  test 0.012345;
  [%expect {| 0.012345 |}];
  test 123456789.123;
  [%expect {| 1.2345679E+08 |}]
;;

let%test_unit "round_nearest_half_to_even quickcheck" =
  Quickcheck.test ~trials:200 (Int.gen_incl (-100_000_000) 100_000_000) ~f:(fun i ->
    let x = float i /. 10. in
    let y = Float.round_nearest_half_to_even x in
    let f = Float.round_nearest x in
    let is_tie = Int.( % ) i 10 = 5 in
    assert (
      (is_tie && Float.mod_float y 2. = 0. && Float.abs (y -. x) = 0.5)
      || ((not is_tie) && y = f));
    let x' = Float.one_ulp `Up x in
    let x'' = Float.one_ulp `Down x in
    assert (Float.round_nearest_half_to_even x' = Float.round_nearest x');
    assert (Float.round_nearest_half_to_even x'' = Float.round_nearest x''))
;;

let%expect_test "robust_sign" =
  let test n = print_s [%sexp (Float.robust_sign n : Sign.t)] in
  test 1e-6;
  [%expect "Pos"];
  test 1e-8;
  [%expect "Zero"];
  test (-1e-6);
  [%expect "Neg"];
  test (-1e-8);
  [%expect "Zero"];
  test (-0.);
  [%expect "Zero"];
  test 0.;
  [%expect "Zero"];
  test Float.neg_infinity;
  [%expect "Neg"];
  (* preserve this old behavior of [sign] *)
  test Float.nan;
  [%expect "Zero"]
;;

(* Make sure float comparison didn't accidentally get redefined using [compare]. *)
let%test _ = not (Float.( < ) Float.nan 0.)

(* When we put a similar in base/test, it doesn't behave the same, and undesirable
   versions of [Float.is_positive] that allocate when we put the test here don't allocate
   when we put the test there.  So, we put the test here. *)
let%expect_test (_ [@tags "64-bits-only", "x-library-inlining-sensitive"]) =
  let a = [| 1. |] in
  (* a.(0) is unboxed *)
  let one = 1. in
  (* [one] is boxed *)
  ignore (require_no_allocation [%here] (fun () -> Float.( > ) a.(0) 0.) : bool);
  [%expect {| |}];
  ignore (require_no_allocation [%here] (fun () -> Float.compare a.(0) 0. > 0) : bool);
  [%expect {| |}];
  ignore (require_no_allocation [%here] (fun () -> Float.is_positive a.(0)) : bool);
  [%expect {| |}];
  ignore (require_no_allocation [%here] (fun () -> Float.is_positive one) : bool);
  [%expect {| |}]
;;

let%test_module "round_significant" =
  (module struct
    let round_significant = Float.round_significant

    let%test_unit "key values" =
      [%test_result: float]
        (round_significant ~significant_digits:3 0.0045678)
        ~expect:0.00457;
      [%test_result: float]
        (round_significant ~significant_digits:3 123456.)
        ~expect:123000.;
      [%test_result: float] (round_significant ~significant_digits:3 0.) ~expect:0.;
      [%test_result: float]
        (round_significant ~significant_digits:3 Float.nan)
        ~expect:Float.nan;
      [%test_result: float]
        (round_significant ~significant_digits:3 Float.infinity)
        ~expect:Float.infinity;
      [%test_result: float]
        (round_significant ~significant_digits:3 Float.neg_infinity)
        ~expect:Float.neg_infinity;
      [%test_result: float]
        (round_significant ~significant_digits:1 (-5.85884163457842E+100))
        ~expect:(-6E+100);
      [%test_result: float]
        (round_significant ~significant_digits:16 (-129361178280336660.))
        ~expect:(-129361178280336700.);
      (* An example where it appears like we don't round to even (since the argument is
         under-represented as a float). *)
      [%test_result: float]
        (round_significant ~significant_digits:11 4.36083208835)
        ~expect:4.3608320883
    ;;

    let%test_unit ("round_significant vs sprintf quickcheck 1" [@tags "64-bits-only"]) =
      for significant_digits = 1 to 16 do
        let open Quickcheck in
        test
          Float.gen_without_nan
          ~trials:10_000
          ~sexp_of:(fun float -> [%message "" (float : float) (significant_digits : int)])
          ~f:(fun x ->
            let s = sprintf "%.*g" significant_digits x |> Float.of_string in
            assert (
              s = round_significant ~significant_digits x
              || s = round_significant ~significant_digits (Float.one_ulp `Up x)
              || s = round_significant ~significant_digits (Float.one_ulp `Down x)))
      done
    ;;

    let%test_unit ("round_significant vs sprintf quickcheck 2" [@tags "64-bits-only"]) =
      (* this test is much more likely to exercise cases when we're off by an ulp *)
      let num_digits_gen = Int.gen_incl 1 18 in
      let digits_gen num_digits =
        let x = Int63.(pow (of_int 10) (of_int num_digits) - of_int 1) in
        Int63.gen_incl Int63.(~-x) x
      in
      let scale_gen = Int.gen_incl (-20) 20 in
      let sf_gen = Int.gen_incl 1 18 in
      Quickcheck.test
        ~trials:1000
        (Quickcheck.Generator.tuple3
           (Quickcheck.Generator.bind num_digits_gen ~f:digits_gen)
           scale_gen
           sf_gen)
        ~f:(fun (digits, scale, sf) ->
          let x =
            if scale > 0
            then Int63.to_float digits *. (10. ** float scale)
            else Int63.to_float digits /. (10. ** float (-scale))
          in
          let r = round_significant ~significant_digits:sf x in
          let r1 = round_significant ~significant_digits:sf (Float.one_ulp `Up x) in
          let r2 = round_significant ~significant_digits:sf (Float.one_ulp `Down x) in
          let s = sprintf "%.*g" sf x |> Float.of_string in
          assert (s = r || s = r1 || s = r2))
    ;;

    let%test "0 significant digits" =
      Exn.does_raise (fun () ->
        ignore (round_significant ~significant_digits:0 1.3 : float))
    ;;
  end)
;;

let%test_module "round_decimal" =
  (module struct
    let round_decimal = Float.round_decimal

    let%test_unit "key values" =
      [%test_result: float] (round_decimal ~decimal_digits:3 0.0045678) ~expect:0.005;
      [%test_result: float] (round_decimal ~decimal_digits:0 0.0045678) ~expect:0.;
      [%test_result: float] (round_decimal ~decimal_digits:0 1.0045678) ~expect:1.;
      [%test_result: float] (round_decimal ~decimal_digits:3 123456.) ~expect:123456.;
      [%test_result: float] (round_decimal ~decimal_digits:(-3) 123456.) ~expect:123000.;
      [%test_result: float] (round_decimal ~decimal_digits:3 0.) ~expect:0.;
      [%test_result: float] (round_decimal ~decimal_digits:3 Float.nan) ~expect:Float.nan;
      [%test_result: float]
        (round_decimal ~decimal_digits:3 Float.infinity)
        ~expect:Float.infinity;
      [%test_result: float]
        (round_decimal ~decimal_digits:3 Float.neg_infinity)
        ~expect:Float.neg_infinity;
      [%test_result: float]
        (round_decimal ~decimal_digits:(-100) (-5.85884163457842E+100))
        ~expect:(-6E+100);
      [%test_result: float]
        (round_decimal ~decimal_digits:0 (-129361178280336660.))
        ~expect:(-129361178280336660.);
      [%test_result: float]
        (round_decimal ~decimal_digits:(-2) (-129361178280336660.))
        ~expect:(-129361178280336700.);
      [%test_result: float]
        (round_decimal ~decimal_digits:10 4.36083208835)
        ~expect:4.3608320883
    ;;

    let%test_unit ("round_decimal vs sprintf quickcheck 1" [@tags "64-bits-only"]) =
      for decimal_digits = 1 to 16 do
        let open Quickcheck in
        test
          Float.gen_without_nan
          ~trials:10_000
          ~sexp_of:(fun float -> [%message "" (float : float) (decimal_digits : int)])
          ~f:(fun x ->
            let s = sprintf "%.*f" decimal_digits x |> Float.of_string in
            assert (
              s = round_decimal ~decimal_digits x
              || s = round_decimal ~decimal_digits (Float.one_ulp `Up x)
              || s = round_decimal ~decimal_digits (Float.one_ulp `Down x)))
      done
    ;;
  end)
;;

open! Float

let test_class quickcheck_generator expect =
  Quickcheck.test quickcheck_generator ~f:(fun float ->
    let actual = classify float in
    if not (Int.equal (Class.compare actual expect) 0)
    then
      raise_s
        [%message
          "generator produced float in wrong class"
            (float : t)
            (expect : Class.t)
            (actual : Class.t)])
;;

let%test_unit _ = test_class gen_zero Zero
let%test_unit _ = test_class gen_subnormal Subnormal
let%test_unit _ = test_class gen_normal Normal
let%test_unit _ = test_class gen_infinite Infinite
let%test_unit _ = test_class gen_nan Nan

(* Additional tests of Base.Float requiring the Gc module *)

let%expect_test (_ [@tags "64-bits-only"]) =
  require_no_allocation [%here] (fun () ->
    [%test_result: Int63.t] (int63_round_nearest_exn 0.8) ~expect:(Int63.of_int_exn 1));
  [%expect {| |}]
;;

(* We have to carefully allow the compiler to unbox t.(0) to test that each function
   doesn't force it to re-box the float.  We need the extra fun per function to allow the
   compiler to inline each one separately around t.(0). *)
let%expect_test ("iround does not force re-boxing" [@tags "x-library-inlining-sensitive"])
  =
  let exception Stop_the_test in
  let require_no_float_boxing f =
    try
      Ref.set_temporarily
        on_print_cr
        (fun _ -> raise Stop_the_test)
        ~f:(fun () ->
          List.iter (List.range (-19) 24) ~f:(fun e ->
            let scale = 10. ** float e in
            for _ = 0 to 14_000 do
              let t = [| Random.float scale -. (scale /. 2.) |] in
              (* When the result doesn't fit, [f] will raise.  In that case, we allow
                 allocation, so we don't check anything here.

                 We manually check the Gc stats rather than use [require_no_allocation],
                 because the latter is too slow to use thousands of times in a loop.  *)
              match
                let minor_before = Gc.minor_words () in
                let major_before = Gc.major_words () in
                let (_result : int) = f t in
                let minor_after = Gc.minor_words () in
                let major_after = Gc.major_words () in
                ( `minor (Int.( - ) minor_after minor_before)
                , `major (Int.( - ) major_after major_before) )
              with
              (* When the result doesn't fit, [f] will raise.  In that case, we allow
                 allocation, so we don't check anything here. *)
              | exception _ -> ()
              | `minor minor, `major major ->
                require_equal [%here] (module Int) minor 0;
                require_equal [%here] (module Int) major 0
            done))
    with
    | Stop_the_test -> ()
  in
  require_no_float_boxing (fun t -> Float.iround_down_exn t.(0));
  [%expect {| |}];
  require_no_float_boxing (fun t -> Float.iround_nearest_exn t.(0));
  [%expect {| |}];
  require_no_float_boxing (fun t -> Float.iround_up_exn t.(0));
  [%expect {| |}];
  require_no_float_boxing (fun t -> Float.iround_towards_zero_exn t.(0));
  [%expect {| |}];
  require_no_float_boxing (fun t -> Stdlib.int_of_float t.(0));
  [%expect {| |}]
;;

let%expect_test "Float.validate_positive doesn't allocate on success" =
  require_no_allocation [%here] (fun () ->
    ignore (Sys.opaque_identity (validate_positive 1.) : Validate.t));
  [%expect {| |}]
;;

let%test_module _ =
  (module struct
    let check v expect =
      match Validate.result v, expect with
      | Ok (), `Ok | Error _, `Error -> ()
      | r, expect ->
        raise_s [%message "mismatch" (r : unit Or_error.t) (expect : [ `Ok | `Error ])]
    ;;

    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) nan) `Error
    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) infinity) `Error
    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) neg_infinity) `Error
    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) (-1.)) `Error
    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) 0.) `Ok
    let%test_unit _ = check (validate_lbound ~min:(Incl 0.) 1.) `Ok
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) nan) `Error
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) infinity) `Error
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) neg_infinity) `Error
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) (-1.)) `Ok
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) 0.) `Ok
    let%test_unit _ = check (validate_ubound ~max:(Incl 0.) 1.) `Error
  end)
;;
