open! Core
open! Expect_test_helpers_core
open! Percent

module Stable_unit_test (T : sig
  type t = Percent.t

  include Binable with type t := t
  include Sexpable with type t := t
end) (Version : sig
  val v : int
end) =
Stable_unit_test.Make (struct
  include T

  let equal = equal

  let tests =
    [ of_mult 0.375, "37.5%", "\000\000\000\000\000\000\216?"
    ; of_mult 4.5, "4.5x", "\000\000\000\000\000\000\018@"
    ; of_mult 0.0002, "2bp", "-C\028\235\2266*?"
    ; ( of_mult 0.000075
      , (if Int.(Version.v < 3) then "0.75bp" else "7.5E-1bp")
      , "a2U0*\169\019?" )
    ]
  ;;
end)

let%test_module "Percent.V1.Bin_shape_same_as_float" =
  (module Stable_unit_test
            (Stable.V1.Bin_shape_same_as_float)
            (struct
              let v = 1
            end))
;;

let%test_module "Percent.V3" =
  (module Stable_unit_test
            (Stable.V3)
            (struct
              let v = 3
            end))
;;

let%test_module "Percent.V2" =
  (module Stable_unit_test
            (Stable.V2)
            (struct
              let v = 2
            end))
;;

let%test_module "Percent" =
  (module Stable_unit_test
            (Percent)
            (struct
              let v = 2
            end))
;;

let%expect_test "bin_digests" =
  (* [V2] intentionally has a bin_digest distinct from float's: changing
     a protocol to use float vs. percent is usually a breaking change.
  *)
  List.iter
    [ "float", [%bin_digest: float]
    ; "v1", [%bin_digest: Stable.V1.Bin_shape_same_as_float.t]
    ; "option_v1", [%bin_digest: Stable.Option.V1.Bin_shape_same_as_float.t]
    ; "v2", [%bin_digest: Stable.V2.t]
    ; "option_v2", [%bin_digest: Stable.Option.V2.t]
    ; "v3", [%bin_digest: Stable.V3.t]
    ; "option_v3", [%bin_digest: Stable.Option.V3.t]
    ]
    ~f:(fun (label, bin_digest) -> printf "%20s %s\n" label bin_digest);
  [%expect
    {|
        float 1fd923acb2dd9c5d401ad5b08b1d40cd
           v1 1fd923acb2dd9c5d401ad5b08b1d40cd
    option_v1 1fd923acb2dd9c5d401ad5b08b1d40cd
           v2 608ef16f40e15a75b1942a12465f47df
    option_v2 7d0312ebcdefb728502ea27a959a389b
           v3 320416d3338cb49f5c3ce6b8632f7717
    option_v3 21451618c9bda19b005e0ba1e4abbd2a
    |}]
;;

let%expect_test "rounding" =
  let p = Percent.of_string "0.0123456%" in
  let tests =
    [ Percent.round_significant p ~significant_digits:4
    ; Percent.round_decimal_mult p ~decimal_digits:4
    ; Percent.round_decimal_percentage p ~decimal_digits:4
    ; Percent.round_decimal_bp p ~decimal_digits:4
    ]
  in
  printf !"%{sexp:Percent.t list}" tests;
  [%expect "(1.235bp 1bp 1.23bp 1.2346bp)"]
;;

let%test_unit {|
  This does not round-trip via V2 because
  of_percentage 3.638 = of_mult 0.036379999999999996
  while V2 rounds to 6 significant digits only.  But it does round-trip via V3.
  |}
  =
  let module V2 = Stable.V2 in
  let module V3 = Stable.V3 in
  let t = of_percentage 3.638 in
  [%test_result: int] (compare t (V2.t_of_sexp (V2.sexp_of_t t))) ~expect:(-1);
  [%test_result: int] (compare t (V3.t_of_sexp (V3.sexp_of_t t))) ~expect:0;
  (* however, if we use [of_percentage_slow_more_accurate], we are fine: *)
  let t = of_percentage_slow_more_accurate 3.638 in
  [%test_result: int] (compare t (V2.t_of_sexp (V2.sexp_of_t t))) ~expect:0;
  [%test_result: int] (compare t (V3.t_of_sexp (V3.sexp_of_t t))) ~expect:0
;;

let%test_unit _ =
  (* [t_of_sexp] *)
  List.iter
    ~f:(fun (string, expected) ->
      assert (equal (t_of_sexp (Sexp.Atom string)) (of_mult expected)))
    [ "30%", 0.3
    ; "3123bp", 0.3123
    ; "3.17x", 3.17
    ; "0.0003x", 0.0003
    ; "0%", 0.
    ; "0bp", 0.
    ; "0x", 0.
    ; "0.000%", 0.
    ; "0.00bp", 0.
    ; "0.00x", 0.
    ; "3.1e5%", 3100.
    ; "3.1e5bp", 31.
    ; "3.1e5x", 310000.
    ; "10%", 0.1
    ; "110%", 1.1
    ; "0.1x", 0.1
    ; "1.1x", 1.1
    ; "0.001x", 0.001
    ; "1bp", 0.0001
    ; "10bp", 0.001
    ; "100bp", 0.01
    ; "1000bp", 0.1
    ; "11000bp", 1.1
    ; "1.1e4bp", 1.1
    ; "50%", 0.5
    ]
;;

let%test_unit _ =
  (* [sexp_of_t] and [t_of_sexp] *)
  List.iter
    ~f:(fun (t1, expected) ->
      let t1 = of_mult t1 in
      let s = Sexp.to_string (sexp_of_t t1) in
      assert (String.equal s expected);
      let t2 = t_of_sexp (Sexp.of_string s) in
      assert (equal t1 t2))
    [ 0.3, "30%"
    ; 0.335, "33.5%"
    ; 0.00335, "33.5bp"
    ; 33.46, "33.46x"
    ; 0.1, "10%"
    ; 0.99, "99%"
    ; 1., "1x"
    ; 10., "10x"
    ; 0.001, "10bp"
    ; 0.0001, "1bp"
    ; 0.00001, "0.1bp"
    ; 0.5, "50%"
    ]
;;

let%test_unit _ =
  (* [to_string'] and [to_string_f] *)
  List.iter
    ~f:(fun (t, precision, format', expect, expectf) ->
      let t = of_mult t in
      let output' = format t (Format.compact_E ~precision) in
      assert (String.equal output' expect);
      let outputf = format t format' in
      assert (String.equal outputf expectf))
    [ 0.3, 3, Format.decimal ~precision:3, "30%", "30.000%"
    ; 0.333, 4, Format.decimal ~precision:2, "33.3%", "33.30%"
    ; 0.333333, 4, Format.decimal ~precision:2, "33.33%", "33.33%"
    ; 0.000333, 2, Format.decimal ~precision:2, "3.3bp", "3.33bp"
    ]
;;

let%expect_test _ =
  print_and_check_comparable_sexps
    [%here]
    (module Percent)
    [ Percent.zero; Percent.of_bp 15.; Percent.of_percentage 15.; Percent.of_mult 15. ];
  [%expect
    {|
    (Set (0x 15bp 15% 15x))
    (Map (
      (0x   0)
      (15bp 1)
      (15%  2)
      (15x  3)))
    |}]
;;

let%expect_test "accept float in set and map sexps" =
  let module Of_string (M : Sexpable.S1) = struct
    type t = string M.t [@@deriving sexp]
  end
  in
  let test (module M : Sexpable) string =
    print_s (M.sexp_of_t (M.t_of_sexp (Sexp.of_string string)))
  in
  test (module Percent.Set) {| (0 0.0001 0.01 1) |};
  [%expect {| (0x 1bp 1% 1x) |}];
  test
    (module Of_string (Percent.Map))
    {|
    ((0      "arbitrary value")
     (0.0001 "arbitrary value")
     (0.01   "arbitrary value")
     (1      "arbitrary value")) |};
  [%expect
    {|
    ((0x  "arbitrary value")
     (1bp "arbitrary value")
     (1%  "arbitrary value")
     (1x  "arbitrary value"))
    |}]
;;

let%expect_test "generator" =
  Quickcheck.test
    ~sexp_of:sexp_of_t
    ~shrinker:Percent.quickcheck_shrinker
    Percent.quickcheck_generator
    ~f:(fun t -> Validate.maybe_raise (Percent.validate t));
  [%expect {| |}]
;;

let%expect_test ("to_mult and of_mult no boxing in arrays" [@tags "fast-flambda"]) =
  let float_arr = Array.init 1 ~f:(fun i -> Float.of_int i) in
  let percent_arr = Array.create ~len:1 Percent.zero in
  require_no_allocation [%here] (fun () ->
    percent_arr.(0) <- Percent.of_mult float_arr.(0);
    float_arr.(0) <- Percent.to_mult percent_arr.(0))
;;

let%test "Percent.Option cannot represent nan" =
  let nan = Percent.of_mult Float.nan in
  Percent.Option.((not (some_is_representable nan)) && is_none (some nan))
;;

let%expect_test "always-percentage format" =
  let f x =
    let p = Percent.of_string x in
    print_string [%string "%{p#Percent} -> %{p#Percent.Always_percentage}\n"]
  in
  let cases = [ "0x"; "3.2bp"; "5x"; "75%" ] in
  List.iter ~f cases;
  [%expect {|
    0x -> 0%
    3.2bp -> 0.032%
    5x -> 500%
    75% -> 75%
    |}];
  let f x =
    let p = Percent.of_string x in
    printf
      !"%{sexp: Percent.t} -> %s\n"
      p
      (Percent.Always_percentage.format p (Percent.Format.decimal ~precision:8))
  in
  List.iter cases ~f;
  [%expect
    {|
    0x -> 0.00000000%
    3.2bp -> 0.03200000%
    5x -> 500.00000000%
    75% -> 75.00000000%
    |}]
;;

let%expect_test "round-trippable sexp format" =
  let f p =
    print_string
      (sprintf
         "%20s | %23s | %26s | %26s | %26s \n"
         [%string "%{p#Percent}"]
         [%string "%{p#Percent.Almost_round_trippable}"]
         [%string "%{p#Percent.Almost_round_trippable.Always_percentage}"]
         [%string "%{p#Percent.Stable.V3}"]
         [%string "%{p#Percent.Stable.V3.Always_percentage}"]);
    let pp = Percent.Stable.V3.sexp_of_t p |> Percent.Stable.V3.t_of_sexp in
    [%test_result: Percent.t] pp ~expect:p;
    [%test_result: Float.t] (Percent.to_mult pp) ~expect:(Percent.to_mult p)
  in
  let cases =
    [ "0x"
    ; "3.2bp"
    ; "5x"
    ; "75%"
    ; "3.638%"
    ; "1.004175x"
    ; "0.00171x"
    ; Percent.of_bp 17.1 |> Percent.to_string_round_trippable
    ; "0.123457x"
    ; "12.3456789%"
    ; "12.3456789bp"
    ; "101.7275%"
    ; "100.4175%"
    ; "1.276001e-14x"
    ; "1.394002e13x"
    ; "1.394002e14x"
    ; "1.394002e15x"
    ; "1_512.347_130_211%"
    ; "15_12_._3471_30_21_1%"
    ; "     1.2e+13bp"
    ; "+7.3%"
    ; "  +13.9E-11%"
    ; "\n\n\t  +15bp"
    ]
    |> List.map ~f:Percent.of_string
  in
  List.iter ~f cases;
  [%expect
    {|
             0x |                      0x |                         0% |                         0x |                         0%
          3.2bp |                   3.2bp |                     0.032% |                      3.2bp |                     0.032%
             5x |                      5x |                       500% |                         5x |                       500%
            75% |                     75% |                        75% |                        75% |                        75%
         3.638% |                  3.638% |                     3.638% |                     3.638% |                     3.638%
       1.00418x |               1.004175x |                  100.4175% |                  1.004175x |                  100.4175%
         17.1bp |                  17.1bp |                     0.171% |                     17.1bp |                     0.171%
         17.1bp |                  17.1bp |                     0.171% |       17.100000000000001bp |       0.17100000000000001%
       12.3457% |                12.3457% |                   12.3457% |                   12.3457% |                   12.3457%
       12.3457% |             12.3456789% |                12.3456789% |                12.3456789% |                12.3456789%
      12.3457bp |            12.3456789bp |               0.123456789% |               12.3456789bp |               0.123456789%
       1.01727x |               1.017275x |                  101.7275% |                  1.017275x |                  101.7275%
       1.00418x |               1.004175x |                  100.4175% |                  1.004175x |                  100.4175%
    1.276E-10bp |          1.276001E-10bp |              1.276001E-12% |             1.276001E-10bp |              1.276001E-12%
     1.394E+13x |         13940020000000x |          1394002000000000% |            13940020000000x |          1394002000000000%
     1.394E+14x |           1.394002E+14x |              1.394002E+16% |           139400200000000x |         13940020000000000%
     1.394E+15x |           1.394002E+15x |              1.394002E+17% |              1.394002E+15x |              1.394002E+17%
       15.1235x |         15.12347130211x |            1512.347130211% |            15.12347130211x |            1512.347130211%
       15.1235x |         15.12347130211x |            1512.347130211% |            15.12347130211x |            1512.347130211%
       1.2E+09x |             1200000000x |              120000000000% |                1200000000x |              120000000000%
           7.3% |                    7.3% |                       7.3% |                       7.3% |                       7.3%
     1.39E-08bp |               1.39E-8bp |                  1.39E-10% |                  1.39E-8bp |                  1.39E-10%
           15bp |                    15bp |                      0.15% |                       15bp |                      0.15%
    |}];
  Quickcheck.test Float.gen_finite ~trials:13 ~sexp_of:Float.sexp_of_t ~f:(fun x ->
    f (Percent.of_percentage x));
  [%expect
    {|
    -3.95086E-282bp | -3.9508629434578E-282bp |     -3.9508629434578E-284% | -3.9508629434577648E-282bp |  -3.9508629434577648E-284%
          -17.1388% |       -17.138797521591% |          -17.138797521591% |       -17.138797521591187% |       -17.138797521591187%
               2.5% |                    2.5% |                       2.5% |                       2.5% |                       2.5%
     -2.27374E-11bp |  -2.2737367533735E-11bp |      -2.2737367533735E-13% |  -2.2737367533735293E-11bp |   -2.2737367533735293E-13%
           4.40104x |        4.4010383674822x |           440.10383674822% |        4.4010383674821787x |        440.10383674821787%
     -3.38108E+270x |  -3.3810849992683E+270x |     -3.3810849992683E+272% |  -3.3810849992682578E+270x |  -3.3810849992682578E+272%
       2.02299E+14x |    2.0229874165757E+14x |       2.0229874165757E+16% |        202298741657569.28x |         20229874165756928%
       0.00217557bp |    2.1755695343018E-3bp |        2.1755695343018E-5% |    2.1755695343017578E-3bp |     2.1755695343017578E-5%
           84.0552% |         84.05517578125% |            84.05517578125% |            84.05517578125% |            84.05517578125%
           7.44064x |        7.4406372011417x |           744.06372011417% |        7.4406372011416533x |        744.06372011416533%
     1.96589E-316bp |  1.9658872048023E-316bp |      1.9658872048023E-318% |    1.96588720480232E-316bp |     1.96588720480232E-318%
            3.125bp |       3.1249999999773bp |         0.031249999999773% |       3.1249999999772627bp |      0.031249999999772627%
          -2.18305x |       -2.1830518341064x |          -218.30518341064% |       -2.1830518341064451x |       -218.30518341064451%
    |}];
  Quickcheck.test Float.gen_finite ~trials:1_000 ~sexp_of:Float.sexp_of_t ~f:(fun x ->
    (* check for accurate round trip *)
    let assert_rt x ~f ~g =
      [%test_eq: float] x (x |> Percent.of_mult |> f |> g |> Percent.to_mult)
    in
    (* check if the precision loss due to the round trip is no more than 2 ulps *)
    let assert_close x ~f ~g =
      let z = x |> Percent.of_mult |> f |> g |> Percent.to_mult in
      let x = Float.to_int64_preserve_order_exn x in
      let z = Float.to_int64_preserve_order_exn z in
      assert (Int64.(x - z <= of_int 2))
    in
    assert_rt x ~f:Percent.to_string_round_trippable ~g:Percent.of_string;
    (* test that [Stable.V3] and [Stable.V3.Always_percentage] sexp conversions are both
       round-trippable and inter-operable with each other *)
    assert_rt x ~f:Percent.Stable.V3.sexp_of_t ~g:Percent.Stable.V3.t_of_sexp;
    assert_rt
      x
      ~f:Percent.Stable.V3.sexp_of_t
      ~g:Percent.Stable.V3.Always_percentage.t_of_sexp;
    assert_rt
      x
      ~f:Percent.Stable.V3.Always_percentage.sexp_of_t
      ~g:Percent.Stable.V3.t_of_sexp;
    assert_rt
      x
      ~f:Percent.Stable.V3.Always_percentage.sexp_of_t
      ~g:Percent.Stable.V3.Always_percentage.t_of_sexp;
    let y = sprintf "%.15g" x |> Float.of_string in
    if Float.(abs y < max_finite_value /. 100.)
    then (
      (* [(to|of)_(percentage|bp)_slow_more_accurate] work by shifting the decimal point
         in the string representation of the underlying float.  Since floats have
         guaranteed 15 decimal digits of precision across their range, we take [y] to be
         [x] rounded to 15 decimal digits and test that [y] round-trips accurately (while
         [x] might not). *)
      assert_rt
        y
        ~f:Percent.to_percentage_slow_more_accurate
        ~g:Percent.of_percentage_slow_more_accurate;
      (* Also test that if we mix up simple [(to|of)_percentage] functions with their
         [slow_more_accurate] versions, the results are not off by more than two ulps. *)
      assert_close x ~f:Percent.to_percentage_slow_more_accurate ~g:Percent.of_percentage;
      assert_close x ~f:Percent.to_percentage ~g:Percent.of_percentage_slow_more_accurate;
      assert_close x ~f:Percent.to_percentage ~g:Percent.of_percentage);
    if Float.(abs y < max_finite_value /. 10000.)
    then (
      assert_rt y ~f:Percent.to_bp_slow_more_accurate ~g:Percent.of_bp_slow_more_accurate;
      assert_close x ~f:Percent.to_bp_slow_more_accurate ~g:Percent.of_bp;
      assert_close x ~f:Percent.to_bp ~g:Percent.of_bp_slow_more_accurate;
      assert_close x ~f:Percent.to_bp ~g:Percent.of_bp))
;;

let%expect_test "Percent.Map serialization" =
  let pm =
    [ Percent.of_string "315.01%", 1
    ; Percent.of_string "3.638%", 2
    ; Percent.of_string "12.3456789bp", 3
    ; Percent.of_mult Float.pi, 4
    ]
    |> Percent.Map.of_alist_exn
  in
  printf !"%{sexp:int Percent.Map.t}\n" pm;
  printf !"%{sexp:int Percent.Stable.V3.Map.t}\n" pm;
  [%expect
    {|
    ((12.3457bp 3) (3.638% 2) (3.14159x 4) (3.1501x 1))
    ((12.3456789bp 3) (3.638% 2) (3.1415926535897931x 4) (3.1501x 1))
    |}]
;;

let%expect_test "nans and infs" =
  [ Float.nan; Float.infinity; Float.neg_infinity ]
  |> List.iter ~f:(fun p ->
       let p = Percent.of_mult p in
       let s1 = Percent.to_string p in
       let s2 = Percent.Stable.V3.to_string p in
       printf "%s | %s\n" s1 s2;
       let p1 = Percent.of_string_allow_nan_and_inf s1 in
       let p2 = Percent.Stable.V3.of_string_allow_nan_and_inf s2 in
       assert (Percent.((is_nan p1 && is_nan p2) || p1 = p2)));
  [%expect {|
    NANbp | NANx
    INFx | INFx
    -INFx | -INFx
    |}];
  [ "nanx"
  ; "NANbp"
  ; "NaN%"
  ; "+nanbp"
  ; "-nan%"
  ; "-nAnx"
  ; "infx"
  ; "InFiNiTybp"
  ; "inf%"
  ; "-INFx"
  ; "+INFx"
  ; "-iNfiNiTybp"
  ; "-Inf%"
  ]
  |> List.iter ~f:(fun s ->
       let p1 = Percent.of_string_allow_nan_and_inf s in
       let p2 = Percent.Stable.V3.of_string_allow_nan_and_inf s in
       assert (Percent.((is_nan p1 && is_nan p2) || p1 = p2)))
;;

let%expect_test "slow_more_accurate" =
  (* 70.18%: An example where [of_percentage] and [of_bp] both introduce ugly-looking
     rounding errors. *)
  let x = 70.18 in
  [ Percent.of_percentage
  ; Percent.of_bp
  ; Percent.of_percentage_slow_more_accurate
  ; Percent.of_bp_slow_more_accurate
  ]
  |> List.iter ~f:(fun f -> printf !"%{Percent#round_trippable}\n" (f x));
  [%expect
    {|
    70.180000000000009%
    70.180000000000008bp
    70.18%
    70.18bp
    |}];
  (* 57.2%: An example where [to_percentage] and [to_bp] both introduce ugly-looking
     rounding errors. *)
  let p = Percent.of_mult 0.572 in
  [ Percent.to_percentage
  ; Percent.to_bp
  ; Percent.to_percentage_slow_more_accurate
  ; Percent.to_bp_slow_more_accurate
  ]
  |> List.iter ~f:(fun f -> printf !"%{Float}\n" (f p));
  [%expect {|
    57.199999999999996
    5719.9999999999991
    57.2
    5720.
    |}];
  (* Find some more examples where [to_percentage] and [to_bp] introduce errors. *)
  for i = 0 to 100 do
    let p = Percent.of_mult (float i /. 100.) in
    if Float.( <> ) (Percent.to_percentage p) (Percent.to_percentage_slow_more_accurate p)
       && Float.( <> ) (Percent.to_bp p) (Percent.to_bp_slow_more_accurate p)
    then printf !"%{Float} %{Float}\n" (Percent.to_percentage p) (Percent.to_bp p)
  done;
  [%expect
    {|
    7.0000000000000009 700.00000000000011
    14.000000000000002 1400.0000000000002
    28.000000000000004 2800.0000000000005
    56.000000000000007 5600.0000000000009
    56.999999999999993 5699.9999999999991
    |}]
;;

let%expect_test "parse errors" =
  List.iter
    [ "+34.27"
    ; " \n +12%"
    ; "+ 12%"
    ; "  +-31%"
    ; "-+17bp"
    ; "ZZZbp"
    ; "++11.2%"
    ; ""
    ; "%"
    ; "-x"
    ; "+%"
    ; "ee%"
    ; "..bp"
    ; "nan%"
    ; "+inf%"
    ; "-infinity%"
    ]
    ~f:(fun s ->
    let s1 =
      try Percent.of_string s |> Percent.to_string_round_trippable with
      | exn -> Exn.to_string_mach exn
    in
    printf "%s\n====\n" s1);
  [%expect
    {|
    (Failure"Percent.of_string: must end in x, %, or bp: +34.27")
    ====
    12%
    ====
    (Failure"Unexpected character when parsing Percent.t: ' ' in '+ 12'")
    ====
    (Of_sexp_error"float_of_sexp: (Failure float_of_string)"(invalid_sexp +-.31))
    ====
    (Of_sexp_error"float_of_sexp: (Failure float_of_string)"(invalid_sexp -0.0+17))
    ====
    (Failure"Unexpected character when parsing Percent.t: 'Z' in 'ZZZ'")
    ====
    (Of_sexp_error"float_of_sexp: (Failure float_of_string)"(invalid_sexp ++.112))
    ====
    (Failure"Percent.of_string: must end in x, %, or bp: ")
    ====
    (Of_sexp_error"float_of_sexp: (Failure float_of_string)"(invalid_sexp""))
    ====
    (Of_sexp_error"float_of_sexp: (Failure float_of_string)"(invalid_sexp -))
    ====
    (Of_sexp_error"float_of_sexp: (Failure float_of_string)"(invalid_sexp +))
    ====
    (Failure"Error parsing Percent.t: too many Es in 'ee'")
    ====
    (Failure"Error parsing Percent.t: too many decimal points in '..'")
    ====
    (Failure"Unexpected character when parsing Percent.t: 'n' in 'nan'")
    ====
    (Failure"Unexpected character when parsing Percent.t: 'i' in '+inf'")
    ====
    (Failure"Unexpected character when parsing Percent.t: 'i' in '-infinity'")
    ====
    |}]
;;
