open OUnit;;
open Core

let test =
  "common" >:::
  [ "% and /%" >::
    (fun () ->
       let gen_int_pair () =
         (Quickcheck_deprecated.uig (), abs (Quickcheck_deprecated.uig ()))
       in
       let modulus_invariant (a, b) =
         let r = a % b in
         let q = a /% b in
         r >= 0 && a = q * b + r
       in
       Quickcheck_deprecated.laws_exn "modulus invariant"
         1000 gen_int_pair modulus_invariant
    );

    "memoize" >::
    (fun () ->
       let f x = x * x in
       let memo_f = Memo.general f in
       Quickcheck_deprecated.laws_exn "memoize"
         1000 Quickcheck_deprecated.uig (fun i -> f i = memo_f i)
    );

    "nan" >::
    (fun () ->
       "fmin1" @? (Float.is_nan (Float.min 1. Float.nan));
       "fmin2" @? (Float.is_nan (Float.min Float.nan 0.));
       "fmin3" @? (Float.is_nan (Float.min Float.nan Float.nan));
       "fmax1" @? (Float.is_nan (Float.max 1. Float.nan));
       "fmax2" @? (Float.is_nan (Float.max Float.nan 0.));
       "fmax3" @? (Float.is_nan (Float.max Float.nan Float.nan));
       "fmin_inan1" @? (1. = (Float.min_inan 1. Float.nan));
       "fmin_inan2" @? (0. = (Float.min_inan Float.nan 0.));
       "fmin_inan3" @? (Float.is_nan (Float.min_inan Float.nan Float.nan));
       "fmax_inan1" @? (1. = (Float.max_inan 1. Float.nan));
       "fmax_inan2" @? (0. = (Float.max_inan Float.nan 0.));
       "fmax_inan3" @? (Float.is_nan (Float.max_inan Float.nan Float.nan));
    );

    "round" >::
    (fun () ->
       "zero" @? (Float.iround_exn ~dir:`Nearest 0.2 = 0);
       "negative zero" @? (Float.iround_exn ~dir:`Nearest (-0.2) = 0);
       "positive" @? (Float.iround_exn ~dir:`Nearest 3.4 = 3);
       "negative" @? (Float.iround_exn ~dir:`Nearest (-3.4) = -3);
    );

  ]


