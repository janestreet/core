open OUnit;;
open Core.Std

let test =
  "common" >:::
    [ "% and /%" >::
        (fun () ->
           let gen_int_pair () = (Quickcheck.uig (), abs (Quickcheck.uig ())) in
           let modulus_invariant (a, b) =
             let r = a % b in
             let q = a /% b in
             r >= 0 && a = q * b + r
           in
           Quickcheck.laws_exn "modulus invariant"
             1000 gen_int_pair modulus_invariant
        );

      "memoize" >::
        (fun () ->
           let f x = x * x in
           let memo_f = Memo.general f in
           Quickcheck.laws_exn "memoize"
             1000 Quickcheck.uig (fun i -> f i = memo_f i)
        );

      "nan" >::
        (fun () ->
          let nan = 0. /. 0. in
          "fmin1" @? (Float.is_nan (Float.min 1. nan));
          "fmin2" @? (Float.is_nan (Float.min nan 0.));
          "fmin3" @? (Float.is_nan (Float.min nan nan));
          "fmax1" @? (Float.is_nan (Float.max 1. nan));
          "fmax2" @? (Float.is_nan (Float.max nan 0.));
          "fmax3" @? (Float.is_nan (Float.max nan nan));
          "fmin_inan1" @? (1. = (Float.min_inan 1. nan));
          "fmin_inan2" @? (0. = (Float.min_inan nan 0.));
          "fmin_inan3" @? (Float.is_nan (Float.min_inan nan nan));
          "fmax_inan1" @? (1. = (Float.max_inan 1. nan));
          "fmax_inan2" @? (0. = (Float.max_inan nan 0.));
          "fmax_inan3" @? (Float.is_nan (Float.max_inan nan nan));
        );

      "round" >::
        (fun () ->
          "zero" @? (Float.iround_nearest_exn 0.2 = 0);
          "negative zero" @? (Float.iround_nearest_exn (-0.2) = 0);
          "positive" @? (Float.iround_nearest_exn 3.4 = 3);
          "negative" @? (Float.iround_nearest_exn (-3.4) = -3);
        );

    ]


