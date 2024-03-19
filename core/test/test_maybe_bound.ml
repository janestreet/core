open! Core
open! Import
open! Maybe_bound

let%test_module "[As_lower_bound.compare], [As_upper_bound.compare]" =
  (module struct
    module Ord = struct
      type t =
        | O1
        | O2
        | O3
      [@@deriving compare, enumerate, sexp_of]
    end

    type t = Ord.t Maybe_bound.t [@@deriving enumerate, sexp_of]
    type lower = Ord.t As_lower_bound.t [@@deriving compare]
    type upper = Ord.t As_upper_bound.t [@@deriving compare]

    let sorted_lower = List.sort all ~compare:[%compare: lower]
    let sorted_upper = List.sort all ~compare:[%compare: upper]

    let%expect_test _ =
      print_s [%sexp (sorted_lower : t list)];
      [%expect
        {|
        (Unbounded
          (Incl O1)
          (Excl O1)
          (Incl O2)
          (Excl O2)
          (Incl O3)
          (Excl O3))
        |}];
      print_s [%sexp (sorted_upper : t list)];
      [%expect
        {|
        ((Excl O1)
         (Incl O1)
         (Excl O2)
         (Incl O2)
         (Excl O3)
         (Incl O3)
         Unbounded)
        |}]
    ;;

    let%test _ = List.is_sorted_strictly sorted_lower ~compare:[%compare: lower]
    let%test _ = List.is_sorted_strictly sorted_upper ~compare:[%compare: upper]

    let test_compare compare bound1 bound2 ok =
      let comparison = compare bound1 bound2 in
      require
        [%here]
        (ok comparison)
        ~if_false_then_print_s:
          (lazy
            [%message
              "comparison produced unexpected value"
                (bound1 : t)
                (bound2 : t)
                (comparison : int)])
    ;;

    let test_sorted compare sorted =
      let sorted = Array.of_list sorted in
      let len = Array.length sorted in
      for i = 0 to len - 1 do
        test_compare compare sorted.(i) sorted.(i) (fun c -> c = 0);
        for j = i + 1 to len - 1 do
          test_compare compare sorted.(i) sorted.(j) (fun c -> c < 0);
          test_compare compare sorted.(j) sorted.(i) (fun c -> c > 0)
        done
      done
    ;;

    let%expect_test _ = test_sorted [%compare: lower] sorted_lower
    let%expect_test _ = test_sorted [%compare: upper] sorted_upper

    let%expect_test "relationship between [As_lower_bound.compare] and [is_lower_bound]" =
      List.iter all ~f:(fun t1 ->
        List.iter all ~f:(fun t2 ->
          List.iter Ord.all ~f:(fun a ->
            let compare = Ord.compare in
            if As_lower_bound.compare compare t1 t2 <= 0
               && is_lower_bound t2 ~of_:a ~compare
            then require [%here] (is_lower_bound t1 ~of_:a ~compare))))
    ;;

    let%expect_test "relationship between [As_upper_bound.compare] and [is_upper_bound]" =
      List.iter all ~f:(fun t1 ->
        List.iter all ~f:(fun t2 ->
          List.iter Ord.all ~f:(fun a ->
            let compare = Ord.compare in
            if As_upper_bound.compare compare t1 t2 <= 0
               && is_upper_bound t1 ~of_:a ~compare
            then require [%here] (is_upper_bound t2 ~of_:a ~compare))))
    ;;
  end)
;;
