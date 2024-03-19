open! Core
open! Import
open! Day_of_week

let num_days_in_week = 7

let%test_module "Day_of_week.V1" =
  (module Stable_unit_test.Make (struct
    include Stable.V1

    let equal = [%compare.equal: t]

    let tests =
      [ Sun, "SUN", "\000"
      ; Mon, "MON", "\001"
      ; Tue, "TUE", "\002"
      ; Wed, "WED", "\003"
      ; Thu, "THU", "\004"
      ; Fri, "FRI", "\005"
      ; Sat, "SAT", "\006"
      ]
    ;;
  end))
;;

let%test _ = List.is_sorted all ~compare

let%test "to_string_long output parses with of_string" =
  List.for_all all ~f:(fun d -> d = (to_string_long d |> of_string))
;;

let%test _ = Int.(num_days ~from:Mon ~to_:Tue = 1)
let%test _ = Int.(num_days ~from:Tue ~to_:Mon = 6)

let%test "num_days is inverse to shift" =
  let all_days = [ Sun; Mon; Tue; Wed; Thu; Fri; Sat ] in
  List.for_all (List.cartesian_product all_days all_days) ~f:(fun (from, to_) ->
    let i = num_days ~from ~to_ in
    Int.(0 <= i && i < num_days_in_week) && shift from i = to_)
;;

let%expect_test "validate sexp grammar" =
  Sexp_grammar_validation.validate_grammar (module Day_of_week) |> require_ok [%here];
  [%expect
    {|
    (Variant
     ((case_sensitivity Case_insensitive)
      (clauses
       ((Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name 0) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name 1) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name 2) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name 3) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name 4) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name 5) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name 6) (clause_kind Atom_clause))))))
        (No_tag ((name SUN) (clause_kind Atom_clause)))
        (No_tag ((name MON) (clause_kind Atom_clause)))
        (No_tag ((name TUE) (clause_kind Atom_clause)))
        (No_tag ((name WED) (clause_kind Atom_clause)))
        (No_tag ((name THU) (clause_kind Atom_clause)))
        (No_tag ((name FRI) (clause_kind Atom_clause)))
        (No_tag ((name SAT) (clause_kind Atom_clause)))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name Sunday) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name Monday) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name Tuesday) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name Wednesday) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name Thursday) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name Friday) (clause_kind Atom_clause))))))
        (Tag
         ((key sexp_grammar.completion-suggested)
          (value false)
          (grammar (No_tag ((name Saturday) (clause_kind Atom_clause))))))))))
    |}]
;;
