open! Core
open! Import
open! Month

let%test_module "Month.V1" =
  (module Stable_unit_test.Make (struct
    include Stable.V1

    let equal t1 t2 = Int.( = ) 0 (compare t1 t2)

    let tests =
      let module V = Variant in
      let c rank sexp bin_io tests variant =
        assert (Int.( = ) variant.V.rank rank);
        (variant.V.constructor, sexp, bin_io) :: tests
      in
      Variants.fold
        ~init:[]
        ~jan:(c 0 "Jan" "\000")
        ~feb:(c 1 "Feb" "\001")
        ~mar:(c 2 "Mar" "\002")
        ~apr:(c 3 "Apr" "\003")
        ~may:(c 4 "May" "\004")
        ~jun:(c 5 "Jun" "\005")
        ~jul:(c 6 "Jul" "\006")
        ~aug:(c 7 "Aug" "\007")
        ~sep:(c 8 "Sep" "\008")
        ~oct:(c 9 "Oct" "\009")
        ~nov:(c 10 "Nov" "\010")
        ~dec:(c 11 "Dec" "\011")
    ;;
  end))
;;

let%test _ = Int.( = ) (List.length all) 12

let%test_unit _ =
  [%test_result: t]
    (List.fold (List.tl_exn all) ~init:Jan ~f:(fun last cur ->
       assert (Int.( = ) (compare last cur) (-1));
       cur))
    ~expect:Dec
;;

let%test _ = Set.equal (Set.of_list [ Jan ]) (Set.t_of_sexp Sexp.(List [ Atom "0" ]))
let%test _ = Poly.( = ) (sexp_of_t Jan) (Sexp.Atom "Jan")
let%test _ = Jan = t_of_sexp (Sexp.Atom "Jan")
let%test _ = Exn.does_raise (fun () -> t_of_sexp (Sexp.Atom "0"))
let%test _ = shift Jan 12 = Jan
let%test _ = shift Jan (-12) = Jan
let%test _ = shift Jan 16 = May
let%test _ = shift Jan (-16) = Sep
let%test _ = shift Sep 1 = Oct
let%test _ = shift Sep (-1) = Aug

let%expect_test "validate sexp grammar" =
  Sexp_grammar_validation.validate_grammar (module Month) |> require_ok [%here];
  [%expect
    {|
    (Variant
     ((case_sensitivity Case_sensitive_except_first_character)
      (clauses
       ((No_tag ((name Jan) (clause_kind Atom_clause)))
        (No_tag ((name Feb) (clause_kind Atom_clause)))
        (No_tag ((name Mar) (clause_kind Atom_clause)))
        (No_tag ((name Apr) (clause_kind Atom_clause)))
        (No_tag ((name May) (clause_kind Atom_clause)))
        (No_tag ((name Jun) (clause_kind Atom_clause)))
        (No_tag ((name Jul) (clause_kind Atom_clause)))
        (No_tag ((name Aug) (clause_kind Atom_clause)))
        (No_tag ((name Sep) (clause_kind Atom_clause)))
        (No_tag ((name Oct) (clause_kind Atom_clause)))
        (No_tag ((name Nov) (clause_kind Atom_clause)))
        (No_tag ((name Dec) (clause_kind Atom_clause)))))))
    |}]
;;
