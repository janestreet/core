open OUnit;;
open Core

let s1 = Set.Poly.of_list ["a"; "b"; "c"; "d"]
(*let m2 = Map.of_alist ["a",1; "c",-3; "d",4; "e",5]*)

let test =
  "pSet" >:::
  [ "sexp" >::
    (fun () ->
       let s = "(a b c d)" in
       let s1' = Set.Poly.t_of_sexp string_of_sexp (Sexp.of_string s) in
       "of_sexp1" @? (Set.equal s1' s1);
       let s_dup = "(a b a d)" in
       let s_dup = Sexp.of_string s_dup in
       assert_raises
         (Sexplib.Conv.Of_sexp_error (
            Failure "Set.t_of_sexp: duplicate element in set",
            (sexp_of_string "a")))
         (fun () -> Set.Poly.t_of_sexp string_of_sexp s_dup)
    );
  ]
