open Core.Std
open Quickcheck

TEST_UNIT "count vs length" =
  Quickcheck.test
    (* (\* Initial example that fails on NaN: *\)
     * Generator.(list float) *)
    (* Working example that filters out NaN: *)
    Generator.(list (float_between
                       ~lower_bound:(Incl Float.neg_infinity)
                       ~upper_bound:(Incl Float.infinity)
                       ~nan:Without))
    (* (\* Alternate version of filtering out NaN: *\)
     * Generator.(list (filter float ~f:(Fn.non Float.is_nan))) *)
    ~sexp_of:<:sexp_of< float list >>
    ~f:(fun float_list ->
      <:test_result< int >>
        (List.count float_list ~f:(fun x -> x = x))
        ~expect:(List.length float_list))

let list_gen elt_gen =
  Generator.(recursive (fun list_gen ->
    variant2 unit (tuple2 elt_gen list_gen)
    >>| function
    | `A () -> []
    | `B (head, tail) -> head :: tail))

let sexp_gen =
  Generator.(recursive (fun sexp_gen ->
    variant2 string (list_gen sexp_gen)
    >>| function
    | `A atom -> Sexp.Atom atom
    | `B list -> Sexp.List list))

