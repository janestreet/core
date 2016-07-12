open Core.Std
open Quickcheck

let%test_unit "count vs length" =
  Quickcheck.test
    (* (\* Initial example that fails on NaN: *\)
     * (List.gen Float.gen) *)
    (* Working example that filters out NaN: *)
    (List.gen (Float.gen_between
                 ~lower_bound:(Incl Float.neg_infinity)
                 ~upper_bound:(Incl Float.infinity)
                 ~nan:Without))
    (* (\* Alternate version of filtering out NaN: *\)
     * (List.gen (Generator.filter Float.gen ~f:(Fn.non Float.is_nan))) *)
    (* (\* Simplest version: *\)
     * (List.gen Float.gen_without_nan) *)
    ~sexp_of:[%sexp_of: float list]
    ~f:(fun float_list ->
      [%test_result: int]
        (List.count float_list ~f:(fun x -> x = x))
        ~expect:(List.length float_list))

let list_gen elt_gen =
  Generator.(recursive (fun f ~size ->
    if size = 0
    then return []
    else begin
      elt_gen          >>= fun head ->
      f ~size:(size-1) >>= fun tail ->
      return (head :: tail)
    end))

let sexp_gen =
  Generator.(recursive (fun f ~size ->
    if size = 0
    then (String.gen                  >>| fun atom -> Sexp.Atom atom)
    else (list_gen (f ~size:(size-1)) >>| fun list -> Sexp.List list)))
