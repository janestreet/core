open! Core

let%test_unit "String.exists doesn't allocate" =
  let initial_words = Gc.minor_words () in
  assert (String.exists "FOOBAR" ~f:Char.is_uppercase);
  assert (not (String.exists "FOOBAR" ~f:Char.is_lowercase));
  let allocated = Gc.minor_words () - initial_words in
  [%test_result: int] allocated ~expect:0
;;

let%test_unit "String.for_all doesn't allocate" =
  let initial_words = Gc.minor_words () in
  assert (String.for_all "FOOBAR" ~f:Char.is_uppercase);
  assert (not (String.for_all "FOOBAR" ~f:Char.is_lowercase));
  let allocated = Gc.minor_words () - initial_words in
  [%test_result: int] allocated ~expect:0
;;

let%test_unit "String.is_suffix doesn't allocate" =
  let initial_words = Gc.minor_words () in
  assert (String.is_suffix "FOOBAR" ~suffix:"BAR");
  assert (not (String.is_suffix "FOOBAR" ~suffix:"BUZ"));
  let allocated = Gc.minor_words () - initial_words in
  [%test_result: int] allocated ~expect:0
;;

let%test_unit "String.is_prefix doesn't allocate" =
  let initial_words = Gc.minor_words () in
  assert (String.is_prefix "FOOBAR" ~prefix:"FOO");
  assert (not (String.is_prefix "FOOBAR" ~prefix:"FUZ"));
  let allocated = Gc.minor_words () - initial_words in
  [%test_result: int] allocated ~expect:0
;;

let%test_unit "String.Caseless.compare is consistent with String.compare of lowercase" =
  let quickcheck_generator =
    Quickcheck.Generator.tuple2 String.quickcheck_generator String.quickcheck_generator
  in
  let sexp_of = [%sexp_of: string * string] in
  (* make sure we can generate strings that are identical *)
  Quickcheck.test_can_generate quickcheck_generator ~sexp_of ~f:(fun (x, y) ->
    String.equal x y);
  (* make sure we can generate strings that are the only the same after case folding *)
  Quickcheck.test_can_generate quickcheck_generator ~sexp_of ~f:(fun (x, y) ->
    String.equal (String.lowercase x) (String.lowercase y) && not (String.equal x y));
  (* make sure we can generate a prefix of the other string, after case folding *)
  Quickcheck.test_can_generate quickcheck_generator ~sexp_of ~f:(fun (x, y) ->
    String.is_prefix (String.lowercase x) ~prefix:(String.lowercase y)
    && (not (String.equal (String.lowercase x) (String.lowercase y)))
    && not (String.is_prefix x ~prefix:y));
  (* ... and in the other direction *)
  Quickcheck.test_can_generate quickcheck_generator ~sexp_of ~f:(fun (x, y) ->
    String.is_prefix (String.lowercase y) ~prefix:(String.lowercase x)
    && (not (String.equal (String.lowercase y) (String.lowercase x)))
    && not (String.is_prefix y ~prefix:x));
  (* now make sure our comparisons work *)
  Quickcheck.test quickcheck_generator ~sexp_of ~f:(fun (x, y) ->
    [%test_result: int]
      (String.Caseless.compare x y)
      ~expect:(String.compare (String.lowercase x) (String.lowercase y)))
;;
