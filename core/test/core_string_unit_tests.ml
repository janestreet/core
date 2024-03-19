open! Core
open Expect_test_helpers_core

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

let%expect_test "Test unicode string quickcheck generators" =
  let test (module Utf : String.Utf) =
    let test_can_generate =
      Quickcheck.test_can_generate
        [%quickcheck.generator: Utf.t]
        ~sexp_of:[%sexp_of: Utf.t]
    in
    test_can_generate ~f:(fun utf -> String.is_empty (Utf.to_string utf));
    test_can_generate ~f:(fun utf -> String.length (Utf.to_string utf) > 10);
    Quickcheck.test_distinct_values
      [%quickcheck.generator: Utf.t]
      ~sexp_of:[%sexp_of: Utf.t]
      ~trials:1_000
      ~distinct_values:500
      ~compare:Utf.compare;
    Quickcheck.iter [%quickcheck.generator: Utf.t] ~f:(fun utf ->
      assert (Utf.is_valid (Utf.to_string utf)))
  in
  test (module String.Utf8);
  test (module String.Utf16le);
  test (module String.Utf16be);
  test (module String.Utf32le);
  test (module String.Utf32be);
  [%expect {| |}]
;;

(* Test [String.split] and [String.Utf*.split] using the same tests. *)
let%test_module "split" =
  (module struct
    module type Elt = sig
      type t [@@deriving equal, quickcheck ~generator ~shrinker, sexp_of]
    end

    module type Str = sig
      type elt
      type t [@@deriving equal, quickcheck ~generator ~shrinker, sexp_of]

      val mem : t -> elt -> bool
      val of_list : elt list -> t
      val to_list : t -> elt list
      val split : t -> on:elt -> t list
    end

    let make_examples
      (type t elt)
      (module Str : Str with type t = t and type elt = elt)
      ~of_char
      =
      let empty = Str.of_list [] in
      let delimiter = of_char '\n' in
      let chars size = Str.of_list (List.init size ~f:(Fn.const (of_char 'a'))) in
      [ [ empty ]
      ; [ empty; empty ]
      ; [ empty; empty; empty ]
      ; [ empty; empty; empty; empty ]
      ; [ empty; chars 1; empty ]
      ; [ chars 1; chars 1 ]
      ; [ chars 1; empty; chars 1 ]
      ; [ chars 2 ]
      ; [ chars 3 ]
      ; [ empty; chars 2; empty ]
      ; [ empty; empty; chars 1; empty; empty ]
      ; [ chars 1; empty; chars 1; empty; chars 1 ]
      ]
      |> List.map ~f:(fun parts -> parts, delimiter)
    ;;

    let concat
      (type t elt)
      (module Str : Str with type t = t and type elt = elt)
      (parts : t list)
      ~(delimiter : elt)
      =
      parts
      |> List.map ~f:Str.to_list
      |> List.intersperse ~sep:[ delimiter ]
      |> List.concat
      |> Str.of_list
    ;;

    let test_split
      (type t elt)
      (module Elt : Elt with type t = elt)
      (module Str : Str with type t = t and type elt = elt)
      ~examples
      =
      let open Base_quickcheck in
      let module M = struct
        (* Generate parts to join and a delimiter to join them with. *)
        type t = Str.t list * Elt.t [@@deriving quickcheck ~generator ~shrinker, sexp_of]

        (* Skip values split that should not round-trip: empty lists, and parts
           containing the delimiter. *)
        let is_valid (parts, delimiter) =
          (not (List.is_empty parts))
          && List.for_all parts ~f:(fun str -> not (Str.mem str delimiter))
        ;;

        let quickcheck_generator = Generator.filter quickcheck_generator ~f:is_valid
      end
      in
      (* Now run the tests of [split] behavior. *)
      quickcheck_m
        [%here]
        (module M)
        ~examples
        ~f:(fun (parts, delimiter) ->
          let str = concat (module Str) parts ~delimiter in
          let split =
            (* Re-split the whole [str] into parts. *)
            Str.split str ~on:delimiter
          in
          (* Test that we got back where we started. *)
          require_equal
            [%here]
            (module struct
              type t = Str.t list [@@deriving equal, sexp_of]
            end)
            split
            parts)
    ;;

    let%expect_test "round-trip via concat" =
      let module String = struct
        include String

        let mem t c = mem t c
      end
      in
      let examples = make_examples (module String) ~of_char:Fn.id in
      List.iter examples ~f:(fun (parts, delimiter) ->
        printf "\"%s\"\n" (String.escaped (concat (module String) parts ~delimiter)));
      (* Show the examples that we ensure are tested. *)
      [%expect
        {|
        ""
        "\n"
        "\n\n"
        "\n\n\n"
        "\na\n"
        "a\na"
        "a\n\na"
        "aa"
        "aaa"
        "\naa\n"
        "\n\na\n\n"
        "a\n\na\n\na"
        |}];
      test_split (module Char) (module String) ~examples;
      [%expect {| |}];
      let test_split utf_module =
        let examples = make_examples utf_module ~of_char:Uchar.of_char in
        test_split (module Uchar) utf_module ~examples
      in
      test_split (module String.Utf8);
      [%expect {| |}];
      test_split (module String.Utf16le);
      [%expect {| |}];
      test_split (module String.Utf16be);
      [%expect {| |}];
      test_split (module String.Utf32le);
      [%expect {| |}];
      test_split (module String.Utf32be);
      [%expect {| |}]
    ;;
  end)
;;
