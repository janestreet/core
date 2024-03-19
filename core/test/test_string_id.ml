open! Core
open! Import
open! String_id
open! Int.Replace_polymorphic_compare

module Unit_tests_util = struct
  let to_bin_str t ~bin_writer_t ~bin_size_t =
    let write = bin_writer_t.Bin_prot.Type_class.write in
    let size = bin_size_t t in
    let buf = Bigstring.create size in
    let len = write buf ~pos:0 t in
    assert (len = size);
    Bigstring.to_string buf
  ;;

  let string_to_bin_str t =
    let bin_writer_t = String.bin_writer_t in
    let bin_size_t = String.bin_size_t in
    to_bin_str t ~bin_writer_t ~bin_size_t
  ;;
end

(* Test [Make_with_validate_without_pretty_printer]. *)

(* Even integers are the only valid identifiers *)
module Even_int_id =
  Make_with_validate_without_pretty_printer
    (struct
      let module_name = "Even_int_id"

      let validate s =
        if Int.of_string s % 2 <> 0
        then Or_error.error_s [%message "Not a valid Even_int_id" ~_:(s : string)]
        else Ok ()
      ;;

      let include_default_validation = true
    end)
    ()

let even_int_id_to_bin_str t =
  let bin_writer_t = Even_int_id.bin_writer_t in
  let bin_size_t = Even_int_id.bin_size_t in
  Unit_tests_util.to_bin_str t ~bin_writer_t ~bin_size_t
;;

let even_int_id_of_bin_str str =
  Even_int_id.bin_reader_t.Bin_prot.Type_class.read
    (Bigstring.of_string str)
    ~pos_ref:(ref 0)
;;

let%test_unit "string roundtrip" =
  [%test_result: string]
    (Even_int_id.of_string "14" |> Even_int_id.to_string)
    ~expect:"14"
;;

let%test_unit "sexp roundtrip" =
  [%test_result: string]
    (Even_int_id.t_of_sexp (Sexp.of_string "14")
     |> Even_int_id.sexp_of_t
     |> Sexp.to_string)
    ~expect:"14"
;;

let%test_unit "bin_prot roundrip" =
  [%test_result: string]
    (Even_int_id.of_string "14"
     |> even_int_id_to_bin_str
     |> even_int_id_of_bin_str
     |> Even_int_id.to_string)
    ~expect:"14"
;;

let print_error or_error =
  match or_error with
  | Ok _ -> printf "<OK>"
  | Error error -> Error.to_string_hum error |> printf "%s"
;;

let%expect_test "of string failure" =
  Or_error.try_with (fun () -> Even_int_id.of_string "15") |> print_error;
  [%expect {| (Invalid_argument "(\"Not a valid Even_int_id\"15)") |}]
;;

let%expect_test "of sexp failure" =
  Or_error.try_with (fun () -> Even_int_id.t_of_sexp (Sexp.of_string "15")) |> print_error;
  [%expect {| (Of_sexp_error "(\"Not a valid Even_int_id\"15)" (invalid_sexp 15)) |}]
;;

let%expect_test "set of sexp failure" =
  Or_error.try_with (fun () -> Even_int_id.Set.t_of_sexp (Sexp.of_string "(15)"))
  |> print_error;
  [%expect {| (Of_sexp_error "(\"Not a valid Even_int_id\"15)" (invalid_sexp 15)) |}]
;;

let%expect_test "of bin prot failure" =
  Or_error.try_with (fun () ->
    even_int_id_of_bin_str (Unit_tests_util.string_to_bin_str "15"))
  |> print_error;
  [%expect {| (Invalid_argument "(\"Not a valid Even_int_id\"15)") |}]
;;

module M =
  Make_without_pretty_printer
    (struct
      let module_name = "test"
    end)
    ()

let%test_unit "string roundtrip" =
  [%test_result: string] (M.of_string "FOOBAR" |> M.to_string) ~expect:"FOOBAR"
;;

let%test_unit "sexp roundtrip" =
  [%test_result: string]
    (M.t_of_sexp (Sexp.of_string "FOOBAR") |> M.sexp_of_t |> Sexp.to_string)
    ~expect:"FOOBAR"
;;

let string_id_to_bin_str t =
  let bin_writer_t = M.bin_writer_t in
  let bin_size_t = M.bin_size_t in
  Unit_tests_util.to_bin_str t ~bin_writer_t ~bin_size_t
;;

let string_id_of_bin_str str =
  M.bin_reader_t.Bin_prot.Type_class.read (Bigstring.of_string str) ~pos_ref:(ref 0)
;;

let%test_unit "bin_prot roundrip" =
  [%test_result: string]
    (M.of_string "FOOBAR" |> string_id_to_bin_str |> string_id_of_bin_str |> M.to_string)
    ~expect:"FOOBAR"
;;

let%test_unit "whitespace inside is OK" =
  [%test_result: string] (M.of_string "FOO  BAR" |> M.to_string) ~expect:"FOO  BAR"
;;

let print_error or_error =
  match or_error with
  | Ok _ -> printf "<OK>"
  | Error error -> Error.to_string_hum error |> printf "%s"
;;

let%expect_test "of string failure - empty string" =
  Or_error.try_with (fun () -> M.of_string "") |> print_error;
  [%expect {| (Invalid_argument "'' is not a valid test because it is empty") |}]
;;

let%expect_test "of string failure - whitespace after" =
  Or_error.try_with (fun () -> M.of_string "FOOBAR ") |> print_error;
  [%expect
    {|
    (Invalid_argument
     "'FOOBAR ' is not a valid test because it has whitespace on the edge")
    |}]
;;

let%expect_test "of string failure - whitespace before" =
  Or_error.try_with (fun () -> M.of_string " FOOBAR") |> print_error;
  [%expect
    {|
    (Invalid_argument
     "' FOOBAR' is not a valid test because it has whitespace on the edge")
    |}]
;;

let%expect_test "of sexp failure" =
  Or_error.try_with (fun () -> M.t_of_sexp (Sexp.of_string "\"FOOBAR \"")) |> print_error;
  [%expect
    {|
    (Of_sexp_error
     "'FOOBAR ' is not a valid test because it has whitespace on the edge"
     (invalid_sexp "FOOBAR "))
    |}]
;;

let%expect_test "set of sexp failure" =
  Or_error.try_with (fun () -> M.Set.t_of_sexp (Sexp.of_string "(\"FOOBAR \")"))
  |> print_error;
  [%expect
    {|
    (Of_sexp_error
     "'FOOBAR ' is not a valid test because it has whitespace on the edge"
     (invalid_sexp "FOOBAR "))
    |}]
;;

let%expect_test "of bin prot failure" =
  Or_error.try_with (fun () ->
    string_id_of_bin_str (Unit_tests_util.string_to_bin_str "FOOBAR "))
  |> print_error;
  [%expect
    {|
    (Invalid_argument
     "'FOOBAR ' is not a valid test because it has whitespace on the edge")
    |}]
;;

let%test_unit "String_id's of_string shouldn't allocate on success" =
  let initial_words = Gc.minor_words () in
  ignore (M.of_string "FOOBAR" : M.t);
  let allocated = Gc.minor_words () - initial_words in
  [%test_result: int] allocated ~expect:0
;;

let%test_module "Verify reading/writing stable table sexp" =
  (module struct
    let expected_sexp = Sexp.of_string "((alpha beta) (delta gamma))"

    let table =
      let s = String_id.Table.create () in
      Hashtbl.add_exn s ~key:(of_string "delta") ~data:(of_string "gamma");
      Hashtbl.add_exn s ~key:(of_string "alpha") ~data:(of_string "beta");
      s
    ;;

    let%expect_test "sexp_of_t" =
      print_s [%sexp (table : t String_id.Stable.V1.Table.t)];
      [%expect {|
        ((alpha beta)
         (delta gamma))
        |}]
    ;;

    let%test_unit "t_of_sexp" =
      let loaded_table = [%of_sexp: t String_id.Stable.V1.Table.t] expected_sexp in
      assert (Hashtbl.equal String_id.equal loaded_table table)
    ;;
  end)
;;

let%expect_test "include_default_validation" =
  let test ~include_default_validation =
    let module M =
      Make_with_validate
        (struct
          let module_name = "M"

          let validate s =
            if String.( = ) s "foo" then error_s [%message "no foo"] else Ok ()
          ;;

          let include_default_validation = include_default_validation
        end)
        ()
    in
    List.iter [ "foobar"; "foo"; "  bar  "; " bar"; "bar "; "" ] ~f:(fun s ->
      print_s
        [%message s ~_:(Or_error.try_with (fun () -> M.of_string s) : M.t Or_error.t)])
  in
  test ~include_default_validation:false;
  [%expect
    {|
    (foobar (Ok foobar))
    (foo (Error (Invalid_argument "\"no foo\"")))
    ("  bar  " (Ok "  bar  "))
    (" bar" (Ok " bar"))
    ("bar " (Ok "bar "))
    ("" (Ok ""))
    |}];
  test ~include_default_validation:true;
  [%expect
    {|
    (foobar (Ok foobar))
    (foo (Error (Invalid_argument "\"no foo\"")))
    ("  bar  " (
      Error (
        Invalid_argument
        "'  bar  ' is not a valid M because it has whitespace on the edge")))
    (" bar" (
      Error (
        Invalid_argument
        "' bar' is not a valid M because it has whitespace on the edge")))
    ("bar " (
      Error (
        Invalid_argument
        "'bar ' is not a valid M because it has whitespace on the edge")))
    ("" (Error (Invalid_argument "'' is not a valid M because it is empty")))
    |}]
;;
