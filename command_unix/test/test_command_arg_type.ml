open! Core
open! Import

type t =
  | Foo
  | Bar
  | Baz
[@@deriving sexp_of, variants]

let arg_type ~case_sensitive =
  let f acc (variant : t Variant.t) = (variant.name, variant.constructor) :: acc in
  Variants.fold ~init:[] ~foo:f ~bar:f ~baz:f
  |> Command.Arg_type.of_alist_exn ~case_sensitive
;;

let parse ~arg_type str =
  print_s [%sexp (Command.Arg_type.For_testing.parse arg_type str : t Or_error.t)]
;;

let complete ~arg_type part =
  let completions = Command.Arg_type.For_testing.complete arg_type Univ_map.empty ~part in
  print_s [%sexp (completions : string list)]
;;

let%expect_test "case sensitive (default) parsing" =
  let arg_type = arg_type ~case_sensitive:true in
  parse ~arg_type "foo";
  [%expect {| (Error (Failure "valid arguments: {Bar,Baz,Foo}")) |}];
  parse ~arg_type "fOo";
  [%expect {| (Error (Failure "valid arguments: {Bar,Baz,Foo}")) |}];
  parse ~arg_type "Foo";
  [%expect {| (Ok Foo) |}]
;;

let%expect_test "case sensitive (default) completion" =
  let arg_type = arg_type ~case_sensitive:true in
  (* Nothing here. *)
  complete ~arg_type "f";
  [%expect {| () |}];
  complete ~arg_type "FO";
  [%expect {| () |}];
  (* But this works. *)
  complete ~arg_type "Fo";
  [%expect {| (Foo) |}]
;;

let%expect_test "case insensitive parsing" =
  let arg_type = arg_type ~case_sensitive:false in
  parse ~arg_type "foo";
  [%expect {| (Ok Foo) |}];
  parse ~arg_type "fOo";
  [%expect {| (Ok Foo) |}];
  parse ~arg_type "Foo";
  [%expect {| (Ok Foo) |}];
  parse ~arg_type "wrong";
  [%expect {| (Error (Failure "valid arguments (case insensitive): {Bar,Baz,Foo}")) |}]
;;

let%expect_test "case insensitive completion" =
  let arg_type = arg_type ~case_sensitive:false in
  (* All of these work. Note we match capitalization of our completions, because bash
     won't accept them otherwise. *)
  complete ~arg_type "f";
  [%expect {| (foo) |}];
  complete ~arg_type "FO";
  [%expect {| (FOo) |}];
  complete ~arg_type "fO";
  [%expect {| (fOo) |}];
  complete ~arg_type "Fo";
  [%expect {| (Foo) |}]
;;

let%expect_test "[of_map] duplicate keys" =
  let test keys ~case_sensitive =
    show_raise (fun () ->
      List.map keys ~f:(fun k -> k, ())
      |> Map.of_alist_exn (module String)
      |> Command.Arg_type.of_map ~case_sensitive)
  in
  test [ "a"; "b"; "B"; "c" ] ~case_sensitive:true;
  [%expect {| "did not raise" |}];
  test [ "a"; "b"; "B"; "c" ] ~case_sensitive:false;
  [%expect {| (raised (Command.Spec.Arg_type.of_alist_exn (duplicate_keys ((B b))))) |}]
;;

let%expect_test "[of_alist_exn] duplicate keys" =
  let test keys ~case_sensitive =
    show_raise (fun () ->
      List.map keys ~f:(fun k -> k, ()) |> Command.Arg_type.of_alist_exn ~case_sensitive)
  in
  test [ "a"; "b"; "B"; "c"; "c" ] ~case_sensitive:true;
  [%expect {| (raised (Command.Spec.Arg_type.of_alist_exn (duplicate_keys ((c c))))) |}];
  test [ "a"; "b"; "B"; "c"; "c" ] ~case_sensitive:false;
  [%expect
    {|
      (raised (
        Command.Spec.Arg_type.of_alist_exn (
          duplicate_keys (
            (b B)
            (c c))))) |}]
;;
