open! Core
open! Import

type t =
  | Foo
  | Bar
  | Baz
[@@deriving sexp_of, variants, enumerate]

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

module _ = struct
  module T = struct
    type nonrec t =
      | A
      | B
      | C
    [@@deriving enumerate, sexp_of]

    let to_string = function
      | A -> "alpha"
      | B -> "bravo"
      | C -> "charlie"
    ;;
  end

  include T

  let arg_type = Command.Arg_type.enumerated (module T)

  let parse str =
    print_s [%sexp (Command.Arg_type.For_testing.parse arg_type str : t Or_error.t)]
  ;;

  let%expect_test "basic test" =
    parse "alpha";
    [%expect {| (Ok A) |}];
    parse "bravo";
    [%expect {| (Ok B) |}];
    parse "charlie";
    [%expect {| (Ok C) |}];
    parse (Sexp.to_string [%sexp (A : t)]);
    [%expect {| (Error (Failure "valid arguments: {alpha,bravo,charlie}")) |}]
  ;;

  let%expect_test "help text" =
    let command =
      Command.basic
        ~summary:"this displays the help text for enumerated arg types: stringing"
        (let%map_open.Command (_ : t) = anon ("T" %: arg_type)
         and (_ : t) = flag "f" (required arg_type) ~doc:"demo" in
         fun () -> ())
    in
    Command_unix.run ~argv:[ "prog"; "-help" ] command;
    [%expect
      {|
      this displays the help text for enumerated arg types: stringing

        prog T

      === flags ===

        -f demo                    . (can be: alpha, bravo, charlie)
        [-build-info]              . print info about this build and exit
        [-version]                 . print the version of this build and exit
        [-help], -?                . print this help text and exit

      (command.ml.Exit_called (status 0)) |}]
  ;;
end

module _ = struct
  module T = struct
    type nonrec t =
      | A of t
      | B of bool
      | C
    [@@deriving enumerate, sexp_of]
  end

  include T

  let arg_type = Command.Arg_type.enumerated_sexpable (module T)

  let parse str =
    print_s [%sexp (Command.Arg_type.For_testing.parse arg_type str : t Or_error.t)]
  ;;

  let%expect_test "basic test" =
    parse "B";
    [%expect
      {|
      (Error (
        Failure "valid arguments: {(A Bar),(A Baz),(A Foo),(B false),(B true),C}")) |}];
    parse "C";
    [%expect {| (Ok C) |}]
  ;;

  let%expect_test "help text" =
    let command =
      Command.basic
        ~summary:"this displays the help text for enumerated arg types: sexping"
        (let%map_open.Command (_ : t) = anon ("T" %: arg_type)
         and (_ : t) = flag "f" (required arg_type) ~doc:"demo" in
         fun () -> ())
    in
    Command_unix.run ~argv:[ "prog"; "-help" ] command;
    [%expect
      {|
      this displays the help text for enumerated arg types: sexping

        prog T

      === flags ===

        -f demo                    . (can be: (A Bar), (A Baz), (A Foo), (B false), (B
                                     true), C)
        [-build-info]              . print info about this build and exit
        [-version]                 . print the version of this build and exit
        [-help], -?                . print this help text and exit

      (command.ml.Exit_called (status 0)) |}]
  ;;
end

module _ = struct
  module T = struct
    type t =
      | Unique_prefix
      | Non_unique_alpha
      | Non_unique_beta
    [@@deriving compare, enumerate, sexp_of]
  end

  include T

  let arg_type = Command.Arg_type.enumerated_sexpable (module T)

  let fully_specified_arg_type =
    Command.Arg_type.enumerated_sexpable ~accept_unique_prefixes:false (module T)
  ;;

  let test str =
    let parse arg_type = Command.Arg_type.For_testing.parse arg_type str in
    let partials_allowed = parse arg_type in
    let no_partials_allowed = parse fully_specified_arg_type in
    if [%compare.equal: t Or_error.t] partials_allowed no_partials_allowed
    then print_s [%sexp (partials_allowed : t Or_error.t)]
    else
      print_s
        [%message (partials_allowed : t Or_error.t) (no_partials_allowed : t Or_error.t)]
  ;;

  let%expect_test _ =
    test "U";
    [%expect
      {|
      ((partials_allowed (Ok Unique_prefix))
       (no_partials_allowed (
         Error (
           Failure
           "valid arguments: {Non_unique_alpha,Non_unique_beta,Unique_prefix}")))) |}];
    test "this isn't any kind of prefix";
    [%expect
      {|
      (Error (
        Failure "valid arguments: {Non_unique_alpha,Non_unique_beta,Unique_prefix}")) |}];
    test "Non_unique_";
    [%expect
      {|
      (Error (
        Failure "valid arguments: {Non_unique_alpha,Non_unique_beta,Unique_prefix}")) |}];
    test "Non_unique_a";
    [%expect
      {|
      ((partials_allowed (Ok Non_unique_alpha))
       (no_partials_allowed (
         Error (
           Failure
           "valid arguments: {Non_unique_alpha,Non_unique_beta,Unique_prefix}")))) |}];
    test "Non_unique_beta";
    [%expect {| (Ok Non_unique_beta) |}]
  ;;
end
