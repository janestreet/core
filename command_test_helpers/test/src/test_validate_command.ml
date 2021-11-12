open! Core
open! Import

let group subcommands = Command.group ~summary:"" subcommands
let basic param = Command.basic ~summary:"" param

let exec path_to_exe child_subcommand =
  Command.exec ~summary:"" ~path_to_exe ~child_subcommand ()
;;

let get_output_or_error command args ~f =
  (match f command args with
   | Ok () -> ()
   | Error error -> print_s [%sexp (error : Error.t)]
   | exception exn -> print_s [%sexp "Raised", (exn : exn)]);
  expect_test_output [%here]
;;

(* Opam cannot distinguish between dependencies of the implementation and dependencies of
   the tests, so [core > patdiff -> core tests] would be a dependency cycle at the opam
   package level.

   Instead we approximate a diff as a shared prefix, a shared suffix, and one big
   substitution in between. Good enough for the simple diffs in this test file. *)
let hacky_diff_because_patdiff_breaks_the_public_release ~prev ~next =
  let shared_prefix = String.common_prefix2 prev next in
  let shared_suffix = String.common_suffix2 prev next in
  let get_unshared_lines_and_prepend str ~prepend =
    String.sub
      str
      ~pos:(String.length shared_prefix)
      ~len:String.(length str - length shared_prefix - length shared_suffix)
    |> String.split_lines
    |> List.map ~f:(fun s -> prepend ^ s)
  in
  [ String.split_lines shared_prefix
  ; get_unshared_lines_and_prepend prev ~prepend:"-|"
  ; get_unshared_lines_and_prepend next ~prepend:"+|"
  ; String.split_lines shared_suffix
  ]
  |> List.concat
  |> String.concat ~sep:"\n"
;;

let test command args =
  let validate_command_line =
    get_output_or_error command args ~f:(fun command args ->
      Command_test_helpers.validate_command_line (Command_unix.shape command)
      |> Or_error.bind ~f:(fun f -> f args))
  in
  let validate_command =
    get_output_or_error command args ~f:Command_test_helpers.validate_command
  in
  match String.( = ) validate_command_line validate_command with
  | true -> print_endline validate_command (* either would work, since they're the same *)
  | false ->
    print_endline "Diff of validate_command (-) vs validate_command_line (+):";
    hacky_diff_because_patdiff_breaks_the_public_release
      ~prev:validate_command
      ~next:validate_command_line
    |> print_endline
;;

module _ = struct
  let test =
    let command = basic (Command.Param.return Fn.id) in
    group [ "foo1", group [ "bar1", command; "qux", command ]; "foo2", command ] |> test
  ;;

  let%expect_test "subcommand__missing_subcommand" =
    test [ "foo1" ];
    [%expect
      {|
        CMD foo1 SUBCOMMAND

      === subcommands ===

        bar1                       .
        qux                        .
        help                       . explain a given subcommand (perhaps recursively)

      missing subcommand for command CMD foo1
      (command.ml.Exit_called (status 1)) |}]
  ;;

  let%expect_test "subcommand__present_subcommand" =
    test [ "foo1"; "bar1" ];
    [%expect {| |}]
  ;;

  let%expect_test "subcommand__ambiguous_prefix" =
    test [ "foo" ];
    [%expect
      {|
        CMD SUBCOMMAND

      === subcommands ===

        foo1                       .
        foo2                       .
        version                    . print version information
        help                       . explain a given subcommand (perhaps recursively)

      subcommand foo is an ambiguous prefix: foo1, foo2
      (command.ml.Exit_called (status 1)) |}]
  ;;

  let%expect_test "subcommand__unambiguous_prefix" =
    test [ "foo1"; "q" ];
    [%expect {| |}]
  ;;
end

module _ = struct
  let%expect_test "misc__check_command_doesn't_actually_run" =
    let command =
      basic
        (let%map_open.Command s = flag "-print" (required string) ~doc:"" in
         fun () -> print_endline s)
    in
    test command [ "-print"; "don't print me" ];
    [%expect {| |}]
  ;;

  let%expect_test "misc__cannot_validate_exec_commands" =
    let exec_dev_null = exec (`Absolute "/dev/null") [] in
    test exec_dev_null [];
    [%expect
      {|
        ("[Exec _] commands are not validated to avoid unexpected external dependencies."
         (exec_info (
           (summary     "")
           (working_dir ELIDED-IN-TEST)
           (path_to_exe /dev/null)
           (child_subcommand ())))) |}]
  ;;
end

module _ = struct
  open Command.Param

  let unit_flag ?aliases ?full_flag_required name arg_type =
    flag ?aliases ?full_flag_required name arg_type ~doc:"" |> map ~f:ignore
  ;;

  let test params args =
    let param = List.fold params ~init:(return ()) ~f:(map2 ~f:const) in
    test (basic (map param ~f:const)) args
  ;;

  let%expect_test "params__full_flag_required" =
    let test_full_flag_required args =
      test [ unit_flag "-foo" (required int) ~full_flag_required:() ] args
    in
    (* Validating by reconstruction cannot test full-flag-required *)
    test_full_flag_required [ "-f"; "1" ];
    [%expect
      {|
      Diff of validate_command (-) vs validate_command_line (+):
      -|Error parsing command line:
      -|
      -|  unknown flag -f
      -|
      -|For usage information, run
      -|
      -|  CMD -help
      -|
      -|(command.ml.Exit_called (status 1)) |}]
  ;;

  module _ = struct
    let test_prefix = test [ unit_flag "-foobar" no_arg; unit_flag "-fubar" no_arg ]

    let%expect_test "params_prefixes__unambiguous_prefix" =
      test_prefix [ "-fo" ];
      [%expect {| |}]
    ;;

    let%expect_test "params_prefixes__ambiguous_prefix" =
      test_prefix [ "-f" ];
      [%expect
        {|
          Error parsing command line:

            flag -f is an ambiguous prefix: -foobar, -fubar

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}]
    ;;
  end

  module _ = struct
    let test_flags args = test [ unit_flag "-a" no_arg; unit_flag "-b" (listed int) ] args

    let%expect_test "params_arg_types__not_passing_nonrequired_flags" =
      test_flags [ "-a" ];
      [%expect {| |}]
    ;;

    let%expect_test "params_arg_types__passing_invalid_anonymous_args" =
      test_flags [ "-a"; "_" ];
      [%expect
        {|
          Error parsing command line:

            too many anonymous arguments

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}]
    ;;

    let%expect_test "params_arg_types__passing_wrong_arg_type" =
      (* Validating by reconstruction cannot test arg type, only arity *)
      test_flags [ "-b"; "_" ];
      [%expect
        {|
        Diff of validate_command (-) vs validate_command_line (+):
        -|Error parsing command line:
        -|
        -|  failed to parse -b value "_".
        -|  (Failure "Int.of_string: \"_\"")
        -|
        -|For usage information, run
        -|
        -|  CMD -help
        -|
        -|(command.ml.Exit_called (status 1)) |}]
    ;;

    let%expect_test "params_arg_types__passing_correct_arg_type" =
      test_flags [ "-b"; "1" ];
      [%expect {||}]
    ;;

    let%expect_test "params_arg_types__passing_flag_but_no_arg" =
      test_flags [ "-b" ];
      [%expect
        {|
          Error parsing command line:

            missing argument for flag -b

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}]
    ;;

    let%expect_test "params_arg_types__passing_arg_which_looks_like_a_flag" =
      (* Validating by reconstruction cannot test arg type, only arity *)
      test_flags [ "-b"; "-b" ];
      [%expect
        {|
          Diff of validate_command (-) vs validate_command_line (+):
          -|Error parsing command line:
          -|
          -|  failed to parse -b value "-b".
          -|  (Failure "Int.of_string: \"-b\"")
          -|
          -|For usage information, run
          -|
          -|  CMD -help
          -|
          -|(command.ml.Exit_called (status 1)) |}]
    ;;

    let%expect_test "params_arg_types__passing_flag_twice_but_arg_once" =
      test_flags [ "-b"; "-b"; "_" ];
      (* Wrong error raised: Validating by reconstruction cannot
         test arg type, only arity *)
      [%expect
        {|
        Diff of validate_command (-) vs validate_command_line (+):
        Error parsing command line:


        -|failed to parse -b value "-b".
        -|  (Failure "Int.of_string: \"-b\"")
        +|too many anonymous arguments


        For usage information, run

          CMD -help

        (command.ml.Exit_called (status 1)) |}]
    ;;
  end

  module _ = struct
    let test_alias = test [ unit_flag "-a" no_arg ~aliases:[ "-b" ] ]

    let%expect_test "params_aliases__original_flag_passed" =
      test_alias [ "-a" ];
      [%expect {| |}]
    ;;

    let%expect_test "params_aliases__alias_passed" =
      test_alias [ "-b" ];
      [%expect {| |}]
    ;;

    let%expect_test "params_aliases__non_alias_passed" =
      test_alias [ "-c" ];
      [%expect
        {|
          Error parsing command line:

            unknown flag -c

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}]
    ;;

    let%expect_test "params_aliases__internal_alias_exluded_from_help" =
      test [] [ "--help" ];
      [%expect
        {|
            CMD

          === flags ===

            [-build-info]              . print info about this build and exit
            [-version]                 . print the version of this build and exit
            [-help], -?                . print this help text and exit

          (command.ml.Exit_called (status 0)) |}]
    ;;
  end

  module _ = struct
    let test_anons_vs_flag_args =
      let unit_anon f = anon (f ("A" %: int) |> map_anons ~f:ignore) in
      test [ unit_anon Fn.id; unit_flag "-a" (optional int); unit_flag "-b" no_arg ]
    ;;

    let%expect_test "params_anonymous_flags_and_args__just_anon" =
      test_anons_vs_flag_args [ "1" ];
      [%expect {| |}]
    ;;

    let%expect_test "params_anonymous_flags_and_args__anon_and_arg" =
      test_anons_vs_flag_args [ "1"; "-a"; "2" ];
      [%expect {| |}]
    ;;

    let%expect_test "params_anonymous_flags_and_args__no_required_anon" =
      test_anons_vs_flag_args [ "-a"; "2" ];
      [%expect
        {|
          Error parsing command line:

            missing anonymous argument: A

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}]
    ;;

    let%expect_test "params_anonymous_flags_and_args__anon_and_no-arg_flag" =
      test_anons_vs_flag_args [ "-b"; "3" ];
      [%expect {| |}]
    ;;
  end

  module _ = struct
    let test_escape args =
      let escape =
        [ flag ~doc:"" "--" escape |> map ~f:(Option.map ~f:(List.map ~f:Int.of_string)) ]
      in
      test escape args
    ;;

    let%expect_test "params_escape__incorrect_escaped_arg_type" =
      test_escape [ "--"; "foo" ];
      (* Wrong error raised: Validation by reconstruction cannot
         handle side-effects like escape args *)
      [%expect
        {|
          Diff of validate_command (-) vs validate_command_line (+):
          Error parsing command line:


          -|(Failure "Int.of_string: \"foo\"")
          +|too many anonymous arguments


          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}]
    ;;

    let%expect_test "params_escape__correct_escaped_arg_type" =
      (* Validation by reconstruction cannot handle side-effects like escape args *)
      test_escape [ "--"; "1" ];
      [%expect
        {|
          Diff of validate_command (-) vs validate_command_line (+):
          +|Error parsing command line:
          +|
          +|  too many anonymous arguments
          +|
          +|For usage information, run
          +|
          +|  CMD -help
          +|
          +|(command.ml.Exit_called (status 1)) |}]
    ;;
  end

  module _ = struct
    let print ~make_args n result =
      "$ CMD" :: make_args n |> String.concat ~sep:" " |> print_endline;
      print_endline result
    ;;

    let test_param_types param args =
      test
        [ (let%map_open.Command _ = param in
           Fn.id)
        ]
        args
    ;;

    module _ = struct
      let make_args = List.init ~f:(const [ "-a"; "1" ]) >> List.concat
      let print = print ~make_args

      let test f n =
        let param = unit_flag "-a" (f int) in
        let args = make_args n in
        test_param_types param args
      ;;

      let%expect_test "params_counting_flags__listed" =
        let test n =
          test listed n;
          print n [%expect.output]
        in
        test 0;
        [%expect {| $ CMD |}];
        test 1;
        [%expect {| $ CMD -a 1 |}];
        test 2;
        [%expect {| $ CMD -a 1 -a 1 |}]
      ;;

      let%expect_test "params_counting_flags__optional" =
        let test n =
          test optional n;
          print n [%expect.output]
        in
        test 0;
        [%expect {| $ CMD |}];
        test 1;
        [%expect {| $ CMD -a 1 |}];
        test 2;
        [%expect
          {|
          $ CMD -a 1 -a 1
          Error parsing command line:

            flag -a passed more than once

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}]
      ;;

      let%expect_test "params_counting_flags__required" =
        let test n =
          test required n;
          print n [%expect.output]
        in
        test 0;
        [%expect
          {|
          $ CMD
          Error parsing command line:

            missing required flag: -a

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}];
        test 1;
        [%expect {| $ CMD -a 1 |}];
        test 2;
        [%expect
          {|
          $ CMD -a 1 -a 1
          Error parsing command line:

            flag -a passed more than once

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}]
      ;;
    end

    module _ = struct
      let make_args = List.init ~f:(Int.succ >> Int.to_string)
      let print = print ~make_args

      let test f n =
        let param = anon (f ("A" %: int)) in
        let args = make_args n in
        test_param_types param args
      ;;

      let%expect_test "params_counting_anonymous_args__sequence" =
        let test n =
          test sequence n;
          print n [%expect.output]
        in
        test 0;
        [%expect {| $ CMD |}];
        test 1;
        [%expect {| $ CMD 1 |}];
        test 2;
        [%expect {| $ CMD 1 2 |}]
      ;;

      let%expect_test "params_counting_anonymous_args__maybe" =
        let test n =
          test maybe n;
          print n [%expect.output]
        in
        test 0;
        [%expect {| $ CMD |}];
        test 1;
        [%expect {| $ CMD 1 |}];
        test 2;
        [%expect
          {|
          $ CMD 1 2
          Error parsing command line:

            too many anonymous arguments

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}]
      ;;

      let%expect_test "params_counting_anonymous_args__required" =
        let test n =
          test Fn.id n;
          print n [%expect.output]
        in
        test 0;
        [%expect
          {|
          $ CMD
          Error parsing command line:

            missing anonymous argument: A

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}];
        test 1;
        [%expect {| $ CMD 1 |}];
        test 2;
        [%expect
          {|
          $ CMD 1 2
          Error parsing command line:

            too many anonymous arguments

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}]
      ;;

      let%expect_test "params_counting_anonymous_args__required_and_maybe" =
        let test n =
          test_param_types
            (let%map_open.Command _ = anon ("A" %: int)
             and _ = anon (maybe ("B" %: int)) in
             Fn.id)
            (make_args n);
          print n [%expect.output]
        in
        test 0;
        [%expect
          {|
          $ CMD
          Error parsing command line:

            missing anonymous argument: A

          For usage information, run

            CMD -help

          (command.ml.Exit_called (status 1)) |}];
        test 1;
        [%expect {| $ CMD 1 |}];
        test 2;
        [%expect {| $ CMD 1 2 |}]
      ;;
    end
  end
end
