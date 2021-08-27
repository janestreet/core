open! Core
open! Import

let group subcommands = Command.group ~summary:"" subcommands
let basic param = Command.basic ~summary:"" param

let exec path_to_exe child_subcommand =
  Command.exec ~summary:"" ~path_to_exe ~child_subcommand ()
;;

let validate command args () = Command_test_helpers.validate_command command args
let require_ok ?cr f = require_does_not_raise ?cr [%here] (f >> ok_exn)
let require_error ?cr f = require_does_raise ?cr [%here] (f >> ok_exn)

let%expect_test "subcommand" =
  let test =
    let command = basic (Command.Param.return Fn.id) in
    group [ "foo1", group [ "bar1", command; "qux", command ]; "foo2", command ]
    |> validate
  in
  require_error (test [ "foo1" ]);
  [%expect
    {|
      CMD foo1 SUBCOMMAND

    === subcommands ===

      bar1                       .
      qux                        .
      help                       . explain a given subcommand (perhaps recursively)

    missing subcommand for command CMD foo1
    (command.ml.Exit_called (status 1)) |}];
  require_ok (test [ "foo1"; "bar1" ]);
  [%expect {| |}];
  require_error (test [ "foo" ]);
  [%expect
    {|
      CMD SUBCOMMAND

    === subcommands ===

      foo1                       .
      foo2                       .
      version                    . print version information
      help                       . explain a given subcommand (perhaps recursively)

    subcommand foo is an ambiguous prefix: foo1, foo2
    (command.ml.Exit_called (status 1)) |}];
  require_ok (test [ "foo1"; "q" ]);
  [%expect {| |}];
  ignore ()
;;

let%expect_test "exec" =
  let dev_null = exec (`Absolute "/dev/null") [] in
  require_error (validate dev_null []);
  [%expect
    {|
    ("Cannot validate [Exec _] commands" (
      exec_info (
        (summary     "")
        (working_dir ELIDED-IN-TEST)
        (path_to_exe /dev/null)
        (child_subcommand ())))) |}];
  ignore ()
;;

let%expect_test "anons" =
  let test n param =
    let command = basic param in
    List.init n ~f:(List.init ~f:(Int.succ >> Int.to_string))
    |> List.iter ~f:(fun args ->
      print_endline (String.concat ~sep:" " ("$" :: "CMD" :: args));
      Result.iter_error (validate command args ()) ~f:(fun error ->
        print_s [%sexp (error : Error.t)]))
  in
  let test3 f =
    test
      3
      (let%map_open.Command _ = anon (f ("A" %: int)) in
       Fn.id)
  in
  test3 Fn.id;
  [%expect
    {|
    $ CMD
    Error parsing command line:

      missing anonymous argument: A

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    $ CMD 1
    $ CMD 1 2
    Error parsing command line:

      too many anonymous arguments

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  test3 Command.Param.maybe;
  [%expect
    {|
    $ CMD
    $ CMD 1
    $ CMD 1 2
    Error parsing command line:

      too many anonymous arguments

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  test3 Command.Param.sequence;
  [%expect {|
    $ CMD
    $ CMD 1
    $ CMD 1 2 |}];
  test
    4
    (let%map_open.Command _ =
       let%map_open.Command a = anon ("A" %: int)
       and b = anon (maybe ("B" %: int)) in
       a, b
     in
     Fn.id);
  [%expect
    {|
    $ CMD
    Error parsing command line:

      missing anonymous argument: A

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    $ CMD 1
    $ CMD 1 2
    $ CMD 1 2 3
    Error parsing command line:

      too many anonymous arguments

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  require_error
    (validate
       (basic
          (let%map_open.Command _ = anon ("A" %: int) in
           Fn.id))
       [ "not a valid int" ])
    ~cr:Comment;
  [%expect
    {|
    Error parsing command line:

      failed to parse A value "not a valid int"
      (Failure "Int.of_string: \"not a valid int\"")

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  ignore ()
;;

let%expect_test "flags" =
  let open Command.Param in
  let unit_anon f = anon (f ("A" %: int) |> map_anons ~f:ignore) in
  let unit_flag ?aliases ?full_flag_required name arg_type =
    flag ?aliases ?full_flag_required name arg_type ~doc:"" |> map ~f:ignore
  in
  let test params =
    let param = List.fold params ~init:(return ()) ~f:(map2 ~f:const) in
    validate (basic (map param ~f:const))
  in
  let test_prefix = test [ unit_flag "-foobar" no_arg; unit_flag "-fubar" no_arg ] in
  require_ok (test_prefix [ "-fo" ]);
  [%expect {| |}];
  require_ok (test_prefix [ "-fu" ]);
  [%expect {| |}];
  require_error (test_prefix [ "-f" ]);
  [%expect
    {|
    Error parsing command line:

      flag -f is an ambiguous prefix: -foobar, -fubar

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  let test_flags = test [ unit_flag "-a" no_arg; unit_flag "-b" (listed int) ] in
  require_ok (test_flags [ "-a" ]);
  [%expect {| |}];
  require_error (test_flags [ "-a"; "_" ]);
  [%expect
    {|
    Error parsing command line:

      too many anonymous arguments

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  require_error (test_flags [ "-b"; "_" ]) ~cr:Comment;
  [%expect
    {|
    Error parsing command line:

      failed to parse -b value "_".
      (Failure "Int.of_string: \"_\"")

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  require_ok (test_flags [ "-b"; "1" ]);
  [%expect {| |}];
  require_error (test_flags [ "-b" ]);
  [%expect
    {|
    Error parsing command line:

      missing argument for flag -b

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  require_error (test_flags [ "-b"; "-b" ]) ~cr:Comment;
  [%expect
    {|
    Error parsing command line:

      failed to parse -b value "-b".
      (Failure "Int.of_string: \"-b\"")

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  require_error (test_flags [ "-b"; "-b"; "_" ]);
  [%expect
    {|
    Error parsing command line:

      failed to parse -b value "-b".
      (Failure "Int.of_string: \"-b\"")

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  let test_alias = test [ unit_flag "-a" no_arg ~aliases:[ "-b" ] ] in
  require_ok (test_alias [ "-a" ]);
  [%expect {| |}];
  require_ok (test_alias [ "-b" ]);
  [%expect {| |}];
  require_error (test_alias [ "-c" ]);
  [%expect
    {|
    Error parsing command line:

      unknown flag -c

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  let test_anons_vs_flag_args =
    test [ unit_anon Fn.id; unit_flag "-a" (optional int); unit_flag "-b" no_arg ]
  in
  require_ok (test_anons_vs_flag_args [ "1" ]);
  [%expect {| |}];
  require_ok (test_anons_vs_flag_args [ "1"; "-a"; "2" ]);
  [%expect {| |}];
  require_error (test_anons_vs_flag_args [ "-a"; "2" ]);
  [%expect
    {|
    Error parsing command line:

      missing anonymous argument: A

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  require_ok (test_anons_vs_flag_args [ "-b"; "3" ]);
  [%expect {| |}];
  let test_escape =
    test
      [ flag ~doc:"" "--" escape |> map ~f:(Option.map ~f:(List.map ~f:Int.of_string)) ]
  in
  require_error (test_escape [ "--"; "foo" ]);
  [%expect
    {|
    Error parsing command line:

      (Failure "Int.of_string: \"foo\"")

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  require_ok (test_escape [ "--"; "1" ]) ~cr:Comment;
  [%expect {| |}];
  let test_alias_excluded_from_help = test [] in
  require_ok (test_alias_excluded_from_help [ "--help" ]);
  [%expect
    {|
      CMD

    === flags ===

      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0)) |}];
  let test_full_flag_required =
    test [ unit_flag "-foo" (required int) ~full_flag_required:() ]
  in
  require_error (test_full_flag_required [ "-f"; "1" ]) ~cr:Comment;
  [%expect
    {|
    Error parsing command line:

      unknown flag -f

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  let test_num_occurrences f =
    let test = test [ unit_flag "-a" (f int) ] in
    List.init 3 ~f:(List.init ~f:(const [ "-a"; "1" ]) >> List.concat)
    |> List.iter ~f:(fun args ->
      print_endline (String.concat ~sep:" " ("$" :: "CMD" :: args));
      Result.iter_error (test args ()) ~f:(fun error ->
        print_s [%sexp (error : Error.t)]))
  in
  test_num_occurrences listed;
  [%expect {|
    $ CMD
    $ CMD -a 1
    $ CMD -a 1 -a 1 |}];
  test_num_occurrences optional;
  [%expect
    {|
    $ CMD
    $ CMD -a 1
    $ CMD -a 1 -a 1
    Error parsing command line:

      flag -a passed more than once

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  test_num_occurrences required;
  [%expect
    {|
    $ CMD
    Error parsing command line:

      missing required flag: -a

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1))
    $ CMD -a 1
    $ CMD -a 1 -a 1
    Error parsing command line:

      flag -a passed more than once

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  ()
;;

let%expect_test "does not execute function" =
  let command =
    basic
      (let%map_open.Command s = flag "-print" (required string) ~doc:"" in
       fun () -> print_endline s)
  in
  validate command [ "-print"; "don't print me" ] () |> ok_exn;
  [%expect {||}];
  ()
;;
