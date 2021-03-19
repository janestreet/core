open! Core
open! Import

(* the command param we are going to pretend we care about testing *)
let param =
  let open Command.Param in
  choose_one
    ~if_nothing_chosen:(Default_to "default")
    [ flag "foo" (optional string) ~doc:"TXT foo doc"
    ; flag "bar" (optional string) ~doc:"TXT bar doc"
    ]
;;

(* build a little model of the CLI for this command *)
let commander_cli =
  unstage
    (Command_test_helpers.parse_command_line
       ~path:[ "COMMANDER" ]
       ~summary:"the CLI for the foo-bar service"
       param)
;;

let commander_arg args =
  with_return (fun { return } ->
    commander_cli ~on_success:return args;
    raise_s [%message "failed to parse" (args : string list)])
;;

let commander args =
  commander_cli ~on_success:(fun a -> print_s [%sexp (a : string)]) args
;;

let%expect_test "interface" =
  commander [ "-help" ];
  [%expect
    {|
    the CLI for the foo-bar service

      COMMANDER

    === flags ===

      [-bar TXT]                 . bar doc
      [-foo TXT]                 . foo doc
      [-build-info]              . print info about this build and exit
      [-version]                 . print the version of this build and exit
      [-help], -?                . print this help text and exit

    (command.ml.Exit_called (status 0))
  |}]
;;

let%expect_test "printing parsed inputs" =
  commander [ "-foo"; "ABC" ];
  [%expect {|
    ABC
  |}];
  commander [ "-bar"; "DEF" ];
  [%expect {|
    DEF
  |}];
  ()
;;

let%expect_test "bad command lines" =
  show_raise (fun () -> commander [ "-baz" ]);
  [%expect
    {|
    Error parsing command line:

      unknown flag -baz

    For usage information, run

      COMMANDER -help

    (raised (command.ml.Exit_called (status 1)))
  |}];
  show_raise (fun () -> commander [ "-foo"; "ABC"; "-bar"; "DEF" ]);
  [%expect
    {|
    Error parsing command line:

      Cannot pass more than one of these:
        -foo
        -bar

    For usage information, run

      COMMANDER -help

    (raised (command.ml.Exit_called (status 1)))
  |}];
  ()
;;

let%expect_test "parsing values" =
  let a = commander_arg [ "-foo"; "XYZ" ] in
  print_s [%sexp (a : string)];
  [%expect {| XYZ |}];
  let b = commander_arg [ "-bar"; "XYZ" ] in
  require_equal [%here] (module String) a b;
  ()
;;
