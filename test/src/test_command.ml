open! Core
open Poly
open! Import
open! Expect_test_helpers_core
open! Command
open! Command.Private
module Expect_test_config = Core.Expect_test_config

let run ~argv ?when_parsing_succeeds ?verbose_on_parse_error command =
  try Command.run ~argv ?when_parsing_succeeds ?verbose_on_parse_error command with
  | exn -> print_s [%message "raised" ~_:(exn : Exn.t)]
;;

let%test_module "word wrap" =
  (module struct
    let%test _ = word_wrap "" 10 = []

    let short_word = "abcd"

    let%test _ = word_wrap short_word (String.length short_word) = [ short_word ]
    let%test _ = word_wrap "abc\ndef\nghi" 100 = [ "abc"; "def"; "ghi" ]

    let long_text =
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus fermentum \
       condimentum eros, sit amet pulvinar dui ultrices in."
    ;;

    let%test _ =
      word_wrap long_text 1000
      = [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus fermentum \
           condimentum eros, sit amet pulvinar dui ultrices in."
        ]
    ;;

    let%test _ =
      word_wrap long_text 39
      = (*
           .........1.........2.........3.........4
           1234567890123456789012345678901234567890
        *)
      [ "Lorem ipsum dolor sit amet, consectetur"
      ; "adipiscing elit. Vivamus fermentum"
      ; "condimentum eros, sit amet pulvinar dui"
      ; "ultrices in."
      ]
    ;;

    (* no guarantees: too-long words just overhang the soft bound *)
    let%test _ =
      word_wrap long_text 2
      = [ "Lorem"
        ; "ipsum"
        ; "dolor"
        ; "sit"
        ; "amet,"
        ; "consectetur"
        ; "adipiscing"
        ; "elit."
        ; "Vivamus"
        ; "fermentum"
        ; "condimentum"
        ; "eros,"
        ; "sit"
        ; "amet"
        ; "pulvinar"
        ; "dui"
        ; "ultrices"
        ; "in."
        ]
    ;;
  end)
;;

let%test_unit _ =
  let path =
    Path.empty
    |> Path.append ~subcommand:"foo/bar.exe"
    |> Path.append ~subcommand:"bar"
    |> Path.append ~subcommand:"bar"
    |> Path.append ~subcommand:"baz"
  in
  [%test_result: string list]
    (Path.parts path)
    ~expect:[ "foo/bar.exe"; "bar"; "bar"; "baz" ];
  let path = Path.replace_first path ~from:"bar" ~to_:"qux" in
  [%test_result: string list]
    (Path.parts path)
    ~expect:[ "foo/bar.exe"; "qux"; "bar"; "baz" ];
  ()
;;

let%expect_test "[Path.to_string], [Path.to_string_dots]" =
  let path =
    Path.create ~path_to_exe:"foo/bar/baz.exe"
    |> Path.append ~subcommand:"qux"
    |> Path.append ~subcommand:"foo"
    |> Path.append ~subcommand:"bar"
  in
  print_string (Path.to_string path);
  [%expect {| baz.exe qux foo bar |}];
  print_string (Path.to_string_dots path);
  [%expect {| . . . bar |}];
  ()
;;

let%test_module "[Anons]" =
  (module struct
    open Private.Anons

    let%test _ = String.equal (normalize "file") "FILE"
    let%test _ = String.equal (normalize "FiLe") "FILE"
    let%test _ = String.equal (normalize "<FiLe>") "<FiLe>"
    let%test _ = String.equal (normalize "(FiLe)") "(FiLe)"
    let%test _ = String.equal (normalize "[FiLe]") "[FiLe]"
    let%test _ = String.equal (normalize "{FiLe}") "{FiLe}"
    let%test _ = String.equal (normalize "<file") "<file"
    let%test _ = String.equal (normalize "<fil>a") "<fil>a"

    let%test _ =
      try
        ignore (normalize "");
        false
      with
      | _ -> true
    ;;

    let%test _ =
      try
        ignore (normalize " file ");
        false
      with
      | _ -> true
    ;;

    let%test _ =
      try
        ignore (normalize "file ");
        false
      with
      | _ -> true
    ;;

    let%test _ =
      try
        ignore (normalize " file");
        false
      with
      | _ -> true
    ;;
  end)
;;

let%test_module "Cmdline.extend" =
  (module struct
    let path_of_list subcommands =
      List.fold
        subcommands
        ~init:(Path.create ~path_to_exe:"exe")
        ~f:(fun path subcommand -> Path.append path ~subcommand)
    ;;

    let extend path =
      match path with
      | [ "foo"; "bar" ] -> [ "-foo"; "-bar" ]
      | [ "foo"; "baz" ] -> [ "-foobaz" ]
      | _ -> [ "default" ]
    ;;

    let test path args expected =
      let expected = Cmdline.of_list expected in
      let observed =
        let path = path_of_list path in
        let args = Cmdline.of_list args in
        Cmdline.extend args ~extend ~path
      in
      [%compare.equal: Cmdline.t] expected observed
    ;;

    let%test _ =
      test [ "foo"; "bar" ] [ "anon"; "-flag" ] [ "anon"; "-flag"; "-foo"; "-bar" ]
    ;;

    let%test _ = test [ "foo"; "baz" ] [] [ "-foobaz" ]
    let%test _ = test [ "zzz" ] [ "x"; "y"; "z" ] [ "x"; "y"; "z"; "default" ]
  end)
;;

let%expect_test "[choose_one] duplicate name" =
  show_raise ~hide_positions:true (fun () ->
    let open Param in
    choose_one
      [ flag "-foo" (optional int) ~doc:""; flag "-foo" (optional int) ~doc:"" ]
      ~if_nothing_chosen:Raise);
  [%expect
    {|
    (raised (
      "[Command.Spec.choose_one] called with duplicate name"
      (-foo)
      lib/core_kernel/src/command.ml:LINE:COL)) |}]
;;

let run_command param ~args =
  run ~argv:("__exe_name__" :: args) (Command.basic ~summary:"" param)
;;

let%expect_test "[choose_one] any flagless params" =
  let param =
    lazy
      (let open Param in
       choose_one [ return None; return (Some 1) ] ~if_nothing_chosen:Return_none)
  in
  show_raise (fun () -> force param);
  [%expect {| (raised "[choose_one] expects choices to read command-line arguments.") |}]
;;

let%expect_test "[choose_one]" =
  let test (type b) ~(if_nothing_chosen : (_, b) Command.Param.If_nothing_chosen.t) args =
    run_command
      ~args
      (let open Command.Let_syntax in
       let%map_open arg =
         choose_one
           ~if_nothing_chosen
           (List.map [ "-foo"; "-bar" ] ~f:(fun name ->
              flag name (no_arg_some name) ~doc:""))
       in
       fun () ->
         match if_nothing_chosen with
         | Default_to _ -> print_s [%message (arg : string)]
         | Raise -> print_s [%message (arg : string)]
         | Return_none -> print_s [%message (arg : string option)])
  in
  test ~if_nothing_chosen:Raise [];
  [%expect
    {|
    Error parsing command line:

      Must pass one of these:
        -bar
        -foo

    For usage information, run

      __exe_name__ -help

    (raised (command.ml.Exit_called (status 1))) |}];
  test [] ~if_nothing_chosen:Return_none;
  [%expect {| (arg ()) |}];
  test ~if_nothing_chosen:Return_none [ "-bar" ];
  [%expect {| (arg (-bar)) |}];
  test [] ~if_nothing_chosen:(Default_to "default");
  [%expect {| (arg default) |}];
  test ~if_nothing_chosen:Raise [ "-foo" ];
  [%expect {| (arg -foo) |}];
  test ~if_nothing_chosen:Raise [ "-bar" ];
  [%expect {| (arg -bar) |}];
  test ~if_nothing_chosen:Raise [ "-foo"; "-bar" ];
  [%expect
    {|
    Error parsing command line:

      Cannot pass more than one of these:
        -foo
        -bar

    For usage information, run

      __exe_name__ -help

    (raised (command.ml.Exit_called (status 1))) |}]
;;

let%expect_test "nested [choose_one]" =
  let test args =
    run_command
      ~args
      (let open Command.Let_syntax in
       let%map_open arg =
         choose_one
           ~if_nothing_chosen:Raise
           [ (let%map foo = flag "foo" no_arg ~doc:""
              and bar = flag "bar" no_arg ~doc:"" in
              if foo || bar then Some (`Foo_bar (foo, bar)) else None)
           ; (let%map baz = flag "baz" no_arg ~doc:""
              and qux = flag "qux" no_arg ~doc:"" in
              if baz || qux then Some (`Baz_qux (baz, qux)) else None)
           ]
       in
       fun () ->
         print_s [%message (arg : [ `Foo_bar of bool * bool | `Baz_qux of bool * bool ])])
  in
  test [];
  [%expect
    {|
    Error parsing command line:

      Must pass one of these:
        -bar,-foo
        -baz,-qux

    For usage information, run

      __exe_name__ -help

    (raised (command.ml.Exit_called (status 1))) |}];
  test [ "-foo"; "-baz" ];
  [%expect
    {|
    Error parsing command line:

      Cannot pass more than one of these:
        -baz,-qux
        -bar,-foo

    For usage information, run

      __exe_name__ -help

    (raised (command.ml.Exit_called (status 1))) |}];
  test [ "-foo" ];
  [%expect {| (arg (Foo_bar (true false))) |}];
  test [ "-bar" ];
  [%expect {| (arg (Foo_bar (false true))) |}]
;;

let%expect_test "parse error with subcommand" =
  let test arguments =
    run
      ~argv:("exe" :: "subcommand" :: arguments)
      (Command.group
         ~summary:""
         [ ( "subcommand"
           , Command.basic
               ~summary:""
               (let%map_open.Command required_flag =
                  flag "required-flag" (required string) ~doc:""
                in
                fun () -> print_s [%message (required_flag : string)]) )
         ])
  in
  test [];
  [%expect
    {|
    Error parsing command line:

      missing required flag: -required-flag

    For usage information, run

      exe subcommand -help

    (raised (command.ml.Exit_called (status 1))) |}];
  test [ "-foo" ];
  [%expect
    {|
    Error parsing command line:

      unknown flag -foo

    For usage information, run

      exe subcommand -help

    (raised (command.ml.Exit_called (status 1))) |}]
;;

let%test_unit _ =
  [ "/", "./foo", "/foo"
  ; "/tmp", "/usr/bin/grep", "/usr/bin/grep"
  ; "/foo", "bar", "/foo/bar"
  ; "foo", "bar", "foo/bar"
  ; "foo", "../bar", "foo/../bar"
  ]
  |> List.iter ~f:(fun (dir, path, expected) ->
    [%test_eq: string] (abs_path ~dir path) expected)
;;

let%expect_test "choose_one strings" =
  let open Param in
  let to_string param = Spec.to_string_for_choose_one param in
  print_string (to_string (flag "-a" no_arg ~doc:""));
  [%expect {| -a |}];
  print_string
    (to_string
       (map2 ~f:Tuple2.create (flag "-a" no_arg ~doc:"") (flag "-b" no_arg ~doc:"")));
  [%expect {| -a,-b |}];
  print_string
    (to_string
       (map2
          ~f:Tuple2.create
          (flag "-a" no_arg ~doc:"")
          (flag "-b" (optional int) ~doc:"")));
  [%expect {| -a,-b |}];
  printf
    !"%{sexp: string Or_error.t}"
    (Or_error.try_with (fun () ->
       to_string
         (map2
            ~f:Tuple2.create
            (flag "-a" no_arg ~doc:"")
            (flag "-b,c" (optional int) ~doc:""))));
  [%expect
    {|
    (Error
     ("For simplicity, [Command.Spec.choose_one] does not support names with commas."
      (-b,c) *)) (glob) |}];
  print_string
    (to_string
       (map2 ~f:Tuple2.create (anon ("FOO" %: string)) (flag "-a" no_arg ~doc:"")));
  [%expect {| -a,FOO |}];
  print_string
    (to_string
       (map2
          ~f:Tuple2.create
          (anon ("FOO" %: string))
          (map2 ~f:Tuple2.create (flag "-a" no_arg ~doc:"") (flag "-b" no_arg ~doc:""))));
  [%expect {| -a,-b,FOO |}];
  print_string
    (to_string
       (map2 ~f:Tuple2.create (anon (maybe ("FOO" %: string))) (flag "-a" no_arg ~doc:"")));
  [%expect {| -a,FOO |}];
  print_string
    (to_string
       (map2 ~f:Tuple2.create (anon ("fo{}O" %: string)) (flag "-a" no_arg ~doc:"")));
  [%expect {| -a,fo{}O |}]
;;

let%test_unit "multiple runs" =
  let r = ref (None, "not set") in
  let command =
    let open Let_syntax in
    basic
      ~summary:"test"
      [%map_open
        let a = flag "int" (optional int) ~doc:"INT some number"
        and b = anon ("string" %: string) in
        fun () -> r := a, b]
  in
  let test args expect =
    run command ~argv:((Sys.get_argv ()).(0) :: args);
    [%test_result: int option * string] !r ~expect
  in
  test [ "foo"; "-int"; "23" ] (Some 23, "foo");
  test [ "-int"; "17"; "bar" ] (Some 17, "bar");
  test [ "baz" ] (None, "baz")
;;

let%expect_test "[?verbose_on_parse_error]" =
  let test ?verbose_on_parse_error () =
    run
      ~argv:[ "__exe_name__" ]
      ?verbose_on_parse_error
      (Command.basic
         ~summary:""
         (let open Command.Let_syntax in
          let%map_open () =
            let%map () = return () in
            raise_s [%message "Fail!"]
          in
          fun () -> ()))
  in
  test ?verbose_on_parse_error:None ();
  [%expect
    {|
    Error parsing command line:

      Fail!

    For usage information, run

      __exe_name__ -help

    (raised (command.ml.Exit_called (status 1))) |}];
  test ~verbose_on_parse_error:true ();
  [%expect
    {|
    Error parsing command line:

      Fail!

    For usage information, run

      __exe_name__ -help

    (raised (command.ml.Exit_called (status 1))) |}];
  test ~verbose_on_parse_error:false ();
  [%expect {|
    Fail!
    (raised (command.ml.Exit_called (status 1))) |}]
;;

let%expect_test "illegal flag names" =
  let test name =
    show_raise (fun () ->
      Command.basic
        ~summary:""
        (let%map_open.Command bool = flag name no_arg ~doc:"" in
         fun () -> ignore (bool : bool)))
  in
  test "-no-spaces";
  [%expect {| "did not raise" |}];
  test "no-spaces";
  [%expect {| "did not raise" |}];
  test "-";
  [%expect {| (raised (Failure "invalid flag name: \"-\"")) |}];
  test "has whitespace";
  [%expect
    {|
    (raised (
      Failure "invalid flag name (contains whitespace): \"has whitespace\"")) |}]
;;

let%expect_test "escape flag type" =
  let test args =
    run_command
      ~args
      (let%map_open.Command dash_dash = flag "--" escape ~doc:"... escape flag"
       and also_an_escape_flag =
         flag "-also-an-escape-flag" escape ~doc:"... escape flag"
       and other_flag = flag "-other-flag" no_arg ~doc:"" in
       fun () ->
         print_s
           [%message
             "args"
               (dash_dash : string list option)
               (also_an_escape_flag : string list option)
               (other_flag : bool)])
  in
  test [ "-help" ];
  [%expect
    {|
      __exe_name__

    === flags ===

      [-- ...]                    escape flag
      [-also-an-escape-flag ...]  escape flag
      [-other-flag]
      [-build-info]               print info about this build and exit
      [-version]                  print the version of this build and exit
      [-help]                     print this help text and exit
                                  (alias: -?)

    (command.ml.Exit_called (status 0)) |}];
  test [];
  [%expect
    {|
    (args
      (dash_dash           ())
      (also_an_escape_flag ())
      (other_flag false)) |}];
  test [ "-other-flag" ];
  [%expect
    {|
    (args
      (dash_dash           ())
      (also_an_escape_flag ())
      (other_flag true)) |}];
  test [ "--"; "-other-flag" ];
  [%expect
    {|
    (args (dash_dash ((-other-flag))) (also_an_escape_flag ()) (other_flag false)) |}];
  test [ "--"; "foo"; ""; "-bar"; "-anon"; "lorem ipsum"; "-also-an-escape-flag" ];
  [%expect
    {|
    (args
      (dash_dash ((foo "" -bar -anon "lorem ipsum" -also-an-escape-flag)))
      (also_an_escape_flag ())
      (other_flag false)) |}];
  test [ "-also-an-escape-flag" ];
  [%expect {|
    (args (dash_dash ()) (also_an_escape_flag (())) (other_flag false)) |}];
  test [ "-also-an-escape-flag"; "-other-flag"; "--" ];
  [%expect
    {|
    (args
      (dash_dash ())
      (also_an_escape_flag ((-other-flag --)))
      (other_flag false)) |}]
;;

let ignored_flags names =
  let open Command.Param in
  all_unit
    (List.map names ~f:(fun name ->
       flag name no_arg ~doc:"" |> map ~f:(ignore : bool -> unit)))
;;

let%expect_test "[and_arg_names]" =
  let test names =
    run_command
      ~args:[]
      (let%map_open.Command (), arg_names = and_arg_names (ignored_flags names) in
       fun () -> print_s [%message (arg_names : string list)])
  in
  test [];
  [%expect {| (arg_names ()) |}];
  test [ "foo" ];
  [%expect {| (arg_names (-foo)) |}];
  test [ "foo"; "bar" ];
  [%expect {| (arg_names (-bar -foo)) |}]
;;

let%expect_test "[and_arg_name]" =
  let test names =
    show_raise (fun () -> Command.Param.and_arg_name (ignored_flags names))
  in
  test [];
  [%expect {| (raised ("[and_arg_name] expects exactly one name, got" ())) |}];
  test [ "foo" ];
  [%expect {| "did not raise" |}];
  test [ "foo"; "bar" ];
  [%expect {| (raised ("[and_arg_name] expects exactly one name, got" (-bar -foo))) |}]
;;

let truncate_long_lines output =
  String.split output ~on:'\n'
  |> List.map ~f:(fun line ->
    if String.length line > 80 then String.prefix line 80 ^ " ..." else line)
  |> String.concat ~sep:"\n"
;;

let%expect_test "double-dash built-in flags" =
  let command =
    let group name cmd = Command.group ~summary:"" [ name, cmd ] in
    Command.basic
      ~summary:""
      (let%map_open.Command () = return () in
       fun () -> print_endline "hello world")
    |> group "bird"
    |> group "blue"
  in
  let run_test_command args =
    run ~argv:("__exe_name__" :: args) command;
    [%expect.output]
  in
  let run_with_both_flags args flag =
    let output1 = run_test_command (args @ [ "-" ^ flag ]) in
    let output2 = run_test_command (args @ [ "--" ^ flag ]) in
    print_string (truncate_long_lines output1);
    require_compare_equal [%here] (module String) output1 output2
  in
  run_with_both_flags [] "help";
  [%expect
    {|
      __exe_name__ SUBCOMMAND

    === subcommands ===

      blue
      version  print version information
      help     explain a given subcommand (perhaps recursively)

    (command.ml.Exit_called (status 0)) |}];
  run_with_both_flags [] "build-info";
  [%expect
    {|
    ((username"")(hostname"")(kernel"")(build_time(1969-12-31 19:00:00.000000-05:00) ...
    (command.ml.Exit_called (status 0)) |}];
  run_with_both_flags [] "version";
  [%expect {|
    NO_VERSION_UTIL
    (command.ml.Exit_called (status 0)) |}];
  run_with_both_flags [ "blue" ] "help";
  [%expect
    {|
      __exe_name__ blue SUBCOMMAND

    === subcommands ===

      bird
      help  explain a given subcommand (perhaps recursively)

    (command.ml.Exit_called (status 0))|}];
  run_with_both_flags [ "blue"; "bird" ] "help";
  [%expect
    {|
      __exe_name__ blue bird

    === flags ===

      [-help]  print this help text and exit
               (alias: -?)

    (command.ml.Exit_called (status 0))|}];
  run_test_command [ "blue"; "bird" ] |> print_string;
  [%expect {| hello world |}];
  run_test_command [ "blue"; "bird"; "--" ] |> print_string;
  [%expect
    {|
    Error parsing command line:

      unknown flag --

    For usage information, run

      __exe_name__ blue bird -help

    (raised (command.ml.Exit_called (status 1))) |}]
;;

let%expect_test "when_parsing_succeeds" =
  let command =
    Command.basic
      ~summary:""
      (let%map_open.Command input = flag "input" (required string) ~doc:"input" in
       fun () -> print_endline input)
  in
  let run args =
    run
      ~when_parsing_succeeds:(fun () -> print_endline "Parsing Succeeded")
      ~argv:("__exe_name__" :: args)
      command
  in
  run [ "-input"; "test" ];
  [%expect {|
    Parsing Succeeded
    test |}];
  run [ "-help" ];
  [%expect
    {|
      __exe_name__

    === flags ===

      -input _       input
      [-build-info]  print info about this build and exit
      [-version]     print the version of this build and exit
      [-help]        print this help text and exit
                     (alias: -?)

    (command.ml.Exit_called (status 0)) |}];
  run [];
  [%expect
    {|
    Error parsing command line:

      missing required flag: -input

    For usage information, run

      __exe_name__ -help

    (raised (command.ml.Exit_called (status 1))) |}]
;;
