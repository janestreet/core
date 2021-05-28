open! Core
open! Import
module Unix = Core_unix

let%expect_test "simple" =
  let param =
    let%map_open.Command (_ : _) = anon ("BOOL" %: bool)
    and (_ : _) = flag "--" escape ~doc:""
    and (_ : _) = flag "-foo" no_arg ~doc:""
    and (_ : _) = flag "-bar" (optional bool) ~doc:"" in
    ()
  in
  Command_test_helpers.complete param ~args:[ "" ];
  [%expect {|
    false
    true
    (command.ml.Exit_called (status 0)) |}];
  Command_test_helpers.complete param ~args:[ "t" ];
  [%expect {|
    true
    (command.ml.Exit_called (status 0)) |}];
  Command_test_helpers.complete param ~args:[ "t"; "-" ];
  [%expect
    {|
    --
    -bar
    -build-info
    -foo
    -help
    -version
    (command.ml.Exit_called (status 0)) |}];
  Command_test_helpers.complete param ~args:[ "t"; "-f" ];
  [%expect {|
    -foo
    (command.ml.Exit_called (status 0)) |}];
  Command_test_helpers.complete param ~args:[ "t"; "-f" ] ~which_arg:0;
  [%expect {|
    true
    (command.ml.Exit_called (status 0)) |}];
  Command_test_helpers.complete param ~args:[ "t"; "-b" ];
  [%expect {|
    -bar
    -build-info
    (command.ml.Exit_called (status 0)) |}];
  Command_test_helpers.complete param ~args:[ "t"; "-bar" ];
  [%expect {|
    -bar
    (command.ml.Exit_called (status 0)) |}];
  Command_test_helpers.complete param ~args:[ "t"; "-bar"; "" ];
  [%expect {|
    false
    true
    (command.ml.Exit_called (status 0)) |}];
  Command_test_helpers.complete param ~args:[ "t"; "--"; "" ];
  [%expect {|
    (command.ml.Exit_called (status 0)) |}];
  Command_test_helpers.complete param ~args:[ "-" ];
  [%expect
    {|
    --
    -bar
    -build-info
    -foo
    -help
    -version
    (command.ml.Exit_called (status 0)) |}]
;;

(* A previous implementation of [test_complete] undid changes to environment variables.
   Testing that this implementation does not. *)
let%expect_test "side effects" =
  let var = "FOO" in
  let set_by_complete = "set by [complete]" in
  let state = ref "" in
  let complete _env ~part:_ =
    state := set_by_complete;
    Unix.putenv ~key:var ~data:set_by_complete;
    []
  in
  let param =
    let%map_open.Command () = anon ("_" %: Arg_type.create ~complete (const ())) in
    print_cr [%here] [%message "This shouldn't have run."];
    state := "set by param";
    Unix.putenv ~key:var ~data:"set by param"
  in
  Command_test_helpers.complete param ~args:[ "" ];
  require_compare_equal [%here] (module String) !state set_by_complete;
  require_compare_equal [%here] (module String) (Sys.getenv_exn var) set_by_complete;
  [%expect {| (command.ml.Exit_called (status 0)) |}]
;;
