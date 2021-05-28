open! Core
open! Import

(** Not all tests of command completion need to go here. But this is the catch-all place
    for completion tests that don't have a better home. *)

let param =
  let%map_open.Command (_ : bool) = anon ("BOOL" %: bool)
  and (_ : bool list) = anon (sequence ("BOOL" %: bool)) in
  ()
;;

let test args = Command_test_helpers.complete param ~args

let%expect_test "completion of anons" =
  test [ "" ];
  [%expect {|
    false
    true
    (command.ml.Exit_called (status 0)) |}];
  test [ "t" ];
  [%expect {|
    true
    (command.ml.Exit_called (status 0)) |}];
  test [ "f" ];
  [%expect {|
    false
    (command.ml.Exit_called (status 0)) |}];
  test [ "true"; "" ];
  [%expect {|
    false
    true
    (command.ml.Exit_called (status 0)) |}];
  test [ "true"; "t" ];
  [%expect {|
    true
    (command.ml.Exit_called (status 0)) |}];
  (* First argument is invalid, but we can still complete later arguments. *)
  test [ "bool"; "" ];
  [%expect {|
    false
    true
    (command.ml.Exit_called (status 0)) |}];
  ()
;;

let%expect_test "completion after [-help]" =
  test [ "-" ];
  [%expect
    {|
    -build-info
    -help
    -version
    (command.ml.Exit_called (status 0)) |}];
  test [ "-h" ];
  [%expect {|
    -help
    (command.ml.Exit_called (status 0)) |}];
  test [ "-help"; "" ];
  [%expect {|
    false
    true
    (command.ml.Exit_called (status 0)) |}]
;;

let%expect_test "special debug flags shouldn't offer autocomplete suggestions" =
  let module Simple_group = Test_command_completion_shared.Simple_group in
  let exit_called = "(command.ml.Exit_called (status 0))" in
  let render_output s =
    String.split_lines s |> List.filter ~f:(Fn.non (String.equal exit_called))
  in
  let test command ~all_prefixes_of_subcommand:subcommand =
    let results =
      let%map.List subcommand =
        List.init (List.length subcommand + 1) ~f:(List.take subcommand)
      and flag = [ "-build-info"; "-help"; "-version" ] in
      let args = List.concat [ subcommand; [ flag; "" ] ] in
      Command_test_helpers.complete_command command ~args;
      subcommand, flag, render_output [%expect.output]
    in
    Ascii_table.to_string_noattr
      (let open Ascii_table.Column in
       [ create "subcommand" (fun (x, _, _) -> String.concat x ~sep:" ")
       ; create "flag" (fun (_, x, _) -> x)
       ; create "alternatives" (fun (_, _, x) -> String.concat x ~sep:", ")
       ])
      results
    |> print_string
  in
  test Simple_group.basic ~all_prefixes_of_subcommand:[];
  [%expect
    {|
    ┌────────────┬─────────────┬──────────────┐
    │ subcommand │ flag        │ alternatives │
    ├────────────┼─────────────┼──────────────┤
    │            │ -build-info │ false, true  │
    │            │ -help       │ false, true  │
    │            │ -version    │ false, true  │
    └────────────┴─────────────┴──────────────┘ |}];
  test Simple_group.group ~all_prefixes_of_subcommand:[ "basic" ];
  [%expect
    {|
    ┌────────────┬─────────────┬──────────────────────┐
    │ subcommand │ flag        │ alternatives         │
    ├────────────┼─────────────┼──────────────────────┤
    │            │ -build-info │ basic, help, version │
    │            │ -help       │ basic, help, version │
    │            │ -version    │ basic, help, version │
    │ basic      │ -build-info │                      │
    │ basic      │ -help       │ false, true          │
    │ basic      │ -version    │                      │
    └────────────┴─────────────┴──────────────────────┘ |}];
  test Simple_group.command ~all_prefixes_of_subcommand:[ "group"; "basic" ];
  [%expect
    {|
    ┌─────────────┬─────────────┬──────────────────────┐
    │ subcommand  │ flag        │ alternatives         │
    ├─────────────┼─────────────┼──────────────────────┤
    │             │ -build-info │ group, help, version │
    │             │ -help       │ group, help, version │
    │             │ -version    │ group, help, version │
    │ group       │ -build-info │                      │
    │ group       │ -help       │ basic, help          │
    │ group       │ -version    │                      │
    │ group basic │ -build-info │                      │
    │ group basic │ -help       │ false, true          │
    │ group basic │ -version    │                      │
    └─────────────┴─────────────┴──────────────────────┘ |}]
;;

let%expect_test "demo [complete_subcommands]" =
  let module Simple_group = Test_command_completion_shared.Simple_group in
  let test ?complete_subcommands ~args command =
    Command_test_helpers.complete_command command ?complete_subcommands ~args
  in
  test Simple_group.command ?complete_subcommands:None ~args:[ "g" ];
  [%expect {|
    group
    (command.ml.Exit_called (status 0)) |}];
  let complete_subcommands ~path ~part choices =
    print_s
      [%message
        "offered choices"
          (choices : string list list)
          (path : string list)
          (part : string)];
    Some [ "we"; "just"; "give"; "back"; "what"; "was"; "returned" ]
  in
  test
    Simple_group.command
    ~complete_subcommands
    ~args:[ "we-can-put-anything-here-command-doesn't-seem-to-care" ];
  [%expect
    {|
    ("offered choices"
      (choices (
        (group basic)
        (group help)
        (version)
        (help)))
      (path (__exe_name__))
      (part we-can-put-anything-here-command-doesn't-seem-to-care))
    we just give back what was returned
    (command.ml.Exit_called (status 0)) |}];
  test
    Simple_group.command
    ~complete_subcommands
    ~args:[ "will-not"; "offer-completions" ];
  [%expect {| (command.ml.Exit_called (status 0)) |}]
;;
