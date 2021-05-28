open! Core
open! Import

let%expect_test "raising in arg types" =
  let test =
    let arg_type =
      Command.Arg_type.map Command.Param.bool ~f:(fun b ->
        if b then b else failwith "must be true")
    in
    Command_test_helpers.parse_command_line Command.Param.(anon ("ARG" %: arg_type))
    |> unstage
  in
  (* does not raise at all *)
  require_does_not_raise [%here] (fun () -> test [ "true" ]);
  [%expect {| |}];
  (* raises in constructed arg type *)
  require_does_raise [%here] (fun () -> test [ "not a boolean" ]);
  [%expect
    {|
    Error parsing command line:

      failed to parse ARG value "not a boolean"
      (Failure "valid arguments: {false,true}")

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}];
  (* raises in map function *)
  require_does_raise [%here] (fun () -> test [ "false" ]);
  [%expect
    {|
    Error parsing command line:

      failed to parse ARG value "false"
      (Failure "must be true")

    For usage information, run

      CMD -help

    (command.ml.Exit_called (status 1)) |}]
;;
