open Core.Std
open Core_extended.Std

let of_cc = Deprecated_command.of_core_command

let () =
  let basic_command =
    Command.basic ~summary:"simple test"
      Command.Spec.(empty
                    +> flag "-thing" (required string) ~doc:"THING the thing"
                    +> anon ("foobar" %: string))
      (fun thing foobar () -> printf "thing: %s, foobar: %s" thing foobar)
  in
  let group_command =
    Command.group ~summary:"group test"
      [ "ze-basic-command", basic_command ]
  in
  let command =
    Deprecated_command.group ~summary:"test"
      [ "simple-test"      , basic_command |> of_cc
      ; "some-group"       , group_command  |> of_cc
      ; "the-nested-foobar", Common.command ~include_nested:() () |> of_cc
      ]
  in
  Deprecated_command.run command
