open Core
open Command.Let_syntax

let command =
  Command.basic ~summary:"demo of word wrap for long flag descriptions"
    [%map_open
      let foo =
        flag "-foo" no_arg ~doc:" Lorem ipsum dolor sit amet, consectetur \
                                 adipiscing elit. Vivamus fermentum condimentum eros, \
                                 sit amet pulvinar dui ultrices in."
      in
      fun () ->
        ignore foo
    ]

let () = Command.run command

