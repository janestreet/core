open Core.Std

let command =
  Command.basic ~summary:"demo of word wrap for long flag descriptions"
    Command.Spec.(
      empty
      +> flag "-foo" no_arg ~doc:" Lorem ipsum dolor sit amet, consectetur
        adipiscing elit. Vivamus fermentum condimentum eros, sit amet
        pulvinar dui ultrices in."
    )
    (fun _ () -> ())


let () = Command.run command

