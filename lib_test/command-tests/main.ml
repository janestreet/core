open Core.Std

let () = Command.run (Common.command ~include_nested:() ())
