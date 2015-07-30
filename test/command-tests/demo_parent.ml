open Core.Std

(* Ensure that programs querying [Command.shape] of this executable preserve the
   environment. *)
let () = assert ("test" = Sys.getenv_exn "ENVVAR")

let exec_child =
  Command.exec ~summary:"this command is in another executable"
    ~path_to_exe:(`Relative_to_me "demo_child.exe") ()

let command =
  Command.group ~summary:"parent part of Command.exec demo" [
    ("child", exec_child)
  ]

let () = Command.run command
