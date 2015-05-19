open Core.Std

let exec_parent =
  Command.exec ~summary:"this command is in another executable"
    ~path_to_exe:(`Relative_to_me "demo_parent.exe") ()

let command =
  Command.group ~summary:"grandparent part of Command.exec demo" [
    ("parent", exec_parent)
  ]

let () = Command.run command
