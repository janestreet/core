open! Core

let summary = "test uses of [env] parameter in [Command]"
let path_to_exe = `Absolute "/usr/bin/env"
let noop = Command.exec ~summary:"Do nothing to environment" ~path_to_exe ()

let clear =
  Command.exec
    ~summary:"Clear out environment variables"
    ~env:(`Replace [])
    ~path_to_exe
    ()
;;

let set_one =
  Command.exec
    ~summary:"Add one variable"
    ~env:(`Override [ "TEST_VAR", Some "TEST_VALUE" ])
    ~path_to_exe
    ()
;;

let command = Command.group ~summary [ "clear", clear; "noop", noop; "set-one", set_one ]
let () = Command_unix.run command
