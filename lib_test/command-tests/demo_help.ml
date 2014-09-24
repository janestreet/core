open Core.Std

type pong = Foo | Bar | Baz

let pong_arg =
  Command.Spec.Arg_type.of_alist_exn [
    ("foo", Foo);
    ("bar", Bar);
    ("baz", Baz);
  ]

let readme () =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et\n\
   ante a nibh scelerisque ultrices. Fusce consectetur dictum ante quis\n\
   commodo."

let basic =
  Command.basic ~summary:"this command does stuff here" ~readme
    Command.Spec.(
      empty
      +> flag "-ping" no_arg ~doc:" make sure ping is doing the same stuff"
      +> flag "-pong" (optional pong_arg) ~doc:"NAME which pong should do the stuff"
      +> anon ("OXIDE" %: pong_arg) (* Note: "-" is tacked onto flag names if missing *)
      +> anon (maybe ("path" %: file)) (* Note: "path" becomes "PATH" in help output *)
    )
    (fun _ _ _ _ () ->
       print_endline "doing stuff here!")

let command =
  Command.group ~summary:"this command does stuff" [
    ("jab", basic);
    ("JIB", basic); (* Note: subcommand names are automatically lowercased *)
    ( "adverb"
    , Command.group ~summary:"this command does more stuff" ~readme [
      ("drolly", basic);
      ("opposable", basic);
    ]);
  ]

let () = Command.run command
