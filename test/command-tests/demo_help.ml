open Core.Std

type pong = Foo | Bar | Baz | Baa_baa

let pong_arg =
  Command.Spec.Arg_type.of_alist_exn [
    ("foo", Foo);
    ("bar", Bar);
    ("baz", Baz);
    ("baa\\ baa", Baa_baa);
  ]

let readme () =
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et\n\
   ante a nibh scelerisque ultrices. Fusce consectetur dictum ante quis\n\
   commodo."

let basic =
  Command.basic' ~summary:"this command does stuff here" ~readme
    Command.Param.(
      flag "-ping" no_arg ~doc:" make sure ping is doing the same stuff"
      @> flag "-pong" (optional pong_arg) ~doc:"NAME which pong should do the stuff"
      @> anon ("OXIDE" %: pong_arg)
      @> anon ("DIODE" %: pong_arg)
      @> anon (maybe ("path" %: file)) (* Note: "path" becomes "PATH" in help output *)
      @> nil
    )
    (fun _ _ _ _ _ () ->
       print_endline "doing stuff here!")

let command =
  Command.group ~preserve_subcommand_order:() ~summary:"this command does stuff" [
    ("jab", basic);
    ("JIB", basic); (* Note: subcommand names are automatically lowercased *)
    ( "adverb"
    , Command.group ~summary:"this command does more stuff" ~readme [
      ("drolly", basic);
      ("opposable", basic);
    ]);
    ( "with-body"
    , Command.group ~summary:"this command does more stuff"
        ~body:(fun ~path ->
          print_endline "BEGIN body code";
          print_endline ("  path = (" ^ String.concat ~sep:" " path ^ ")");
          print_endline "  (running this instead of reporting a missing subcommand)";
          print_endline "END body code";
        ) [
        ("drolly", basic);
        ("opposable", basic);
      ]);
    ("ordered"
    , Command.group ~summary:"group with unsorted subcommands" ~preserve_subcommand_order:() [
      ("zzz", basic);
      ("aaa", basic);
    ]);
  ]

let () = Command.run command
