open Core.Std

let summary = "summary"

let basic =
  Command.basic' ~summary
    Command.Param.(
      path
      @> help
      @> args
      @> flag "foo" (optional int) ~doc:"NUM flag value"
      @> anon (maybe ("BAR" %: int))
      @> nil
    )
    (fun path help args _foo _bar () ->
       printf !"path = %{sexp:string list}\n" path;
       printf !"args = %{sexp:string list}\n" args;
       printf !"help = \n%s" (Lazy.force help)
    )

let group name subcommands = (name, Command.group ~summary subcommands)

let command =
  Command.group ~summary [
    ("foo", basic);
    ("bar", Command.group ~summary [("baz", basic)]);
  ]

let () = Command.run command
