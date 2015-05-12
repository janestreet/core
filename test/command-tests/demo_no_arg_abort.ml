open Core.Std

let dump_readme () : never_returns =
  print_endline "This is the contents of README.txt.";
  exit 0

let dump_grammar () : never_returns =
  print_endline "This is the contents of GRAMMAR.txt.";
  exit 0

type args = {
  foo : int option;
  anon : int option;
} with sexp_of

let command =
  Command.basic ~summary:"demonstrate the no_arg_abort flag type"
    Command.Spec.(
      empty
      +> flag "-readme" (no_arg_abort ~exit:dump_readme)
           ~doc: " display README.txt and quit"
      +> flag "-grammar" (no_arg_abort ~exit:dump_grammar)
           ~doc: " display GRAMMAR.txt and quit"
      +> flag "-foo" (optional int)
           ~doc:"NUM numeric flag"
      +> anon (maybe ("NUM" %: int))
    )
    (fun () () foo anon () ->
       printf !"entering main with args: %{sexp:args}\n" {foo; anon};
       print_endline "doing stuff ...";
    )

let () = Command.run command
