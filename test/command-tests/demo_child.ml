open Core.Std

(* Ensure that programs querying [Command.shape] of this executable preserve the
   environment. *)
let () = assert ("test" = Sys.getenv_exn "ENVVAR")

type args = {
  date : Date.t;
  file : string option;
  num : int;
  bool : bool option;
} with sexp_of

let command =
  Command.basic' ~summary:"child part of Command.exec demo"
    ~readme:(fun () ->
      "readme text. readme text. readme text. readme text. readme text. readme\n\
       text. readme text. readme text. readme text. readme text. readme text. readme\n\
       text. readme text. readme text.")
    Command.Param.(
      flag "date" (required date) ~doc:"DATE a required date argument"
      @> flag "file" (optional file) ~doc:"FILE an optional file argument"
      @> anon ("NUM" %: int)
      @> anon (maybe ("BOOL" %: bool))
      @> nil
    )
    (fun date file num bool () ->
       printf !"Entering main: %{Sexp}\n" (sexp_of_args {date; file; num; bool})
    )

let () = Command.run command
