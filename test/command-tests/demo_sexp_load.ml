open Core.Std

type config = {foo : int; bar : string} with sexp

let command =
  Command.basic' ~summary:"demonstrate sexp loading during command line parsing"
    Command.Param.(
      step ~f:(fun m file ->
        (* this is kind of a strange thing to do in [step], but imagine a case where we
           want to validate some argument against the contents of a config file. *)
        m (Sexp.load_sexp_conv_exn file config_of_sexp))
      @@ anon ("CONFIG-FILE" %: file)
      @> nil
    )
    (fun config () ->
       Sexp.output_hum stdout (sexp_of_config config))

let () = Command.run command
