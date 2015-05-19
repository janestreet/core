TEST_MODULE = struct
  open Core.Std

  let command =
    Command.exec ()
      ~summary:"constructed Command.t"
      ~path_to_exe:(`Relative_to_me "../demo_grandparent.exe")

  let subcommands t =
    let rec loop : Command.Shape.t -> _ = function
      | Basic -> []
      | Group subs -> subs
      | Exec thunk -> loop (thunk ())
    in
    loop (Command.shape t)

  let rec subcommand_hierarchy t : Sexp.t =
    let subs = subcommands t in
    List (List.map subs ~f:(fun (sub, t) ->
      Sexp.List [Atom sub; subcommand_hierarchy t]))

  TEST_UNIT =
    <:test_result< Sexp.t >> (subcommand_hierarchy command)
      ~expect:(Sexp.of_string
                 "((help    ()) \
                \n (version ()) \
                \n (parent ( \
                \n   (help    ()) \
                \n   (version ()) \
                \n   (child   ()))))")

end
