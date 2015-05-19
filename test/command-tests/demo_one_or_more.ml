open Core.Std

let command =
  Command.basic' ~summary:"Command.Spec.{one_or_more,non_empty_sequence} demo"
    Command.Param.(
      flag "foo" (one_or_more string) ~doc:"X required listed flag"
      @> anon (non_empty_sequence ("Y" %: string))
      @> nil
    )
    (fun xs ys () ->
       let info name zs =
         let zs = fst zs :: snd zs in
         Info.create name zs <:sexp_of<string list>>
       in
       let info =
         Info.of_list [
           info "xs" xs;
           info "ys" ys;
         ]
       in
       print_endline (Info.to_string_hum info)
    )

let () = Command.run command
