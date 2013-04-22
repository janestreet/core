open Core.Std

open Core.Std.Command

let spec =
  Spec.(
    empty
    +> flag "-fondue" no_arg ~doc:" make sure fondue is doing the same stuff"
    +> flag "-gilt" (optional string) ~doc:"NAME which gilt should do the stuff"
    +> anon ("OXIDE" %: int)
    +> anon (maybe ("PATH" %: file))
  )

let leaf =
  basic ~summary:"this command does stuff here"
    spec (fun _ _ _ _ () -> print_endline "doing stuff here!")

let leaf_no_summary =
  basic ~summary:""
    spec (fun _ _ _ _ () -> print_endline "doing stuff here!")

let readme () =
"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque et
ante a nibh scelerisque ultrices. Fusce consectetur dictum ante quis
commodo. Aenean fringilla lacus eget magna interdum sit amet fringilla
eros eleifend. Proin auctor lectus quis ipsum varius tempor. Proin
fermentum luctus ipsum, in eleifend ipsum euismod in. Lorem ipsum
dolor sit amet, consectetur adipiscing elit. Suspendisse tempor neque
cursus ante cursus imperdiet. Quisque convallis dolor a nisl aliquam
nec sagittis lectus adipiscing. Quisque mattis vehicula metus vel
ultrices. Pellentesque vel erat a lacus volutpat mollis eget tristique
mi. Nulla auctor tristique condimentum. Fusce lobortis elementum
feugiat. Cras quam nibh, laoreet lacinia rhoncus vel, fringilla eget
erat. Vestibulum in feugiat metus."

let leaf_with_readme =
  basic ~summary:"this command does stuff here" ~readme
    spec (fun _ _ _ _ () -> print_endline "doing stuff here!")

type internals = { path : string list; args : string list; help : string; } with sexp

let leaf_show_internals =
  basic ~summary:"display internal values" ~readme
    Spec.(empty +> path +> help +> args)
    (fun path help args () ->
      let help = Lazy.force help in
      Sexp.output_hum stdout (sexp_of_internals {path; help; args}))

let () =
  run
    (group ~summary:"this command does stuff" [
      ("jab", leaf_with_readme);
      ("moonshot", leaf);
      ("mixture", leaf);
      ( "adverb"
      , group ~summary:"this command does more stuff" ~readme [
        ("nemeses",  leaf_show_internals);
        ("drolly", leaf);
        ("opposable", leaf_no_summary);
      ]);
    ])

let () = ()
