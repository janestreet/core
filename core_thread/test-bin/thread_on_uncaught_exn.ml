open Core

let thread_main () =
  printf "Start of thread_main\n%!";
  failwith "exception thrown out of thread_main"

let main ~create_thread ~join_thread =
  Core.at_exit (fun () -> printf "Core.at_exit callback ran\n%!");
  Caml.at_exit (fun () -> printf "Caml.at_exit callback ran\n%!");
  printf "About to create thread\n%!";
  let thread = create_thread thread_main () in
  join_thread thread;
  printf "Thread joined; main done\n%!"

let core_command =
  Command.basic
    ~summary:"Test Core_thread.create's ~on_uncaught_exn argument"
    [%map_open.Command
      let on_uncaught_exn =
        let arg_type =
          Command.Arg_type.of_alist_exn ~list_values_in_help:false
            [ "print-to-stderr", `Print_to_stderr
            ; "kill-whole-process", `Kill_whole_process
            ]
        in
        flag
          "-on-uncaught-exn"
          (required arg_type)
          ~doc:"print-to-stderr Value to pass as ~on_uncaught_exn"
      in
      fun () ->
        main
          ~create_thread:(Core_thread.create ~on_uncaught_exn)
          ~join_thread:Core_thread.join
    ]

let caml_command =
  let main () =
    main
      ~create_thread:Caml_threads.Thread.create
      ~join_thread:Caml_threads.Thread.join
  in
  Command.basic
    ~summary:"Test Caml_threads.Thread.create's behaviour when exns are thrown"
    (Command.Param.return main)

let command =
  Command.group
    ~summary:"Demonstrate behaviour when exns are thrown out of Thread.create"
    [ "core", core_command
    ; "caml", caml_command
    ]

let () = Command_unix.run command
