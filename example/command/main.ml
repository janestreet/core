open Core

(* BEGIN -- useful utilities *)

let flag_prompt_if_missing name of_string ~doc =
  let open Command.Param in
  let arg = Arg_type.create of_string in
  (let open Command.Let_syntax in
   [%map_open
     let value = flag ("-" ^ name) (optional arg) ~doc
     in
     match value with
     | Some v -> v
     | None ->
       printf "enter %s: %!" name;
       match In_channel.input_line In_channel.stdin with
       | None -> failwith "no value entered. aborting."
       | Some line -> (of_string line)
   ])

(* END -- useful utilities *)

module Sing = struct
  module Note = struct
    type t = A | B | C | D | E | F | G [@@deriving sexp]
    let of_string x = t_of_sexp (Sexp.Atom x)
    let arg_type = Command.Arg_type.create of_string
  end
  let command =
    Command.basic ~summary:"sing a song"
      (let open Command.Let_syntax in
       [%map_open
          (* flags *)
         let slow     = flag "slow" ~aliases:["AA";"-BB"] no_arg ~doc:" sing slow"
         and loudness = flag "-loudness" (optional int)
                          ~doc:"N how loud to sing (number of decibels)"
         and date     = flag "-date" (optional date) ~doc:"DATE the date"
         and notes    = flag "-note" (listed Note.arg_type) ~doc:"NOTE a note"
         (* anonymous arguments *)
         and song = anon ("NAME" %: string)
         and _foo = anon ("FOO" %: string)
         and _bar = anon (sequence ("BAR" %: string))
         in
         fun () ->
           (* command body *)
           print_endline (if slow then "slow" else "fast");
           printf "loudness = %s\n"
             (Option.value ~default:"none"
                (Option.map ~f:Int.to_string loudness));
           printf "date = %s\n"
             (Option.value ~default:"no date"
                (Option.map date ~f:Date.to_string));
           printf "song name = %s\n" song;
           List.iter notes ~f:(fun note ->
             print_endline
               (Sexp.to_string_hum (
                  Sexp.List [Sexp.Atom "note"; Note.sexp_of_t note])))
       ])
end

let revision_flag =
  let open Command.Param in
  flag "-revision" ~doc:"REV revision number" (required string)

module Hg_log = struct
  let command =
    Command.basic ~summary:"show a point in hg history"
      (let open Command.Let_syntax in
       [%map_open
         let revision = revision_flag
         and print = flag "-print" no_arg ~doc:" display all changes (not just a summary)"
         in
         fun () ->
           ignore (revision, print)
       ])
end

module Hg_cat = struct
  let command =
    Command.basic ~summary:"cat a file from hg history"
      (let open Command.Let_syntax in
       [%map_open
         let revision = revision_flag
         and file = anon ("FILE" %: string)
         in
         fun () ->
           ignore (revision, file)
       ])
end

module Cat = struct
  open Async (* async has its own Command overlay module that introduces functions for
                constructing commands with a body that returns a [Deferred.t]. *)
  let command =
    Command.async ~summary:"example async command: cat a file to stdout"
      (let open Command.Let_syntax in
       let%map_open path = anon ("FILE" %: string)
       in
       let open Deferred.Let_syntax in
       fun () ->
         Reader.with_file path ~f:(fun r ->
           Pipe.iter_without_pushback (Reader.pipe r) ~f:(fun chunk ->
             Writer.write (Lazy.force Writer.stdout) chunk))
         >>= fun _ ->
         return ()
      )
end

module Prompting = struct
  let command =
    Command.basic ~summary:"command demonstrating prompt-if-missing flags"
      (let open Command.Let_syntax in
       [%map_open
         let revision = flag "-rev" (required string) ~doc:" print stuff"
         and id = flag_prompt_if_missing "id" Fn.id ~doc:" whatever"
         in
         fun () ->
           print_endline "MAIN STARTED";
           printf "revision = %s\n%!" revision;
           printf "id = %s\n%!" id
       ])
  ;;
end

module Composite = struct

  type t = {
    foo : int;
    bar : string option;
    baz : float list;
  } [@@deriving fields, sexp]

  let t_param =
    (let open Command.Let_syntax in
     [%map_open
       let foo = flag "foo" (required int)    ~doc:"N foo factor"
       and bar = flag "bar" (optional string) ~doc:"B error bar (optional)"
       and baz = flag "baz" (listed float)    ~doc:"X whatever (listed)"
       in
       {foo; bar; baz}
     ])
  ;;

  let command =
    Command.basic ~summary:"example using a composite record param"
      (let open Command.Let_syntax in
       [%map_open
         let t = t_param in
         fun () ->
           print_endline (Sexp.to_string_hum (sexp_of_t t))
       ])
  ;;
end

module Complex_anons = struct
  let command =
    Command.basic ~summary:"command with complex anonymous argument structure"
      (let open Command.Let_syntax in
       [%map_open
         let a = anon ("A" %: string)
         and b = anon ("B" %: string)
         and rest =
           anon (maybe
                   (t3
                      ("C" %: string)
                      ("D" %: string)
                      (maybe
                         (t3
                            ("E" %: string)
                            ("F" %: string)
                            (sequence ("G" %: string))))))
         in
         fun () ->
           printf "A = %s\n" a;
           printf "B = %s\n" b;
           Option.iter rest ~f:(fun (c, d, rest) ->
             printf "C = %s\n" c;
             printf "D = %s\n" d;
             Option.iter rest ~f:(fun (e, f, gs) ->
               printf "E = %s\n" e;
               printf "F = %s\n" f;
               List.iter gs ~f:(fun g ->
                 printf "G = %s\n" g;
               )
             )
           )
       ])
  ;;
end

module Goodies = struct
  let command =
    Command.basic ~summary:"demo of how to get various backdoor values"
      (let open Command.Let_syntax in
       [%map_open
         let help = help
         and path = path
         and args = args
         and _ = flag "t" (optional string) ~doc:""
         and _ = flag "-fail" no_arg ~doc:" die, die, die!"
         in
         fun () ->
           print_endline "PATH:";
           List.iter path ~f:(fun x -> print_endline ("  " ^ x));
           print_endline "ARGS:";
           List.iter args ~f:(fun x -> print_endline ("  " ^ x));
           print_endline "HELP!";
           print_endline (Lazy.force help)
       ])
  ;;
end

module Long_flag_description = struct
  let command =
    Command.basic ~summary:"demo of word wrap for long flag descriptions"
      (let open Command.Let_syntax in
       [%map_open
         let foo =
           flag "-foo" no_arg ~doc:" Lorem ipsum dolor sit amet, consectetur \
                                    adipiscing elit. Vivamus fermentum condimentum eros, \
                                    sit amet pulvinar dui ultrices in."
         in
         fun () ->
           ignore foo
       ])
  ;;
end

module Command_dot_exec = struct
  let command =
    Command.exec
      ~summary:"example usage of [Command.exec]"
      ~path_to_exe:(`Relative_to_me "main_no_recur.exe")
      ()
  ;;
end

let command =
  let commands =
    [ ("sing", Sing.command)
    ; ("hg",
       Command.group ~summary:"commands sharing a flag specification"
         [ ("log", Hg_log.command)
         ; ("cat", Hg_cat.command)
         ])
    ; ("cat", Cat.command)
    ; ("prompting", Prompting.command)
    ; ("composite", Composite.command)
    ; ("complex-anons", Complex_anons.command)
    ; ("sub", Command.group ~summary:"a subcommand" [ ("goodies", Goodies.command) ])
    ; ("long-flag-description", Long_flag_description.command)
    ; ("command-dot-exec", Command_dot_exec.command)
    ]
  in
  Command.group ~summary:"command examples" commands
;;

let () = Command.run command
