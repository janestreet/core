open Core.Std

module Command = Core.Std.Command

(* BEGIN -- useful utilities *)

include struct
  open Async.Std
  let uses_async : (unit -> int Deferred.t, unit -> unit) Command.Spec.t =
    Command.Spec.step (fun finished () ->
      printf "uses_async\n%!";
      upon (finished ()) Shutdown.shutdown;
      let () = never_returns (Scheduler.go ()) in
      ())
end

let flag_prompt_if_missing name of_string ~doc =
  let open Command.Spec in
  let arg = Arg_type.create of_string in
  map (flag ("-" ^ name) (optional arg) ~doc) ~f:(function
    | Some v -> v
    | None ->
      printf "enter %s: %!" name;
      match In_channel.input_line stdin with
      | None -> failwith "no value entered. aborting."
      | Some line -> (of_string line)
  )

let fields_flag spec ~doc s field =
  let open Command.Spec in
  let name = Fieldslib.Field.name field in
  let name = String.tr ~target:'_' ~replacement:'-' name in
  s +> flag ("-" ^ name) spec ~doc

(* END -- useful utilities *)

module Sing = struct
  module Note = struct
    type t = A | B | C | D | E | F | G with sexp
    let of_string x = t_of_sexp (Sexp.Atom x)
    let arg_type = Command.Spec.Arg_type.create of_string
  end
  let command =
    Command.basic ~summary:"sing a song"
      Command.Spec.(
        (* flags *)
        step (fun k slow -> k ~slow)
        +> flag "slow" ~aliases:["AA";"-BB"] no_arg ~doc:" sing slow"
        +> flag "-loudness" (optional int)
          ~doc:"N how loud to sing (number of decibels)"
        +> flag "-date" (optional date) ~doc:"DATE the date"
        +> flag "-note" (listed Note.arg_type) ~doc:"NOTE a note"
        (* anonymous arguments *)
        +> anon ("NAME" %: string)
        +> anon ("FOO" %: string)
        +> anon (sequence ("BAR" %: string))
      )
      (fun ~slow loudness date notes song _ _ () ->
        (* ... your code here... *)
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
      )
end

let revision_flag =
  let open Command.Spec in
  flag "-revision" ~doc:"REV revision number" (required string)

module Hg_log = struct
  let command =
    Command.basic ~summary:"show a point in hg history"
      Command.Spec.(
        empty
        +> revision_flag
        +> flag "-print" no_arg ~doc:" display all changes (not just a summary)")
      (fun revision print () ->
        (* ... your code here ... *)
        ignore (revision, print)
      )
end

module Hg_cat = struct
  let command =
    Command.basic ~summary:"cat a file from hg history"
      Command.Spec.(empty +> revision_flag +> anon ("FILE" %: string))
      (fun revision file () ->
        (* ... your code here ... *)
        ignore (revision, file)
      )
end

module Cat = struct
  open Async.Std
  let command =
    Command.basic ~summary:"example async command: cat a file to stdout"
      Command.Spec.(empty +> anon ("FILE" %: string) ++ uses_async)
      (fun path () ->
        Reader.with_file path ~f:(fun r ->
          Pipe.iter_without_pushback (Reader.pipe r) ~f:(fun chunk ->
            Writer.write (Lazy.force Writer.stdout) chunk))
        >>= fun _ ->
        return 0)
end

module Prompting = struct
  let command =
    Command.basic ~summary:"command demonstrating prompt-if-missing flags"
      Command.Spec.(
        (* flags *)
        empty
        +> flag "-rev" (required string) ~doc:" print stuff"
        +> flag_prompt_if_missing "id" Fn.id ~doc:" whatever"
      )
      (fun revision id () ->
        (* ... your code here ... *)
        print_endline "MAIN STARTED";
        printf "revision = %s\n%!" revision;
        printf "id = %s\n%!" id
      )
end

module Fields = struct

  type t = {
    foo : int;
    bar : string option;
    baz : float list;
  } with fields, sexp

  let main t =
    (* ... your code here ... *)
    print_endline (Sexp.to_string_hum (sexp_of_t t))

  let command =
    Command.basic ~summary:"example using fieldslib"
      Command.Spec.(
        Fields.fold
          ~init:(step Fn.id)
          ~foo:(fields_flag (required int)    ~doc:"N foo factor")
          ~bar:(fields_flag (optional string) ~doc:"B error bar (optional)")
          ~baz:(fields_flag (listed float)    ~doc:"X whatever (listed)"))
      (fun foo bar baz () ->
        main {foo; bar; baz})

end

module Fields_with_default = struct

  type t = {
    foo : int;
    bar : string option;
    baz : float list;
  } with fields, sexp

  let default = {
    foo = 1;
    bar = None;
    baz = [];
  }

  let main t () =
    (* ... your code here ... *)
    print_endline (Sexp.to_string_hum (sexp_of_t t))

  let fields_flag spec ~doc acc field =
    let name = Fieldslib.Field.name field in
    let name = String.tr ~target:'_' ~replacement:'-' name in
    let f main (record:t) value =
      main (Fieldslib.Field.fset field record value)
    in
    let open Command.Spec in
    step f ++ acc +> flag ("-" ^ name) spec ~doc

  let command =
    Command.basic ~summary:"example using fieldslib"
      Command.Spec.(
        Fields.fold
          ~init:(empty +> const default)
          ~foo:(fields_flag (required int)    ~doc:"N foo factor")
          ~bar:(fields_flag (optional string) ~doc:"B error bar (optional)")
          ~baz:(fields_flag (listed float)    ~doc:"X whatever (listed)"))
      main

end

module Complex_anons = struct
  let command =
    Command.basic ~summary:"command with complex anonymous argument structure"
      Command.Spec.(
        empty
        +> anon ("A" %: string)
        +> anon ("B" %: string)
        +> anon (maybe (t3
            ("C" %: string)
            ("D" %: string)
            (maybe (t3
              ("E" %: string)
              ("F" %: string)
              (sequence ("G" %: string))))))
      )
      (fun a b rest () ->
        (* ... your code here... *)
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
      )
end

module Goodies = struct
  let command =
    Command.basic ~summary:"demo of how to get various backdoor values"
      Command.Spec.(
        empty
        +> help
        +> path
        +> args
        +> flag "t" (optional string) ~doc:""
        +> flag "-fail" no_arg ~doc:" die, die, die!"
      )
      (fun help path args _ _ () ->
        print_endline "PATH:";
        List.iter path ~f:(fun x -> print_endline ("  " ^ x));
        print_endline "ARGS:";
        List.iter args ~f:(fun x -> print_endline ("  " ^ x));
        print_endline "HELP!";
        print_endline (Lazy.force help)
      )
end

module Long_flag_description = struct
  let command =
    Command.basic ~summary:"demo of word wrap for long flag descriptions"
      Command.Spec.(
        empty
        +> flag "-foo" no_arg ~doc:" Lorem ipsum dolor sit amet, consectetur
          adipiscing elit. Vivamus fermentum condimentum eros, sit amet
          pulvinar dui ultrices in."
      )
      (fun _ () -> ())
end

let command =
  Command.group ~summary:"fcommand examples"
  [
    ("sing", Sing.command);
    ("hg",
      Command.group ~summary:"commands sharing a flag specification" [
        ("log", Hg_log.command);
        ("cat", Hg_cat.command);
      ]);
    ("cat", Cat.command);
    ("prompting", Prompting.command);
    ("fields", Fields.command);
    ("fields-with-default", Fields_with_default.command);
    ("complex-anons", Complex_anons.command);
    ("sub",
      Command.group ~summary:"a subcommand" [
        ("goodies", Goodies.command);
      ]);
    ("long-flag-description", Long_flag_description.command);
  ]

let () = Exn.handle_uncaught ~exit:true (fun () -> Command.run command)

