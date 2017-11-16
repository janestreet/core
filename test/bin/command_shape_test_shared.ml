(* These tests check that [Command.shape] returns regardless of the order in which the
   child writes and closes its stderr and stdout.  Returning an error is a successful
   outcome.

   We also used to have some tests where the child process hung and never closed its
   stdout/stderr, but that seems like too rare a failure mode to handle. We've been
   running vulnerable code for years without seeing it. *)
open! Core

let summary = "test failure modes of Command shape extraction"

type t =
  | Write_stdout
  | Write_stderr
  | Close_stdout
  | Close_stderr
[@@deriving compare, enumerate, sexp]

let instructions_variable = "THE_INSTRUCTIONS"

(* Can't use Command because it'll behave nicely in response to the shape extraction. *)
let in_child () =
  let buf = String.make (64 * 1024 + 1) 'a' in
  Sys.getenv_exn instructions_variable
  |> Sexp.of_string
  |> [%of_sexp: t list]
  |> List.iter ~f:(function
    | Write_stdout -> ignore (Unix.write_substring Unix.stdout ~buf : int)
    | Write_stderr -> ignore (Unix.write_substring Unix.stderr ~buf : int)
    | Close_stdout -> Unix.close Unix.stdout
    | Close_stderr -> Unix.close Unix.stderr);
;;

let parent_cmd =
  let open Command.Let_syntax in
  [%map_open
    let instructions = anon (sequence ("INSTRUCTION" %: sexp_conv [%of_sexp: t])) in
    fun () ->
      let path_to_exe = `Relative_to_me "./command_shape_test_child.exe" in
      let cmd = Command.exec ~summary:"" ~path_to_exe () in
      Unix.putenv ~key:instructions_variable
        ~data:(sprintf !"%{sexp:t list}" instructions);
      match
        Or_error.try_with (fun () ->
          Command.Shape.Fully_forced.create (Command.shape cmd))
      with
      | Ok (_ : Command.Shape.Fully_forced.t) -> assert false
      | Error e ->
        match Error.sexp_of_t e with
        | List (Atom "cannot parse command shape":: _) ->
          print_endline "[Command.shape] returned but failed to parse, as expected";
        | _ -> assert false
  ]
;;

let in_parent () = Command.run (Command.basic ~summary parent_cmd)
