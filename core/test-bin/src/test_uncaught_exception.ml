open! Core
open! Async
open Expect_test_helpers_async

let%expect_test "uncaught exception at module init" =
  let prog = "just_raise.exe" in
  (* Copied from ppx/ppx_expect/collector/check_backtraces.mll *)
  let is_backtrace_line line =
    [ "Raised at "; "Called from "; "Raised by primitive operation " ]
    |> List.exists ~f:(fun prefix -> String.is_prefix line ~prefix)
  in
  let%bind () =
    within_temp_dir
      ~links:[ "../bin/just_raise.exe", `In_path_as, prog ]
      (fun () ->
        let%bind (exit_status : int) = Sys.command (Sys.quote prog) in
        let output = [%expect.output] in
        print_endline [%string "%{prog} exited with status %{exit_status#Int}"];
        print_endline "";
        output
        |> String.split_lines
        |> List.map ~f:(fun line ->
             if is_backtrace_line line then "<backtrace lines elided>" else line)
        (* Don't depend on backtrace contents, which are unstable. *)
        |> List.remove_consecutive_duplicates ~equal:String.equal
        (* Don't depend on backtrace line count. *)
        |> List.iter ~f:print_endline;
        return ())
  in
  [%expect
    {|
    just_raise.exe exited with status 2

    Uncaught exception:

      (just_raise.ml.E 42)

    <backtrace lines elided>
    |}];
  return ()
;;
