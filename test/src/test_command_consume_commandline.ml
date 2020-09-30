open! Core
open! Import


module Foobar = struct
  type t =
    | Foo
    | Bar
  [@@deriving sexp_of]

  let arg_type = Command.Arg_type.of_alist_exn [ "Foo", Foo; "Bar", Bar ]
end

let param =
  let%map_open.Command anon = escape_anon ~final_anon:("foobar" %: Foobar.arg_type)
  and flag = flag "-flag" (optional string) ~doc:"" in
  anon, flag
;;

let%expect_test "execution" =
  let on_success ((foobar, rest), flag) =
    print_s [%message (foobar : Foobar.t) (rest : string list) (flag : string option)]
  in
  let test = (Command_test_helpers.parse_command_line param |> unstage) ~on_success in
  (* [-dash] isn't a known flag, but this works: *)
  test [ "Foo"; "-dash"; "stuff" ];
  [%expect {| ((foobar Foo) (rest (-dash stuff)) (flag ())) |}];
  (* Before we start [anon] parsing, known flags are parsed: *)
  test [ "-flag"; "flag"; "Foo"; "-dash"; "string" ];
  [%expect {| ((foobar Foo) (rest (-dash string)) (flag (flag))) |}];
  (* But after, they're ignored-both the implicit [-help] and our explicit [-flag]. *)
  test [ "Foo"; "-dash"; "-help"; "-flag"; "flag" ];
  [%expect {| ((foobar Foo) (rest (-dash -help -flag flag)) (flag ())) |}]
;;

let%expect_test "anon after [anon_escape]" =
  require_does_raise [%here] (fun () ->
    Command.basic
      ~summary:"this won't validate"
      (let%map_open.Command () = return ()
       and last, rest = escape_anon ~final_anon:("LAST" %: string)
       and after = anon ("AFTER" %: string) in
       fun () ->
         print_s [%message (last : string) (rest : string list) (after : string)]));
  [%expect
    {|
    (Failure
     "the grammar LAST [ARG ...] AFTER for anonymous arguments is not supported because there is the possibility for arguments (AFTER) following a variable number of arguments (LAST [ARG ...]).  Supporting such grammars would complicate the implementation significantly.") |}]
;;

let%expect_test "completion" =
  Command_test_helpers.complete param ~args:[ "" ];
  [%expect {|
    Bar
    Foo
    (command.ml.Exit_called (status 0)) |}];
  Command_test_helpers.complete param ~args:[ "Foo"; "" ];
  [%expect {| (command.ml.Exit_called (status 0)) |}];
  Command_test_helpers.complete param ~args:[ "-" ];
  [%expect
    {|
    -build-info
    -flag
    -help
    -version
    (command.ml.Exit_called (status 0)) |}]
;;
