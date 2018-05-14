open! Core
open! Import
open! Expect_test_helpers_kernel
open! Command
open! Command.Private

let%test_module "word wrap" =
  (module struct
    let word_wrap = Format.V1.word_wrap

    let%test _ = word_wrap "" 10 = []

    let short_word = "abcd"

    let%test _ = word_wrap short_word (String.length short_word) = [short_word]

    let%test _ = word_wrap "abc\ndef\nghi" 100 = ["abc"; "def"; "ghi"]

    let long_text =
      "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus \
       fermentum condimentum eros, sit amet pulvinar dui ultrices in."

    let%test _ = word_wrap long_text 1000 =
                 ["Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus \
                   fermentum condimentum eros, sit amet pulvinar dui ultrices in."]

    let%test _ = word_wrap long_text 39 =
      (*
                    .........1.........2.........3.........4
                    1234567890123456789012345678901234567890
                 *)
                 ["Lorem ipsum dolor sit amet, consectetur";
                  "adipiscing elit. Vivamus fermentum";
                  "condimentum eros, sit amet pulvinar dui";
                  "ultrices in."]

    (* no guarantees: too-long words just overhang the soft bound *)
    let%test _ = word_wrap long_text 2 =
                 ["Lorem"; "ipsum"; "dolor"; "sit"; "amet,"; "consectetur";
                  "adipiscing"; "elit."; "Vivamus"; "fermentum"; "condimentum";
                  "eros,"; "sit"; "amet"; "pulvinar"; "dui"; "ultrices"; "in."]

  end)

let%test_unit _ =
  let path =
    Path.empty
    |> Path.add ~subcommand:"foo"
    |> Path.add ~subcommand:"bar"
    |> Path.add ~subcommand:"bar"
    |> Path.add ~subcommand:"baz"
  in
  [%test_result: string list] (Path.commands path) ~expect:["foo"; "bar"; "bar"; "baz"];
  let path = Path.replace_first path ~from:"bar" ~to_:"qux" in
  [%test_result: string list] (Path.commands path) ~expect:["foo"; "qux"; "bar"; "baz"];
  ()

let%test_module "[Anons]" =
  (module struct
    open Private.Anons

    let%test _ = String.equal (normalize "file")   "FILE"
    let%test _ = String.equal (normalize "FiLe")   "FILE"
    let%test _ = String.equal (normalize "<FiLe>") "<FiLe>"
    let%test _ = String.equal (normalize "(FiLe)") "(FiLe)"
    let%test _ = String.equal (normalize "[FiLe]") "[FiLe]"
    let%test _ = String.equal (normalize "{FiLe}") "{FiLe}"
    let%test _ = String.equal (normalize "<file" ) "<file"
    let%test _ = String.equal (normalize "<fil>a") "<fil>a"
    let%test _ = try ignore (normalize ""        ); false with _ -> true
    let%test _ = try ignore (normalize " file "  ); false with _ -> true
    let%test _ = try ignore (normalize "file "   ); false with _ -> true
    let%test _ = try ignore (normalize " file"   ); false with _ -> true
  end)

let%test_module "Cmdline.extend" =
  (module struct
    let path_of_list subcommands =
      List.fold subcommands ~init:(Path.root "exe") ~f:(fun path subcommand ->
        Path.add path ~subcommand)

    let extend path =
      match path with
      | ["foo"; "bar"] -> ["-foo"; "-bar"]
      | ["foo"; "baz"] -> ["-foobaz"]
      | _ -> ["default"]

    let test path args expected =
      let expected = Cmdline.of_list expected in
      let observed =
        let path = path_of_list path in
        let args = Cmdline.of_list args in
        Cmdline.extend args ~extend ~path
      in
      Pervasives.(=) expected observed

    let%test _ = test ["foo"; "bar"] ["anon"; "-flag"] ["anon"; "-flag"; "-foo"; "-bar"]
    let%test _ = test ["foo"; "baz"] []                ["-foobaz"]
    let%test _ = test ["zzz"]        ["x"; "y"; "z"]   ["x"; "y"; "z"; "default"]
  end)

let%test_unit "choose_one" =
  let open Param in
  let should_raise reason flags =
    match Param.choose_one flags ~if_nothing_chosen:`Raise with
    | exception _ -> ()
    | _ -> failwiths "failed to raise despite" reason [%sexp_of: string]
  in
  should_raise "duplicate names" [
    flag "-foo" (optional int) ~doc:"";
    flag "-foo" (optional int) ~doc:"";
  ];
;;

let%test_unit _ = [
  "/",    "./foo",         "/foo";
  "/tmp", "/usr/bin/grep", "/usr/bin/grep";
  "/foo", "bar",           "/foo/bar";
  "foo",  "bar",           "foo/bar";
  "foo",  "../bar",        "foo/../bar";
] |> List.iter ~f:(fun (dir, path, expected) ->
  [%test_eq: string] (abs_path ~dir path) expected)
;;

(* testing claims made in the mli about order of evaluation and [flags_of_args_exn] *)
let%test_module "Command.Spec.flags_of_args_exn" =
  (module struct

    let args q = [
      ( "flag1", Arg.Unit (fun () -> Queue.enqueue q 1), "enqueue 1");
      ( "flag2", Arg.Unit (fun () -> Queue.enqueue q 2), "enqueue 2");
      ( "flag3", Arg.Unit (fun () -> Queue.enqueue q 3), "enqueue 3");
    ]

    let parse argv =
      let q = Queue.create () in
      let command = basic_spec ~summary:"" (Spec.flags_of_args_exn (args q)) Fn.id in
      run ~argv command;
      Queue.to_list q

    let%test _ = parse ["foo.exe";"-flag1";"-flag2";"-flag3"] = [1;2;3]
    let%test _ = parse ["foo.exe";"-flag2";"-flag3";"-flag1"] = [1;2;3]
    let%test _ = parse ["foo.exe";"-flag3";"-flag2";"-flag1"] = [1;2;3]

  end)

let%expect_test "choose_one strings" =
  let open Param in
  let to_string = Spec.to_string_for_choose_one in
  print_string (to_string begin
    flag "-a" no_arg ~doc:""
  end);
  [%expect {| -a |} ];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (flag "-a" no_arg ~doc:"")
      (flag "-b" no_arg ~doc:"")
  end);
  [%expect {| -a,-b |} ];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (flag "-a" no_arg ~doc:"")
      (flag "-b" (optional int) ~doc:"")
  end);
  [%expect {| -a,-b |} ];
  printf !"%{sexp: string Or_error.t}"
    (Or_error.try_with (fun () ->
       to_string begin
         map2 ~f:Tuple2.create
           (flag "-a" no_arg ~doc:"")
           (flag "-b,c" (optional int) ~doc:"")
       end));
  [%expect {|
    (Error
     ("For simplicity, [Command.Spec.choose_one] does not support names with commas."
      (-b,c) *)) (glob) |}];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (anon ("FOO" %: string))
      (flag "-a" no_arg ~doc:"")
  end);
  [%expect {| -a,FOO |} ];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (anon ("FOO" %: string))
      (map2 ~f:Tuple2.create
         (flag "-a" no_arg ~doc:"")
         (flag "-b" no_arg ~doc:""))
  end);
  [%expect {| -a,-b,FOO |} ];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (anon (maybe ("FOO" %: string)))
      (flag "-a" no_arg ~doc:"")
  end);
  [%expect {| -a,FOO |} ];
  print_string (to_string begin
    map2 ~f:Tuple2.create
      (anon ("fo{}O" %: string))
      (flag "-a" no_arg ~doc:"")
  end);
  [%expect {| -a,fo{}O |} ];
;;

let%test_unit "multiple runs" =
  let r = ref (None, "not set") in
  let command =
    let open Let_syntax in
    basic ~summary:"test"
      [%map_open
        let a = flag "int" (optional int) ~doc:"INT some number"
        and b = anon ("string" %: string)
        in
        fun () -> r := (a, b)
      ]
  in
  let test args expect =
    run command ~argv:(Sys.argv.(0) :: args);
    [%test_result: int option * string] !r ~expect
  in
  test ["foo"; "-int"; "23"] (Some 23, "foo");
  test ["-int"; "17"; "bar"] (Some 17, "bar");
  test ["baz"]               (None,    "baz");
;;

