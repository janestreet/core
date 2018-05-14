open! Core
open! Import
open! Filename

let%expect_test "temporary file names contain [.tmp.]" =
  let file = temp_file "foo" "" in
  Sys.remove file;
  require [%here] (".tmp." = String.sub file ~pos:(String.length file - 11) ~len:5)
    ~if_false_then_print_s:(lazy [%message (file : string)]);
  [%expect {| |}];
;;

let%test _ = split_extension "/foo/my_file"       = ("/foo/my_file", None)
let%test _ = split_extension "/foo/my_file.txt"   = ("/foo/my_file", Some "txt")
let%test _ = split_extension "/foo/my_file.1.txt" = ("/foo/my_file.1", Some "txt")
let%test _ = split_extension "/home/c.falls/my_file"       = ("/home/c.falls/my_file", None)
let%test _ = split_extension "/home/c.falls/my_file.txt"   = ("/home/c.falls/my_file", Some "txt")
let%test _ = split_extension "/home/c.falls/my_file.1.txt" = ("/home/c.falls/my_file.1", Some "txt")
let%test _ = split_extension "my_file"       = ("my_file", None)
let%test _ = split_extension "my_file.txt"   = ("my_file", Some "txt")
let%test _ = split_extension "my_file.1.txt" = ("my_file.1", Some "txt")
let%test _ = split_extension "/my_file"       = ("/my_file", None)
let%test _ = split_extension "/my_file.txt"   = ("/my_file", Some "txt")
let%test _ = split_extension "/my_file.1.txt" = ("/my_file.1", Some "txt")

let%test_unit _ =
  List.iter
    ~f:(fun (path, pieces) ->
      [%test_result: string] ~expect:path (of_parts pieces);
      [%test_result: string list] ~expect:pieces (parts path))
    [ "/tmp/foo/bar/baz",  ["/"; "tmp"; "foo"; "bar"; "baz"]
    ; ".",                 ["."]
    ; "/",                 ["/"]
    ; "./foo",             ["."; "foo"]
    ]

let%test _ = parts "/tmp/foo/bar/baz/" = ["/"; "tmp"; "foo"; "bar"; "baz"]
let%test _ = parts "//tmp//foo//bar"   = ["/"; "tmp"; "foo"; "bar"]
let%test _ = parts ""                  = ["."]
let%test _ = parts "./"                = ["."]
let%test _ = parts "./."               = ["."]
let%test _ = parts "././."             = ["."; "."]
let%test _ = parts "foo"               = ["."; "foo"]
let%test _ = parts "./foo/"            = ["."; "foo"]
let%test _ = parts "./foo/."           = ["."; "foo"; "."]

let%test _ = of_parts ["."; "."; "."] = "././."
