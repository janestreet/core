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
