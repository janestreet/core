open! Core
open! Import

let%expect_test "temporary file names contain [.tmp.]" =
  let file = Filename_unix.temp_file "foo" "" in
  Sys_unix.remove file;
  require
    [%here]
    (String.equal ".tmp." (String.sub file ~pos:(String.length file - 11) ~len:5))
    ~if_false_then_print_s:(lazy [%message (file : string)]);
  [%expect {| |}]
;;
