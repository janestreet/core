open! Core
open! Unix
open! Expect_test_helpers_kernel

let%expect_test "[mkdtemp] dir name contains [.tmp.]" =
  let dir = mkdtemp "foo" in
  rmdir dir;
  require [%here] (".tmp." = String.sub dir ~pos:(String.length dir - 11) ~len:5)
    ~if_false_then_print_s:(lazy [%message (dir : string)]);
  [%expect {| |}];
;;

let%expect_test "[mkstemp] file name contains [.tmp.]" =
  let file, fd = mkstemp "foo" in
  unlink file;
  close fd;
  require [%here] (".tmp." = String.sub file ~pos:(String.length file - 11) ~len:5)
    ~if_false_then_print_s:(lazy [%message (file : string)]);
  [%expect {| |}];
;;
