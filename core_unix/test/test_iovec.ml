open! Core
open! Async
open! Import
open Unix.IOVec

let print_of_string ?pos ?len str =
  let iovec = of_string ?pos ?len str in
  print_s [%sexp (iovec : string t)]
;;

let%expect_test "[IOVec.of_string] on empty string" =
  print_of_string "";
  [%expect {|
    ((buf "")
     (pos 0)
     (len 0)) |}];
  print_of_string "foo" ~len:0;
  [%expect {|
    ((buf foo)
     (pos 0)
     (len 0)) |}];
  print_of_string "foo" ~pos:3;
  [%expect {|
    ((buf foo)
     (pos 3)
     (len 0)) |}];
  return ()
;;

let%expect_test "[IOVec.of_string] on full string" =
  print_of_string "foo";
  [%expect {|
    ((buf foo)
     (pos 0)
     (len 3)) |}];
  print_of_string "foo" ~len:3;
  [%expect {|
    ((buf foo)
     (pos 0)
     (len 3)) |}];
  return ()
;;

let%expect_test "[IOVec.of_string] on trailing end of string" =
  print_of_string "foo" ~pos:1;
  [%expect {|
    ((buf foo)
     (pos 1)
     (len 2)) |}];
  print_of_string "foo" ~pos:1 ~len:2;
  [%expect {|
    ((buf foo)
     (pos 1)
     (len 2)) |}];
  return ()
;;

let%expect_test "[IOVec.of_string] on string too short" =
  require_does_raise [%here] (fun () -> print_of_string "foo" ~pos:4);
  [%expect {| (Invalid_argument "IOVec.of_string: pos > length buf") |}];
  require_does_raise [%here] (fun () -> print_of_string "foo" ~len:4);
  [%expect {| (Invalid_argument "IOVec.of_string: pos + len > length buf") |}];
  require_does_raise [%here] (fun () -> print_of_string "foo" ~pos:1 ~len:3);
  [%expect {| (Invalid_argument "IOVec.of_string: pos + len > length buf") |}];
  return ()
;;

let print_of_bigstring ?pos ?len str =
  let iovec = of_bigstring ?pos ?len (Bigstring.of_string str) in
  print_s [%sexp (iovec : bigstring t)]
;;

let%expect_test "[IOVec.of_bigstring] on empty bigstring" =
  print_of_bigstring "";
  [%expect {|
    ((buf "")
     (pos 0)
     (len 0)) |}];
  print_of_bigstring "foo" ~len:0;
  [%expect {|
    ((buf foo)
     (pos 0)
     (len 0)) |}];
  print_of_bigstring "foo" ~pos:3;
  [%expect {|
    ((buf foo)
     (pos 3)
     (len 0)) |}];
  return ()
;;

let%expect_test "[IOVec.of_bigstring] on full bigstring" =
  print_of_bigstring "foo";
  [%expect {|
    ((buf foo)
     (pos 0)
     (len 3)) |}];
  print_of_bigstring "foo" ~len:3;
  [%expect {|
    ((buf foo)
     (pos 0)
     (len 3)) |}];
  return ()
;;

let%expect_test "[IOVec.of_bigstring] on trailing end of bigstring" =
  print_of_bigstring "foo" ~pos:1;
  [%expect {|
    ((buf foo)
     (pos 1)
     (len 2)) |}];
  print_of_bigstring "foo" ~pos:1 ~len:2;
  [%expect {|
    ((buf foo)
     (pos 1)
     (len 2)) |}];
  return ()
;;

let%expect_test "[IOVec.of_bigstring] on bigstring too short" =
  require_does_raise [%here] (fun () -> print_of_bigstring "foo" ~pos:4);
  [%expect {| (Invalid_argument "IOVec.of_bigstring: pos > length buf") |}];
  require_does_raise [%here] (fun () -> print_of_bigstring "foo" ~len:4);
  [%expect {| (Invalid_argument "IOVec.of_bigstring: pos + len > length buf") |}];
  require_does_raise [%here] (fun () -> print_of_bigstring "foo" ~pos:1 ~len:3);
  [%expect {| (Invalid_argument "IOVec.of_bigstring: pos + len > length buf") |}];
  return ()
;;
