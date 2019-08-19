open! Core
open! Import
open! Sys
open! Sys.Private

let%test_unit _ =
  [%test_eq: string] (unix_quote "") {|''|};
  [%test_eq: string] (unix_quote "a ") {|'a '|};
  [%test_eq: string] (unix_quote "a'") {|'a'\'''|};
  [%test_eq: string] (unix_quote "a/b/+share+") {|a/b/+share+|};
  [%test_eq: string] (unix_quote "x\000y") "'x\000y'"
;;

let%test _ =
  let argv = get_argv () in
  [%equal: [ `Native | `Bytecode ]]
    (execution_mode ())
    (if String.is_suffix argv.(0) ~suffix:".exe"
     || String.is_suffix argv.(0) ~suffix:".native"
     then `Native
     else `Bytecode)
;;

let%test _ =
  let size = c_int_size () in
  size >= 16 && size <= Sys.word_size
;;

let%expect_test _ =
  let old_sys_argv = Sys.get_argv () in
  Sys.override_argv [| "THIS"; "IS"; "A"; "TEST" |];
  print_s [%sexp (Sys.get_argv () : string array)];
  Sys.override_argv old_sys_argv;
  print_s [%sexp ([%equal: string array] (Sys.get_argv ()) old_sys_argv : bool)];
  [%expect {|
    (THIS IS A TEST)
    true |}]
;;
