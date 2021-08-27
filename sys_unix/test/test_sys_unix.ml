open! Core
open! Import
open! Sys

let%test _ =
  let argv = get_argv () in
  [%equal: [ `Native | `Bytecode ]]
    (Sys_unix.execution_mode ())
    (if String.is_suffix argv.(0) ~suffix:".exe"
     || String.is_suffix argv.(0) ~suffix:".native"
     then `Native
     else `Bytecode)
;;

let%test _ =
  let size = Sys_unix.c_int_size () in
  size >= 16 && size <= Sys.word_size_in_bits
;;

let%expect_test _ =
  let old_sys_argv = Sys.get_argv () in
  Sys_unix.override_argv [| "THIS"; "IS"; "A"; "TEST" |];
  print_s [%sexp (Sys.get_argv () : string array)];
  Sys_unix.override_argv old_sys_argv;
  print_s [%sexp ([%equal: string array] (Sys.get_argv ()) old_sys_argv : bool)];
  [%expect {|
    (THIS IS A TEST)
    true |}]
;;
