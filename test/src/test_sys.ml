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

let%test _ = execution_mode () =
             (if String.is_suffix argv.(0) ~suffix:".exe" ||
                 String.is_suffix argv.(0) ~suffix:".native"
              then `Native else `Bytecode)

let%test _ = let size = c_int_size () in size >= 16 && size <= Sys.word_size
