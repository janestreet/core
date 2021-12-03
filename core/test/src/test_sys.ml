open! Core
open! Import
open! Sys.Private

let%test_unit _ =
  [%test_eq: string] (unix_quote "") {|''|};
  [%test_eq: string] (unix_quote "a ") {|'a '|};
  [%test_eq: string] (unix_quote "a'") {|'a'\'''|};
  [%test_eq: string] (unix_quote "a/b/+share+") {|a/b/+share+|};
  [%test_eq: string] (unix_quote "x\000y") "'x\000y'"
;;
