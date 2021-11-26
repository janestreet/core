open! Core
open! Import
open! Int63

let%expect_test _ =
  print_string [%bin_digest: t];
  [%expect {| 2b528f4b22f08e28876ffe0239315ac2 |}]
;;
