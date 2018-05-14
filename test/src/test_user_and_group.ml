open! Core
open! Import
open! User_and_group

let%test _ = equal (create ~user:"foo" ~group:"bar") (of_string "foo:bar")
