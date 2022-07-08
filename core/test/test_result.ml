open! Core
open! Import
open! Result

let%test_module "Result.V1" = (module Stable_unit_test.Make (Stable.V1_stable_unit_test))
