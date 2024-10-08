open! Core
open! Import
open! Result
module%test [@name "Result.V1"] _ = Stable_unit_test.Make (Stable.V1_stable_unit_test)
