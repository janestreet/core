open! Core
open! Import
open! Bounded_index

let%test_module "V1" =
  (module Stable_unit_test.Make (struct
       module M = Make (struct
           let label = "index"
           let module_name = "Test_bounded_index"
         end)

       type t = M.Stable.V1.t [@@deriving sexp, bin_io, compare]

       let equal x y = compare x y = 0
       let make index min max = M.create index ~min ~max

       let tests =
         [ make 0 0 0, "(index 0 of 0 to 0)", "\000\000\000"
         ; make 0 0 1, "(index 0 of 0 to 1)", "\000\000\001"
         ; make 1 0 1, "(index 1 of 0 to 1)", "\001\000\001"
         ; make 0 0 2, "(index 0 of 0 to 2)", "\000\000\002"
         ; make 1 0 2, "(index 1 of 0 to 2)", "\001\000\002"
         ; make 2 0 2, "(index 2 of 0 to 2)", "\002\000\002"
         ; make 1 1 1, "(index 1 of 1 to 1)", "\001\001\001"
         ; make 1 1 2, "(index 1 of 1 to 2)", "\001\001\002"
         ; make 2 1 2, "(index 2 of 1 to 2)", "\002\001\002"
         ; make 1 1 3, "(index 1 of 1 to 3)", "\001\001\003"
         ; make 2 1 3, "(index 2 of 1 to 3)", "\002\001\003"
         ; make 3 1 3, "(index 3 of 1 to 3)", "\003\001\003"
         ; ( make 499_999_999 0 999_999_999
           , "(index 499_999_999 of 0 to 999_999_999)"
           , "\253\255\100\205\029\000\253\255\201\154\059" )
         ; ( make 500_000_000 1 1_000_000_000
           , "(index 500_000_000 of 1 to 1_000_000_000)"
           , "\253\000\101\205\029\001\253\000\202\154\059" )
         ]
       ;;
     end))
;;
