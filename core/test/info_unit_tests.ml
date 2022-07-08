open! Core

let%test_unit "[create_s sexp] produces an info whose [sexp_of_t] is [sexp]" =
  let sexp = [%sexp "foo"] in
  assert (phys_equal [%sexp (Info.create_s sexp : Info.t)] sexp)
;;

let%test_module "Info.Stable.V2" =
  (module Stable_unit_test.Make (struct
       include Info.Stable.V2

       let equal t t' = compare t t' = 0

       let tests =
         let here =
           { Source_code_position.pos_fname = "test.ml"
           ; pos_lnum = 1
           ; pos_bol = 2
           ; pos_cnum = 3
           }
         in
         let info0 = Info.of_string "test" in
         let info1 = Info.create ~here "test" 0.5 Float.sexp_of_t in
         [ info0, "test", "\001\004test"
         ; ( info1
           , "(test 0.5 test.ml:1:1)"
           , "\004\004test\000\0030.5\001\007test.ml\001\002\003" )
         ; ( Info.tag info1 ~tag:"tag"
           , "(tag(test 0.5 test.ml:1:1))"
           , "\005\003tag\004\004test\000\0030.5\001\007test.ml\001\002\003" )
         ; ( Info.tag_arg info1 "tag" 1.8 Float.sexp_of_t
           , "(tag 1.8(test 0.5 test.ml:1:1))"
           , "\006\003tag\000\0031.8\004\004test\000\0030.5\001\007test.ml\001\002\003" )
         ; ( Info.of_list [ info0; info1 ]
           , "(test(test 0.5 test.ml:1:1))"
           , "\007\000\002\001\004test\004\004test\000\0030.5\001\007test.ml\001\002\003" )
         ; ( Info.of_thunk (fun () -> failwith "Error")
           , "(Could_not_construct(Failure Error))"
           , "\000\001\002\000\007Failure\000\005Error" )
         ; ( Info.of_exn (Failure "Error")
           , "(Failure Error)"
           , "\002\001\002\000\007Failure\000\005Error" )
         ; ( Info.of_exn (Failure "Error") ~backtrace:(`This "backtrace")
           , "((Failure Error)backtrace)"
           , "\b\003\001\002\000\007Failure\000\005Error\tbacktrace" )
         ]
       ;;
     end))
;;
