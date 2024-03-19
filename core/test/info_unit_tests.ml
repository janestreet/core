open! Core

let%test_unit "[create_s sexp] produces an info whose [sexp_of_t] is [sexp]" =
  let sexp = [%sexp "foo"] in
  assert (phys_equal [%sexp (Info.create_s sexp : Info.t)] sexp)
;;

let%expect_test "Info.Stable.V2" =
  Expect_test_helpers_core.print_and_check_stable_type
    [%here]
    (module Info.Stable.V2)
    (let here =
       { Source_code_position.pos_fname = "test.ml"
       ; pos_lnum = 1
       ; pos_bol = 2
       ; pos_cnum = 3
       }
     in
     let info0 = Info.of_string "test" in
     let info1 = Info.create ~here "test" 0.5 Float.sexp_of_t in
     [ info0
     ; info1
     ; Info.tag info1 ~tag:"tag"
     ; Info.tag_arg info1 "tag" 1.8 Float.sexp_of_t
     ; Info.of_list [ info0; info1 ]
     ; Info.of_thunk (fun () -> failwith "Error")
     ; Info.of_exn (Failure "Error")
     ; Info.of_exn (Failure "Error") ~backtrace:(`This "backtrace")
     ]);
  [%expect
    {|
    (bin_shape_digest 52966f4a49a77bfdff668e9cc61511b3)
    ((sexp   test)
     (bin_io "\001\004test"))
    ((sexp (test 0.5 test.ml:1:1))
     (bin_io "\004\004test\000\0030.5\001\007test.ml\001\002\003"))
    ((sexp (tag (test 0.5 test.ml:1:1)))
     (bin_io "\005\003tag\004\004test\000\0030.5\001\007test.ml\001\002\003"))
    ((sexp (tag 1.8 (test 0.5 test.ml:1:1)))
     (bin_io
      "\006\003tag\000\0031.8\004\004test\000\0030.5\001\007test.ml\001\002\003"))
    ((sexp (test (test 0.5 test.ml:1:1)))
     (bin_io
      "\007\000\002\001\004test\004\004test\000\0030.5\001\007test.ml\001\002\003"))
    ((sexp (Could_not_construct (Failure Error)))
     (bin_io "\000\001\002\000\007Failure\000\005Error"))
    ((sexp (Failure Error)) (bin_io "\002\001\002\000\007Failure\000\005Error"))
    ((sexp ((Failure Error) (backtrace)))
     (bin_io "\b\003\001\002\000\007Failure\000\005Error\tbacktrace"))
    |}]
;;
