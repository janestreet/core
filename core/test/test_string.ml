open! Core
open! Import
open! String

let%test_module "slice" =
  (module struct
    (*TEST = slice "hey" 0 0 = ""*)
    (* This is what I would expect *)
    let%test _ = slice "hey" 0 0 = "hey"

    (* But this is what we get! *)
    let%test _ = slice "hey" 0 1 = "h"
    let%test _ = slice "hey" 0 2 = "he"
    let%test _ = slice "hey" 0 3 = "hey"
    let%test _ = slice "hey" 1 1 = ""
    let%test _ = slice "hey" 1 2 = "e"
    let%test _ = slice "hey" 1 3 = "ey"
    let%test _ = slice "hey" 2 2 = ""
    let%test _ = slice "hey" 2 3 = "y"
    let%test _ = slice "hey" 3 3 = ""
    let%test _ = slice "hey" 0 (-1) = "he"
    let%test _ = slice "hey" (-1) 0 = "y"
    let%test _ = slice "hey" (-3) (-2) = "h"
  end)
;;

let%test_module "nget" =
  (module struct
    let%expect_test _ = require_equal [%here] (module Char) (nget "hey" (-3)) 'h'
    let%expect_test _ = require_equal [%here] (module Char) (nget "hey" 2) 'y'

    let%expect_test _ =
      require_does_raise [%here] (fun () -> nget "hey" (-100));
      [%expect {| (Invalid_argument "index out of bounds") |}]
    ;;
  end)
;;

let%test_module "Caseless Comparable" =
  (module struct
    let%test _ =
      Int.equal (Base.Map.find_exn (Caseless.Map.of_alist_exn [ "a", 4; "b", 5 ]) "A") 4
    ;;

    let%test _ = Base.Set.mem (Caseless.Set.of_list [ "hello"; "world" ]) "heLLO"
    let%test _ = Int.equal (Base.Set.length (Caseless.Set.of_list [ "a"; "A" ])) 1
  end)
;;

let%test_module "Caseless Hash" =
  (module struct
    let%test _ = Int.equal (Caseless.hash "Hello") (Caseless.hash "HELLO")
  end)
;;

let%test_module "take_while" =
  (module struct
    let f = Char.is_digit
    let%test_unit _ = [%test_result: t] (take_while "123abc456" ~f) ~expect:"123"
    let%test_unit _ = [%test_result: t] (rtake_while "123abc456" ~f) ~expect:"456"
    let%test_unit _ = [%test_result: t] (take_while "123456" ~f) ~expect:"123456"
    let%test_unit _ = [%test_result: t] (rtake_while "123456" ~f) ~expect:"123456"
  end)
;;

let%test_module "Verify reading/writing stable table sexp" =
  (module struct
    let expected_sexp = Sexp.of_string "((alpha beta) (delta gamma))"

    let table =
      let s = String.Table.create () in
      Hashtbl.add_exn s ~key:"delta" ~data:"gamma";
      Hashtbl.add_exn s ~key:"alpha" ~data:"beta";
      s
    ;;

    let%expect_test "sexp_of_t" =
      print_s [%sexp (table : string String.Stable.V1.Table.t)];
      [%expect {|
        ((alpha beta)
         (delta gamma))
        |}]
    ;;

    let%test_unit "t_of_sexp" =
      let loaded_table = [%of_sexp: string String.Stable.V1.Table.t] expected_sexp in
      assert (Hashtbl.equal String.equal loaded_table table)
    ;;
  end)
;;

let%expect_test "Hashtbl.merge of String.Table and String.Stable.V1.Table" =
  Expect_test_helpers_core.require_does_not_raise [%here] (fun () ->
    let result =
      Hashtbl.merge
        (String.Table.t_of_sexp int_of_sexp [%sexp []])
        (String.Stable.V1.Table.t_of_sexp int_of_sexp [%sexp []])
        ~f:(fun ~key:_ x -> Some x)
    in
    require [%here] (Hashtbl.is_empty result));
  [%expect {| |}]
;;
