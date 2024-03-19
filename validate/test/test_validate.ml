open! Base
open! Validate
open Expect_test_helpers_core

let print t = List.iter (errors t) ~f:Stdlib.print_endline

let%expect_test "empty" =
  let res =
    name_list
      ""
      [ name "ok1" pass
      ; name "ok2" pass
      ; name_list "sub" [ name "ok3" pass; name "ok4" pass ]
      ]
  in
  print res;
  [%expect {| |}]
;;

let%expect_test "nesting" =
  let res =
    name_list
      "nesting"
      [ name "ok" pass
      ; name "top" (fail "fail")
      ; name_list
          "sub0"
          [ name "sub1" (fail "fail")
          ; name "sub2" (fail "fail")
          ; name_list "sub3" [ name "sub4" (fail "fail"); name "sub5" (fail "fail") ]
          ]
      ]
  in
  print res;
  [%expect
    {|
    (nesting.top fail)
    (nesting.sub0.sub1 fail)
    (nesting.sub0.sub2 fail)
    (nesting.sub0.sub3.sub4 fail)
    (nesting.sub0.sub3.sub5 fail)
    |}]
;;

let%expect_test "Validate.all" =
  print
    (all
       [ (fun _ -> fail "a")
       ; (fun _ -> pass)
       ; (fun _ -> fail "b")
       ; (fun _ -> pass)
       ; (fun _ -> fail "c")
       ]
       ());
  [%expect {|
    ("" a)
    ("" b)
    ("" c)
    |}]
;;

let%expect_test _ =
  print (first_failure pass (fail "foo"));
  [%expect {| ("" foo) |}]
;;

let%expect_test _ =
  print (first_failure (fail "foo") (fail "bar"));
  [%expect {| ("" foo) |}]
;;

let two_errors = of_list [ fail "foo"; fail "bar" ]

let%expect_test _ =
  print (first_failure two_errors (fail "snoo"));
  [%expect {|
    ("" foo)
    ("" bar)
    |}]
;;

let%expect_test _ =
  print (first_failure (fail "snoo") two_errors);
  [%expect {| ("" snoo) |}]
;;

let%expect_test _ =
  let v () =
    if true then failwith "This unit validation raises";
    Validate.pass
  in
  print (protect v ());
  [%expect
    {|
    (""
     ("Exception raised during validation"
      (Failure "This unit validation raises")))
    |}]
;;

let%expect_test "try_with" =
  let v () = failwith "this function raises" in
  print (try_with v);
  [%expect
    {| ("" ("Exception raised during validation" (Failure "this function raises"))) |}]
;;

type t = { x : bool } [@@deriving fields ~direct_iterators:fold]

let%expect_test "typical use of Validate.field_direct_folder doesn't allocate on success" =
  let validate_x = Staged.unstage (Validate.field_direct_folder Validate.pass_bool) in
  let validate t =
    Fields.Direct.fold t ~init:[] ~x:validate_x |> Validate.of_list |> Validate.result
  in
  let t = { x = true } in
  require_no_allocation [%here] (fun () -> ignore (validate t : unit Or_error.t))
;;

let%expect_test "Validate.all doesn't allocate on success" =
  let checks = List.init 5 ~f:(Fn.const Validate.pass_bool) in
  require_no_allocation [%here] (fun () -> ignore (Validate.all checks true : Validate.t))
;;

let%expect_test "Validate.combine doesn't allocate on success" =
  require_no_allocation [%here] (fun () ->
    ignore (Validate.combine Validate.pass Validate.pass : Validate.t))
;;
