open! Core
open! Import

let invariant bag =
  Bag.invariant ignore bag;
  require [%here] (Bag.length bag >= 0);
  require_equal [%here] (module Bool) (Bag.length bag = 0) (Bag.is_empty bag)
;;

let test bag =
  invariant bag;
  print_s [%sexp (bag : int Bag.t)]
;;

let%expect_test "create" =
  let b = Bag.create () in
  test b;
  [%expect {| () |}]
;;

let%expect_test "add1" =
  let b = Bag.create () in
  ignore (Bag.add b 1 : int Bag.Elt.t);
  test b;
  [%expect {| (1) |}]
;;

let%expect_test "add2" =
  let b = Bag.create () in
  ignore (Bag.add b 1 : int Bag.Elt.t);
  ignore (Bag.add b 2 : int Bag.Elt.t);
  test b;
  [%expect {| (2 1) |}]
;;

let%expect_test "remove" =
  let b = Bag.create () in
  Bag.remove b (Bag.add b 1);
  invariant b;
  test b;
  [%expect {| () |}]
;;

let%expect_test "remove2" =
  let b = Bag.create () in
  let e1 = Bag.add b 1 in
  ignore (Bag.add b 2 : int Bag.Elt.t);
  Bag.remove b e1;
  test b;
  [%expect {| (2) |}]
;;

let%expect_test "add100" =
  let b = Bag.create () in
  let n = 20 in
  for i = 1 to n do
    ignore (Bag.add b i : int Bag.Elt.t);
    invariant b
  done;
  require_equal [%here] (module Int) (Bag.length b) n;
  for _ = 1 to n do
    (match Bag.remove_one b with
     | None -> assert false
     | Some _ -> ());
    invariant b
  done;
  test b;
  [%expect {| () |}]
;;

include Base_test_helpers.Test_container.Test_S1 (Bag)

let%expect_test "[iter] does not allocate" =
  let t = Bag.create () in
  Bag.add_unit t ();
  require_no_allocation [%here] (fun () -> Bag.iter t ~f:ignore)
;;
