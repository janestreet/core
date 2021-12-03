open! Core
open! Import
include Base_test_helpers.Test_container.Test_S1 (Bag)

let%expect_test "[iter] does not allocate" =
  let t = Bag.create () in
  Bag.add_unit t ();
  require_no_allocation [%here] (fun () -> Bag.iter t ~f:ignore)
;;
