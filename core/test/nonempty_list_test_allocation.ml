open! Core
open! Expect_test_helpers_core

let%expect_test ("[to_list] does not allocate more than one new constructor"
  [@tags "no-js"])
  =
  (* [to_list] should at most allocate a new list node, which takes 3 minor words *)
  let list : int list = Sys.opaque_identity [ 2; 3 ] in
  let _ : int list =
    require_allocation_does_not_exceed (Minor_words 3) (fun () -> 1 :: list)
  in
  [%expect {| |}];
  (* In fact, [to_list] is able to be optimized to the identity function (and so doesn't
     allocate) *)
  let list = Sys.opaque_identity Nonempty_list.[ 1; 2; 3 ] in
  let _ : int list =
    require_no_allocation (fun () -> Nonempty_list.to_list list |> Sys.opaque_identity)
  in
  [%expect {| |}];
  (* Double check we also correctly handle the non-value versions. *)
  let%template list =
    Sys.opaque_identity
      ([ Int64_u.of_int 2; Int64_u.of_int 3 ] : (int64# Nonempty_list.t[@kind bits64]))
  in
  let _ : (int64# List.t[@kind bits64]) =
    require_no_allocation (fun () ->
      (Nonempty_list.to_list [@kind bits64]) list |> Sys.opaque_identity)
  in
  [%expect {| |}]
;;
