open Core
open Expect_test_helpers_core

let%expect_test "[unsafe_destroy] destroy after destroy" =
  let bs = Bigstring.create 100 in
  let () = Bigstring.unsafe_destroy bs in
  require [%here] (Exn.does_raise (fun () -> Bigstring.unsafe_destroy bs))
;;

let%expect_test "[unsafe_destroy_and_resize]" =
  let bigstring_6 = Bigstring.of_string "ABCDEF" in
  bigstring_6.{5} <- '.';
  printf "%s" (Bigstring.to_string bigstring_6);
  [%expect {| ABCDE. |}];
  let bigstring_1 = Bigstring.unsafe_destroy_and_resize bigstring_6 ~len:1 in
  printf "%d" (Bigstring.length bigstring_6);
  [%expect {| 0 |}];
  require_does_raise [%here] (fun () -> bigstring_6.{1} <- 'F');
  [%expect {| (Invalid_argument "index out of bounds") |}];
  printf "%d" (Bigstring.length bigstring_1);
  [%expect {| 1 |}];
  printf "%s" (Bigstring.to_string bigstring_1);
  [%expect {| A |}];
  require_does_raise [%here] (fun () -> bigstring_1.{1} <- 'F');
  [%expect {| (Invalid_argument "index out of bounds") |}];
  bigstring_1.{0} <- 'X';
  let bigstring_3 = Bigstring.unsafe_destroy_and_resize bigstring_1 ~len:3 in
  printf "%d" (Bigstring.length bigstring_1);
  [%expect {| 0 |}];
  printf "%d" (Bigstring.length bigstring_3);
  [%expect {| 3 |}];
  Bigstring.From_string.blito () ~src:"YZ" ~dst:bigstring_3 ~dst_pos:1;
  printf "%s" (Bigstring.to_string bigstring_3);
  [%expect {| XYZ |}];
  require_does_raise [%here] (fun () -> bigstring_3.{5} <- 'F');
  [%expect {| (Invalid_argument "index out of bounds") |}]
;;

let%expect_test ("[unsafe_destroy_and_resize], proxy failure" [@tags "no-js"]) =
  let bigstring = Bigstring.create 5 in
  printf "%d" (Bigstring.length bigstring);
  [%expect {| 5 |}];
  let bigstring = Bigstring.unsafe_destroy_and_resize bigstring ~len:10 in
  printf "%d" (Bigstring.length bigstring);
  [%expect {| 10 |}];
  let _shared = Bigstring.sub_shared bigstring in
  require_does_raise [%here] (fun () ->
    let (_ : Bigstring.t_frozen) = Bigstring.unsafe_destroy_and_resize bigstring ~len:5 in
    ());
  [%expect {| (Failure "bigstring_realloc: bigstring has proxy") |}]
;;

let%test "[unsafe_destroy_and_resize] destroy after destroy" =
  let bs = Bigstring.create 100 in
  let _bs' = Bigstring.unsafe_destroy_and_resize bs ~len:100 in
  Exn.does_raise (fun () -> Bigstring.unsafe_destroy bs)
;;

let%test_unit "[unsafe_destroy_and_resize] no double-free on finalise" =
  let old_gc = Gc.get () in
  let new_gc =
    { old_gc with Gc.Control.minor_heap_size = 1; Gc.Control.space_overhead = 1 }
  in
  Gc.set new_gc;
  let rec loop i =
    let bs = Bigstring.create 1000 in
    let _bs' = Bigstring.unsafe_destroy_and_resize bs ~len:1000 in
    if i > 0 then loop (i - 1) else ()
  in
  loop 1_000_000;
  Gc.set old_gc
;;
