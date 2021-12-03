open! Core
open! Expect_test_helpers_core
open! Ephemeron

type heap_block = int ref Heap_block.t [@@deriving sexp_of]

let heap_block i = ref i |> Heap_block.create_exn

let print t =
  print_s [%message
    ""
      ~key:(get_key t : heap_block option)
      ~data:(get_data t : heap_block option)]
;;

let%expect_test "data isn't nulled by [set_key None]" [@tags "no-js"] =
  let t = create () in
  let data = heap_block 14 in
  set_data t (Some data);
  set_key t (Some (heap_block 13));
  set_key t None;
  print t;
  [%expect {|
    ((key ()) (data (14))) |}];
  print_s [%message (data : heap_block)];
  [%expect {|
    (data 14) |}];
;;

let%expect_test "\
data is nulled when the key becomes unreachable, even if the data is reachable"
                  [@tags "no-js"] =
  let t = create () in
  let data = heap_block 14 in
  set_data t (Some data);
  Gc.compact ();
  print t;
  [%expect {|
    ((key ()) (data (14))) |}];
  set_key t (Some (heap_block 13));
  Gc.compact ();
  print t;
  [%expect {|
    ((key  ())
     (data ())) |}];
  print_s [%message (data : heap_block)];
  [%expect {|
    (data 14) |}];
;;

let%expect_test "data is kept alive by the key" [@tags "no-js"] =
  let t = create () in
  print t;
  [%expect {|
    ((key  ())
     (data ())) |}];
  let key = heap_block 13 in
  set_key t (Some key);
  print t;
  [%expect {|
    ((key (13)) (data ())) |}];
  let data = heap_block 14 in
  set_data t (Some data);
  print t;
  [%expect {|
    ((key  (13))
     (data (14))) |}];
  Gc.compact ();
  print t;
  [%expect {|
    ((key  (13))
     (data (14))) |}];
  print_s [%message (data : heap_block)];
  [%expect {|
    (data 14) |}];
  (* No more references to [data], but it still is kept alive by [t] while [key] is
     alive. *)
  Gc.compact ();
  print t;
  [%expect {|
    ((key  (13))
     (data (14))) |}];
  print_s [%message (key : heap_block)];
  (* No more references to [key], so both fields in [t] are nulled. *)
  [%expect {|
    (key 13) |}];
  Gc.compact ();
  print t;
  [%expect {|
    ((key  ())
     (data ())) |}];
;;

let%expect_test "finalizers and clearing" [@tags "no-js"] =
  let t = create () in
  let key = heap_block 14 in
  let data = heap_block 13 in
  set_key t (Some key);
  set_data t (Some data);
  Gc.Expert.add_finalizer key  (fun _ -> print_s [%message "finalized key" ]);
  Gc.Expert.add_finalizer data (fun _ -> print_s [%message "finalized data"]);
  Gc.compact ();
  [%expect {|
    "finalized data"
    "finalized key" |}];
  print t;
  [%expect {|
    ((key  (14))
     (data (13))) |}];
  Gc.compact ();
  print t;
  [%expect {|
    ((key  ())
     (data ())) |}];
;;
