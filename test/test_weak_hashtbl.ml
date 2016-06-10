open! Core.Std
open! Async.Std
open! Expect_test_helpers.Std
open! Core.Std.Weak_hashtbl

let create () = create Int.hashable

let data int = ref int |> Heap_block.create_exn

type data = int ref Heap_block.t [@@deriving sexp_of]

let%expect_test "[add_exn], [find], [mem], [replace], [remove]" =
  let t = create () in
  let print_mem i = print_s [%message (i : int ) ~mem:(mem t i : bool)] in
  let key = 13 in
  print_mem key;
  let%bind () = [%expect {|
    ((i   13)
     (mem false)) |}] in
  add_exn t ~key ~data:(data key);
  print_mem key;
  let%bind () = [%expect {|
    ((i   13)
     (mem true)) |}] in
  let print_find () = print_s [%message (find t key : data option)] in
  print_find ();
  let%bind () = [%expect {|
    ("find t key" (13)) |}] in
  replace t ~key ~data:(data 14);
  print_find ();
  let%bind () = [%expect {|
    ("find t key" (14)) |}] in
  remove t key;
  print_find ();
  let%bind () = [%expect {|
    ("find t key" ()) |}] in
  return ();
;;

let%expect_test "[key_is_using_space], [reclaim_space_for_keys_with_unused_data]" =
  let t = create () in
  let key = 13 in
  let print () =
    print_s [%message
      ""
        ~key_is_using_space:(key_is_using_space t key : bool)
        ~mem:(mem t key : bool)]
  in
  print ();
  let%bind () = [%expect {|
    ((key_is_using_space false)
     (mem                false)) |}] in
  add_exn t ~key ~data:(data ());
  print ();
  let%bind () = [%expect {|
    ((key_is_using_space true)
     (mem                true)) |}] in
  Gc.compact ();
  print ();
  let%bind () = [%expect {|
    ((key_is_using_space true)
     (mem                false)) |}] in
  reclaim_space_for_keys_with_unused_data t;
  print ();
  let%bind () = [%expect {|
    ((key_is_using_space false)
     (mem                false)) |}] in
  return ();
;;

let%expect_test "[set_run_when_unused_data]" =
  let t = create () in
  let key = 13 in
  let ran = ref false in
  let print () = print_s [%message (ran : bool ref)] in
  set_run_when_unused_data t ~thread_safe_f:(fun () -> ran := true);
  Gc.compact ();
  print ();
  let%bind () = [%expect {|
    (ran false) |}] in
  let data = data key in
  add_exn t ~key ~data;
  Gc.compact ();
  print ();
  let%bind () = [%expect {|
    (ran false) |}] in
  print_s [%message (data : data)];
  let%bind () = [%expect {|
    (data 13) |}] in
  Gc.compact ();
  print ();
  let%bind () = [%expect {|
    (ran true) |}] in
  return ();
;;
