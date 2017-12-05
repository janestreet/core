open! Core
open! Async
open! Expect_test_helpers
open! Core.Weak_hashtbl

let create () = create (module Int)

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

let%expect_test _ =
  let module M = struct
    type t =
      (* [mutable foo] to force the compiler to allocate the record on the heap. *)
      { mutable foo : int
      ; bar         : int
      ; baz         : string
      }
  end
  in
  let open M in
  let block foo = Heap_block.create_exn ({ foo; bar = 0; baz = "hello" }, 0) in
  let tbl = create () in
  let stabilize () =
    Gc.full_major ();
    reclaim_space_for_keys_with_unused_data tbl;
  in
  let add k b = ignore (find_or_add tbl k ~default:(fun () -> !b)) in
  (* We put the blocks in refs and manually blackhole them, so that the unit test will
     pass with the bytecode compiler. *)
  let b1 = ref (block 1) in
  let b2 = ref (block 2) in
  let b3 = ref (block 3) in
  let b4 = ref (block 4) in
  let blackhole b = b := block 0 in
  let k1 = 1 in
  let k2 = 2 in
  let k3 = 3 in
  add k1 b1;
  add k2 b2;
  add k3 b3;
  (* Checking [is_absent k] is stronger than checking that [is_none (find tbl k)].  We
     want to make sure that a key has been removed from the table, and in particular rule
     out the case where the key is in the table but the corresponding weak is none. *)
  let is_absent k = not (key_is_using_space tbl k) in
  let is_block k b =
    match find tbl k with
    | None -> false
    | Some v -> phys_equal v b
  in
  require [%here] (is_block k1 !b1);
  require [%here] (is_block k2 !b2);
  require [%here] (is_block k3 !b3);
  blackhole b1;
  stabilize ();
  require [%here] (is_absent k1);
  require [%here] (is_block k2 !b2);
  require [%here] (is_block k3 !b3);
  blackhole b2;
  stabilize ();
  require [%here] (is_absent k1);
  require [%here] (is_absent k2);
  require [%here] (is_block k3 !b3);
  replace tbl ~key:k3 ~data:!b4;
  blackhole b3;
  stabilize ();
  require [%here] (is_block k3 !b4);
  blackhole b4;
  stabilize ();
  require [%here] (is_absent k3);
  return ();
;;
