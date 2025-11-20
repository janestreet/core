open! Core
open! Gc

module%test [@name "gc"] [@tags "no-js"] _ = struct
  (* The idea underlying this test is that minor_words does not allocate any memory. Hence
       the subsequent call to quick_stat should report exactly the same number. Also:

       1) This test may fail if the float is so large that it cannot fit in a 64bit int.

       2) We run this in a loop because the each call to [quick_stat] allocates minor_data
       and this number should be picked up by [minor_words] *)
  (*
       let%test_unit _ =
       for _ = 1 to 0 do
       let mw1 = minor_words () in
       let st = quick_stat () in
       let mw2 = Float.iround_towards_zero_exn st.Stat.minor_words in
       assert (mw1 = mw2)
       done
       ;;
    *)

  (* The point of doing a [minor] in the tests below is that [st] is still live and will
       be promoted during the minor GC, thereby changing both the promoted words and the
       major words in each iteration of the loop *)
  (*
       let%test_unit _ =
       for _ = 1 to 1000 do
       let mw1 = major_words () in
       let st = quick_stat () in
       minor ();
       let mw2 = Float.iround_towards_zero_exn st.Stat.major_words in
       assert (mw1 = mw2)
       done
       ;;
    *)

  let%test_unit _ =
    for _ = 1 to 1000 do
      let mw1 = promoted_words () in
      let st = quick_stat () in
      minor ();
      let mw2 = Float.iround_towards_zero_exn st.Stat.promoted_words in
      assert (mw1 = mw2)
    done
  ;;

  let%test_unit _ = assert (major_words () + minor_words () = major_plus_minor_words ())

  let stat_eq func projection =
    (* In the stub the record is allocated after getting the stats, so we must ensure
       [func] is called first. *)
    let x = func () in
    let y = projection (quick_stat ()) in
    x = y
  ;;

  let%test_unit _ =
    for _ = 1 to 1000 do
      assert (stat_eq minor_collections Stat.minor_collections);
      minor ();
      assert (stat_eq minor_collections Stat.minor_collections)
    done
  ;;

  let%test_unit _ =
    for _ = 1 to 250 do
      assert (stat_eq major_collections Stat.major_collections);
      major ();
      assert (stat_eq major_collections Stat.major_collections)
    done
  ;;

  let%test_unit _ =
    for _ = 1 to 250 do
      assert (stat_eq compactions Stat.compactions);
      compact ();
      assert (stat_eq compactions Stat.compactions)
    done
  ;;

  (*
     let%test_unit _ =
       let check () =
       assert (stat_eq heap_chunks Stat.heap_chunks);
       assert (stat_eq heap_words Stat.heap_words);
       assert (stat_eq top_heap_words Stat.top_heap_words)
       in
       check ();
       let r = ref [] in
       let n = heap_chunks () in
       while not (heap_chunks () > n) do
       check ();
       r := Bytes.create 128 :: !r
       done;
       (* Don't let flambda2 drop r, producing an infinite loop *)
       let _ = (Sys.opaque_identity !r : bytes list) in
       check ()
       ;;
  *)

  let%test "is_zero_alloc does not allocate" =
    let open Gc.For_testing in
    is_zero_alloc (fun () -> ignore (is_zero_alloc Fn.id : bool))
  ;;

  let show_raise_hide_backtraces f =
    Dynamic.with_temporarily Backtrace.elide true ~f:(fun () ->
      match Or_error.try_with f with
      | Ok _ -> print_string "did not raise"
      | Error err ->
        Error.sexp_of_t err |> Expect_test_helpers_base.print_s ~hide_positions:true)
  ;;

  let non_allocating_function () = 1 + 2
  let allocating_function () = ref 0

  let%expect_test "[assert_no_allocation]" =
    show_raise_hide_backtraces (fun () ->
      Gc.For_testing.assert_no_allocation non_allocating_function);
    [%expect {| did not raise |}];
    show_raise_hide_backtraces (fun () ->
      Gc.For_testing.assert_no_allocation allocating_function);
    [%expect
      {|
      ("allocation detected"
        (here lib/core/test/core_gc_unit_tests.ml:LINE:COL)
        (allocation_report (
          (major_words_allocated 0)
          (minor_words_allocated 2)))
        (allocation_log ((
          (size_in_words 2)
          (is_major      false)
          (backtrace     "<backtrace elided in test>")))))
      |}];
    (* Location information should be excluded when [here] is [Lexing.dummy_pos], which is
       the default value of [here] in the external version of Base for [%call_pos]
       arguments *)
    show_raise_hide_backtraces (fun () ->
      Gc.For_testing.assert_no_allocation ~here:Lexing.dummy_pos allocating_function);
    [%expect
      {|
      ("allocation detected"
        (allocation_report (
          (major_words_allocated 0)
          (minor_words_allocated 2)))
        (allocation_log ((
          (size_in_words 2)
          (is_major      false)
          (backtrace     "<backtrace elided in test>")))))
      |}]
  ;;
end

let%test_unit _ =
  let r = ref () in
  let weak = Stdlib.Weak.create 1 in
  Stdlib.Weak.set weak 0 (Some r);
  Stdlib.Gc.compact ();
  assert (
    match Stdlib.Weak.get weak 0 with
    | None -> false
    | Some _ -> true);
  keep_alive (r, r)
;;

let%expect_test "stat diff" =
  let before = Gc.stat () in
  let after =
    { before with
      minor_words = before.minor_words +. 100.
    ; compactions = before.compactions + 7
    }
  in
  print_s [%sexp (Gc.Stat.diff after before : Gc.Stat.t)];
  [%expect
    {|
    ((minor_words 100) (promoted_words 0) (major_words 0) (minor_collections 0)
     (major_collections 0) (heap_words 0) (heap_chunks 0) (live_words 0)
     (live_blocks 0) (free_words 0) (free_blocks 0) (largest_free 0)
     (fragments 0) (compactions 7) (top_heap_words 0) (stack_size 0)
     (forced_major_collections 0))
    |}]
;;

let empty_gc_stat =
  { Gc.Stat.minor_words = 0.
  ; promoted_words = 0.
  ; major_words = 0.
  ; minor_collections = 0
  ; major_collections = 0
  ; heap_words = 0
  ; heap_chunks = 0
  ; live_words = 0
  ; live_blocks = 0
  ; free_words = 0
  ; free_blocks = 0
  ; largest_free = 0
  ; fragments = 0
  ; compactions = 0
  ; top_heap_words = 0
  ; stack_size = 0
  ; forced_major_collections = 0
  }
;;

let%expect_test "stat add" =
  let first = { empty_gc_stat with minor_words = 5.; heap_words = 10; compactions = 3 } in
  let second =
    { empty_gc_stat with minor_words = 3.; live_blocks = 7; compactions = 1 }
  in
  print_s [%sexp (Gc.Stat.add first second : Gc.Stat.t)];
  [%expect
    {|
    ((minor_words 8) (promoted_words 0) (major_words 0) (minor_collections 0)
     (major_collections 0) (heap_words 10) (heap_chunks 0) (live_words 0)
     (live_blocks 7) (free_words 0) (free_blocks 0) (largest_free 0)
     (fragments 0) (compactions 4) (top_heap_words 0) (stack_size 0)
     (forced_major_collections 0))
    |}]
;;

let[@inline never] create_and_leak_uncollectable_value () =
  let thing_to_leak = [| "please don't collect me" |] |> Sys.opaque_identity in
  Gc.Expert.add_finalizer_ignore thing_to_leak (fun _ ->
    raise_s [%message "that should not have been collected"]);
  Gc.Expert.leak thing_to_leak
;;

let%test_unit "leak prevents collection" =
  create_and_leak_uncollectable_value ();
  Gc.full_major ()
;;

let%expect_test ("can't attach a finalizer to non-heap-blocks" [@tags "no-js"]) =
  Expect_test_helpers_core.show_raise (fun () ->
    Gc.Expert.add_finalizer_exn 8 (fun _ -> print_s [%message "collected"]));
  [%expect {| (raised (Failure "Heap_block.create_exn called with non heap block")) |}]
;;

let create_finalizer_reference_loop ~add_finalizer_exn =
  let array = Array.create ~len:10 'x' in
  let accidentally_captured = Some array |> Sys.opaque_identity in
  add_finalizer_exn array (fun _ ->
    match accidentally_captured with
    | None -> assert false
    | Some x ->
      print_s [%sexp "collected"];
      assert (Char.( = ) x.(0) 'x'));
  array
;;

let%expect_test ("finalizer reference loop: memory leak" [@tags "no-js"]) =
  let _array =
    create_finalizer_reference_loop ~add_finalizer_exn:Gc.Expert.add_finalizer_exn
  in
  (* Unfortunately, this creates a memory leak. Despite the array and its finalizer not
     having any inbound references, it never gets collected. *)
  Gc.full_major ()
;;

let%expect_test ("finalizer reference loop: protection from memory leak" [@tags "no-js"]) =
  let _array =
    create_finalizer_reference_loop
      ~add_finalizer_exn:Gc.Expert.With_leak_protection.add_finalizer_exn
  in
  (* The leak protection using ephemerons fixes the bug seen above! *)
  Gc.full_major ();
  [%expect {| collected |}]
;;
