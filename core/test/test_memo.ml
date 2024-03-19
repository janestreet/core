open! Core
open! Import
open! Memo

let%expect_test "general" =
  let f x = x * x in
  let memo_f = Memo.general f in
  quickcheck_m
    [%here]
    (module Int)
    ~f:(fun x -> require_equal [%here] (module Int) (f x) (memo_f x))
;;

let%test_module "lru" =
  (module struct
    let count = ref 0

    (* number of times f underlying function is run *)
    let f =
      general ~cache_size_bound:3 (fun i ->
        incr count;
        i)
    ;;

    let%test _ = f 0 = 0
    let%test _ = !count = 1
    let%test _ = f 1 = 1
    let%test _ = !count = 2
    let%test _ = f 0 = 0
    let%test _ = !count = 2
    let%test _ = f 3 = 3

    (* cache full *)
    let%test _ = !count = 3
    let%test _ = f 4 = 4

    (* evict 1 *)
    let%test _ = !count = 4
    let%test _ = f 0 = 0
    let%test _ = !count = 4
    let%test _ = f 1 = 1

    (* recompute 1 *)
    let%test _ = !count = 5
  end)
;;

let%test_module "comparable" =
  (module struct
    module Env = struct
      type t =
        { should_raise : bool ref
        ; f : int -> int
        }

      let create () =
        let should_raise = ref false in
        let f =
          of_comparable
            (module Int)
            (fun i ->
              print_s [%message "Computing value" (i : int)];
              if !should_raise then raise_s [%message "I should raise"];
              i + 1)
        in
        { should_raise; f }
      ;;
    end

    let%expect_test "Ensure no re computation" =
      let { Env.should_raise; f } = Env.create () in
      printf "%d" (f 5);
      [%expect {|
        ("Computing value" (i 5))
        6
        |}];
      should_raise := true;
      printf "%d" (f 5);
      [%expect {| 6 |}]
    ;;

    let%expect_test "What happens if f raises" =
      let { Env.should_raise; f } = Env.create () in
      should_raise := true;
      show_raise (fun () -> printf "%d" (f 5));
      [%expect
        {|
        ("Computing value" (i 5))
        (raised "I should raise")
        |}];
      should_raise := false;
      show_raise (fun () -> printf "%d" (f 5));
      [%expect {| (raised "I should raise") |}]
    ;;
  end)
;;

let%expect_test "[general] on recursive functions" =
  let rec fib n =
    printf "fib %d\n" n;
    if n < 2 then n else fib (n - 1) + fib (n - 2)
  in
  let fib : int -> int = Memo.general ~hashable:Int.hashable fib in
  printf "%d\n" (fib 2);
  printf "%d\n" (fib 5);
  printf "%d\n" (fib 2);
  printf "%d\n" (fib 5);
  [%expect
    {|
    fib 2
    fib 0
    fib 1
    1
    fib 5
    fib 3
    fib 1
    fib 2
    fib 0
    fib 1
    fib 4
    fib 2
    fib 0
    fib 1
    fib 3
    fib 1
    fib 2
    fib 0
    fib 1
    5
    1
    5
    |}]
;;

let%expect_test "recursive" =
  let fib fib n =
    printf "fib %d\n" n;
    if n < 2 then n else fib (n - 1) + fib (n - 2)
  in
  let fib : int -> int = Memo.recursive ~hashable:Int.hashable fib in
  printf "%d\n" (fib 2);
  printf "%d\n" (fib 5);
  printf "%d\n" (fib 10);
  [%expect
    {|
    fib 2
    fib 0
    fib 1
    1
    fib 5
    fib 3
    fib 4
    5
    fib 10
    fib 8
    fib 6
    fib 7
    fib 9
    55
    |}]
;;

let%expect_test "infinite loop" =
  let call_count = ref 0 in
  let f =
    Memo.recursive ~hashable:Int.hashable (fun f x ->
      incr call_count;
      if !call_count > 100
      then (
        print_endline "infinite loop";
        ())
      else f x)
  in
  f 0;
  [%expect {| infinite loop |}]
;;

let%expect_test "recursive memo initialization effects" =
  let fib fib =
    printf "initialization\n";
    fun n ->
      printf "fib %d\n" n;
      if n < 2 then n else fib (n - 1) + fib (n - 2)
  in
  let fib : int -> int = Memo.recursive ~hashable:Int.hashable fib in
  [%expect {| initialization |}];
  printf "%d\n" (fib 2);
  printf "%d\n" (fib 5);
  [%expect
    {|
    fib 2
    fib 0
    fib 1
    1
    fib 5
    fib 3
    fib 4
    5
    |}]
;;
