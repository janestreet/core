open! Import
open! Core
open! Map

let%expect_test "Tree.add duplicate does not allocate" =
  let tree = Tree.of_alist_exn ~comparator:Int.comparator [ 1, 1; 2, 2 ] in
  let result =
    require_no_allocation [%here] (fun () ->
      Tree.add ~comparator:Int.comparator tree ~key:2 ~data:2)
  in
  print_s [%sexp (result : _ Map_intf.Or_duplicate.t)];
  [%expect {| Duplicate |}]
;;

let%expect_test "t_of_sexp raises on duplicate key" =
  let no_dup_sexp = Sexp.of_string "((a 1) (b 2) (c 3) (d 4))" in
  require_equal
    [%here]
    (module Sexp)
    no_dup_sexp
    ([%sexp_of: int String.Map.t] ([%of_sexp: int String.Map.t] no_dup_sexp));
  let dup_sexp = Sexp.of_string "((a 1) (b 2) (a 3) (d 4))" in
  require_does_raise [%here] (fun () -> [%of_sexp: int String.Map.t] dup_sexp);
  [%expect {| (Of_sexp_error "Map.t_of_sexp_direct: duplicate key" (invalid_sexp a)) |}]
;;

let%expect_test "bin_read_t raises on duplicate key" =
  let max_n = 20 in
  let bstr = Bigstring.create ((2 * max_n) + 1) in
  for n = 0 to max_n do
    let m1 = Int.Map.of_alist_exn (List.init n ~f:(fun x -> x, succ x)) in
    let pos = Int.Map.bin_write_t Int.bin_write_t bstr ~pos:0 m1 in
    require_equal [%here] (module Int) pos ((2 * n) + 1);
    let pos_ref = ref 0 in
    let m2 = Int.Map.bin_read_t Int.bin_read_t bstr ~pos_ref in
    require_equal [%here] (module Int) !pos_ref ((2 * n) + 1);
    require_equal
      [%here]
      (module struct
        type t = int Int.Map.t [@@deriving equal, sexp_of]
      end)
      m1
      m2;
    if n >= 2
    then (
      bstr.{1} <- 'x';
      bstr.{3} <- 'x';
      pos_ref := 0;
      require_does_raise [%here] (fun () ->
        Int.Map.bin_read_t Int.bin_read_t bstr ~pos_ref);
      [%expect {| (Failure "Map.bin_read_t: duplicate element in map") |}];
      require_equal [%here] (module Int) !pos_ref ((2 * n) + 1))
  done
;;

let%expect_test "Symmetric_diff_element.map_data" =
  let print x = print_s [%sexp (x : (string, int) Symmetric_diff_element.t)] in
  let f x = x + 1 in
  print (Symmetric_diff_element.map_data ("foo", `Left 1) ~f);
  [%expect {| (foo (Left 2)) |}];
  print (Symmetric_diff_element.map_data ("foo", `Right 5) ~f);
  [%expect {| (foo (Right 6)) |}];
  print (Symmetric_diff_element.map_data ("foo", `Unequal (10, 12)) ~f);
  [%expect {| (foo (Unequal (11 13))) |}]
;;

let%expect_test "Symmetric_diff_element.{left,right}" =
  let values = [ "foo", `Left 1; "bar", `Right 2; "baz", `Unequal (3, 4) ] in
  let go f =
    List.iter values ~f:(fun sd_elt ->
      printf
        !"%{sexp: (string, int) Symmetric_diff_element.t} => %{sexp: int option}\n"
        sd_elt
        (f sd_elt))
  in
  go Symmetric_diff_element.left;
  [%expect
    {|
    (foo (Left 1)) => (1)
    (bar (Right 2)) => ()
    (baz (Unequal (3 4))) => (3)
    |}];
  go Symmetric_diff_element.right;
  [%expect
    {|
    (foo (Left 1)) => ()
    (bar (Right 2)) => (2)
    (baz (Unequal (3 4))) => (4)
    |}]
;;

let%expect_test _ =
  let open Expect_test_helpers_core in
  print_and_check_stable_type
    [%here]
    (module struct
      type t = int Map.M(Int).t [@@deriving bin_io, compare, sexp]
    end)
    ([ []; [ 1 ]; [ 1; 2 ]; [ 1; 2; 3 ] ]
     |> List.map ~f:(fun xs ->
          Map.of_alist_exn (module Int) (xs |> List.map ~f:(fun x -> x, x + 1))));
  [%expect
    {|
    (bin_shape_digest ed73a010af8ffc32cab7411d6be2d676)
    ((sexp ()) (bin_io "\000"))
    ((sexp ((1 2))) (bin_io "\001\001\002"))
    ((sexp (
       (1 2)
       (2 3)))
     (bin_io "\002\001\002\002\003"))
    ((sexp (
       (1 2)
       (2 3)
       (3 4)))
     (bin_io "\003\001\002\002\003\003\004"))
    |}]
;;

let%expect_test "remove does not allocate too much if there's nothing to do" =
  let map1 = Map.of_alist_exn (module Int) [ 1, "one"; 3, "three" ] in
  (* This is a regression test. Previously this no-op remove allocated 16 words.

     Ideally, we wouldn't allocate the pair returned by [Tree0.remove] either. *)
  Expect_test_helpers_core.require_allocation_does_not_exceed
    (Minor_words 3)
    [%here]
    (fun () -> ignore (Sys.opaque_identity (Map.remove map1 2) : string Map.M(Int).t))
;;

let%expect_test "[map_keys]" =
  let test m c ~f =
    print_s
      [%sexp
        (Map.map_keys c ~f m : [ `Duplicate_key of string | `Ok of string String.Map.t ])]
  in
  let map = Int.Map.of_alist_exn [ 1, "one"; 2, "two"; 3, "three" ] in
  test map (module String) ~f:Int.to_string;
  [%expect {| (Ok ((1 one) (2 two) (3 three))) |}];
  test map (module String) ~f:(fun x -> Int.to_string (x / 2));
  [%expect {| (Duplicate_key 1) |}]
;;
