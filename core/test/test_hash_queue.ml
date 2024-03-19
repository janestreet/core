open! Core
open! Import
open! Hash_queue

module Hq_arg = struct
  include String

  let hash (x : t) = Hashtbl.hash x
  let sexp_of_t = Sexplib.Conv.sexp_of_string
end

module Hq = Hash_queue.Make (Hq_arg)

let%expect_test _ =
  let hq = Hq.create () in
  let inv () = Hq.invariant hq in
  (* tests over empty queue *)
  inv ();
  assert (Hq.is_empty hq);
  assert (Option.is_none (Hq.dequeue_front hq));
  require_does_raise [%here] (fun () -> Hq.dequeue_front_exn hq);
  [%expect {| "Hash_queue.dequeue_exn: empty queue" |}];
  assert (Option.is_none (Hq.dequeue_front_with_key hq));
  require_does_raise [%here] (fun () -> Hq.dequeue_front_with_key_exn hq);
  [%expect {| "Hash_queue.dequeue_with_key: empty queue" |}];
  Hq.dequeue_all hq ~f:(fun _ -> assert false);
  assert (Poly.( = ) (Hq.remove hq "foobar") `No_such_key);
  require_does_raise [%here] (fun () -> Hq.remove_exn hq "foobar");
  [%expect {| ("Hash_queue.remove_exn: unknown key" foobar) |}];
  assert (Poly.( = ) (Hq.replace hq "foobar" 0) `No_such_key);
  require_does_raise [%here] (fun () -> Hq.replace_exn hq "foobar" 0);
  [%expect {| ("Hash_queue.replace_exn: unknown key" foobar) |}];
  assert (List.is_empty (Hq.foldi hq ~init:[] ~f:(fun ac ~key:_ ~data:_ -> () :: ac)));
  assert (List.is_empty (Hq.fold hq ~init:[] ~f:(fun ac _ -> () :: ac)));
  Hq.iteri hq ~f:(fun ~key:_ ~data:_ -> assert false);
  (* test with 10 elems *)
  let n = 10 in
  for i = 1 to n do
    assert (Poly.( = ) (Hq.enqueue_back hq (string_of_int i) i) `Ok);
    inv ()
  done;
  assert (Hq.length hq = n);
  assert (
    [%equal: (string * int) list]
      (List.rev (Hq.foldi hq ~init:[] ~f:(fun ac ~key ~data -> (key, data) :: ac)))
      (List.init n ~f:(fun i ->
         let i = i + 1 in
         string_of_int i, i)));
  assert (
    [%equal: int list]
      (List.rev (Hq.fold hq ~init:[] ~f:(fun ac data -> data :: ac)))
      (List.init n ~f:(fun i -> i + 1)));
  Hq.iteri hq ~f:(fun ~key ~data -> assert (String.( = ) key (string_of_int data)));
  (* test removing the first element from the queue *)
  let sum = ref 0 in
  Hq.iter hq ~f:(fun x -> sum := !sum + x);
  assert (!sum = n * (n + 1) / 2);
  assert (Hq.mem hq "1");
  ignore (Hq.dequeue_front hq : int option);
  inv ();
  assert (not (Hq.mem hq "1"));
  assert (Hq.length hq = n - 1);
  (* remove the last *)
  assert (Poly.( = ) (Hq.remove hq (string_of_int n)) `Ok);
  (* double remove *)
  assert (Poly.( = ) (Hq.remove hq (string_of_int n)) `No_such_key);
  inv ();
  assert (Hq.length hq = n - 2);
  (* remove everything *)
  let num = ref 0 in
  Hq.dequeue_all hq ~f:(fun _ -> num := !num + 1);
  inv ();
  assert (!num = n - 2);
  assert (Hq.is_empty hq);
  inv ();
  Hq.clear hq;
  assert (Hq.is_empty hq);
  (* add 100 *)
  for i = 1 to 100 do
    assert (Poly.( = ) (Hq.enqueue_back hq (string_of_int i) i) `Ok)
  done;
  (* double booking *)
  assert (Poly.( = ) (Hq.enqueue_back hq "42" 42) `Key_already_present);
  require_does_raise [%here] (fun () -> Hq.enqueue_back_exn hq "42" 42);
  [%expect {| ("Hash_queue.enqueue_exn: duplicate key" 42) |}];
  assert (Poly.( = ) (Hq.replace hq "1" 42) `Ok);
  assert ([%equal: int option] (Hq.lookup hq "1") (Some 42));
  assert (Hq.lookup_exn hq "1" = 42);
  assert ([%equal: (string * int) option] (Hq.dequeue_front_with_key hq) (Some ("1", 42)));
  assert (Poly.( = ) (Hq.replace hq "1" 42) `No_such_key);
  require_does_raise [%here] (fun () -> Hq.replace_exn hq "1" 42);
  [%expect {| ("Hash_queue.replace_exn: unknown key" 1) |}];
  assert ([%equal: int option] (Hq.lookup hq "1") None);
  require_does_raise [%here] (fun () : int -> Hq.lookup_exn hq "1");
  [%expect {| (Not_found_s ("Hashtbl.find_exn: not found" 1)) |}];
  Hq.clear hq;
  assert (Hq.is_empty hq);
  let add i = Hq.enqueue_back_exn hq (Int.to_string i) i in
  List.iter [ 1; 2; 3 ] ~f:add;
  assert ([%equal: string list] [ "1"; "2"; "3" ] (Hq.keys hq));
  assert ([%equal: (string * int) list] [ "1", 1; "2", 2; "3", 3 ] (Hq.to_alist hq));
  require_does_raise [%here] (fun () -> Hq.iter hq ~f:(fun _ -> add 13));
  [%expect
    {| (Failure "It is an error to modify a Hash_queue.t while iterating over it.") |}];
  require_does_raise [%here] (fun () ->
    Hq.iter hq ~f:(fun _ -> ignore (Hq.remove hq "foo" : [ `No_such_key | `Ok ])));
  [%expect
    {| (Failure "It is an error to modify a Hash_queue.t while iterating over it.") |}];
  [%test_result: int] (Hq.lookup_and_move_to_back_exn hq "2") ~expect:2;
  [%test_result: string * int] (Hq.dequeue_front_with_key_exn hq) ~expect:("1", 1);
  [%test_result: string * int] (Hq.dequeue_front_with_key_exn hq) ~expect:("3", 3);
  [%test_result: string * int] (Hq.dequeue_front_with_key_exn hq) ~expect:("2", 2);
  [%test_result: (string * int) option] (Hq.dequeue_front_with_key hq) ~expect:None
;;

let%expect_test "enqueue_front, enqueue_front_exn" =
  let open Expect_test_helpers_core in
  let hq = Hq.create () in
  let ok_exn = function
    | `Key_already_present -> raise_s [%message "Key already present"]
    | `Ok -> ()
  in
  for i = 1 to 10 do
    Hq.enqueue_front hq (Int.to_string i) i |> ok_exn
  done;
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (10 9 8 7 6 5 4 3 2 1) |}];
  let hq_rev = Hq.create () in
  for i = 1 to 10 do
    Hq.enqueue_back hq_rev (Int.to_string i) i |> ok_exn
  done;
  print_s [%sexp (Hq.to_list hq_rev : int list)];
  [%expect {| (1 2 3 4 5 6 7 8 9 10) |}];
  (* Test the _exn version *)
  let hq2 = Hq.create () in
  let n = 10 in
  for i = 1 to n do
    Hq.enqueue_front_exn hq2 (string_of_int i) i
  done;
  require [%here] ([%compare.equal: int list] (Hq.to_list hq2) (Hq.to_list hq))
;;

let test_enqueue_fn (back_or_front : [ `back | `front ]) enqueue_fn =
  let hq, hq' = Hq.create (), Hq.create () in
  let n = 10 in
  for i = 1 to n do
    assert (Poly.( = ) (Hq.enqueue hq back_or_front (string_of_int i) i) `Ok);
    assert (Poly.( = ) (enqueue_fn hq' (string_of_int i) i) `Ok)
  done;
  [%test_result: int list] (Hq.to_list hq) ~expect:(Hq.to_list hq');
  (* make sure they both return [`Key_already_present] *)
  [%test_result: [ `Key_already_present | `Ok ]]
    (Hq.enqueue hq back_or_front (string_of_int 5) 5)
    ~expect:`Key_already_present;
  [%test_result: [ `Key_already_present | `Ok ]]
    (enqueue_fn hq' (string_of_int 5) 5)
    ~expect:`Key_already_present
;;

let%expect_test "enqueue t `back := enqueue_back" = test_enqueue_fn `back Hq.enqueue_back

let%expect_test "enqueue t `front := enqueue_front" =
  test_enqueue_fn `front Hq.enqueue_front
;;

let%expect_test "lookup_and_move_to_back_exn" =
  let hq = Hq.create () in
  let n = 10 in
  for i = 1 to n do
    Hq.enqueue_back_exn hq (string_of_int i) i
  done;
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (1 2 3 4 5 6 7 8 9 10) |}];
  print_s [%sexp (Hq.lookup_and_move_to_back_exn hq "5" : int)];
  [%expect {| 5 |}];
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (1 2 3 4 6 7 8 9 10 5) |}]
;;

let%expect_test "lookup_and_move_to_front_exn" =
  let hq = Hq.create () in
  let n = 10 in
  for i = 1 to n do
    Hq.enqueue_back_exn hq (string_of_int i) i
  done;
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (1 2 3 4 5 6 7 8 9 10) |}];
  print_s [%sexp (Hq.lookup_and_move_to_front_exn hq "5" : int)];
  [%expect {| 5 |}];
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (5 1 2 3 4 6 7 8 9 10) |}]
;;

let%expect_test "dequeue_back, dequeue_back_exn" =
  let open Expect_test_helpers_core in
  let make_hq () =
    let hq = Hq.create () in
    for i = 1 to 10 do
      Hq.enqueue_front_exn hq (Int.to_string i) i
    done;
    hq
  in
  let hq = make_hq () in
  for _ = 1 to 11 do
    print_s [%sexp (Hq.dequeue_back hq : int option)]
  done;
  [%expect
    {|
    (1)
    (2)
    (3)
    (4)
    (5)
    (6)
    (7)
    (8)
    (9)
    (10)
    ()
    |}];
  let hq = make_hq () in
  for _ = 1 to 10 do
    print_s [%sexp (Hq.dequeue_back_exn hq : int)]
  done;
  [%expect {|
    1
    2
    3
    4
    5
    6
    7
    8
    9
    10
    |}]
;;

let%expect_test "drop" =
  let open Expect_test_helpers_core in
  let make_hq () =
    let hq = Hq.create () in
    for i = 1 to 10 do
      Hq.enqueue_back_exn hq (Int.to_string i) i
    done;
    hq
  in
  let hq = make_hq () in
  Hq.drop_back ~n:3 hq;
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (1 2 3 4 5 6 7) |}];
  let hq = make_hq () in
  Hq.drop_front ~n:4 hq;
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (5 6 7 8 9 10) |}];
  (* Check when n > length of list *)
  let hq = make_hq () in
  Hq.drop_back ~n:20 hq;
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| () |}]
;;

let%expect_test "lookup_and_remove" =
  let hq = Hq.create () in
  let n = 10 in
  for i = 1 to n do
    Hq.enqueue_back_exn hq (Int.to_string i) i
  done;
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (1 2 3 4 5 6 7 8 9 10) |}];
  print_s [%sexp (Hq.lookup_and_remove hq "5" : int option)];
  [%expect {| (5) |}];
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (1 2 3 4 6 7 8 9 10) |}];
  print_s [%sexp (Hq.lookup_and_remove hq "11" : int option)];
  [%expect {| () |}];
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (1 2 3 4 6 7 8 9 10) |}]
;;

let%expect_test "last" =
  let hq = Hq.create () in
  let n = 10 in
  for i = 1 to n do
    Hq.enqueue_back_exn hq (Int.to_string i) i
  done;
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (1 2 3 4 5 6 7 8 9 10) |}];
  print_s [%sexp (Hq.last hq : int option)];
  [%expect {| (10) |}];
  print_s [%sexp (Hq.last_with_key hq : (string * int) option)];
  [%expect {| ((10 10)) |}]
;;

let%expect_test "[copy] returns an identical but independent hash queue" =
  let hq = Hq.create () in
  let n = 10 in
  for i = 1 to n do
    Hq.enqueue_back_exn hq (Int.to_string i) i
  done;
  let copy = Hq.copy hq in
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (1 2 3 4 5 6 7 8 9 10) |}];
  print_s [%sexp (Hq.to_list copy : int list)];
  [%expect {| (1 2 3 4 5 6 7 8 9 10) |}];
  Hq.drop_front hq;
  Hq.drop_back copy;
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (2 3 4 5 6 7 8 9 10) |}];
  print_s [%sexp (Hq.to_list copy : int list)];
  [%expect {| (1 2 3 4 5 6 7 8 9) |}];
  print_s [%sexp (Hq.lookup_and_move_to_front_exn hq "5" : int)];
  [%expect {| 5 |}];
  print_s [%sexp (Hq.lookup_and_move_to_back_exn copy "6" : int)];
  [%expect {| 6 |}];
  print_s [%sexp (Hq.to_list hq : int list)];
  [%expect {| (5 2 3 4 6 7 8 9 10) |}];
  print_s [%sexp (Hq.to_list copy : int list)];
  [%expect {| (1 2 3 4 5 7 8 9 6) |}]
;;

let%expect_test "[replace_or_enqueue_back] adds a new element to the back when the key \
                 does not exist"
  =
  let hq = Hq.create () in
  Hq.replace_or_enqueue_back hq "key1" "value1";
  Hq.replace_or_enqueue_back hq "key2" "value2";
  print_s [%sexp (hq : string Hq.t)];
  [%expect {|
    ((key1 value1)
     (key2 value2))
    |}]
;;

let%expect_test "[replace_or_enqueue_front] adds a new element to the front when the key \
                 does not exist"
  =
  let hq = Hq.create () in
  Hq.replace_or_enqueue_front hq "key1" "value1";
  Hq.replace_or_enqueue_front hq "key2" "value2";
  print_s [%sexp (hq : string Hq.t)];
  [%expect {|
    ((key2 value2)
     (key1 value1))
    |}]
;;

let%expect_test "[replace_or_enqueue_front] replaces a value when the key already exists \
                 and front was given"
  =
  let hq = Hq.create () in
  Hq.replace_or_enqueue_front hq "key" "value1";
  Hq.replace_or_enqueue_front hq "key" "value2";
  print_s [%sexp (hq : string Hq.t)];
  [%expect {| ((key value2)) |}]
;;

let%expect_test "[replace_or_enqueue_back] replaces a value when the key already exists \
                 and back was given"
  =
  let hq = Hq.create () in
  Hq.replace_or_enqueue_back hq "key" "value1";
  Hq.replace_or_enqueue_back hq "key" "value2";
  print_s [%sexp (hq : string Hq.t)];
  [%expect {| ((key value2)) |}]
;;

let%expect_test "[replace_or_enqueue] raises with the correct message when the hash \
                 queue is being read"
  =
  let hq = Hq.create () in
  Hq.replace_or_enqueue_front hq "key1" "value1";
  require_does_raise [%here] (fun () ->
    Hq.iter hq ~f:(fun _ -> Hq.replace_or_enqueue_front hq "key2" "value2"));
  [%expect
    {| (Failure "It is an error to modify a Hash_queue.t while iterating over it.") |}]
;;
