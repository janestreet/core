open! Core
open! Import
open! Doubly_linked
include Test_container.Test_S1 (Doubly_linked)

let show t =
  print_s
    [%message
      (length t : int)
        (is_empty t : bool)
        (first t : int option)
        (last t : int option)
        (to_list t : int list)]
;;

let%expect_test "empty" =
  let t = create () in
  show t;
  [%expect
    {|
    (("length t"   0)
     ("is_empty t" true)
     ("first t"   ())
     ("last t"    ())
     ("to_list t" ()))
    |}];
  require_none [%here] [%sexp_of: int] (remove_first t);
  require_none [%here] [%sexp_of: int] (remove_last t)
;;

let%expect_test "singleton" =
  let t = create () in
  let elt = insert_first t 13 in
  show t;
  [%expect
    {|
    (("length t"   1)
     ("is_empty t" false)
     ("first t"   (13))
     ("last t"    (13))
     ("to_list t" (13)))
    |}];
  require [%here] (is_first t elt);
  require [%here] (is_last t elt)
;;

let%expect_test "pair" =
  let t = create () in
  let elt2 = insert_first t 14 in
  let elt1 = insert_first t 13 in
  show t;
  [%expect
    {|
    (("length t"   2)
     ("is_empty t" false)
     ("first t" (13))
     ("last t"  (14))
     ("to_list t" (13 14)))
    |}];
  require [%here] (is_first t elt1);
  require [%here] (is_last t elt2)
;;

let%expect_test "of_list" =
  for i = 0 to 5 do
    let l = List.init i ~f:Fn.id in
    let t = of_list l in
    require_equal
      [%here]
      (module struct
        type t = int list [@@deriving equal, sexp_of]
      end)
      (to_list t)
      l
  done
;;

let%expect_test "clear" =
  for i = 0 to 5 do
    let t = of_list (List.init i ~f:Fn.id) in
    clear t;
    require [%here] (is_empty t)
  done
;;

let%expect_test "transfer" =
  for i1 = 0 to 3 do
    let l1 = List.init i1 ~f:Fn.id in
    for i2 = 0 to 3 do
      let l2 = List.init i2 ~f:Fn.id in
      let t1 = of_list l1 in
      let t2 = of_list l2 in
      transfer ~src:t1 ~dst:t2;
      require [%here] (is_empty t1);
      require_equal
        [%here]
        (module struct
          type t = int list [@@deriving equal, sexp_of]
        end)
        (to_list t2)
        (l2 @ l1)
    done
  done
;;

let%expect_test "transfer2" =
  let l1 = create () in
  let e = insert_first l1 9 in
  let l2 = create () in
  transfer ~src:l1 ~dst:l2;
  remove l2 e;
  require [%here] (is_empty l1);
  require [%here] (is_empty l2)
;;

let%expect_test "self-transfer" =
  let t = of_list [] in
  require_does_raise [%here] (fun () -> transfer ~src:t ~dst:t);
  [%expect {| (Core__Doubly_linked.Transfer_src_and_dst_are_same_list) |}]
;;

let%expect_test "transfer3" =
  let src = of_list [ 1 ] in
  let elt = insert_last src 4 in
  let dst = of_list [ 1; 2; 3; 4 ] in
  print_s [%message (src : int t) (dst : int t)];
  [%expect {| ((src (1 4)) (dst (1 2 3 4))) |}];
  transfer ~src ~dst;
  print_s [%message (src : int t) (dst : int t)];
  [%expect {| ((src ()) (dst (1 2 3 4 1 4))) |}];
  require_does_not_raise [%here] (fun () -> ignore (next dst elt : _ Elt.t option))
;;

let%expect_test "insert and remove" =
  let module Elt = struct
    include Elt

    type t = unit Elt.t [@@deriving sexp_of]
  end
  in
  let module Elt_option = struct
    type t = Elt.t option [@@deriving equal, sexp_of]
  end
  in
  let t = create () in
  let is_elts elts =
    require_equal
      [%here]
      (module struct
        type t = unit list [@@deriving equal, sexp_of]
      end)
      (to_list t)
      (List.map elts ~f:Elt.value);
    let rec loop elt elts =
      match elt, elts with
      | None, [] -> ()
      | Some elt, elt' :: elts ->
        require_equal [%here] (module Elt) elt elt';
        loop (next t elt) elts
      | _ -> assert false
    in
    loop (first_elt t) elts;
    (match elts with
     | [] -> ()
     | elt :: elts ->
       require_none [%here] [%sexp_of: Elt.t] (prev t elt);
       require [%here] (is_first t elt);
       require_equal [%here] (module Elt_option) (first_elt t) (Some elt);
       List.iter elts ~f:(fun elt -> require [%here] (not (is_first t elt)));
       ignore
         (List.fold elts ~init:elt ~f:(fun prev_elt elt ->
            require_equal [%here] (module Elt_option) (prev t elt) (Some prev_elt);
            elt)
           : Elt.t));
    match List.rev elts with
    | [] -> ()
    | elt :: elts ->
      (* [elt] is the last elt.  each of [elts] is not last. *)
      require_none [%here] [%sexp_of: Elt.t] (next t elt);
      require [%here] (is_last t elt);
      require_equal [%here] (module Elt_option) (last_elt t) (Some elt);
      List.iter elts ~f:(fun elt -> require [%here] (not (is_last t elt)));
      ignore
        (List.fold elts ~init:elt ~f:(fun next_elt elt ->
           assert (Option.equal Elt.equal (next t elt) (Some next_elt));
           elt)
          : Elt.t)
  in
  let elt1 = insert_first t () in
  is_elts [ elt1 ];
  let elt2 = insert_first t () in
  is_elts [ elt2; elt1 ];
  let elt3 = insert_last t () in
  is_elts [ elt2; elt1; elt3 ];
  remove t elt1;
  is_elts [ elt2; elt3 ];
  let elt4 = insert_after t elt2 () in
  is_elts [ elt2; elt4; elt3 ];
  let elt5 = insert_before t elt2 () in
  is_elts [ elt5; elt2; elt4; elt3 ];
  ignore (remove_last t : _ option);
  is_elts [ elt5; elt2; elt4 ];
  ignore (remove_first t : _ option);
  is_elts [ elt2; elt4 ];
  ignore (remove_first t : _ option);
  is_elts [ elt4 ];
  ignore (remove_first t : _ option);
  is_elts []
;;

let%expect_test "removed element doesn't belong to list" =
  let t = of_list [ 1 ] in
  let e = insert_first t 2 in
  invariant ignore t;
  require_some [%here] (remove_first t);
  require_does_raise [%here] (fun () -> is_last t e);
  [%expect {| (Core__Doubly_linked.Elt_does_not_belong_to_list) |}]
;;

let%expect_test _ =
  let t = of_list [ 1 ] in
  let e = insert_first t 3 in
  invariant ignore t;
  remove t e;
  invariant ignore t;
  require_some [%here] (first_elt t)
;;

let%expect_test _ =
  let t = of_list [ 1; 2 ] in
  require_does_not_raise [%here] (fun _ -> [%test_result: int] ~expect:1 (first_exn t));
  require_does_not_raise [%here] (fun _ -> [%test_result: int] ~expect:2 (last_exn t))
;;

let%expect_test _ =
  let l1 = of_list [ 1; 2; 3 ] in
  let l2 = copy l1 in
  transfer ~src:l2 ~dst:l1;
  invariant ignore l1;
  print_s [%message (l1 : int t) (l2 : int t)];
  [%expect {| ((l1 (1 2 3 1 2 3)) (l2 ())) |}]
;;

let%expect_test "length" =
  let l1 = of_list [ 1; 2; 3; 4 ] in
  print_s [%message (length l1 : int)];
  [%expect {| ("length l1" 4) |}];
  ignore (insert_last l1 4 : _ Elt.t);
  print_s [%message (length l1 : int)];
  [%expect {| ("length l1" 5) |}];
  let l2 = of_list [ 1; 2; 3 ] in
  print_s [%message (length l2 : int)];
  [%expect {| ("length l2" 3) |}];
  transfer ~src:l1 ~dst:l2;
  print_s [%message (length l1 : int) (length l2 : int)];
  [%expect {|
    (("length l1" 0)
     ("length l2" 8))
    |}]
;;

let%test_unit "iter_elt" =
  List.iter
    [ []; [ 1 ]; [ 2; 3 ] ]
    ~f:(fun l ->
      let sum = ref 0 in
      iter_elt (of_list l) ~f:(fun elt -> sum := !sum + Elt.value elt);
      [%test_result: int] !sum ~expect:(List.fold l ~init:0 ~f:( + )))
;;

module type Testable = sig
  type t [@@deriving compare, sexp_of]
end

let expect_test (type a) (module M : Testable with type t = a) t (expect : M.t list) =
  [%test_result: M.t list] (to_list t) ~expect
;;

let expect_test_int = expect_test (module Int)

let%test_module "move/set functions" =
  (module struct
    let n = 5

    let test k expected =
      let t = create () in
      let a = Array.init n ~f:(fun i -> insert_last t i) in
      k t a;
      invariant ignore t;
      assert (length t = n);
      expect_test_int t expected
    ;;

    let%test_unit _ = test (fun _ _ -> ()) [ 0; 1; 2; 3; 4 ]
    let%test_unit _ = test (fun t a -> move_to_front t a.(4)) [ 4; 0; 1; 2; 3 ]
    let%test_unit _ = test (fun t a -> move_to_front t a.(3)) [ 3; 0; 1; 2; 4 ]
    let%test_unit _ = test (fun t a -> move_to_front t a.(2)) [ 2; 0; 1; 3; 4 ]
    let%test_unit _ = test (fun t a -> move_to_front t a.(1)) [ 1; 0; 2; 3; 4 ]
    let%test_unit _ = test (fun t a -> move_to_front t a.(0)) [ 0; 1; 2; 3; 4 ]
    let%test_unit _ = test (fun t a -> move_to_back t a.(0)) [ 1; 2; 3; 4; 0 ]
    let%test_unit _ = test (fun t a -> move_to_back t a.(1)) [ 0; 2; 3; 4; 1 ]
    let%test_unit _ = test (fun t a -> move_to_back t a.(2)) [ 0; 1; 3; 4; 2 ]
    let%test_unit _ = test (fun t a -> move_to_back t a.(3)) [ 0; 1; 2; 4; 3 ]
    let%test_unit _ = test (fun t a -> move_to_back t a.(4)) [ 0; 1; 2; 3; 4 ]

    let%test_unit _ =
      test (fun t a -> move_before t a.(2) ~anchor:a.(1)) [ 0; 2; 1; 3; 4 ]
    ;;

    let%test_unit _ =
      test (fun t a -> move_before t a.(2) ~anchor:a.(0)) [ 2; 0; 1; 3; 4 ]
    ;;

    let%test_unit _ =
      test (fun t a -> move_before t a.(1) ~anchor:a.(0)) [ 1; 0; 2; 3; 4 ]
    ;;

    let%test_unit _ =
      test (fun t a -> move_before t a.(0) ~anchor:a.(2)) [ 1; 0; 2; 3; 4 ]
    ;;

    let%test_unit _ =
      test (fun t a -> move_before t a.(0) ~anchor:a.(1)) [ 0; 1; 2; 3; 4 ]
    ;;

    let%test_unit _ =
      test (fun t a -> move_before t a.(3) ~anchor:a.(2)) [ 0; 1; 3; 2; 4 ]
    ;;

    let%test_unit _ =
      test (fun t a -> move_before t a.(2) ~anchor:a.(3)) [ 0; 1; 2; 3; 4 ]
    ;;

    let%test_unit _ = test (fun t a -> move_after t a.(1) ~anchor:a.(3)) [ 0; 2; 3; 1; 4 ]
    let%test_unit _ = test (fun t a -> move_after t a.(0) ~anchor:a.(2)) [ 1; 2; 0; 3; 4 ]
    let%test_unit _ = test (fun t a -> move_after t a.(1) ~anchor:a.(4)) [ 0; 2; 3; 4; 1 ]
    let%test_unit _ = test (fun t a -> move_after t a.(3) ~anchor:a.(2)) [ 0; 1; 2; 3; 4 ]
    let%test_unit _ = test (fun t a -> move_after t a.(2) ~anchor:a.(3)) [ 0; 1; 3; 2; 4 ]
    let%test_unit _ = test (fun _ a -> Elt.set a.(0) 5) [ 5; 1; 2; 3; 4 ]
    let%test_unit _ = test (fun _ a -> Elt.set a.(2) 6) [ 0; 1; 6; 3; 4 ]
    let%test_unit _ = test (fun _ a -> Elt.set a.(4) 7) [ 0; 1; 2; 3; 7 ]

    let%test_unit _ =
      test
        (fun _ a ->
          Elt.set a.(0) 5;
          Elt.set a.(4) 6;
          Elt.set a.(0) 7)
        [ 7; 1; 2; 3; 6 ]
    ;;
  end)
;;

let%test_module "map functions" =
  (module struct
    let test (type a) (module M : Testable with type t = a) ~n k (expected : a list) =
      let t = create () in
      for i = 0 to n - 1 do
        insert_last t (i * 10) |> (ignore : int Elt.t -> unit)
      done;
      let new_t = k t in
      invariant ignore t;
      invariant ignore new_t;
      assert (length t = n);
      expect_test (module M) new_t expected
    ;;

    let test_int = test (module Int) ~n:5
    let test_string = test (module String) ~n:5
    let%test_unit _ = test_int (map ~f:(( + ) 1)) [ 1; 11; 21; 31; 41 ]
    let%test_unit _ = test_string (map ~f:Int.to_string) [ "0"; "10"; "20"; "30"; "40" ]
    let%test_unit _ = test_int (mapi ~f:( * )) [ 0; 10; 40; 90; 160 ]

    let%test_unit _ =
      test_string
        (mapi ~f:(fun i x -> Int.to_string i ^ "_" ^ Int.to_string x))
        [ "0_0"; "1_10"; "2_20"; "3_30"; "4_40" ]
    ;;

    let%test_unit _ = test_int (filter ~f:(fun x -> x > 15)) [ 20; 30; 40 ]
    let%test_unit _ = test_int (filteri ~f:(fun _ x -> x > 15)) [ 20; 30; 40 ]
    let%test_unit _ = test_int (filteri ~f:(fun i _ -> i < 3)) [ 0; 10; 20 ]

    let%test_unit _ =
      test_int (filter_map ~f:(fun v -> Option.some_if (v < 25) (v + 1))) [ 1; 11; 21 ]
    ;;

    let%test_unit _ =
      test_string
        (filter_mapi ~f:(fun i v -> Option.some_if (i % 2 = 0) (Int.to_string v)))
        [ "0"; "20"; "40" ]
    ;;

    let%test_unit _ = test (module String) ~n:0 (map ~f:Int.to_string) []
    let%test_unit _ = test (module String) ~n:1 (map ~f:Int.to_string) [ "0" ]
    let%test_unit _ = test (module String) ~n:0 (mapi ~f:(fun i _ -> Int.to_string i)) []

    let%test_unit _ =
      test (module String) ~n:1 (mapi ~f:(fun i _ -> Int.to_string i)) [ "0" ]
    ;;
  end)
;;

let%test_module "inplace functions" =
  (module struct
    let n = 5

    let test k expected =
      let t = create () in
      for i = 0 to n - 1 do
        insert_last t (i * 10) |> (ignore : int Elt.t -> unit)
      done;
      k t;
      invariant ignore t;
      expect_test_int t expected
    ;;

    let%test_unit _ = test (map_inplace ~f:(( + ) 1)) [ 1; 11; 21; 31; 41 ]
    let%test_unit _ = test (mapi_inplace ~f:( * )) [ 0; 10; 40; 90; 160 ]
    let%test_unit _ = test (filter_inplace ~f:(fun x -> x > 15)) [ 20; 30; 40 ]
    let%test_unit _ = test (filteri_inplace ~f:(fun _ x -> x > 15)) [ 20; 30; 40 ]
    let%test_unit _ = test (filteri_inplace ~f:(fun i _ -> i < 3)) [ 0; 10; 20 ]

    let%test_unit _ =
      test
        (filter_map_inplace ~f:(fun v -> Option.some_if (v < 25) (v + 1)))
        [ 1; 11; 21 ]
    ;;

    let%test_unit _ =
      test
        (filter_mapi_inplace ~f:(fun i v -> Option.some_if (i % 2 = 0) (v + 1)))
        [ 1; 21; 41 ]
    ;;
  end)
;;

let%test_module "partition functions" =
  (module struct
    let test
      (type a b)
      (module M1 : Testable with type t = a)
      (module M2 : Testable with type t = b)
      ~n
      k
      (expected1 : a list)
      (expected2 : b list)
      =
      let t = create () in
      for i = 0 to n - 1 do
        insert_last t (i * 10) |> (ignore : int Elt.t -> unit)
      done;
      let t1, t2 = k t in
      invariant ignore t;
      invariant ignore t1;
      invariant ignore t2;
      assert (length t = n);
      expect_test (module M1) t1 expected1;
      expect_test (module M2) t2 expected2
    ;;

    let test_int_int = test (module Int) (module Int) ~n:5
    let test_int_string = test (module Int) (module String) ~n:5

    let%test_unit _ =
      test_int_int (partition_tf ~f:(fun v -> v / 10 % 2 = 0)) [ 0; 20; 40 ] [ 10; 30 ]
    ;;

    let%test_unit _ =
      test_int_int (partitioni_tf ~f:(fun _ v -> v / 10 % 2 = 0)) [ 0; 20; 40 ] [ 10; 30 ]
    ;;

    let%test_unit _ =
      test_int_int (partitioni_tf ~f:(fun i _ -> i % 2 = 1)) [ 10; 30 ] [ 0; 20; 40 ]
    ;;

    let%test_unit _ =
      let fst = [ 1; 21; 41 ] in
      let snd = [ "10"; "30" ] in
      let f v = if v / 10 % 2 = 0 then First (v + 1) else Second (Int.to_string v) in
      test_int_string (partition_map ~f) fst snd;
      test_int_string (partition_mapi ~f:(fun _ x -> f x)) fst snd
    ;;

    let%test_unit _ =
      test_int_string
        (partition_mapi ~f:(fun i v ->
           if i % 2 = 1 then First (v + 1) else Second (Int.to_string v)))
        [ 11; 31 ]
        [ "0"; "20"; "40" ]
    ;;
  end)
;;

let create_default () =
  let t = create () in
  for i = 0 to 4 do
    insert_last t (i * 10) |> (ignore : int Elt.t -> unit)
  done;
  t
;;

let%test_unit "findi_elt" =
  let result = findi_elt (create_default ()) ~f:(fun i v -> i % 2 = 0 && v > 20) in
  match result with
  | None -> assert false
  | Some (i, elt) -> assert (i = 4 && Elt.value elt = 40)
;;

let%test_unit "findi_elt" =
  let result = findi_elt (create_default ()) ~f:(fun i _ -> i > 10) in
  assert ([%equal: _ option] result None)
;;

let%expect_test "[iter] does not allocate" =
  let t = create () in
  ignore (insert_first t () : _ Elt.t);
  require_no_allocation [%here] (fun () -> iter t ~f:ignore)
;;

let%expect_test "iteri" =
  iteri (create_default ()) ~f:(fun i v -> printf "f %d %d\n" i v);
  [%expect {|
    f 0 0
    f 1 10
    f 2 20
    f 3 30
    f 4 40
    |}]
;;

let%expect_test "iteri_elt" =
  iteri_elt (create_default ()) ~f:(fun i elt -> printf "f %d %d\n" i (Elt.value elt));
  [%expect {|
    f 0 0
    f 1 10
    f 2 20
    f 3 30
    f 4 40
    |}]
;;

let%expect_test "foldi" =
  foldi (create_default ()) ~init:0 ~f:(fun i acc v ->
    printf "f %d %d %d\n" i acc v;
    acc + i)
  |> printf "result: %d";
  [%expect
    {|
    f 0 0 0
    f 1 0 10
    f 2 1 20
    f 3 3 30
    f 4 6 40
    result: 10
    |}]
;;

let%expect_test "foldi_elt" =
  foldi_elt (create_default ()) ~init:0 ~f:(fun i acc elt ->
    printf "f %d %d %d\n" i acc (Elt.value elt);
    acc + i)
  |> printf "result: %d";
  [%expect
    {|
    f 0 0 0
    f 1 0 10
    f 2 1 20
    f 3 3 30
    f 4 6 40
    result: 10
    |}]
;;

let%expect_test "fold_right_elt" =
  fold_right_elt (create_default ()) ~init:0 ~f:(fun elt acc ->
    printf "f %d %d\n" (Elt.value elt) acc;
    acc + 1)
  |> printf "result: %d";
  [%expect {|
    f 40 0
    f 30 1
    f 20 2
    f 10 3
    f 0 4
    result: 5
    |}]
;;

let%test _ = compare Int.compare (create ()) (create ()) = 0
let%test _ = compare Int.compare (create_default ()) (create_default ()) = 0
let%test _ = compare Int.compare (create_default ()) (create ()) > 0
let%test _ = compare Int.compare (create ()) (create_default ()) < 0
let%test _ = compare Int.compare (of_list [ 1; 2; 3 ]) (of_list [ 1; 2; 4 ]) < 0
let%test _ = compare Int.compare (of_list [ 1; 3; 2 ]) (of_list [ 1; 2; 3 ]) > 0
let%test _ = compare Int.compare (of_list [ 1; 2; 5 ]) (of_list [ 1; 2; 3; 4 ]) > 0
let%test _ = [%equal: int list] (to_list (of_list [])) []
let%test _ = [%equal: int list] (to_list (of_list [ 1; 2; 3 ])) [ 1; 2; 3 ]
let%test _ = [%equal: int array] (to_array (of_array [||])) [||]
let%test _ = [%equal: int array] (to_array (of_array [| 1; 2; 3 |])) [| 1; 2; 3 |]

let%test_unit _ =
  invariant (fun (_ : int) -> ()) (of_list []);
  invariant (fun (_ : int) -> ()) (of_list [ 1; 2; 3 ]);
  invariant (fun (_ : int) -> ()) (of_array [||]);
  invariant (fun (_ : int) -> ()) (of_array [| 1; 2; 3 |])
;;

let%expect_test _ =
  let t1 = create () in
  let t2 = create () in
  let elt = insert_first t1 15 in
  require_does_raise [%here] (fun () -> remove t2 elt);
  [%expect {| (Core__Doubly_linked.Elt_does_not_belong_to_list) |}]
;;

let%expect_test _ =
  let t1 = create () in
  let t2 = create () in
  let elt = insert_first t1 14 in
  let (_ : int Elt.t) = insert_first t2 13 in
  require_does_raise [%here] (fun () -> remove t2 elt);
  [%expect {| (Core__Doubly_linked.Elt_does_not_belong_to_list) |}]
;;

let%expect_test _ =
  let t1 = create () in
  let t2 = create () in
  let elt = insert_first t1 14 in
  require_does_raise [%here] (fun () -> next t2 elt);
  [%expect {| (Core__Doubly_linked.Elt_does_not_belong_to_list) |}]
;;

let%test_unit "mem_elt" =
  let t1 = create () in
  let a = insert_first t1 'a' in
  let b = insert_first t1 'b' in
  [%test_result: bool] (mem_elt t1 a) ~expect:true;
  [%test_result: bool] (mem_elt t1 b) ~expect:true;
  let t2 = create () in
  let b2 = insert_first t2 'b' in
  [%test_result: bool] (mem_elt t2 b2) ~expect:true;
  [%test_result: bool] (mem_elt t1 b2) ~expect:false;
  remove t1 a;
  [%test_result: bool] (mem_elt t1 a) ~expect:false;
  [%test_result: bool] (mem_elt t1 b) ~expect:true;
  remove t1 b;
  [%test_result: bool] (mem_elt t1 a) ~expect:false;
  [%test_result: bool] (mem_elt t1 b) ~expect:false
;;

let%test_module "unchecked_iter" =
  (module struct
    let b = of_list [ 0; 1; 2; 3; 4 ]
    let element b n = Option.value_exn (find_elt b ~f:(fun value -> value = n))
    let remove b n = remove b (element b n)

    let insert_after b n_find n_add =
      ignore (insert_after b (element b n_find) n_add : int Elt.t)
    ;;

    let to_list f =
      let r = ref [] in
      let b = copy b in
      unchecked_iter b ~f:(fun n ->
        r := n :: !r;
        f b n);
      List.rev !r
    ;;

    let%test _ = [%equal: int list] (to_list (fun _ _ -> ())) [ 0; 1; 2; 3; 4 ]

    let%test _ =
      [%equal: int list] (to_list (fun b x -> if x = 0 then remove b 1)) [ 0; 2; 3; 4 ]
    ;;

    let%test _ =
      [%equal: int list] (to_list (fun b x -> if x = 1 then remove b 0)) [ 0; 1; 2; 3; 4 ]
    ;;

    let%test _ =
      [%equal: int list] (to_list (fun b x -> if x = 2 then remove b 1)) [ 0; 1; 2; 3; 4 ]
    ;;

    let%test _ =
      [%equal: int list]
        (to_list (fun b x ->
           if x = 2
           then (
             remove b 4;
             remove b 3)))
        [ 0; 1; 2 ]
    ;;

    let%test _ =
      [%equal: int list]
        (to_list (fun b x -> if x = 2 then insert_after b 1 5))
        [ 0; 1; 2; 3; 4 ]
    ;;

    let%test _ =
      [%equal: int list]
        (to_list (fun b x -> if x = 2 then insert_after b 2 5))
        [ 0; 1; 2; 5; 3; 4 ]
    ;;

    let%test _ =
      [%equal: int list]
        (to_list (fun b x -> if x = 2 then insert_after b 3 5))
        [ 0; 1; 2; 3; 5; 4 ]
    ;;
  end)
;;

let%test_module "mutation during iteration" =
  (module struct
    let xs = [ 1; 2; 3 ]
    let t = of_list xs
    let e = Option.value_exn (first_elt t)

    let require_mutation_didn't_happen () =
      require_equal
        [%here]
        (module struct
          type t = int list [@@deriving equal, sexp_of]
        end)
        (to_list t)
        xs
    ;;

    let%expect_test "remove" =
      require_does_raise [%here] (fun () ->
        iter t ~f:(fun _ -> ignore (remove_first t : int option)));
      [%expect {| (Core__Doubly_linked.Attempt_to_mutate_list_during_iteration) |}];
      require_mutation_didn't_happen ();
      require_does_raise [%here] (fun () ->
        iter t ~f:(fun _ -> ignore (remove_last t : int option)));
      [%expect {| (Core__Doubly_linked.Attempt_to_mutate_list_during_iteration) |}];
      require_mutation_didn't_happen ();
      require_does_raise [%here] (fun () -> iter t ~f:(fun _ -> remove t e));
      [%expect {| (Core__Doubly_linked.Attempt_to_mutate_list_during_iteration) |}];
      require_mutation_didn't_happen ()
    ;;

    let%expect_test "insert" =
      require_does_raise [%here] (fun () ->
        iter t ~f:(fun _ -> ignore (insert_first t 4 : int Elt.t)));
      [%expect {| (Core__Doubly_linked.Attempt_to_mutate_list_during_iteration) |}];
      require_mutation_didn't_happen ();
      require_does_raise [%here] (fun () ->
        iter t ~f:(fun _ -> ignore (insert_last t 5 : int Elt.t)));
      [%expect {| (Core__Doubly_linked.Attempt_to_mutate_list_during_iteration) |}];
      require_mutation_didn't_happen ();
      require_does_raise [%here] (fun () ->
        iter t ~f:(fun _ -> ignore (insert_before t e 6 : int Elt.t)));
      [%expect {| (Core__Doubly_linked.Attempt_to_mutate_list_during_iteration) |}];
      require_mutation_didn't_happen ();
      require_does_raise [%here] (fun () ->
        iter t ~f:(fun _ -> ignore (insert_after t e 7 : int Elt.t)));
      [%expect {| (Core__Doubly_linked.Attempt_to_mutate_list_during_iteration) |}];
      require_mutation_didn't_happen ()
    ;;
  end)
;;
