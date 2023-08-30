open! Core
open! Import
open! Fdeque

let%expect_test _ =
  let show q =
    invariant (ignore : int -> unit) q;
    let list = to_list q in
    print_s [%sexp (list : int list)];
    require_equal [%here] (module Int) (length q) (List.length list);
    require_equal [%here] (module Bool) (is_empty q) (List.is_empty list)
  in
  let q0 = of_list [ 1; 2; 3; 4; 5 ] in
  show q0;
  [%expect {| (1 2 3 4 5) |}];
  let _, q1 = dequeue_front_exn q0 in
  show q1;
  [%expect {| (2 3 4 5) |}];
  let _, q2 = dequeue_front_exn q1 in
  show q2;
  [%expect {| (3 4 5) |}];
  let q3 = enqueue_back q2 0 in
  show q3;
  [%expect {| (3 4 5 0) |}];
  require_equal [%here] (module Int) (peek_front_exn q3) 3;
  require_equal [%here] (module Int) (peek_back_exn q3) 0;
  require_equal
    [%here]
    (module struct
      type t = int list [@@deriving equal, sexp_of]
    end)
    (to_list (drop_front_exn q0))
    (to_list q1);
  require [%here] (not (is_empty q3));
  require [%here] (is_empty (Fn.apply_n_times drop_front_exn ~n:4 q3));
  require_equal
    [%here]
    (module struct
      type t = int list [@@deriving equal, sexp_of]
    end)
    (to_list (enqueue_back (enqueue_back (Fn.apply_n_times drop_front_exn ~n:4 q3) 2) 3))
    [ 2; 3 ]
;;

let%test_unit "list bisimulation" =
  let rec loop iterations q l =
    if iterations <= 0
    then ()
    else (
      let q, l =
        match Random.int 9 with
        | 0 | 1 -> enqueue_back q iterations, l @ [ iterations ]
        | 2 | 3 -> enqueue_front q iterations, [ iterations ] @ l
        | 4 ->
          (match dequeue_front q with
           | None -> q, l
           | Some (_, q) -> q, List.tl_exn l)
        | 5 ->
          (match drop_front_exn q with
           | q -> q, List.tl_exn l
           | exception _ -> q, l)
        | 6 ->
          (match dequeue_back q with
           | None -> q, l
           | Some (_, q) -> q, List.drop_last_exn l)
        | 7 ->
          (match drop_back_exn q with
           | q -> q, List.drop_last_exn l
           | exception _ -> q, l)
        | 8 -> rev q, List.rev l
        | _ -> assert false
      in
      invariant (ignore : int -> unit) q;
      [%test_result: int list] (to_list q) ~expect:l;
      loop (iterations - 1) q l)
  in
  loop 1_000 empty []
;;

let%test_unit _ =
  let open Front_to_back in
  [%test_result: int list] ~expect:[ 1; 2; 3 ] (to_list (of_list [ 1; 2; 3 ]))
;;

let%test_unit _ =
  let open Back_to_front in
  [%test_result: int list] ~expect:[ 1; 2; 3 ] (to_list (of_list [ 1; 2; 3 ]))
;;

let%test_unit _ =
  [%test_result: int list]
    ~expect:[ 1; 2; 3; 4 ]
    (bind (of_list [ [ 1; 2 ]; [ 3; 4 ] ]) ~f:of_list |> to_list)
;;

let%test_unit _ =
  [%test_result: int list]
    ~expect:[ 2; 3; 4; 5 ]
    (map (of_list [ 1; 2; 3; 4 ]) ~f:succ |> to_list)
;;

let%test_unit _ =
  let open Stable.V1 in
  [%test_result: int list]
    ~expect:[ 1; 2; 3 ]
    (to_list (t_of_sexp Int.t_of_sexp (sexp_of_t Int.sexp_of_t (of_list [ 1; 2; 3 ]))))
;;

module type Sequence_testable = sig
  val of_sequence : 'a Sequence.t -> 'a t
  val to_sequence : 'a t -> 'a Sequence.t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
end

let%test_unit "to_sequence & of_sequence round-trip" =
  let example = Front_to_back.of_list [ 1; 2; 3; 4 ] in
  let property (module M : Sequence_testable) =
    [%test_result: int t] ~expect:example (M.of_sequence (M.to_sequence example))
  in
  property (module Front_to_back);
  property (module Back_to_front)
;;

let%test_unit "{to,of}_sequence & {to,of}_list agree" =
  let example = [ 1; 2; 3; 4 ] in
  let property (module M : Sequence_testable) =
    [%test_result: int t]
      ~expect:(M.of_list example)
      (M.of_sequence (Sequence.of_list example));
    let example = M.of_list example in
    [%test_result: int list]
      ~expect:(M.to_list example)
      (Sequence.to_list (M.to_sequence example))
  in
  property (module Front_to_back);
  property (module Back_to_front)
;;

let%test_unit "Arbitrary_order doesn't drop elements" =
  let example = [ 1; 2; 3; 4 ] in
  let expect =
    List.fold example ~init:Int.Map.empty ~f:(fun acc key ->
      Map.update acc key ~f:(fun existing -> 1 + Option.value ~default:0 existing))
  in
  let arbitrary_order_elements =
    example
    |> of_list
    |> Arbitrary_order.to_sequence
    |> Sequence.fold ~init:Int.Map.empty ~f:(fun acc key ->
         Map.update acc key ~f:(fun existing -> 1 + Option.value ~default:0 existing))
  in
  [%test_result: int Int.Map.t] ~expect arbitrary_order_elements
;;
