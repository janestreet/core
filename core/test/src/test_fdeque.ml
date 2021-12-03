open! Core
open! Import
open! Fdeque

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
