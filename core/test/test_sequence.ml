open! Core
open Expect_test_helpers_core

module No_poly_compare = struct
  module Incomparable : sig
    type t [@@deriving compare]

    val create : unit -> t
  end = struct
    type t = unit -> unit

    let compare _ _ = 0
    let next = ref 0

    let create () =
      incr next;
      let x = !next in
      fun () -> failwithf "Should never be called %i" x ()
    ;;
  end

  type 'a t =
    { incomparable : Incomparable.t
    ; value : 'a
    }
  [@@deriving compare]

  let sexp_of_t sexp_of_a t = sexp_of_a t.value
  let create value = { incomparable = Incomparable.create (); value }
end

let%expect_test "No_poly_compare" =
  let inc_x = No_poly_compare.Incomparable.create () in
  (* Unfortunately if we compare [x] with itself it will always work. *)
  show_raise (fun () -> Poly.compare inc_x inc_x);
  [%expect {| "did not raise" |}];
  (* But otherwise we get an exception. *)
  show_raise (fun () -> Poly.compare inc_x (No_poly_compare.Incomparable.create ()));
  [%expect {| (raised (Invalid_argument "compare: functional value")) |}];
  let no_x = No_poly_compare.create 0 in
  show_raise (fun () -> Poly.compare no_x no_x);
  [%expect {| "did not raise" |}];
  show_raise (fun () -> Poly.compare no_x (No_poly_compare.create 0));
  [%expect {| (raised (Invalid_argument "compare: functional value")) |}]
;;

let%expect_test "merge_all" =
  let merge_all = Sequence.merge_all (module Fheap) in
  let compare = No_poly_compare.compare Int.compare in
  (* We take up to 20 elements so we can test infinite sequences. *)
  let list_of_sequence sequence = Sequence.to_list (Sequence.take sequence 20) in
  let sexp_of_sequence sequence =
    [%sexp (list_of_sequence sequence : int No_poly_compare.t list)]
  in
  (* Avoid unnecessary line wrapping of small sexps. *)
  let print_s sexp =
    let string = Sexp.to_string sexp in
    if String.length string < 80 then print_endline string else print_s sexp
  in
  let examples =
    let finite_examples =
      [ []
      ; [ [] ]
      ; [ [ 1 ] ]
      ; [ [ 1; 2 ] ]
      ; [ [ 1 ]; [ 2 ] ]
      ; [ [ 2 ]; [ 1 ] ]
      ; [ [ 1; 2; 3 ] ]
      ; [ [ 1 ]; [ 2 ]; [ 3 ] ]
      ; [ [ 3 ]; [ 2 ]; [ 1 ] ]
      ; [ [ 1; 2 ]; [ 3; 4 ] ]
      ; [ [ 2; 4 ]; [ 1; 3 ] ]
      ; [ [ 1; 4 ]; [ 3 ]; []; [ 2 ] ]
      ]
      |> List.map ~f:(List.map ~f:Sequence.of_list)
    in
    let infinite_examples =
      let naturals = Sequence.unfold ~init:0 ~f:(fun i -> Some (i, i + 1)) in
      [ [ Sequence.cycle_list_exn [ 1 ]; Sequence.cycle_list_exn [ 2 ] ]
      ; [ naturals ]
      ; [ Sequence.filter naturals ~f:(fun i -> i % 2 = 0)
        ; Sequence.filter naturals ~f:(fun i -> i % 2 = 1)
        ]
      ]
    in
    finite_examples @ infinite_examples
    |> List.map ~f:(List.map ~f:(Sequence.map ~f:No_poly_compare.create))
  in
  List.iter examples ~f:(fun example -> print_s [%sexp (example : sequence list)]);
  [%expect
    {|
    ()
    (())
    ((1))
    ((1 2))
    ((1)(2))
    ((2)(1))
    ((1 2 3))
    ((1)(2)(3))
    ((3)(2)(1))
    ((1 2)(3 4))
    ((2 4)(1 3))
    ((1 4)(3)()(2))
    ((1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
     (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2))
    ((0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))
    ((0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38)
     (1 3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39))
    |}];
  List.iter examples ~f:(fun example ->
    print_s [%sexp (merge_all example ~compare : sequence)]);
  [%expect
    {|
    ()
    ()
    (1)
    (1 2)
    (1 2)
    (1 2)
    (1 2 3)
    (1 2 3)
    (1 2 3)
    (1 2 3 4)
    (1 2 3 4)
    (1 2 3 4)
    (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
    (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
    (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
    |}];
  (* During a Quickcheck test we want to bail out after a single failure. *)
  let require_exn here ?if_false_then_print_s bool =
    require here bool ?if_false_then_print_s;
    if not bool then raise_s [%message "failed on random input"]
  in
  let quickcheck_generator =
    let str_gen =
      Quickcheck.Generator.map Int.quickcheck_generator ~f:No_poly_compare.create
    in
    let seq_gen =
      Quickcheck.Generator.map (List.quickcheck_generator str_gen) ~f:(fun list ->
        Sequence.of_list (List.sort list ~compare))
    in
    List.quickcheck_generator seq_gen
  in
  let run test =
    Quickcheck.test
      ~sexp_of:[%sexp_of: sequence list]
      ~examples
      quickcheck_generator
      ~f:test
  in
  (* Test that output is sorted. *)
  run (fun seqs ->
    let seq = merge_all seqs ~compare in
    let list = list_of_sequence seq in
    require_exn
      [%here]
      (List.is_sorted list ~compare)
      ~if_false_then_print_s:(lazy [%sexp (list : int No_poly_compare.t list)]));
  [%expect {| |}];
  (* Test that output is consistent with concat+sort. *)
  run (fun seqs ->
    let merge_all = list_of_sequence (merge_all seqs ~compare) in
    let concat_and_sort =
      let sorted =
        List.map seqs ~f:list_of_sequence |> List.concat |> List.sort ~compare
      in
      List.take sorted 20
    in
    require_exn
      [%here]
      ([%compare.equal: int No_poly_compare.t list] merge_all concat_and_sort)
      ~if_false_then_print_s:
        (lazy
          [%message
            "inconsistent results"
              (merge_all : int No_poly_compare.t list)
              (concat_and_sort : int No_poly_compare.t list)]));
  [%expect {| |}];
  (* Test that sequence is replayable. *)
  run (fun seqs ->
    let seq = merge_all seqs ~compare in
    let list1 = list_of_sequence seq in
    let list2 = list_of_sequence seq in
    require_exn
      [%here]
      ([%compare.equal: int No_poly_compare.t list] list1 list2)
      ~if_false_then_print_s:
        (lazy
          [%message
            "sequence is impure"
              (list1 : int No_poly_compare.t list)
              (list2 : int No_poly_compare.t list)]));
  [%expect {| |}]
;;
