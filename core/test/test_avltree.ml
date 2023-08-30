open! Core
open Expect_test_helpers_core

module Int_option = struct
  type t = int option [@@deriving equal, sexp_of]
end

let invariant tree =
  Avltree.invariant tree ~compare;
  Avltree.iter tree ~f:(fun ~key ~data -> assert (key = data))
;;

let require_is_absent tree int = require [%here] (not (Avltree.mem tree ~compare int))

let require_is_present tree int =
  require [%here] (Avltree.mem tree ~compare int);
  require_equal [%here] (module Int_option) (Avltree.find tree ~compare int) (Some int)
;;

let require_equivalent_set tree set =
  let from_iter = ref Int.Set.empty in
  Avltree.iter tree ~f:(fun ~key:int ~data:_ -> from_iter := Set.add !from_iter int);
  let from_fold =
    Avltree.fold tree ~init:Int.Set.empty ~f:(fun ~key:int ~data:_ set -> Set.add set int)
  in
  require_sets_are_equal [%here] !from_iter from_fold;
  require_sets_are_equal [%here] !from_iter set
;;

let require_ref_mutated f ~to_:expect =
  let r = ref (not expect) in
  let return = f r in
  require_equal [%here] (module Bool) !r expect;
  return
;;

module Operation = struct
  type t =
    | Add
    | Add_if_not_exists
    | Remove
  [@@deriving equal, quickcheck, sexp_of]

  let perform t int avltree set =
    let is_present = Avltree.mem avltree ~compare int in
    let avltree, set =
      match t with
      | Add ->
        let avltree =
          require_ref_mutated ~to_:(not is_present) (fun added ->
            Avltree.add avltree ~replace:true ~compare ~added ~key:int ~data:int)
        in
        require_is_present avltree int;
        let set = Set.add set int in
        avltree, set
      | Add_if_not_exists ->
        let avltree =
          require_ref_mutated ~to_:(not is_present) (fun added ->
            (* if buggy, this will replace the data with the wrong value *)
            Avltree.add
              avltree
              ~replace:false
              ~compare
              ~added
              ~key:int
              ~data:(if is_present then int + 1 else int))
        in
        require_is_present avltree int;
        let set = Set.add set int in
        avltree, set
      | Remove ->
        let avltree =
          require_ref_mutated ~to_:is_present (fun removed ->
            Avltree.remove avltree ~removed ~compare int)
        in
        require_is_absent avltree int;
        let set = Set.remove set int in
        avltree, set
    in
    invariant avltree;
    require_equivalent_set avltree set
  ;;
end

let operation_printed_crs = ref false

let () =
  let old = !on_print_cr in
  on_print_cr
    := fun cr ->
         operation_printed_crs := true;
         old cr
;;

module Operation_sequence = struct
  module Data = struct
    type t = int [@@deriving equal, quickcheck, sexp_of]

    let quickcheck_generator = Base_quickcheck.Generator.small_positive_or_zero_int
  end

  type t = (Operation.t * Data.t) list [@@deriving equal, quickcheck, sexp_of]
end

let size = 1000
let data_sorted = List.init size ~f:Fn.id
let data_reverse_sorted = data_sorted |> List.rev

let add_then_remove_sorted =
  let%bind.List operation = [ Operation.Add; Remove ] in
  let%map.List int = data_sorted in
  operation, int
;;

let add_then_remove_reverse_sorted =
  let%bind.List operation = [ Operation.Add; Remove ] in
  let%map.List int = data_reverse_sorted in
  operation, int
;;

let%expect_test "random operations" =
  quickcheck_m
    [%here]
    (module Operation_sequence)
    ~examples:[ add_then_remove_sorted; add_then_remove_reverse_sorted ]
    ~f:(fun operations ->
      List.fold_until
        operations
        ~init:(Avltree.empty, Int.Set.empty)
        ~f:(fun (t, s) (operation, int) ->
          operation_printed_crs := false;
          Operation.perform operation int t s;
          if !operation_printed_crs then Stop () else Continue (t, s))
        ~finish:(ignore : (int, int) Avltree.t * Int.Set.t -> unit));
  [%expect {| |}]
;;
