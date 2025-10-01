open Core
open Nonempty_list

include (
  Base_test_helpers.Test_container.Test_S1_allow_skipping_tests (struct
    include Nonempty_list

    let of_list l = if List.is_empty l then `Skip_test else `Ok (of_list_exn l)
  end) :
  sig end)

(* Note that at least one expect test must be included for the above functor's tests to
   run.
*)

let t : _ Nonempty_list.t = [ 1; 2; 3; 4 ]

let%expect_test "sexp representations" =
  print_s [%sexp (t : int Nonempty_list.t)];
  [%expect {| (1 2 3 4) |}];
  print_s [%sexp (t : int Nonempty_list.Stable.V2.t)];
  [%expect {| (1 2 3 4) |}];
  print_s [%sexp (t : int Nonempty_list.Stable.V1.t)];
  [%expect {| (1 2 3 4) |}]
;;

let%quick_test "reduce" =
  fun (t : int t) ->
  [%test_result: int]
    (reduce t ~f:( + ))
    ~expect:(List.reduce (to_list t) ~f:( + ) |> Core.Option.value_exn)
;;

let%quick_test "reverse" =
  fun (t : int t) ->
  [%test_result: int list] (reverse t |> to_list) ~expect:(List.rev (to_list t))
;;

let%quick_test "append" =
  fun (t : int t) (l : int list) ->
  [%test_result: int list] (append t l |> to_list) ~expect:(List.append (to_list t) l)
;;

let%quick_test "append'" =
  fun (t : int t) (l : int list) ->
  [%test_result: int list] (append' l t |> to_list) ~expect:(List.append l (to_list t))
;;

let%quick_test "@" =
  fun (t1 : int t) (t2 : int t) ->
  [%test_result: int list]
    (to_list (t1 @ t2))
    ~expect:(List.Infix.( @ ) (to_list t1) (to_list t2))
;;

let%quick_test "unzip" =
  fun (l : (int * char) t) ->
  [%test_result: int list * char list]
    (unzip l |> Tuple2.map_both ~f1:to_list ~f2:to_list)
    ~expect:(List.unzip (to_list l))
;;

let%quick_test "iter" =
  fun (l : int t) ->
  let output = ref [] in
  let f x = output := x :: !output in
  Nonempty_list.iter ~f l;
  let actual = !output in
  output := [];
  List.iter ~f (to_list l);
  let expected = !output in
  [%test_result: int list] actual ~expect:expected
;;

let%quick_test "return" =
  fun (i : int) -> [%test_result: int list] (return i |> to_list) ~expect:(List.return i)
;;

let%quick_test "bind" =
  fun (l : int t) (f : int -> int t) ->
  [%test_result: int list]
    (bind l ~f |> to_list)
    ~expect:(List.bind (to_list l) ~f:(fun i -> to_list (f i)))
;;

let map_or_unequal_lengths (or_unequal_lengths : _ List.Or_unequal_lengths.t) ~f
  : _ List.Or_unequal_lengths.t
  =
  match or_unequal_lengths with
  | Ok l -> (Ok (f l) : _ List.Or_unequal_lengths.t)
  | Unequal_lengths -> Unequal_lengths
;;

let%quick_test "map2" =
  fun (t1 : int t) (t2 : int t) (f : int -> int -> int) ->
  [%test_result: int list List.Or_unequal_lengths.t]
    (map2 t1 t2 ~f |> map_or_unequal_lengths ~f:to_list)
    ~expect:(List.map2 (to_list t1) (to_list t2) ~f)
;;

let%quick_test "map2_exn" =
  fun (t1 : int t) (t2 : int t) (f : int -> int -> int) ->
  [%test_result: int list Or_error.t]
    (Or_error.try_with (fun () -> map2_exn t1 t2 ~f) |> Or_error.map ~f:to_list)
    ~expect:(Or_error.try_with (fun () -> List.map2_exn (to_list t1) (to_list t2) ~f))
;;

let%quick_test "zip" =
  fun (a : int t) (b : int t) ->
  [%test_result: (int * int) list List.Or_unequal_lengths.t]
    (zip a b |> map_or_unequal_lengths ~f:to_list)
    ~expect:(List.zip (to_list a) (to_list b))
;;

let%quick_test "zip_exn" =
  fun (a : int t) (b : int t) ->
  [%test_result: (int * int) list Or_error.t]
    (Or_error.try_with (fun () -> zip_exn a b) |> Or_error.map ~f:to_list)
    ~expect:(Or_error.try_with (fun () -> List.zip_exn (to_list a) (to_list b)))
;;

let%quick_test "zip3" =
  fun (a : int t) (b : int t) (c : int t) ->
  [%test_result: (int * int * int) list List.Or_unequal_lengths.t]
    (zip3 a b c |> map_or_unequal_lengths ~f:to_list)
    ~expect:(List.zip3 (to_list a) (to_list b) (to_list c))
;;

let%quick_test "zip3_exn" =
  fun (a : int t) (b : int t) (c : int t) ->
  [%test_result: (int * int * int) list option]
    (Core.Option.try_with (fun () -> zip3_exn a b c) |> Core.Option.map ~f:to_list)
    ~expect:
      (Core.Option.try_with (fun () -> List.zip3_exn (to_list a) (to_list b) (to_list c)))
;;

let%quick_test "filter" =
  fun (t : int t) (f : int -> bool) ->
  [%test_result: int list] (filter t ~f) ~expect:(List.filter (to_list t) ~f)
;;

let%quick_test "filteri" =
  fun (t : int t) (f : int -> int -> bool) ->
  [%test_result: int list] (filteri t ~f) ~expect:(List.filteri (to_list t) ~f)
;;

let%quick_test "filter_map" =
  fun (t : int t) (f : int -> int option) ->
  [%test_result: int list] (filter_map t ~f) ~expect:(List.filter_map (to_list t) ~f)
;;

let%quick_test "filter_mapi" =
  fun (t : int t) (f : int -> int -> int option) ->
  [%test_result: int list] (filter_mapi t ~f) ~expect:(List.filter_mapi (to_list t) ~f)
;;

let%quick_test "concat" =
  fun (lists : int t t) ->
  [%test_result: int list]
    (concat lists |> to_list)
    ~expect:(List.concat (to_list lists |> List.map ~f:to_list))
;;

let%quick_test "nth" =
  fun (t : int t) (n : int) ->
  [%test_result: int option] (nth t n) ~expect:(List.nth (to_list t) n)
;;

let%quick_test "nth_exn" =
  fun (t : int t) (n : int) ->
  [%test_result: int option]
    (Core.Option.try_with (fun () -> nth_exn t n))
    ~expect:(Core.Option.try_with (fun () -> List.nth_exn (to_list t) n))
;;

let%quick_test "last" =
  fun (l : int t) ->
  [%test_result: int] (last l) ~expect:(List.last (to_list l) |> Core.Option.value_exn)
;;

let%quick_test "drop_last" =
  fun (l : int t) ->
  [%test_result: int list]
    (drop_last l)
    ~expect:(List.drop_last (to_list l) |> Core.Option.value_exn)
;;

let%quick_test "to_sequence" =
  fun (t : int t) ->
  [%test_result: int list] (to_sequence t |> Sequence.to_list) ~expect:(to_list t)
;;

let%quick_test "sort" =
  fun (t : int t) ->
  [%test_result: int list]
    (sort ~compare:Int.compare t |> to_list)
    ~expect:(List.sort ~compare:Int.compare (to_list t))
;;

(* It's a bit tricky to show that this test is doing exactly the right thing. But if you
   [List.permute] the list given to, say, [List.stable_sort], it does fail as expected.
*)
let%quick_test "stable_sort" =
  fun (t : (int * bool) t) ->
  let compare = [%compare: int * _] in
  [%test_result: (int * bool) list]
    (stable_sort ~compare t |> to_list)
    ~expect:(List.stable_sort ~compare (to_list t))
;;

let%quick_test "stable_dedup" =
  fun (t : int t) ->
  [%test_result: int list]
    (stable_dedup ~compare:Int.compare t |> to_list)
    ~expect:(List.stable_dedup ~compare:Int.compare (to_list t))
;;

let%quick_test "dedup_and_sort" =
  fun (t : int t) ->
  [%test_result: int list]
    (dedup_and_sort ~compare:Int.compare t |> to_list)
    ~expect:(List.dedup_and_sort ~compare:Int.compare (to_list t))
;;

let%quick_test "sort_and_group" =
  fun (t : int t) ->
  [%test_result: int list list]
    (sort_and_group ~compare:Int.compare t |> to_list |> List.map ~f:to_list)
    ~expect:(List.sort_and_group ~compare:Int.compare (to_list t))
;;

let%quick_test "group" =
  fun (t : int t) (break : int -> int -> bool) ->
  [%test_result: int list list]
    (group t ~break |> to_list |> List.map ~f:to_list)
    ~expect:(List.group (to_list t) ~break)
;;

let%quick_test "all_equal" =
  fun (l : int t) ->
  let equal = [%equal: int] in
  [%test_result: int option]
    (all_equal ~equal l)
    ~expect:(List.all_equal ~equal (to_list l))
;;

let%quick_test "min_elt'" =
  fun (l : int t) ->
  [%test_result: int option]
    (Some (min_elt' ~compare:Int.compare l))
    ~expect:(List.min_elt (to_list l) ~compare:Int.compare)
;;

let%quick_test "max_elt'" =
  fun (l : int t) ->
  [%test_result: int option]
    (Some (max_elt' ~compare:Int.compare l))
    ~expect:(List.max_elt (to_list l) ~compare:Int.compare)
;;

let%quick_test "map_add_multi" =
  fun (l : (int * int) list) (new_key : int) (new_data : int) ->
  let map = map_of_alist_multi_rev l ~comparator:(module Int) in
  [%test_result: int list Int.Map.t]
    (map_add_multi map ~key:new_key ~data:new_data |> Map.map ~f:to_list)
    ~expect:(Map.add_multi (Map.map map ~f:to_list) ~key:new_key ~data:new_data)
;;

let%quick_test "hashtbl_add_multi" =
  fun (l : (int * int) list) (new_key : int) (new_data : int) ->
  let make_hashtbl () =
    Hashtbl.of_alist_multi (module Int) l |> Hashtbl.map ~f:of_list_exn
  in
  [%test_result: (int * int list) list]
    (let hashtbl = make_hashtbl () in
     hashtbl_add_multi hashtbl ~key:new_key ~data:new_data;
     Hashtbl.map hashtbl ~f:to_list |> Hashtbl.to_alist)
    ~expect:
      (let hashtbl = make_hashtbl () |> Hashtbl.map ~f:to_list in
       Hashtbl.add_multi hashtbl ~key:new_key ~data:new_data;
       Hashtbl.to_alist hashtbl)
;;

let%quick_test "map_of_alist_multi_rev" =
  fun (alist : (int * int) list) ->
  [%test_result: int list Int.Map.t]
    (map_of_alist_multi_rev alist ~comparator:(module Int) |> Map.map ~f:to_list)
    ~expect:(Map.of_alist_multi (module Int) alist |> Map.map ~f:List.rev)
;;

let%quick_test "map_of_alist_multi" =
  fun (alist : (int * int) list) ->
  [%test_result: int list Int.Map.t]
    (map_of_alist_multi alist ~comparator:(module Int) |> Map.map ~f:to_list)
    ~expect:(Map.of_alist_multi (module Int) alist)
;;

let%quick_test "map_of_list_with_key_multi_rev" =
  fun (alist : Date.t list) ->
  let get_key = Date.year in
  [%test_result: Date.t list Int.Map.t]
    (map_of_list_with_key_multi_rev alist ~comparator:(module Int) ~get_key
     |> Map.map ~f:to_list)
    ~expect:(Map.of_list_with_key_multi (module Int) alist ~get_key |> Map.map ~f:List.rev)
;;

let%quick_test "map_of_list_with_key_multi" =
  fun (alist : Date.t list) ->
  let get_key = Date.year in
  [%test_result: Date.t list Int.Map.t]
    (map_of_list_with_key_multi alist ~comparator:(module Int) ~get_key
     |> Map.map ~f:to_list)
    ~expect:(Map.of_list_with_key_multi (module Int) alist ~get_key)
;;

let%quick_test "map_of_sequence_multi_rev" =
  fun (alist : (int * int) list) ->
  let seq = Sequence.of_list alist in
  [%test_result: int list Int.Map.t]
    (map_of_sequence_multi_rev seq ~comparator:(module Int) |> Map.map ~f:to_list)
    ~expect:(Map.of_sequence_multi (module Int) seq |> Map.map ~f:List.rev)
;;

let%quick_test "map_of_sequence_multi" =
  fun (alist : (int * int) list) ->
  let seq = Sequence.of_list alist in
  [%test_result: int list Int.Map.t]
    (map_of_sequence_multi seq ~comparator:(module Int) |> Map.map ~f:to_list)
    ~expect:(Map.of_sequence_multi (module Int) seq)
;;

let%quick_test "combine_errors" =
  fun (rs : (int, int) Result.t t) ->
  [%test_result: (int list, int list) Result.t]
    (match combine_errors rs with
     | Ok t -> Ok (to_list t)
     | Error t -> Error (to_list t))
    ~expect:(Result.combine_errors (to_list rs))
;;

let%quick_test "combine_errors_unit" =
  fun (rs : (unit, int) Result.t t) ->
  [%test_result: (unit, int list) Result.t]
    (match combine_errors_unit rs with
     | Ok () -> Ok ()
     | Error t -> Error (to_list t))
    ~expect:(Result.combine_errors_unit (to_list rs))
;;

let%quick_test "combine_or_errors" =
  fun (oes : int Or_error.t t) ->
  [%test_result: int list Or_error.t]
    (combine_or_errors oes |> Result.map ~f:to_list)
    ~expect:(Or_error.combine_errors (to_list oes))
;;

let%quick_test "combine_or_errors_unit" =
  fun (oes : unit Or_error.t t) ->
  [%test_result: unit Or_error.t]
    (combine_or_errors_unit oes)
    ~expect:(Or_error.combine_errors_unit (to_list oes))
;;

let%quick_test "filter_ok_at_least_one" =
  fun (oes : int Or_error.t t) ->
  [%test_result: int list Or_error.t]
    (filter_ok_at_least_one oes |> Or_error.map ~f:to_list)
    ~expect:(Or_error.filter_ok_at_least_one (to_list oes))
;;

let%quick_test "option_all" =
  fun (os : int option t) ->
  let expected =
    let list_os = to_list os in
    if List.for_all list_os ~f:Core.Option.is_some
    then Some (List.filter_map list_os ~f:Fn.id)
    else None
  in
  [%test_result: int list option]
    (option_all os |> Core.Option.map ~f:to_list)
    ~expect:expected
;;

let%expect_test "stable types" =
  let test sexp_of_t t_of_sexp =
    let sexp = sexp_of_t sexp_of_int t in
    print_s sexp;
    print_s ([%sexp_of: bool] ([%equal: int t] t (t_of_sexp int_of_sexp sexp)))
  in
  test Stable.V1.sexp_of_t Stable.V1.t_of_sexp;
  test Stable.V2.sexp_of_t Stable.V2.t_of_sexp;
  test Stable.V3.sexp_of_t Stable.V3.t_of_sexp;
  test sexp_of_t t_of_sexp;
  [%expect
    {|
    (1 2 3 4)
    true
    (1 2 3 4)
    true
    (1 2 3 4)
    true
    (1 2 3 4)
    true
    |}];
  let test bin_writer_t bin_read_t =
    let bytes = Bin_prot.Writer.to_bytes (bin_writer_t Int.bin_writer_t) t in
    bytes |> [%sexp_of: bytes] |> print_s;
    let buf = Bin_prot.Common.create_buf (Bytes.length bytes) in
    Bin_prot.Common.blit_bytes_buf bytes buf ~len:(Bytes.length bytes);
    let t' = bin_read_t Int.bin_read_t buf ~pos_ref:(ref 0) in
    print_s ([%sexp_of: bool] ([%equal: int t] t t'))
  in
  test Stable.V1.bin_writer_t Stable.V1.bin_read_t;
  test Stable.V2.bin_writer_t Stable.V2.bin_read_t;
  test Stable.V3.bin_writer_t Stable.V3.bin_read_t;
  test Unstable.bin_writer_t Unstable.bin_read_t;
  [%expect
    {|
    "\001\003\002\003\004"
    true
    "\001\003\002\003\004"
    true
    "\004\001\002\003\004"
    true
    "\004\001\002\003\004"
    true
    |}]
;;

let%quick_test "fold" =
  fun (t : int t) (init : int) (f : int -> int -> int) ->
  [%test_result: int] (fold t ~init ~f) ~expect:(List.fold (to_list t) ~init ~f)
;;

let%quick_test "fold_right" =
  fun (t : int t) (init : int) (f : int -> int -> int) ->
  [%test_result: int]
    (fold_right t ~init ~f)
    ~expect:(List.fold_right (to_list t) ~init ~f)
;;

let%quick_test "reduce" =
  fun (t : int t) (f : int -> int -> int) ->
  [%test_result: int option] (Some (reduce t ~f)) ~expect:(List.reduce (to_list t) ~f)
;;

let%quick_test "folding_map" =
  fun (t : int t) (init : int) (f : int -> int -> int * int) ->
  [%test_result: int list]
    (folding_map t ~init ~f |> to_list)
    ~expect:(List.folding_map (to_list t) ~init ~f)
;;

let%quick_test "fold_map" =
  fun (t : int t) (init : int) (f : int -> int -> int * int) ->
  [%test_result: int * int list]
    (let acc, result = fold_map t ~init ~f in
     acc, to_list result)
    ~expect:(List.fold_map (to_list t) ~init ~f)
;;

let%quick_test "mapi" =
  fun (t : string t) (f : int -> string -> int * string) ->
  [%test_result: (int * string) list]
    (mapi t ~f |> to_list)
    ~expect:(List.mapi (to_list t) ~f)
;;

let%quick_test "transpose" =
  fun (t : int t t) ->
  [%test_result: int list list option]
    (transpose t
     |> Core.Option.map ~f:(fun result -> to_list result |> List.map ~f:to_list))
    ~expect:(List.transpose (to_list t |> List.map ~f:to_list))
;;

let%quick_test "transpose_exn" =
  fun (t : int t t) ->
  [%test_result: int list list option]
    (Core.Option.try_with (fun () -> transpose_exn t |> to_list |> List.map ~f:to_list))
    ~expect:
      (Core.Option.try_with (fun () ->
         List.transpose_exn (to_list t |> List.map ~f:to_list)))
;;

let%quick_test "init" =
  fun (n : (int[@generator Int.gen_incl (-10) 10])) (f : int -> int) ->
  (* [n=0] is the one case we expect to differ *)
  if n <> 0
  then
    [%test_result: int list option]
      (Core.Option.try_with (fun () -> init n ~f) |> Core.Option.map ~f:to_list)
      ~expect:(Core.Option.try_with (fun () -> List.init n ~f))
;;

let%quick_test "iteri" =
  fun (l : int t) ->
  let output = ref [] in
  let f i x = output := (i, x) :: !output in
  Nonempty_list.iteri ~f l;
  let actual = !output in
  output := [];
  List.iteri ~f (to_list l);
  let expected = !output in
  [%test_result: (int * int) list] actual ~expect:expected
;;

let%quick_test "findi" =
  fun (t : int t) (f : int -> int -> bool) ->
  [%test_result: (int * int) option] (findi t ~f) ~expect:(List.findi (to_list t) ~f)
;;

let%quick_test "findi_exn" =
  fun (t : int t) (f : int -> int -> bool) ->
  [%test_result: (int * int) option]
    (Core.Option.try_with (fun () -> findi_exn t ~f))
    ~expect:(Core.Option.try_with (fun () -> List.findi_exn (to_list t) ~f))
;;

let%quick_test "find_mapi" =
  fun (t : int t) (f : int -> int -> int option) ->
  [%test_result: int option] (find_mapi t ~f) ~expect:(List.find_mapi (to_list t) ~f)
;;

let%quick_test "counti" =
  fun (t : int t) (f : int -> int -> bool) ->
  [%test_result: int] (counti t ~f) ~expect:(List.counti (to_list t) ~f)
;;

let%quick_test "for_alli" =
  fun (t : int t) (f : int -> int -> bool) ->
  [%test_result: bool] (for_alli t ~f) ~expect:(List.for_alli (to_list t) ~f)
;;

let%quick_test "existsi" =
  fun (t : int t) (f : int -> int -> bool) ->
  [%test_result: bool] (existsi t ~f) ~expect:(List.existsi (to_list t) ~f)
;;

let%quick_test "foldi" =
  fun (t : int t) (init : int list) (f : int -> int list -> int -> int list) ->
  [%test_result: int list] (foldi t ~init ~f) ~expect:(List.foldi (to_list t) ~init ~f)
;;

(* Since the behavior of [Option] functions differs fundamentally between empty and
   non-empty lists, explicitly ensure that we test both cases in addition to whatever
   tests quickcheck generates (even though it's extremely likely that the quickcheck tests
   do contain empty lists. *)
let run_on_empty_and_nonempty_lists f =
  Quickcheck.test [%quickcheck.generator: int list] ~examples:[ []; [ 1 ] ] ~f
;;

let%expect_test "Option" =
  run_on_empty_and_nonempty_lists (fun l ->
    match%optional.Nonempty_list.Option l with
    | None -> assert (List.is_empty l)
    | Some nonempty ->
      assert ([%equal: int Nonempty_list.t] nonempty (Nonempty_list.of_list_exn l)))
;;

let%expect_test "Option does not allocate" =
  run_on_empty_and_nonempty_lists (fun l ->
    let round_tripped =
      Expect_test_helpers_core.require_no_allocation (fun () ->
        match%optional.Nonempty_list.Option l with
        | None -> Sys.opaque_identity Nonempty_list.Option.none
        | Some nonempty ->
          Sys.opaque_identity (Nonempty_list.Option.some (Sys.opaque_identity nonempty)))
    in
    assert (phys_equal l round_tripped))
;;

let%quick_test "remove_consecutive_duplicates" =
  fun (t : int t) (which_to_keep : [ `First | `Last ]) ->
  [%test_result: int list]
    (remove_consecutive_duplicates t ~which_to_keep ~equal:[%equal: int] |> to_list)
    ~expect:
      (List.remove_consecutive_duplicates (to_list t) ~which_to_keep ~equal:[%equal: int])
;;

module%test Partition = struct
  let%expect_test "partition_tf" =
    let test xs =
      let f x = x % 2 = 0 in
      let partition = Nonempty_list.partition_tf xs ~f in
      partition |> [%sexp_of: (int, int) Partition.t] |> print_s;
      assert (
        [%equal: int list * int list]
          ( Partition.fst partition |> Core.Option.value_map ~default:[] ~f:to_list
          , Partition.snd partition |> Core.Option.value_map ~default:[] ~f:to_list )
          (Nonempty_list.partition_tf' xs ~f))
    in
    test t;
    [%expect {| (Both ((2 4) (1 3))) |}];
    test [ 1; 3 ];
    [%expect {| (Snd (1 3)) |}];
    test [ 2; 4 ];
    [%expect {| (Fst (2 4)) |}]
  ;;

  let%expect_test "partition_map" =
    let test xs =
      let f x : _ Either.t =
        if x % 2 = 0 then First x else Second [%string "odd %{x#Int}"]
      in
      let partition = Nonempty_list.partition_map xs ~f in
      partition |> [%sexp_of: (int, string) Partition.t] |> print_s;
      assert (
        [%equal: int list * string list]
          ( Partition.fst partition |> Core.Option.value_map ~default:[] ~f:to_list
          , Partition.snd partition |> Core.Option.value_map ~default:[] ~f:to_list )
          (Nonempty_list.partition_map' xs ~f))
    in
    test t;
    [%expect {| (Both ((2 4) ("odd 1" "odd 3"))) |}];
    test [ 1; 3 ];
    [%expect {| (Snd ("odd 1" "odd 3")) |}];
    test [ 2; 4 ];
    [%expect {| (Fst (2 4)) |}]
  ;;

  let%expect_test "partition_result" =
    let test xs =
      let xs =
        Nonempty_list.map xs ~f:(fun x ->
          if x % 2 = 0 then Ok x else Error [%string "odd %{x#Int}"])
      in
      let partition = Nonempty_list.partition_result xs in
      partition |> [%sexp_of: (int, string) Partition.t] |> print_s;
      assert (
        [%equal: int list * string list]
          ( Partition.fst partition |> Core.Option.value_map ~default:[] ~f:to_list
          , Partition.snd partition |> Core.Option.value_map ~default:[] ~f:to_list )
          (Nonempty_list.partition_result' xs))
    in
    test t;
    [%expect {| (Both ((2 4) ("odd 1" "odd 3"))) |}];
    test [ 1; 3 ];
    [%expect {| (Snd ("odd 1" "odd 3")) |}];
    test [ 2; 4 ];
    [%expect {| (Fst (2 4)) |}]
  ;;
end

module%test Partition3 = struct
  let f x =
    match x % 3 with
    | 0 -> `Fst x
    | 1 -> `Snd (Int.to_float x)
    | 2 -> `Trd (Int.to_string x)
    | _ -> assert false
  ;;

  let convert (partition3 : _ Nonempty_list.Partition3.t) =
    match partition3 with
    | Fst t -> to_list t, [], []
    | Snd t -> [], to_list t, []
    | Trd t -> [], [], to_list t
    | Fst_snd (fsts, snds) -> to_list fsts, to_list snds, []
    | Fst_trd (fsts, trds) -> to_list fsts, [], to_list trds
    | Snd_trd (snds, trds) -> [], to_list snds, to_list trds
    | Fst_snd_trd (fsts, snds, trds) -> to_list fsts, to_list snds, to_list trds
  ;;

  let%expect_test "partition3_map (manual)" =
    let test xs =
      let partition3 = Nonempty_list.partition3_map xs ~f in
      let from_list = List.partition3_map (to_list xs) ~f in
      let three_lists = convert partition3 in
      assert ([%equal: int list * float list * string list] three_lists from_list);
      partition3 |> [%sexp_of: (int, float, string) Partition3.t] |> print_s
    in
    test [ 1; 2; 3; 4 ];
    [%expect {| (Fst_snd_trd (3) (1 4) (2)) |}];
    test [ 1; 3 ];
    [%expect {| (Fst_snd (3) (1)) |}];
    test [ 5; 3 ];
    [%expect {| (Fst_trd (3) (5)) |}];
    test [ 2; 4 ];
    [%expect {| (Snd_trd (4) (2)) |}];
    test [ 6; 3 ];
    [%expect {| (Fst (6 3)) |}];
    test [ 4; 7 ];
    [%expect {| (Snd (4 7)) |}];
    test [ 5; 2 ];
    [%expect {| (Trd (5 2)) |}]
  ;;

  let%quick_test "partition3_map (quickcheck)" =
    fun (l : int t) ->
    [%test_result: int list * float list * string list]
      (Nonempty_list.partition3_map l ~f |> convert)
      ~expect:(List.partition3_map (to_list l) ~f)
  ;;
end
