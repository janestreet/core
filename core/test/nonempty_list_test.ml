open Core
open Expect_test_helpers_core
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

let%expect_test "reduce" =
  let%quick_test prop (t : int t) =
    [%test_result: int]
      (reduce t ~f:( + ))
      ~expect:(List.reduce (to_list t) ~f:( + ) |> Core.Option.value_exn)
  in
  ()
;;

let%expect_test "reverse" =
  let%quick_test prop (t : int t) =
    [%test_result: int list] (reverse t |> to_list) ~expect:(List.rev (to_list t))
  in
  ()
;;

let%expect_test "append" =
  let%quick_test prop (t : int t) (l : int list) =
    [%test_result: int list] (append t l |> to_list) ~expect:(List.append (to_list t) l)
  in
  ()
;;

let%expect_test "append'" =
  let%quick_test prop (t : int t) (l : int list) =
    [%test_result: int list] (append' l t |> to_list) ~expect:(List.append l (to_list t))
  in
  ()
;;

let%expect_test "@" =
  let%quick_test prop (t1 : int t) (t2 : int t) =
    [%test_result: int list]
      (to_list (t1 @ t2))
      ~expect:(List.Infix.( @ ) (to_list t1) (to_list t2))
  in
  ()
;;

let%expect_test "unzip" =
  let%quick_test prop (l : (int * char) t) =
    [%test_result: int list * char list]
      (unzip l |> Tuple2.map_both ~f1:to_list ~f2:to_list)
      ~expect:(List.unzip (to_list l))
  in
  ()
;;

let%expect_test "iter" =
  let%quick_test prop (l : int t) =
    let output = ref [] in
    let f x = output := x :: !output in
    Nonempty_list.iter ~f l;
    let actual = !output in
    output := [];
    List.iter ~f (to_list l);
    let expected = !output in
    [%test_result: int list] actual ~expect:expected
  in
  ()
;;

let%expect_test "return" =
  let%quick_test prop (i : int) =
    [%test_result: int list] (return i |> to_list) ~expect:(List.return i)
  in
  ()
;;

let%expect_test "bind" =
  let%quick_test prop (l : int t) (f : int -> int t) =
    [%test_result: int list]
      (bind l ~f |> to_list)
      ~expect:(List.bind (to_list l) ~f:(fun i -> to_list (f i)))
  in
  ()
;;

let map_or_unequal_lengths (or_unequal_lengths : _ List.Or_unequal_lengths.t) ~f
  : _ List.Or_unequal_lengths.t
  =
  match or_unequal_lengths with
  | Ok l -> (Ok (f l) : _ List.Or_unequal_lengths.t)
  | Unequal_lengths -> Unequal_lengths
;;

let%expect_test "map2" =
  let%quick_test prop (t1 : int t) (t2 : int t) (f : int -> int -> int) =
    [%test_result: int list List.Or_unequal_lengths.t]
      (map2 t1 t2 ~f |> map_or_unequal_lengths ~f:to_list)
      ~expect:(List.map2 (to_list t1) (to_list t2) ~f)
  in
  ()
;;

let%expect_test "map2_exn" =
  let%quick_test prop (t1 : int t) (t2 : int t) (f : int -> int -> int) =
    [%test_result: int list Or_error.t]
      (Or_error.try_with (fun () -> map2_exn t1 t2 ~f) |> Or_error.map ~f:to_list)
      ~expect:(Or_error.try_with (fun () -> List.map2_exn (to_list t1) (to_list t2) ~f))
  in
  ()
;;

let%expect_test "zip" =
  let%quick_test prop (a : int t) (b : int t) =
    [%test_result: (int * int) list List.Or_unequal_lengths.t]
      (zip a b |> map_or_unequal_lengths ~f:to_list)
      ~expect:(List.zip (to_list a) (to_list b))
  in
  ()
;;

let%expect_test "zip_exn" =
  let%quick_test prop (a : int t) (b : int t) =
    [%test_result: (int * int) list Or_error.t]
      (Or_error.try_with (fun () -> zip_exn a b) |> Or_error.map ~f:to_list)
      ~expect:(Or_error.try_with (fun () -> List.zip_exn (to_list a) (to_list b)))
  in
  ()
;;

let%expect_test "zip3" =
  let%quick_test prop (a : int t) (b : int t) (c : int t) =
    [%test_result: (int * int * int) list List.Or_unequal_lengths.t]
      (zip3 a b c |> map_or_unequal_lengths ~f:to_list)
      ~expect:(List.zip3 (to_list a) (to_list b) (to_list c))
  in
  ()
;;

let%expect_test "zip3_exn" =
  let%quick_test prop (a : int t) (b : int t) (c : int t) =
    [%test_result: (int * int * int) list option]
      (Core.Option.try_with (fun () -> zip3_exn a b c) |> Core.Option.map ~f:to_list)
      ~expect:
        (Core.Option.try_with (fun () ->
           List.zip3_exn (to_list a) (to_list b) (to_list c)))
  in
  ()
;;

let%expect_test "filter" =
  let%quick_test prop (t : int t) (f : int -> bool) =
    [%test_result: int list] (filter t ~f) ~expect:(List.filter (to_list t) ~f)
  in
  ()
;;

let%expect_test "filteri" =
  let%quick_test prop (t : int t) (f : int -> int -> bool) =
    [%test_result: int list] (filteri t ~f) ~expect:(List.filteri (to_list t) ~f)
  in
  ()
;;

let%expect_test "filter_map" =
  let%quick_test prop (t : int t) (f : int -> int option) =
    [%test_result: int list] (filter_map t ~f) ~expect:(List.filter_map (to_list t) ~f)
  in
  ()
;;

let%expect_test "filter_mapi" =
  let%quick_test prop (t : int t) (f : int -> int -> int option) =
    [%test_result: int list] (filter_mapi t ~f) ~expect:(List.filter_mapi (to_list t) ~f)
  in
  ()
;;

let%expect_test "concat" =
  let%quick_test prop (lists : int t t) =
    [%test_result: int list]
      (concat lists |> to_list)
      ~expect:(List.concat (to_list lists |> List.map ~f:to_list))
  in
  ()
;;

let%expect_test "nth" =
  let%quick_test prop (t : int t) (n : int) =
    [%test_result: int option] (nth t n) ~expect:(List.nth (to_list t) n)
  in
  ()
;;

let%expect_test "nth_exn" =
  let%quick_test prop (t : int t) (n : int) =
    [%test_result: int option]
      (Core.Option.try_with (fun () -> nth_exn t n))
      ~expect:(Core.Option.try_with (fun () -> List.nth_exn (to_list t) n))
  in
  ()
;;

let%expect_test "last" =
  let%quick_test prop (l : int t) =
    [%test_result: int] (last l) ~expect:(List.last (to_list l) |> Core.Option.value_exn)
  in
  ()
;;

let%expect_test "drop_last" =
  let%quick_test prop (l : int t) =
    [%test_result: int list]
      (drop_last l)
      ~expect:(List.drop_last (to_list l) |> Core.Option.value_exn)
  in
  ()
;;

let%expect_test "to_sequence" =
  let%quick_test prop (t : int t) =
    [%test_result: int list] (to_sequence t |> Sequence.to_list) ~expect:(to_list t)
  in
  ()
;;

let%expect_test "sort" =
  let%quick_test prop (t : int t) =
    [%test_result: int list]
      (sort ~compare:Int.compare t |> to_list)
      ~expect:(List.sort ~compare:Int.compare (to_list t))
  in
  ()
;;

(* It's a bit tricky to show that this test is doing exactly the right thing. But if you
   [List.permute] the list given to, say, [List.stable_sort], it does fail as expected.
*)
let%expect_test "stable_sort" =
  let%quick_test prop (t : (int * bool) t) =
    let compare = [%compare: int * _] in
    [%test_result: (int * bool) list]
      (stable_sort ~compare t |> to_list)
      ~expect:(List.stable_sort ~compare (to_list t))
  in
  ()
;;

let%expect_test "stable_dedup" =
  let%quick_test prop (t : int t) =
    [%test_result: int list]
      (stable_dedup ~compare:Int.compare t |> to_list)
      ~expect:(List.stable_dedup ~compare:Int.compare (to_list t))
  in
  ()
;;

let%expect_test "dedup_and_sort" =
  let%quick_test prop (t : int t) =
    [%test_result: int list]
      (dedup_and_sort ~compare:Int.compare t |> to_list)
      ~expect:(List.dedup_and_sort ~compare:Int.compare (to_list t))
  in
  ()
;;

let%expect_test "sort_and_group" =
  let%quick_test prop (t : int t) =
    [%test_result: int list list]
      (sort_and_group ~compare:Int.compare t |> to_list |> List.map ~f:to_list)
      ~expect:(List.sort_and_group ~compare:Int.compare (to_list t))
  in
  ()
;;

let%expect_test "group" =
  let%quick_test prop (t : int t) (break : int -> int -> bool) =
    [%test_result: int list list]
      (group t ~break |> to_list |> List.map ~f:to_list)
      ~expect:(List.group (to_list t) ~break)
  in
  ()
;;

let%expect_test "all_equal" =
  let%quick_test prop (l : int t) =
    let equal = [%equal: int] in
    [%test_result: int option]
      (all_equal ~equal l)
      ~expect:(List.all_equal ~equal (to_list l))
  in
  ()
;;

let%expect_test "min_elt'" =
  let%quick_test prop (l : int t) =
    [%test_result: int option]
      (Some (min_elt' ~compare:Int.compare l))
      ~expect:(List.min_elt (to_list l) ~compare:Int.compare)
  in
  ()
;;

let%expect_test "max_elt'" =
  let%quick_test prop (l : int t) =
    [%test_result: int option]
      (Some (max_elt' ~compare:Int.compare l))
      ~expect:(List.max_elt (to_list l) ~compare:Int.compare)
  in
  ()
;;

let%expect_test "map_add_multi" =
  let%quick_test prop (l : (int * int) list) (new_key : int) (new_data : int) =
    let map = map_of_alist_multi_rev l ~comparator:(module Int) in
    [%test_result: int list Int.Map.t]
      (map_add_multi map ~key:new_key ~data:new_data |> Map.map ~f:to_list)
      ~expect:(Map.add_multi (Map.map map ~f:to_list) ~key:new_key ~data:new_data)
  in
  ()
;;

let%expect_test "hashtbl_add_multi" =
  let%quick_test prop (l : (int * int) list) (new_key : int) (new_data : int) =
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
  in
  ()
;;

let%expect_test "map_of_alist_multi_rev" =
  let%quick_test prop (alist : (int * int) list) =
    [%test_result: int list Int.Map.t]
      (map_of_alist_multi_rev alist ~comparator:(module Int) |> Map.map ~f:to_list)
      ~expect:(Map.of_alist_multi (module Int) alist |> Map.map ~f:List.rev)
  in
  ()
;;

let%expect_test "map_of_alist_multi" =
  let%quick_test prop (alist : (int * int) list) =
    [%test_result: int list Int.Map.t]
      (map_of_alist_multi alist ~comparator:(module Int) |> Map.map ~f:to_list)
      ~expect:(Map.of_alist_multi (module Int) alist)
  in
  ()
;;

let%expect_test "map_of_list_with_key_multi_rev" =
  let%quick_test prop (alist : Date.t list) =
    let get_key = Date.year in
    [%test_result: Date.t list Int.Map.t]
      (map_of_list_with_key_multi_rev alist ~comparator:(module Int) ~get_key
       |> Map.map ~f:to_list)
      ~expect:
        (Map.of_list_with_key_multi (module Int) alist ~get_key |> Map.map ~f:List.rev)
  in
  ()
;;

let%expect_test "map_of_list_with_key_multi" =
  let%quick_test prop (alist : Date.t list) =
    let get_key = Date.year in
    [%test_result: Date.t list Int.Map.t]
      (map_of_list_with_key_multi alist ~comparator:(module Int) ~get_key
       |> Map.map ~f:to_list)
      ~expect:(Map.of_list_with_key_multi (module Int) alist ~get_key)
  in
  ()
;;

let%expect_test "map_of_sequence_multi_rev" =
  let%quick_test prop (alist : (int * int) list) =
    let seq = Sequence.of_list alist in
    [%test_result: int list Int.Map.t]
      (map_of_sequence_multi_rev seq ~comparator:(module Int) |> Map.map ~f:to_list)
      ~expect:(Map.of_sequence_multi (module Int) seq |> Map.map ~f:List.rev)
  in
  ()
;;

let%expect_test "map_of_sequence_multi" =
  let%quick_test prop (alist : (int * int) list) =
    let seq = Sequence.of_list alist in
    [%test_result: int list Int.Map.t]
      (map_of_sequence_multi seq ~comparator:(module Int) |> Map.map ~f:to_list)
      ~expect:(Map.of_sequence_multi (module Int) seq)
  in
  ()
;;

let%expect_test "combine_errors" =
  let%quick_test prop (rs : (int, int) Result.t t) =
    [%test_result: (int list, int list) Result.t]
      (match combine_errors rs with
       | Ok t -> Ok (to_list t)
       | Error t -> Error (to_list t))
      ~expect:(Result.combine_errors (to_list rs))
  in
  ()
;;

let%expect_test "combine_errors_unit" =
  let%quick_test prop (rs : (unit, int) Result.t t) =
    [%test_result: (unit, int list) Result.t]
      (match combine_errors_unit rs with
       | Ok () -> Ok ()
       | Error t -> Error (to_list t))
      ~expect:(Result.combine_errors_unit (to_list rs))
  in
  ()
;;

let%expect_test "combine_or_errors" =
  let%quick_test prop (oes : int Or_error.t t) =
    [%test_result: int list Or_error.t]
      (combine_or_errors oes |> Result.map ~f:to_list)
      ~expect:(Or_error.combine_errors (to_list oes))
  in
  ()
;;

let%expect_test "combine_or_errors_unit" =
  let%quick_test prop (oes : unit Or_error.t t) =
    [%test_result: unit Or_error.t]
      (combine_or_errors_unit oes)
      ~expect:(Or_error.combine_errors_unit (to_list oes))
  in
  ()
;;

let%expect_test "filter_ok_at_least_one" =
  let%quick_test prop (oes : int Or_error.t t) =
    [%test_result: int list Or_error.t]
      (filter_ok_at_least_one oes |> Or_error.map ~f:to_list)
      ~expect:(Or_error.filter_ok_at_least_one (to_list oes))
  in
  ()
;;

let%expect_test "option_all" =
  let%quick_test prop (os : int option t) =
    let expected =
      let list_os = to_list os in
      if List.for_all list_os ~f:Core.Option.is_some
      then Some (List.filter_map list_os ~f:Fn.id)
      else None
    in
    [%test_result: int list option]
      (option_all os |> Core.Option.map ~f:to_list)
      ~expect:expected
  in
  ()
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

let%expect_test "fold" =
  let%quick_test prop (t : int t) (init : int) (f : int -> int -> int) =
    [%test_result: int] (fold t ~init ~f) ~expect:(List.fold (to_list t) ~init ~f)
  in
  ()
;;

let%expect_test "fold_right" =
  let%quick_test prop (t : int t) (init : int) (f : int -> int -> int) =
    [%test_result: int]
      (fold_right t ~init ~f)
      ~expect:(List.fold_right (to_list t) ~init ~f)
  in
  ()
;;

let%expect_test "reduce" =
  let%quick_test prop (t : int t) (f : int -> int -> int) =
    [%test_result: int option] (Some (reduce t ~f)) ~expect:(List.reduce (to_list t) ~f)
  in
  ()
;;

let%expect_test "folding_map" =
  let%quick_test prop (t : int t) (init : int) (f : int -> int -> int * int) =
    [%test_result: int list]
      (folding_map t ~init ~f |> to_list)
      ~expect:(List.folding_map (to_list t) ~init ~f)
  in
  ()
;;

let%expect_test "fold_map" =
  let%quick_test prop (t : int t) (init : int) (f : int -> int -> int * int) =
    [%test_result: int * int list]
      (let acc, result = fold_map t ~init ~f in
       acc, to_list result)
      ~expect:(List.fold_map (to_list t) ~init ~f)
  in
  ()
;;

let%expect_test "mapi" =
  let%quick_test prop (t : string t) (f : int -> string -> int * string) =
    [%test_result: (int * string) list]
      (mapi t ~f |> to_list)
      ~expect:(List.mapi (to_list t) ~f)
  in
  ()
;;

let%expect_test "transpose" =
  let%quick_test prop (t : int t t) =
    [%test_result: int list list option]
      (transpose t
       |> Core.Option.map ~f:(fun result -> to_list result |> List.map ~f:to_list))
      ~expect:(List.transpose (to_list t |> List.map ~f:to_list))
  in
  ()
;;

let%expect_test "transpose_exn" =
  let%quick_test prop (t : int t t) =
    [%test_result: int list list option]
      (Core.Option.try_with (fun () -> transpose_exn t |> to_list |> List.map ~f:to_list))
      ~expect:
        (Core.Option.try_with (fun () ->
           List.transpose_exn (to_list t |> List.map ~f:to_list)))
  in
  ()
;;

let%expect_test "init" =
  let%quick_test prop (n : (int[@generator Int.gen_incl (-10) 10])) (f : int -> int) =
    (* [n=0] is the one case we expect to differ *)
    if n <> 0
    then
      [%test_result: int list option]
        (Core.Option.try_with (fun () -> init n ~f) |> Core.Option.map ~f:to_list)
        ~expect:(Core.Option.try_with (fun () -> List.init n ~f))
  in
  ()
;;

let%expect_test "iteri" =
  let%quick_test prop (l : int t) =
    let output = ref [] in
    let f i x = output := (i, x) :: !output in
    Nonempty_list.iteri ~f l;
    let actual = !output in
    output := [];
    List.iteri ~f (to_list l);
    let expected = !output in
    [%test_result: (int * int) list] actual ~expect:expected
  in
  ()
;;

let%expect_test "findi" =
  let%quick_test prop (t : int t) (f : int -> int -> bool) =
    [%test_result: (int * int) option] (findi t ~f) ~expect:(List.findi (to_list t) ~f)
  in
  ()
;;

let%expect_test "findi_exn" =
  let%quick_test prop (t : int t) (f : int -> int -> bool) =
    [%test_result: (int * int) option]
      (Core.Option.try_with (fun () -> findi_exn t ~f))
      ~expect:(Core.Option.try_with (fun () -> List.findi_exn (to_list t) ~f))
  in
  ()
;;

let%expect_test "find_mapi" =
  let%quick_test prop (t : int t) (f : int -> int -> int option) =
    [%test_result: int option] (find_mapi t ~f) ~expect:(List.find_mapi (to_list t) ~f)
  in
  ()
;;

let%expect_test "counti" =
  let%quick_test prop (t : int t) (f : int -> int -> bool) =
    [%test_result: int] (counti t ~f) ~expect:(List.counti (to_list t) ~f)
  in
  ()
;;

let%expect_test "for_alli" =
  let%quick_test prop (t : int t) (f : int -> int -> bool) =
    [%test_result: bool] (for_alli t ~f) ~expect:(List.for_alli (to_list t) ~f)
  in
  ()
;;

let%expect_test "existsi" =
  let%quick_test prop (t : int t) (f : int -> int -> bool) =
    [%test_result: bool] (existsi t ~f) ~expect:(List.existsi (to_list t) ~f)
  in
  ()
;;

let%expect_test "foldi" =
  let%quick_test prop
    (t : int t)
    (init : int list)
    (f : int -> int list -> int -> int list)
    =
    [%test_result: int list] (foldi t ~init ~f) ~expect:(List.foldi (to_list t) ~init ~f)
  in
  ()
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

let%expect_test "of_list_or_null" =
  let%quick_test prop (l : int list) =
    [%test_result: int list]
      (Expect_test_helpers_core.require_no_allocation (fun () ->
         match of_list_or_null l, l with
         | Null, [] -> []
         | Null, _ :: _ -> assert false
         | This (_ :: _), [] -> assert false
         | This (x :: xs as nonempty_list), y :: ys ->
           assert (x = y);
           assert (phys_equal xs ys);
           Nonempty_list.to_list nonempty_list))
      ~expect:l
  in
  ()
;;

let%expect_test "remove_consecutive_duplicates" =
  let%quick_test prop (t : int t) (which_to_keep : [ `First | `Last ]) =
    [%test_result: int list]
      (remove_consecutive_duplicates t ~which_to_keep ~equal:[%equal: int] |> to_list)
      ~expect:
        (List.remove_consecutive_duplicates
           (to_list t)
           ~which_to_keep
           ~equal:[%equal: int])
  in
  ()
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
    [%expect
      {|
      (Both (
        (2 4)
        (1 3)))
      |}];
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
    [%expect
      {|
      (Both (
        (2       4)
        ("odd 1" "odd 3")))
      |}];
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
    [%expect
      {|
      (Both (
        (2       4)
        ("odd 1" "odd 3")))
      |}];
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
    [%expect
      {|
      (Fst_snd
        (3)
        (1))
      |}];
    test [ 5; 3 ];
    [%expect
      {|
      (Fst_trd
        (3)
        (5))
      |}];
    test [ 2; 4 ];
    [%expect
      {|
      (Snd_trd
        (4)
        (2))
      |}];
    test [ 6; 3 ];
    [%expect {| (Fst (6 3)) |}];
    test [ 4; 7 ];
    [%expect {| (Snd (4 7)) |}];
    test [ 5; 2 ];
    [%expect {| (Trd (5 2)) |}]
  ;;

  let%expect_test "partition3_map (quickcheck)" =
    let%quick_test prop (l : int t) =
      [%test_result: int list * float list * string list]
        (Nonempty_list.partition3_map l ~f |> convert)
        ~expect:(List.partition3_map (to_list l) ~f)
    in
    ()
  ;;
end

(* Test functions with templated definitions for non-value layouts *)

module%test Nonvalue_layout_tests = struct
  module Generator = Base_quickcheck.Generator
  module Shrinker = Base_quickcheck.Shrinker
  open Generator.Let_syntax

  [%%template
  module
    [@kind k = (bits32, bits64, word, float64)] Test_against_boxed (T : sig
      module Boxed : Unboxed_test_harness.Boxed

      type t [@@deriving sexp_of]

      include
        Unboxed_test_harness.Boxable [@kind k] with type boxed := Boxed.t with type t := t

      val of_int : int -> t
      val to_int : t -> int
    end) : sig end = struct
    module Harness =
      Unboxed_test_harness.Make [@kind k]
        (T)
        (struct
          (* Almost all of these functions allocate, so we're not really concerned about
             testing allocation behavior specifically in optimized builds. *)
          let flambda2 = false

          let filenames_to_suppress_in_backtraces : string list =
            [ "nonempty_list.ml"; "list.ml" ]
          ;;
        end)

    open Harness
    open T

    let filter_fun i x = if i < 0 then to_int x < 1 else to_int x > -1
    let map_fun i x = x |> to_int |> ( + ) i |> ( * ) 2 |> of_int

    let make_iter_fun () =
      let acc = ref 0 in
      (fun i x -> acc := !acc + i + to_int x), acc
    ;;

    (* We generate nonempty lists by generating a boxed nonempty list, then converting *)
    module Nonempty_list_pair = struct
      type t =
        { b_list : Boxed.t Nonempty_list.t
        ; u_list : (T.t Nonempty_list.t[@kind k])
        }
      [@@deriving sexp_of]

      let quickcheck_generator =
        let%bind b_list = [%generator: Boxed.t Nonempty_list.t] in
        let u_list =
          b_list |> Nonempty_list.to_list |> (List.map [@kind value_or_null k]) ~f:unbox
        in
        let u_list = (Nonempty_list.of_list_exn [@kind k]) u_list in
        return { b_list; u_list }
      ;;

      (* To avoid breaking invariants, don't shrink *)
      let quickcheck_shrinker : t Shrinker.t = Quickcheck.Shrinker.empty ()
    end

    let require_equal_nonempty_lists
      ~f_boxed
      ~f_unboxed
      ({ b_list; u_list } : Nonempty_list_pair.t)
      =
      require_compare_equal
        ~is_zero_alloc_with_flambda2:false
        [%here]
        (module struct
          type t = Boxed.t Nonempty_list.t [@@deriving sexp_of, compare]
        end)
        (fun () -> f_boxed b_list)
        (fun () -> f_unboxed u_list |> (Nonempty_list.map [@kind k value_or_null]) ~f:box)
    ;;

    let require_equal_lists
      ~f_boxed
      ~f_unboxed
      ({ b_list; u_list } : Nonempty_list_pair.t)
      =
      require_compare_equal
        ~is_zero_alloc_with_flambda2:false
        [%here]
        (module struct
          type t = Boxed.t List.t [@@deriving sexp_of, compare]
        end)
        (fun () -> f_boxed b_list)
        (fun () -> f_unboxed u_list |> (List.map [@kind k value_or_null]) ~f:box)
    ;;

    let test name ~f =
      let confirm_test = lazy (print_endline {%string|testing [%{name}]|}) in
      quickcheck_m (module Nonempty_list_pair) ~f:(fun pair ->
        force confirm_test;
        f pair)
    ;;

    let test_sexp_of () =
      test "sexp_of" ~f:(fun { b_list; u_list } ->
        require_equal
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (module Sexp)
          (fun () -> [%sexp_of: Boxed.t Nonempty_list.t] b_list)
          (fun () -> [%sexp_of: (T.t Nonempty_list.t[@kind k])] u_list))
    ;;

    let test_iter () =
      test "iter" ~f:(fun { b_list; u_list } ->
        require_equal
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (module Int)
          (fun () ->
            let f, r = make_iter_fun () in
            Nonempty_list.iter b_list ~f:(fun x -> f 0 (unbox x));
            !r)
          (fun () ->
            let f, r = make_iter_fun () in
            (Nonempty_list.iter [@kind k]) u_list ~f:(fun x -> f 0 x);
            !r))
    ;;

    let test_iteri () =
      test "iteri" ~f:(fun { b_list; u_list } ->
        require_equal
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (module Int)
          (fun () ->
            let f, r = make_iter_fun () in
            Nonempty_list.iteri b_list ~f:(fun i x -> f i (unbox x));
            !r)
          (fun () ->
            let f, r = make_iter_fun () in
            (Nonempty_list.iteri [@kind k]) u_list ~f:(fun i x -> f i x);
            !r))
    ;;

    let test_length () =
      test "length" ~f:(fun { b_list; u_list } ->
        require_equal
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (module Int)
          (fun () -> Nonempty_list.length b_list)
          (fun () -> (Nonempty_list.length [@kind k]) u_list))
    ;;

    let test_mapi () =
      test
        "mapi"
        ~f:
          (require_equal_nonempty_lists
             ~f_boxed:(Nonempty_list.mapi ~f:(fun i x -> box (map_fun i (unbox x))))
             ~f_unboxed:((Nonempty_list.mapi [@kind k k]) ~f:map_fun))
    ;;

    let test_map () =
      let map_fun = map_fun 0 in
      test
        "map"
        ~f:
          (require_equal_nonempty_lists
             ~f_boxed:(Nonempty_list.map ~f:(fun x -> box (map_fun (unbox x))))
             ~f_unboxed:((Nonempty_list.map [@kind k k]) ~f:(fun x -> map_fun x)))
    ;;

    let test_concat_map () =
      test
        "concat_map"
        ~f:
          (require_equal_nonempty_lists
             ~f_boxed:
               (Nonempty_list.concat_map ~f:(fun x ->
                  let y i = box (map_fun i (unbox x)) in
                  [ y 1; y 2; y 3 ]))
             ~f_unboxed:
               ((Nonempty_list.concat_map [@kind k k]) ~f:(fun x ->
                  let y i = map_fun i x in
                  [ y 1; y 2; y 3 ])))
    ;;

    let test_filter_map () =
      let filter_fun = filter_fun 0 in
      let map_fun = map_fun 0 in
      test
        "filter_map"
        ~f:
          (require_equal_lists
             ~f_boxed:
               (Nonempty_list.filter_map ~f:(fun x ->
                  if filter_fun (unbox x) then Some (box (map_fun (unbox x))) else None))
             ~f_unboxed:
               ((Nonempty_list.filter_map [@kind k k]) ~f:(fun x ->
                  if filter_fun x
                  then (Some (map_fun x) : (_ Core.Option.t[@kind k]))
                  else None)))
    ;;

    let test_filter_mapi () =
      test
        "filter_mapi"
        ~f:
          (require_equal_lists
             ~f_boxed:
               (Nonempty_list.filter_mapi ~f:(fun i x ->
                  if filter_fun i (unbox x)
                  then Some (box (map_fun i (unbox x)))
                  else None))
             ~f_unboxed:
               ((Nonempty_list.filter_mapi [@kind k k]) ~f:(fun i x ->
                  if filter_fun i x
                  then (Some (map_fun i x) : (_ Core.Option.t[@kind k]))
                  else None)))
    ;;

    let test_hd () =
      test "hd" ~f:(fun { b_list; u_list } ->
        Harness.require_compare_equal_wrapped
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (fun () -> Nonempty_list.hd b_list)
          (fun () -> (Nonempty_list.hd [@kind k]) u_list))
    ;;

    let test_tl () =
      test
        "tl"
        ~f:
          (require_equal_lists
             ~f_boxed:Nonempty_list.tl
             ~f_unboxed:(Nonempty_list.tl [@kind k]))
    ;;

    let test_min_elt' () =
      let compare x y = Int.compare (to_int x) (to_int y) in
      test "min_elt'" ~f:(fun { b_list; u_list } ->
        Harness.require_compare_equal_wrapped
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (fun () ->
            Nonempty_list.min_elt' b_list ~compare:(fun x y ->
              compare (unbox x) (unbox y)))
          (fun () -> (Nonempty_list.min_elt' [@kind k]) u_list ~compare))
    ;;

    let test_max_elt' () =
      let compare x y = Int.compare (to_int x) (to_int y) in
      test "max_elt'" ~f:(fun { b_list; u_list } ->
        Harness.require_compare_equal_wrapped
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (fun () ->
            Nonempty_list.max_elt' b_list ~compare:(fun x y ->
              compare (unbox x) (unbox y)))
          (fun () -> (Nonempty_list.max_elt' [@kind k]) u_list ~compare))
    ;;

    let test_fold_nonempty () =
      let fold_fun acc x = of_int (to_int acc + to_int x) in
      let init_fun (x : t) : t = x in
      test "fold_nonempty" ~f:(fun { b_list; u_list } ->
        Harness.require_compare_equal_wrapped
          ~is_zero_alloc_with_flambda2:false
          [%here]
          (fun () ->
            Core.Nonempty_list.fold_nonempty b_list ~init:Fn.id ~f:(fun acc x ->
              box (fold_fun (unbox acc) (unbox x))))
          (fun () ->
            (Core.Nonempty_list.fold_nonempty [@kind k k])
              u_list
              ~init:init_fun
              ~f:fold_fun))
    ;;

    let test_of_list () =
      (* Test empty list returns None *)
      assert (Core.Option.is_none (Nonempty_list.of_list []));
      assert (Core.Option.is_none ((Nonempty_list.of_list [@kind k]) []));
      (* Test non-empty lists *)
      test
        "of_list"
        ~f:
          (require_equal_nonempty_lists
             ~f_boxed:(fun l ->
               Nonempty_list.of_list (Nonempty_list.to_list l)
               |> Core.Option.value_exn ~here:[%here])
             ~f_unboxed:(fun l ->
               (Nonempty_list.of_list [@kind k]) ((Nonempty_list.to_list [@kind k]) l)
               |> Core.Option.value_exn ~here:[%here]))
    ;;

    let test_of_list_exn () =
      test
        "of_list_exn"
        ~f:
          (require_equal_nonempty_lists
             ~f_boxed:(fun l -> Nonempty_list.of_list_exn (Nonempty_list.to_list l))
             ~f_unboxed:(fun l ->
               (Nonempty_list.of_list_exn [@kind k]) ((Nonempty_list.to_list [@kind k]) l)))
    ;;

    let test_to_list () =
      test
        "to_list"
        ~f:
          (require_equal_lists
             ~f_boxed:Nonempty_list.to_list
             ~f_unboxed:(Nonempty_list.to_list [@kind k]))
    ;;

    let () =
      test_sexp_of ();
      test_iter ();
      test_iteri ();
      test_length ();
      test_mapi ();
      test_map ();
      test_concat_map ();
      test_filter_map ();
      test_filter_mapi ();
      test_hd ();
      test_tl ();
      test_min_elt' ();
      test_max_elt' ();
      test_fold_nonempty ();
      test_of_list ();
      test_of_list_exn ();
      test_to_list ()
    ;;
  end

  let%expect_test "float64" =
    let module _ =
      Test_against_boxed [@kind float64] (struct
        include Float_u
        module Boxed = Float

        let to_int x = iround x |> Core.Option.value ~default:(-5)
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [iter]
      testing [iteri]
      testing [length]
      testing [mapi]
      testing [map]
      testing [concat_map]
      testing [filter_map]
      testing [filter_mapi]
      testing [hd]
      testing [tl]
      testing [min_elt']
      testing [max_elt']
      testing [fold_nonempty]
      testing [of_list]
      testing [of_list_exn]
      testing [to_list]
      |}]
  ;;

  let%expect_test "bits32" =
    let module _ =
      Test_against_boxed [@warning "-60"] [@kind bits32] (struct
        include Int32_u
        module Boxed = Int32

        let of_int = of_int_trunc
        let to_int = to_int_trunc
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [iter]
      testing [iteri]
      testing [length]
      testing [mapi]
      testing [map]
      testing [concat_map]
      testing [filter_map]
      testing [filter_mapi]
      testing [hd]
      testing [tl]
      testing [min_elt']
      testing [max_elt']
      testing [fold_nonempty]
      testing [of_list]
      testing [of_list_exn]
      testing [to_list]
      |}]
  ;;

  let%expect_test "bits64" =
    let module _ =
      Test_against_boxed [@warning "-60"] [@kind bits64] (struct
        include Int64_u
        module Boxed = Int64

        let to_int = to_int_trunc
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [iter]
      testing [iteri]
      testing [length]
      testing [mapi]
      testing [map]
      testing [concat_map]
      testing [filter_map]
      testing [filter_mapi]
      testing [hd]
      testing [tl]
      testing [min_elt']
      testing [max_elt']
      testing [fold_nonempty]
      testing [of_list]
      testing [of_list_exn]
      testing [to_list]
      |}]
  ;;

  let%expect_test "word" =
    let module _ =
      Test_against_boxed [@warning "-60"] [@kind word] (struct
        include Nativeint_u
        module Boxed = Nativeint

        let to_int = to_int_trunc
      end)
    in
    [%expect
      {|
      testing [sexp_of]
      testing [iter]
      testing [iteri]
      testing [length]
      testing [mapi]
      testing [map]
      testing [concat_map]
      testing [filter_map]
      testing [filter_mapi]
      testing [hd]
      testing [tl]
      testing [min_elt']
      testing [max_elt']
      testing [fold_nonempty]
      testing [of_list]
      testing [of_list_exn]
      testing [to_list]
      |}]
  ;;]
end
