open Core.Std

module Bench = Core_extended.Bench
module Test = Bench.Test

let size = 10_000

let alist = List.init size ~f:(fun i -> (i, i))

let of_alist_exn () = ignore ((Int.Map.of_alist_exn alist) : int Int.Map.t)

let of_sorted_array () =
  let sorted_array = Array.init size ~f:(fun i -> (i, i)) in
  ignore (Int.Map.of_sorted_array sorted_array)

let map = Int.Map.of_alist_exn alist

let iter () = Map.iter map ~f:(fun ~key:_ ~data:_ -> ())
let iter2 () = Map.iter2 map map ~f:(fun ~key:_ ~data:_ -> ())
let iter2_naive () =
  let iter2 a b ~f =
    Map.iter a ~f:(fun ~key ~data:_ ->
      match Map.find b key with
      | None -> f `Left
      | Some _ -> f `Both
    )
  in
  iter2 map map ~f:ignore

let add =
  assert(not (Int.Map.mem map size));
  fun () -> ignore (Map.add map ~key:size ~data:size)
;;

let remove =
  let deep_key = fst (Map.min_elt_exn map) in
  fun () -> ignore (Map.remove map deep_key)
;;

let old_map_merge t1 t2 ~f =
  let all_keys =
    List.dedup ~compare (List.append (Map.keys t1) (Map.keys t2))
  in
  List.fold ~init:Map.Poly.empty all_keys
    ~f:(fun t key ->
      let z =
        match Map.find t1 key, Map.find t2 key with
        | None, None -> assert false
        | None, Some v2 -> `Right v2
        | Some v1, None -> `Left v1
        | Some v1, Some v2 -> `Both (v1, v2)
      in
      match f ~key z with
      | None -> t
      | Some data -> Map.add t ~key ~data)
;;

let merge_test do_merge =
  let map2 = Int.Map.of_alist_exn (List.init size ~f:(fun i -> 2*i, 2*i)) in
  fun () ->
    ignore (
      do_merge map map2 ~f:(fun ~key:_ x ->
        match x with
        | `Left a -> Some a
        | `Right a -> Some a
        | `Both (a, b) -> Some (a + b)))
;;

let gen_diff_test m ~number_of_diff ~diff =
  let gen_pair i = sprintf "%6d" i, sprintf "%6d" (i+1) in
  let map1 = String.Map.of_alist_exn (List.init m ~f:gen_pair) in
  let map2 =
    let gen_pair i = gen_pair (i * 16856431 mod m) in
    List.fold (List.init number_of_diff ~f:gen_pair)
      ~init:map1 ~f:(fun acc (key, data) -> Map.add acc ~key ~data)
  in
  fun () ->
    let (_ : _ list) = diff map1 map2 ~data_equal:String.equal in
    ()
;;

let diff_by_iter2 map1 map2 ~data_equal =
  let results = ref [] in
  Map.iter2 map1 map2 ~f:(fun ~key ~data ->
    match data with
    | `Left _ -> results := (key, None) :: !results
    | `Right v -> results := (key, Some v) :: !results
    | `Both (v1, v2) ->
      if not (data_equal v1 v2) then
        results := (key, Some v2) :: !results
  );
  !results
;;

let () =
  Bench.bench [
    Test.create ~name:"Map.of_alist_exn" of_alist_exn;
    Test.create ~name:"Map.of_sorted_array" of_sorted_array;
    Test.create ~name:"Map.add" add;
    Test.create ~name:"Map.iter" iter;
    Test.create ~name:"Map.iter2" iter2;
    Test.create ~name:"Map.iter2_naive" iter2;
    Test.create ~name:"Map.remove" remove;
    Test.create ~name:"Map.merge (new)" (merge_test Map.merge);
    Test.create ~name:"Map.merge (old)" (merge_test old_map_merge);
    Test.create ~name:"Map.symmetric_diff-10"
      (gen_diff_test 100_000 ~number_of_diff:10 ~diff:Map.symmetric_diff);
    Test.create ~name:"Map.symmetric_diff-100"
      (gen_diff_test 100_000 ~number_of_diff:100 ~diff:Map.symmetric_diff);
    Test.create ~name:"Map.symmetric_diff-1000"
      (gen_diff_test 100_000 ~number_of_diff:1000 ~diff:Map.symmetric_diff);
    Test.create ~name:"Map.symmetric_diff_by_iter2-10"
      (gen_diff_test 100_000 ~number_of_diff:10 ~diff:diff_by_iter2);
    Test.create ~name:"Map.symmetric_diff_by_iter2-100"
      (gen_diff_test 100_000 ~number_of_diff:100 ~diff:diff_by_iter2);
    Test.create ~name:"Map.symmetric_diff_by_iter2-1000"
      (gen_diff_test 100_000 ~number_of_diff:1000 ~diff:diff_by_iter2)
  ]
;;
