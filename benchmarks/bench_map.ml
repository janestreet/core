open Core.Std

module Bench = Core_extended.Bench
module Test = Bench.Test

let size = 10_000

let alist =
  List.init size ~f:(fun i -> (i, i))
;;

let of_alist_exn () = ignore (Int.Map.of_alist_exn alist)

let map = Int.Map.of_alist_exn alist

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

let () =
  Bench.bench [
    Test.create ~name:"Map.of_alist_exn" of_alist_exn;
    Test.create ~name:"Map.add" add;
    Test.create ~name:"Map.remove" remove;
    Test.create ~name:"Map.merge (new)" (merge_test Map.merge);
    Test.create ~name:"Map.merge (old)" (merge_test old_map_merge);
  ]
;;
