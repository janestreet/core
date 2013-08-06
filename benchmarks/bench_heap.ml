open Core.Std
module Bench = Core_bench.Bench_main

module Data = struct
  type t = string * int Queue.t

  let range name high =
    let t = Queue.create () in
    for _i = 1 to 1_000_000 do
      Queue.enqueue t (Random.int high);
    done;
    name, t
  ;;

  let small_range  = range "small range" 10
  let medium_range = range "medium range" 100
  let large_range  = range "large range" 1_000_000

  let ascending =
    let t = Queue.create () in
    for i = 1 to 1_000_000 do
      Queue.enqueue  t i;
    done;
    "ascending", t
  ;;

  let next (_, t) =
    let n = Queue.dequeue_exn t in
    Queue.enqueue t n;
    n
  ;;

  let name t = fst t
end

let add_remove_from_existing_heap data initial_size =
  let h = Heap.create ~cmp:Int.compare () in
  for _i = 1 to initial_size do
    Heap.add h (Data.next data);
  done;
  Bench.Test.create ~name:(sprintf "add/remove from heap of size %i (%s)" initial_size
    (Data.name data))
    (fun () ->
      Heap.add h (Data.next data);
      ignore (Heap.pop_exn h))
;;

let heap_sort data size =
  let l =
    let rec loop acc n =
      if n = 0 then acc
      else loop (Data.next data :: acc) (n - 1)
    in
    loop [] size
  in
  Bench.Test.create ~name:(sprintf "sort list of length %i (%s)" size (Data.name data))
    (fun () ->
      let h = Heap.create ~cmp:Int.compare () in
      List.iter l ~f:(fun i -> Heap.add h i);
      try
        let rec loop () =
          ignore (Heap.pop_exn h);
          loop ()
        in
        loop ()
      with
      | _ -> assert (Heap.is_empty h))
;;

let () =
  Bench.bench
    [
      add_remove_from_existing_heap Data.small_range 0;
      add_remove_from_existing_heap Data.small_range 10;
      add_remove_from_existing_heap Data.small_range 1_000;
      add_remove_from_existing_heap Data.small_range 100_000;
      add_remove_from_existing_heap Data.small_range 1_000_000;
      add_remove_from_existing_heap Data.medium_range 0;
      add_remove_from_existing_heap Data.medium_range 10;
      add_remove_from_existing_heap Data.medium_range 1_000;
      add_remove_from_existing_heap Data.medium_range 100_000;
      add_remove_from_existing_heap Data.medium_range 1_000_000;
      add_remove_from_existing_heap Data.large_range 0;
      add_remove_from_existing_heap Data.large_range 10;
      add_remove_from_existing_heap Data.large_range 1_000;
      add_remove_from_existing_heap Data.large_range 100_000;
      add_remove_from_existing_heap Data.large_range 1_000_000;
      add_remove_from_existing_heap Data.ascending 0;
      add_remove_from_existing_heap Data.ascending 10;
      add_remove_from_existing_heap Data.ascending 1_000;
      add_remove_from_existing_heap Data.ascending 100_000;
      add_remove_from_existing_heap Data.ascending 1_000_000;
      heap_sort Data.small_range 10;
      heap_sort Data.small_range 1_000;
      heap_sort Data.small_range 10_000;
      heap_sort Data.small_range 1_000_000;
      heap_sort Data.medium_range 10;
      heap_sort Data.medium_range 1_000;
      heap_sort Data.medium_range 10_000;
      heap_sort Data.medium_range 1_000_000;
      heap_sort Data.large_range 10;
      heap_sort Data.large_range 1_000;
      heap_sort Data.large_range 10_000;
      heap_sort Data.large_range 1_000_000;
      heap_sort Data.ascending 10;
      heap_sort Data.ascending 1_000;
      heap_sort Data.ascending 10_000;
      heap_sort Data.ascending 1_000_000;
    ]
;;
