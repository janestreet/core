open Core
open Core_bench.Std

module Data = struct
  type t =
    { name : string
    ; mutable index : int
    ; data : int array
    }

  let range name high =
    let data = Array.init 1_000_000 ~f:(fun _ -> Random.int high) in
    { name; index = 0; data }
  ;;

  let small_range  = range "small range" 10
  let medium_range = range "medium range" 100
  let large_range  = range "large range" 1_000_000

  let ascending =
    let len = 1_000_000 in
    let data = Array.init len ~f:(fun i -> len - i) in
    { name = "ascending"; index = len - 1; data }
  ;;

  let next t =
    let n = t.data.(t.index) in
    t.index <- t.index - 1;
    if t.index < 0 then t.index <- Array.length t.data - 1;
    n
  ;;

  let name t = t.name

  let reset t = t.index <- Array.length t.data - 1
end

let add_remove_from_existing_heap data initial_size =
  let h = Heap.create ~cmp:Int.compare () in
  for _ = 1 to initial_size do
    Heap.add h (Data.next data);
  done;
  Bench.Test.create ~name:(sprintf "add/remove from heap of size %i (%s)" initial_size
                             (Data.name data))
    (fun () ->
       Heap.add h (Data.next data);
       ignore (Heap.pop_exn h))
;;

let heap_sort data size =
  Bench.Test.create ~name:(sprintf "sort list of length %i (%s)" size (Data.name data))
    (fun () ->
       Data.reset data;
       let h = Heap.create ~cmp:Int.compare () in
       for _ = 1 to size do
         Heap.add h (Data.next data);
       done;
       for _ = 1 to size do
         ignore (Heap.pop_exn h);
       done;
       assert (Heap.is_empty h))
;;

let () =
  Command.run
    (Bench.make_command
       [
         add_remove_from_existing_heap Data.small_range 0;
         add_remove_from_existing_heap Data.small_range 10;
         add_remove_from_existing_heap Data.small_range 1_000;
         add_remove_from_existing_heap Data.small_range 100_000;

         add_remove_from_existing_heap Data.medium_range 0;
         add_remove_from_existing_heap Data.medium_range 10;
         add_remove_from_existing_heap Data.medium_range 1_000;
         add_remove_from_existing_heap Data.medium_range 100_000;

         add_remove_from_existing_heap Data.large_range 0;
         add_remove_from_existing_heap Data.large_range 10;
         add_remove_from_existing_heap Data.large_range 1_000;
         add_remove_from_existing_heap Data.large_range 100_000;

         add_remove_from_existing_heap Data.ascending 0;
         add_remove_from_existing_heap Data.ascending 10;
         add_remove_from_existing_heap Data.ascending 1_000;
         add_remove_from_existing_heap Data.ascending 100_000;

         heap_sort Data.small_range 10;
         heap_sort Data.small_range 1_000;
         heap_sort Data.small_range 100_000;

         heap_sort Data.medium_range 10;
         heap_sort Data.medium_range 1_000;
         heap_sort Data.medium_range 100_000;

         heap_sort Data.large_range 10;
         heap_sort Data.large_range 1_000;
         heap_sort Data.large_range 100_000;

         heap_sort Data.ascending 10;
         heap_sort Data.ascending 1_000;
         heap_sort Data.ascending 100_000;
       ])
;;
