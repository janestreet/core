open Core.Std
open Core_bench.Std

(** A faster alternative implementation of heap. *)
module Array_heap = struct
  type 'a t = {
    lt        : 'a -> 'a -> bool;
    fa        : bool;
    mutable a : 'a array;
    mutable l : int;
  }

  let create ~lt size init =
    let a = Array.create ~len:size init in
    let fa = Obj.tag (Obj.repr a) = Obj.double_array_tag in
    { a; fa; l = 0; lt }

  let swap t i j =
    let a : int array = Obj.magic t.a in
    let e = Array.unsafe_get a i in
    Array.unsafe_set a i (Array.unsafe_get a j);
    Array.unsafe_set a j e

  let down_heap t =
    let rec loop t i l =
      let ir = (i + 1) lsl 1 in
      let il = ir - 1 in
      let a : int array = Obj.magic t.a in
      if ir < l then begin
        let e  : 'a = Obj.magic (Array.unsafe_get a i ) in
        let el : 'a = Obj.magic (Array.unsafe_get a il) in
        let er : 'a = Obj.magic (Array.unsafe_get a ir) in
        let lt = t.lt in
        if lt el er then begin
          if lt el e then begin
            swap t i il;
            loop t il l
          end
        end else if lt er e then begin
          swap t i ir;
          loop t ir l
        end
      end else if il < t.l then begin
        let e  : 'a = Obj.magic (Array.unsafe_get a i ) in
        let el : 'a = Obj.magic (Array.unsafe_get a il) in
        if t.lt el e then
          swap t i il
      end
    in
    let rec loop_fa t i l =
      let ir = (i + 1) lsl 1 in
      let il = ir - 1 in
      let a : float array = Obj.magic t.a in
      if ir < l then begin
        let e  : 'a = Obj.magic (Array.unsafe_get a i ) in
        let el : 'a = Obj.magic (Array.unsafe_get a il) in
        let er : 'a = Obj.magic (Array.unsafe_get a ir) in
        let lt = t.lt in
        if lt el er then begin
          if lt el e then begin
            swap t i il;
            loop_fa t il l
          end
        end else if lt er e then begin
          swap t i ir;
          loop_fa t ir l
        end
      end else if il < t.l then begin
        let e  : 'a = Obj.magic (Array.unsafe_get a i ) in
        let el : 'a = Obj.magic (Array.unsafe_get a il) in
        if t.lt el e then
          swap t i il
      end
    in
    if t.fa then
      loop_fa t 0 t.l
    else
      loop t 0 t.l

  let up_heap t =
    let rec loop t i =
      let i' = (i - 1) lsr 1 in
      let a : int array = Obj.magic t.a in
      let e  : 'a = Obj.magic (Array.unsafe_get a i)  in
      let e' : 'a = Obj.magic (Array.unsafe_get a i') in
      if t.lt e e' then begin
        swap t i i';
        if i' > 0 then
          loop t i'
      end
    in
    let rec loop_fa t i =
      let i' = (i - 1) lsr 1 in
      let a : float array = Obj.magic t.a in
      let e  : 'a = Obj.magic (Array.unsafe_get a i)  in
      let e' : 'a = Obj.magic (Array.unsafe_get a i') in
      if t.lt e e' then begin
        swap t i i';
        if i' > 0 then
          loop_fa t i'
      end
    in
    let l = t.l - 1 in
    if l > 0 then begin
      if t.fa then
        loop_fa t l
      else
        loop t l
    end

  let add t x =
    let l = t.l in
    let l' = l + 1 in
    if l' > Array.length t.a then
      failwith "Heap full";
    Array.unsafe_set t.a l x;
    t.l <- l';
    up_heap t

  let pop_exn t =
    let l = t.l in
    if l = 0 then
      failwith "Heap empty";
    let a = t.a in
    let x = Array.unsafe_get a 0 in
    let l = l - 1 in
    Array.unsafe_set a 0 (Array.unsafe_get a l);
    t.l <- l;
    down_heap t;
    x

  let top_exn t =
    let l = t.l in
    if l = 0 then
      failwith "Heap empty";
    Array.unsafe_get t.a 0

  let is_empty t =
    t.l = 0
end
module Heap = Array_heap

module Data = struct
  type t = {
    name : string;
    t : int array;
    l : int;
    mutable pos : int;
  }

  let range name high =
    let l = (1024 * 1024) - 1 in
    let t = Array.create ~len:(l + 1) 0 in
    for i = 0 to l do
      t.(i) <- Random.int high;
    done;
    { name; t; l; pos = 0 }
  ;;

  let small_range  = range "small range" 10
  let medium_range = range "medium range" 100
  let large_range  = range "large range" 1_000_000

  let ascending =
    let l = (1024 * 1024) - 1 in
    let t = Array.create ~len:(l + 1) 0 in
    for i = 0 to Array.length t - 1 do
      t.(i) <- i;
    done;
    { name = "ascending"; t; l; pos = 0 }
  ;;

  let next t =
    let pos = t.pos in
    let n = Array.unsafe_get t.t pos in
    t.pos <- (pos + 1) land t.l;
    n
  ;;

  let name t = t.name
end

let add_remove_from_existing_heap data initial_size =
  let h = Heap.create ~lt:(fun (x : int) (x' : int) -> x < x') (initial_size + 1) 0 in
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
      let h = Heap.create ~lt:(fun (x : int) (x' : int) -> x < x') size 0 in
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

(*let () =
  let n = Int.of_string Sys.argv.(1) in
  let r = Array.init n ~f:(fun _ -> Random.int n) in
  let ah = Array_heap.create ~lt:(fun (x : int ref) (y : int ref) -> !x < !y) n (ref 0) in
  for i = 0 to n - 1 do
    Array_heap.add ah (ref r.(i))
  done;
  let array_heap_test = Bench.Test.create ~name:"Array_heap" (fun () ->
    let r = !(ref r) in
    let ah = !(ref ah) in
    let last = ref 0 in
    for i = 0 to Array.length r - 1 do
      let top = Array_heap.pop_exn ah in
      assert (!top >= !last);
      last := !top;
      top := !top + Array.unsafe_get r i;
      Array_heap.add ah top
    done)
  in
  let h = Heap.create ~cmp:(fun x y -> Int.compare !x !y) () in
  for i = 0 to Array.length r - 1 do
    Heap.add h (ref r.(i));
  done;
  let heap_test = Bench.Test.create ~name:"Heap" (fun () ->
    let r = !(ref r) in
    let h = !(ref h) in
    for i = 0 to Array.length r - 1 do
      let top = Heap.pop_exn h in
      top := !top + Array.unsafe_get r i;
      Heap.add h top
    done)
  in
  Bench.bench
    ~run_config:(Bench.Run_config.create  ~time_quota:(Time.Span.of_sec 1.) ())
    [array_heap_test; heap_test]*)
