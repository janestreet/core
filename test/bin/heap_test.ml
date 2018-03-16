open OUnit;;
open Core

let rec forever f =
  f ();
  forever f

let to_sorted_list h =
  List.rev (List.init ~f:(fun _ -> Heap.pop_exn h) (Heap.length h))

let random_heap_and_list gen =
  let h = Heap.create ~cmp:compare () in
  let random_list = List.init ~f:(fun _ -> gen ()) 9999 in
  List.iter ~f:(fun i -> ignore(Heap.add h i)) random_list;
  (h,random_list)

let test =
  "heap" >:::
  begin
    let float_heap () = Heap.of_array [| 0.; 1.; 2.; 3.; |] ~cmp:compare in
    let int_heap ()   = Heap.of_array [| 0; 1; 2; 3; |] ~cmp:compare in
    let empty_heap    = Heap.create ~cmp:compare () in
    let random_heap   =
      Heap.of_array (Array.init 100 ~f:(fun _ -> Random.int 100)) ~cmp:compare
    in
    [
      "length" >::
      (fun () ->
         "length=4" @? (Heap.length (int_heap ()) = 4);
      );
      "is_empty" >::
      (fun () ->
         "yup" @? Heap.is_empty empty_heap;
         "nope" @? not (Heap.is_empty (float_heap ()))
      );
      "top" >::
      (fun () ->
         let (h,l) = random_heap_and_list Quickcheck_deprecated.uig in
         "foo" @? (match Heap.top h with
             None -> false
           | Some t -> t = List.hd_exn (List.sort ~compare l));
         "didnaepop" @? (Heap.length h = List.length l)
      );
      "pop" >::
      (fun () ->
         let (h,l) = random_heap_and_list Quickcheck_deprecated.uig in
         "foo" @? (match Heap.pop h with
             None -> false
           | Some t -> t = List.hd_exn (List.sort ~compare l));
         "popped" @? (Heap.length h = List.length l - 1)
      );
      "pop_if" >::
      (fun () ->
         let h = Heap.of_array [| -1; 1; 2; 3; |] ~cmp:compare in
         "dopop" @? (match Heap.pop_if h (fun i -> i < 0) with
             None -> false
           | Some t -> t = -1);
         "afterdopop" @? (Heap.length h = 3);
         "dontpop" @? (match Heap.pop_if h (fun i -> i < 0) with
             None -> true
           | Some _ -> false);
         "afterdontpop" @? (Heap.length h = 3);
         let empty = Heap.create ~cmp:compare () in
         "empty" @? (match Heap.pop_if empty (fun _ -> true) with
             None -> true
           | Some _ -> false)
      );
      "search functions" >::
      (fun () ->
         "yup" @? (Heap.mem (float_heap ()) 0. ~equal:Float.equal);
         "nope" @? not (Heap.mem (float_heap ()) 0.5 ~equal:Float.equal);
         "find" @?
         begin
           let el =
             Option.value_exn (Heap.find (int_heap ()) ~f:(fun e -> e = 2))
           in
           2 = el
         end
      );
      "iter" >::
      (fun () ->
         "content differs" @?
         begin
           let h,l = random_heap_and_list Quickcheck_deprecated.fg in
           (List.sort ~compare:Float.compare l) = (to_sorted_list h)
         end
      );
      "random heap" >::
      (fun () ->
         "rest" @?
         begin
           try forever
                 begin fun () ->
                   let top  = Heap.pop_exn random_heap in
                   let next = Heap.top_exn random_heap in
                   if top > next then raise Exit
                 end
           with
           | _ ->
             if Heap.is_empty random_heap
             then true
             else false
         end
      );
      "sort" >::
      (fun () ->
         "randomints" @? (
           let (h,l) = random_heap_and_list Quickcheck_deprecated.uig in
           to_sorted_list h = List.sort ~compare l
         );
         "randomfloats" @? (
           let (h,l) = random_heap_and_list Quickcheck_deprecated.fg in
           to_sorted_list h = List.sort ~compare l
         )
      )
    ]
  end
