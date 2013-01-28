open OUnit;;
open Core.Std
open Array

let rec forever f =
  f ();
  forever f

let to_sorted_list h =
  List.rev (List.init ~f:(fun _ -> Heap.pop_exn h) (Heap.length h))

let random_heap_and_list gen =
  let h = Heap.create compare in
  let random_list = List.init ~f:(fun _ -> gen ()) 9999 in
  List.iter ~f:(fun i -> ignore(Heap.push h i)) random_list;
  (h,random_list)

let test =
  "heap" >:::
    begin
      let float_heap = Heap.of_array ~min_size:1 compare [| 0.; 1.; 2.; 3.; |] in
      let int_heap = Heap.of_array ~min_size:1 compare [| 0; 1; 2; 3; |] in
      let empty_heap = Heap.create compare in
      let random_heap =
        Heap.of_array compare (Array.init 100 ~f:(fun _ -> Random.int 100))
      in
      [ "of_array" >::
          (fun () ->
             "floats" @? Heap.check_heap_property float_heap;
             "ints" @? Heap.check_heap_property int_heap;
          );
        "length" >::
          (fun () ->
             "length=4" @? (Heap.length int_heap = 4);
          );
        "is_empty" >::
          (fun () ->
             "yup" @? Heap.is_empty empty_heap;
             "nope" @? not (Heap.is_empty float_heap)
          );
        "copy" >::
          (fun () ->
             let copied = Heap.copy float_heap in
             "pop_exn" @? (Heap.pop_exn copied = 0.);
             "same" @? (Heap.length float_heap = 4);
          );
        "copy2" >::
          (fun () ->
            let (heap,_) = random_heap_and_list Quickcheck.fg in
            let copy = Heap.copy heap in
            "same" @? (to_sorted_list heap = to_sorted_list copy)
          );
        "top" >::
          (fun () ->
            let (h,l) = random_heap_and_list Quickcheck.uig in
            "foo" @? (match Heap.top h with
                        None -> false
                      | Some t -> t = List.hd_exn (List.sort ~cmp:compare l));
            "didnaepop" @? (Heap.length h = List.length l)
          );
        "pop" >::
          (fun () ->
            let (h,l) = random_heap_and_list Quickcheck.uig in
            "foo" @? (match Heap.pop h with
                        None -> false
                      | Some t -> t = List.hd_exn (List.sort ~cmp:compare l));
            "popped" @? (Heap.length h = List.length l - 1)
          );
        "cond_pop" >::
          (fun () ->
            let h = Heap.of_array ~min_size:1 compare [| -1; 1; 2; 3; |] in
            "dopop" @? (match Heap.cond_pop h (fun i -> i < 0) with
                        None -> false
                      | Some t -> t = -1);
            "afterdopop" @? (Heap.length h = 3);
            "dontpop" @? (match Heap.cond_pop h (fun i -> i < 0) with
                        None -> true
                      | Some _ -> false);
            "afterdontpop" @? (Heap.length h = 3);
            let empty = Heap.create compare in
            "empty" @? (match Heap.cond_pop empty (fun _ -> true) with
                          None -> true
                        | Some _ -> false)
          );
        "search functions" >::
          (fun () ->
             "yup" @? (Heap.mem float_heap 0.);
             "nope" @? not (Heap.mem float_heap 0.5);
             "find" @?
               begin
                 let heap_el = Heap.find_heap_el_exn int_heap 2 in
                 let el = Heap.heap_el_get_el heap_el in
                 2 = el
               end
          );
        "iter" >::
          (fun () ->
            "content differs" @?
              begin
                let h,l = random_heap_and_list Quickcheck.fg in
                (List.sort ~cmp:Float.compare l) = (to_sorted_list h)
              end
          );
        "random heap" >::
          (fun () ->
             "init" @? Heap.check_heap_property random_heap;
             "rest" @?
               begin
                 try forever
                   begin fun () ->
                     let top  = Heap.pop_exn random_heap in
                     let next = Heap.top_exn random_heap in
                     if top > next then raise Exit
                   end
                 with
                 | Heap.Empty -> true
                 | Exit -> false
               end
          );
        "update" >::
          (fun () ->
             "all" @? (
               let int_heap = Heap.of_array ~min_size:1 compare [| 0; 1; 2; 3; 4; 5; 6 |] in
               let heap_el = Heap.find_heap_el_exn int_heap 1 in
               Heap.update heap_el 5;
               let _ = Heap.pop_exn int_heap in
               let x = Heap.pop_exn int_heap in
               x = 2
             )
          );
        "sort" >::
          (fun () ->
            "randomints" @? (
              let (h,l) = random_heap_and_list Quickcheck.uig in
              to_sorted_list h = List.sort ~cmp:compare l
            );
            "randomfloats" @? (
              let (h,l) = random_heap_and_list Quickcheck.fg in
              to_sorted_list h = List.sort ~cmp:compare l
            )
          )
      ]
    end
