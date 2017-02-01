open OUnit
open Core
open Queue

let test =
  "queue" >:::
  [
    "create" >::
    (fun () ->
       let t = create () in
       assert (is_empty t);
    );
    "container" >::
    (fun () ->
       let module T = Container_test.Test_S1 (Queue) in
       T.test ();
    );
    "enqueue" >::
    (fun () ->
       let t = create () in
       enqueue t 1;
       enqueue t 2;
       enqueue t 3;
       assert (to_list t = [1; 2; 3]);
       assert (peek t = Some 1);
       assert (dequeue t = Some 1);
       assert (to_list t = [2; 3]);
       assert (peek t = Some 2);
       assert (dequeue_exn t = 2);
       assert (to_list t = [3]);
       assert (peek_exn t = 3);
       assert (dequeue t = Some 3);
       assert (to_list t = []);
       assert (peek t = None);
       assert (dequeue t = None);
    );
    "clear" >::
    (fun () ->
       for i = 0 to 5 do
         let t = of_list (List.init i ~f:ident) in
         clear t;
         assert (is_empty t);
       done
    );
    "copy" >::
    (fun () ->
       let t = of_list [1; 2; 3] in
       let t' = copy t in
       ignore (dequeue t);
       assert (to_list t = [2; 3]);
       assert (to_list t' = [1; 2; 3]);
       ignore (clear t');
       assert (to_list t = [2; 3]);
       assert (to_list t' = []);
    );
    "blit_transfer" >::
    (fun () ->
       for i1 = 0 to 3 do
         let l1 = List.init i1 ~f:ident in
         for i2 = 0 to 3 do
           let l2 = List.init i2 ~f:ident in
           let t1 = of_list l1 in
           let t2 = of_list l2 in
           blit_transfer ~src:t1 ~dst:t2 ();
           assert (is_empty t1);
           assert (to_list t2 = l2 @ l1);
         done
       done
    );
    "filter_inplace" >::
    (fun () ->
       let t = of_list [1; 2; 3; 4; 5; 6; 7] in
       filter_inplace t ~f:(fun n -> n mod 2 = 0);
       assert (to_list t = [2; 4; 6]);
       assert (for_all t ~f:(fun n -> n mod 2 = 0));
       assert (exists t ~f:(fun n -> n = 4 || n = 6));
       filter_inplace t ~f:(fun n -> n mod 2 <> 0);
       assert (to_list t = []);
    );
  ]
