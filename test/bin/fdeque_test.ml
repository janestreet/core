open Core
open OUnit;;
open Fdeque

let lpush l q =
  List.fold ~init:q ~f:enqueue_back l

let inv = invariant ignore

let test =
  "fdeque" >:::
  [ "basic" >::
    (fun () ->
       let q0 = lpush [1;2;3;4;5] empty in
       let _ ,q1 = dequeue_front_exn q0 in
       let _ ,q2 = dequeue_front_exn q1 in
       let q3 = enqueue_back q2 0 in
       inv q0;
       inv q1;
       inv q2;
       inv q3;
       "len0" @? (length q0 = 5);
       "len1" @? (length q1 = 4);
       "len2" @? (length q2 = 3);
       "len3" @? (length q3 = 4);
       "q0" @? (to_list q0 = [1;2;3;4;5]);
       "q1" @? (to_list q1 = [2;3;4;5]);
       "q2" @? (to_list q2 = [3;4;5]);
       "q3" @? (to_list q3 = [3;4;5;0]);
       "peek_front_exn3" @? (peek_front_exn q3 = 3);
       "peek_back_exn3" @? (peek_back_exn q3 = 0);
       "drop_front_exn" @? (to_list (drop_front_exn q0) = to_list q1);
       "notempty3" @? not (is_empty q3);
       "empty3" @? is_empty
                     (drop_front_exn
                        (drop_front_exn
                           (drop_front_exn
                              (drop_front_exn q3))));
       "misc" @? (to_list (lpush [2;3]
                             (drop_front_exn
                                (drop_front_exn
                                   (drop_front_exn
                                      (drop_front_exn q3)))))
                  = [2;3]);
    );
    "invariants and list bisimulation" >::
    (fun () ->
       let rec drop_last = function
         | []   -> assert false
         | [_]  -> []
         | x::l -> x :: drop_last l
       in
       ignore (List.fold (List.range 0 1000) ~init:(empty,[]) ~f:(fun (q,l) i ->
         let q,l =
           match Random.int 9 with
           | 0 | 1 -> enqueue_back q i, l @ [i]
           | 2 | 3 -> enqueue_front q i, [i] @ l
           | 4 ->
             (match dequeue_front q with
              | None -> q,l
              | Some (_,q) -> q, List.tl_exn l)
           | 5 ->
             (match drop_front_exn q with
              | q -> q, List.tl_exn l
              | exception _ -> q,l)
           | 6 ->
             (match dequeue_back q with
              | None -> q,l
              | Some (_,q) -> q, drop_last l)
           | 7 ->
             (match drop_back_exn q with
              | q -> q, drop_last l
              | exception _ -> q,l)
           | 8 -> rev q, List.rev l
           | _ -> q,l
         in
         inv q;
         assert (List.equal l (to_list q) ~equal:Int.equal);
         q,l)))
  ]

