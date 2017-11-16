open! Core

module Bench = Core_bench.Bench

let () =
  let d = Deque.create () in
  Deque.enqueue_front d ();
  Bench.bench
    [ Bench.Test.create ~name:"dequeue_push_pop" (fun () ->
        for _ = 1 to 10 do
          Deque.enqueue_front d ();
          Deque.dequeue_front_exn d;
        done);
    ]
;;
