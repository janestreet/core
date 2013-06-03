open Core.Std  let _ = _squelch_unused_module_warning_

module Bench = Core_extended.Deprecated_bench

let () =
  let d = Dequeue.create () in
  Dequeue.enqueue_front d ();
  Bench.bench
    [ Bench.Test.create ~name:"dequeue_push_pop" (fun () ->
        for _i = 1 to 10 do
          Dequeue.enqueue_front d ();
          Dequeue.dequeue_front_exn d;
        done);
    ]
;;
