open Core_bench
open Core

let enqueue_tests =
  List.map [ 10; 1_000_000 ] ~f:(fun n ->
    Bench.Test.create
      ~name:("enqueue " ^ Int.to_string n)
      (fun () ->
        let q = Queue.create () in
        for i = 1 to n do
          Queue.enqueue q i
        done))
;;

(* top unit argument is a cheap guard *)
let enqueue_dequeue_mixed () =
  let test_size = 1_000_000 in
  let seed = Random.State.make [| 1; 2; 3; 4 |] in
  let choices = Array.init test_size ~f:(fun (_ : int) -> Random.State.bool seed) in
  fun () ->
    let q = Queue.create () in
    for i = 1 to test_size do
      Queue.enqueue q i
    done;
    Array.iteri choices ~f:(fun i should_dequeue ->
      if should_dequeue then ignore (Queue.dequeue q : int option) else Queue.enqueue q i)
;;

let queue_pipeline () =
  let q1 = Queue.create () in
  let q2 = Queue.create () in
  let q3 = Queue.create () in
  let q4 = Queue.create () in
  let q5 = Queue.create () in
  fun () ->
    Queue.enqueue q1 1;
    Queue.blit_transfer ~src:q1 ~dst:q2 ();
    Queue.blit_transfer ~src:q2 ~dst:q3 ();
    Queue.blit_transfer ~src:q3 ~dst:q4 ();
    Queue.blit_transfer ~src:q4 ~dst:q5 ();
    ignore (Queue.dequeue_exn q5 : int)
;;

let blit_transfer_tests =
  List.bind [ 0; 1; 2; 4; 8; 16; 32; 64; 128 ] ~f:(fun len ->
    let half_len = len / 2 in
    let src = Queue.create () in
    let dst = Queue.create () in
    for _ = 1 to len do
      Queue.enqueue src 0
    done;
    for _ = 1 to half_len do
      ignore (Queue.dequeue_exn src : int)
    done;
    for _ = 1 to half_len do
      Queue.enqueue src 0
    done;
    [ Bench.Test.create
        ~name:(String.concat [ "blit_transfer "; Int.to_string len ])
        (fun () ->
          Queue.blit_transfer ~src ~dst ();
          Queue.blit_transfer ~src:dst ~dst:src ())
    ])
;;

let tests =
  [ Bench.Test.create ~name:"enqueue_dequeue_mixed" (enqueue_dequeue_mixed ())
  ; Bench.Test.create ~name:"pipeline" (queue_pipeline ())
  ]
  @ blit_transfer_tests
  @ enqueue_tests
  @
  let args = List.init 10 ~f:(fun i -> Float.iround_nearest_exn (2. ** Float.of_int i)) in
  [ Bench.Test.create_indexed ~name:"Queue.enqueue + dequeue" ~args (fun num_elts ->
      let t = Queue.create () in
      for _ = 1 to num_elts do
        Queue.enqueue t ()
      done;
      stage (fun () ->
        Queue.enqueue t ();
        Queue.dequeue_exn t))
  ; Bench.Test.create_indexed
      ~name:"Linked_queue.enqueue + dequeue"
      ~args
      (fun num_elts ->
      let t = Linked_queue.create () in
      for _ = 1 to num_elts do
        Linked_queue.enqueue t ()
      done;
      stage (fun () ->
        Linked_queue.enqueue t ();
        Linked_queue.dequeue_exn t))
  ; Bench.Test.create_indexed ~name:"Deque.enqueue + dequeue" ~args (fun num_elts ->
      let t = Deque.create () in
      for _ = 1 to num_elts do
        Deque.enqueue_front t ()
      done;
      stage (fun () ->
        Deque.enqueue_front t ();
        Deque.dequeue_front_exn t))
  ]
;;

let () = Command_unix.run (Bench.make_command tests)
