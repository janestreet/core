open! Core
open! Nano_mutex

let%test_unit _ =
  let l = create () in
  lock_exn l;
  unlock_exn l;
  match try_lock l with
  | Ok `Not_acquired | Error _ -> assert false
  | Ok `Acquired -> unlock_exn l
;;

let%test_unit _ =
  List.iter
    ([ 2, 100, 0.; 10, 100, 0.; 10, 100, 0.001 ]
     @
     if Sys.word_size = 32
     then [] (* not enough address space when the stack limit is high *)
     else [ 100, 10, 0.001 ])
    ~f:(fun (num_threads, num_iterations, pause_for) ->
      try
        let l = create () in
        let am_holding_lock = ref false in
        let one_thread () =
          Thread.create
            ~on_uncaught_exn:`Print_to_stderr
            (fun () ->
               for _ = 1 to num_iterations do
                 lock_exn l;
                 if !am_holding_lock then failwith "lock multiply acquired";
                 am_holding_lock := true;
                 ignore (Unix.nanosleep pause_for : float);
                 am_holding_lock := false;
                 unlock_exn l
               done)
            ()
        in
        let threads = List.init num_threads ~f:(fun _ -> one_thread ()) in
        List.iter threads ~f:Thread.join
      with
      | exn ->
        failwiths
          ~here:[%here]
          "test failed"
          (num_threads, num_iterations, pause_for, exn)
          [%sexp_of: int * int * float * exn])
;;
