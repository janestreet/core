open! Core

let%test_module "Thread_safe_queue" =
  (module (struct

    let () = Debug.should_print_backtrace := false

    open Thread_safe_queue

    type nonrec 'a t = 'a t

    let invariant = invariant

    let sexp_of_t = sexp_of_t

    let%test_unit _ = ignore (create () |> [%sexp_of: unit t] : Sexp.t)

    let%test_unit _ =
      let t = create () in
      enqueue t 1;
      enqueue t 2;
      enqueue t 3;
      ignore (t |> [%sexp_of: int t] : Sexp.t)
    ;;

    let create      = create
    let dequeue_exn = dequeue_exn
    let enqueue     = enqueue
    let length      = length

    let%test_unit _ =
      let t = create () in
      invariant ignore t;
      assert (length t = 0);
      enqueue t ();
      invariant ignore t;
      assert (length t = 1);
      enqueue t ();
      invariant ignore t;
      assert (length t = 2);
      dequeue_exn t;
      invariant ignore t;
      assert (length t = 1);
      dequeue_exn t;
      invariant ignore t;
      assert (length t = 0)
    ;;

    let%test_unit _ = (* invariant passes with element from pool in the queue *)
      let t = create () in
      for _ = 1 to 3 do
        enqueue t ();
      done;
      for _ = 1 to 2 do
        dequeue_exn t;
      done;
      enqueue t ();
      invariant ignore t
    ;;

    let%test_unit _ =
      let verbose = false in
      let sec = Time.Span.of_sec in
      let quick_pause () = Time.pause (sec 0.00001) in
      let num_elts = 100_000 in
      let batch_size = 10_000 in
      for num_enqueuers = 1 to 1 do
        for num_dequeuers = 1 to 1 do
          if verbose
          then Debug.eprints "testing" (num_enqueuers, num_dequeuers) [%sexp_of: int * int];
          let enqueue_counts = Array.create ~len:num_enqueuers 0 in
          let dequeue_counts = Array.create ~len:num_dequeuers 0 in
          let t = create () in
          let all_threads = ref [] in
          let create_thread f =
            let thread = Thread.create f () in
            all_threads := thread :: !all_threads;
          in
          let num_enqueues = ref 0 in
          let num_dequeues = ref 0 in
          create_thread (fun () ->
            while !num_enqueues < num_elts || !num_dequeues < num_elts do
              if verbose
              then Debug.eprints "current" (!num_enqueues, !num_dequeues)
                     [%sexp_of: int * int];
              Time.pause (sec 1.);
            done);
          for i = 0 to num_enqueuers - 1 do
            create_thread (fun () ->
              let num_in_batch = ref 0 in
              while !num_enqueues < num_elts do
                num_in_batch := 0;
                while !num_enqueues < num_elts && !num_in_batch < batch_size do
                  incr num_in_batch;
                  incr num_enqueues;
                  enqueue_counts.( i ) <- enqueue_counts.( i ) + 1;
                  enqueue t ();
                done;
                quick_pause ();
              done);
          done;
          for i = 0 to num_dequeuers - 1 do
            create_thread (fun () ->
              let num_in_batch = ref 0 in
              while !num_dequeues < num_elts do
                num_in_batch := 0;
                while !num_dequeues < num_elts && !num_in_batch < batch_size do
                  if length t = 0
                  then quick_pause ()
                  else begin
                    dequeue_exn t;
                    incr num_dequeues;
                    dequeue_counts.( i ) <- dequeue_counts.( i ) + 1;
                  end;
                done;
              done);
          done;
          List.iter !all_threads ~f:Thread.join;
          if verbose
          then Debug.eprints "counts" (enqueue_counts, dequeue_counts)
                 [%sexp_of: int array * int array];
        done;
      done
    ;;

    let clear_internal_pool = clear_internal_pool

    let%test_unit _ =
      let t = create () in
      clear_internal_pool t;
      enqueue t ();
      clear_internal_pool t;
      dequeue_exn t;
      clear_internal_pool t;
      enqueue t ();
      clear_internal_pool t
    ;;
  end
           (* This signature constraint is here to remind us to add a unit test whenever the
              interface to [Thread_safe_queue] changes. *)
           : module type of Thread_safe_queue))
