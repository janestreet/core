open Core_kernel.Std

include Gc

let read_finaliser_queue, write_finaliser_queue =
  Thread_safe_queue.create' ()
;;

let maybe_start_finaliser_thread =
  let module M = Nano_mutex in
  let mutex = M.create () in
  let started = ref false in
  let start_finaliser_thread () =
    ignore (Thread.create (fun () -> Fn.forever (fun () ->
      match read_finaliser_queue () with
      | None -> Thread.delay 1.0
      | Some f -> Exn.handle_uncaught ~exit:false f)) ())
  in
  (fun () ->
    if not !started then (* performance hack! *)
      M.critical_section mutex ~f:(fun () ->
        if not !started then
          (started := true; start_finaliser_thread ())))
;;

(* Ocaml permits finalisers to be run in any thread and at any time after the object
   becomes unreachable -- they are essentially concurrent.  This changes forces all
   finaliser code to run sequentially and in a fixed thread. *)
let add_finalizer x f =
  maybe_start_finaliser_thread ();
  let finaliser v = write_finaliser_queue (fun () -> f v) in
  Caml.Gc.finalise finaliser x;
;;

(* [add_finalizer_exn] is the same as [add_finalizer].  However, their types in
   core_gc.mli are different, and the type of [add_finalizer] guarantees that it always
   receives a heap block, which ensures that it will not raise, while [add_finalizer_exn]
   accepts any type, and so may raise. *)
let add_finalizer_exn = add_finalizer

let finalise_release = Caml.Gc.finalise_release
