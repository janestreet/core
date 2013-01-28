(* Test Condition.timedwait *)

(*
  This only tests the pthread_cond_timedwait wrapper and not the underlying
  unix function. Both branches (timeout, success) are tested; all the possible
  race conditions are assumed to be handled properly by pthread and not tested.
*)
open Core.Std
open OUnit

let run () =
  let v_mtx = Mutex.create ()
  and cnd_v_is_true = Condition.create ()
  and v = ref false in

  let v_setter () =
    Thread.delay 0.1;
    Mutex.lock v_mtx;
    v := true;
    Condition.signal cnd_v_is_true;
    Mutex.unlock v_mtx;
  in

  let wait_for_v tmout =
    let timeout = Unix.gettimeofday () +. tmout in
    not (Condition.timedwait cnd_v_is_true v_mtx (Time.of_float timeout))
  in

  Mutex.lock v_mtx;

  (* This condition wait is expected to timeout. *)
  begin
    let timedout = wait_for_v 0.1 in
    assert timedout
  end;

  ignore (Thread.create v_setter ():Thread.t);

  (* Now we have a thread that sets the condition so we expect to not timeout.
  *)
  begin
    let timedout = wait_for_v 0.5 in
    assert (not timedout);
    assert !v
  end



let test = "Condition_test" >::: [
  "test" >:: (fun () ->
    "1" @? (try run (); true with e ->
              eprintf "in cond\
 ition test:%s\n%!" (Exn.to_string e);
              false));
]
