(* Test Condition.timedwait *)

(*
   This only tests the pthread_cond_timedwait wrapper and not the underlying
   unix function. Both branches (timeout, success) are tested; all the possible
   race conditions are assumed to be handled properly by pthread and not tested.
*)
open Core
open OUnit

let rec run ~num_retries =
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
    not (Condition.timedwait cnd_v_is_true v_mtx (Time.of_span_since_epoch (Time.Span.of_sec timeout)))
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
    let rec wait ~num_wait_retries =
      if num_wait_retries = 0 then
        failwithf "condition_test repeatedly failed %d times" num_wait_retries ()
      else
        let timed_out = wait_for_v 0.5 in
        match !v, timed_out with
        | true, false ->
          (* The expected case.  Yay. *)
          ()
        | false, false ->
          (* Should be impossible to get here, since [v_setter] set [v] before signaling. *)
          assert false
        | false, true ->
          (* We timed out.  Maybe machine is loaded.  Try again. *)
          wait ~num_wait_retries:(num_wait_retries - 1)
        | true, true ->
          (* We timed out and the variable was set.  Could happen due to a race.  Try the
             whole experiment again.  *)
          run ~num_retries:(num_retries - 1)
    in
    wait ~num_wait_retries:10
  end
;;


let test = "Condition_test" >::: [
  "test" >:: (fun () ->
    "1" @? (try run ~num_retries:5; true with e ->
      eprintf "in cond\
               ition test:%s\n%!" (Exn.to_string e);
      false));
]
