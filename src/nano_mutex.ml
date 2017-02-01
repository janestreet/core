open! Import

let ok_exn = Or_error.ok_exn

(* A [Blocker.t] is an ordinary mutex and conditional variable used to implement blocking
   when there is lock contention. *)
module Blocker : sig
  type t [@@deriving sexp_of]

  val create : unit -> t
  val critical_section : t -> f:(unit -> 'a) -> 'a
  val wait : t -> unit
  val signal : t -> unit

  val save_unused : t -> unit
end = struct
  (* Our use of mutexes is always via [Mutex.critical_section], so that we always lock
     them and unlock them from a single thread.  So, we use [Mutex0], which is
     error-checking mutexes, which will catch any use that is not what we expect. *)
  module Condition = Condition
  module Mutex = Mutex0

  type t =
    { mutex : Mutex.t sexp_opaque;
      condition : Condition.t sexp_opaque;
    }
  [@@deriving sexp_of]

  (* We keep a cache of unused blockers, since they are relatively costly to create, and
     we should never need very many simultaneously.  We should never need more blockers
     than the number of nano mutexes being simultaneously blocked on, which of course is
     no more than the total number of simultaneous threads. *)
  let unused : t Thread_safe_queue.t = Thread_safe_queue.create ()

  (* [save_unused t] should be called when [t] is no longer in use, so it can be returned
     by a future call of [create]. *)
  let save_unused t = Thread_safe_queue.enqueue unused t

  let create () =
    if Thread_safe_queue.length unused > 0
    then Thread_safe_queue.dequeue_exn unused
    else { mutex = Mutex.create (); condition = Condition.create () }
  ;;

  let critical_section t ~f = Mutex.critical_section t.mutex ~f

  let wait t = Condition.wait t.condition t.mutex

  let signal t = Condition.signal t.condition
end

(* We represent a nano mutex using an OCaml record.  The [id_of_thread_holding_lock] field
   represents whether the mutex is locked or not, and if it is locked, which thread holds
   the lock.  We do not use an [int option] for performance reasons (doing so slows down
   lock+unlock by a factor of almost two).  Instead, we have [id_of_thread_holding_lock =
   bogus_thread_id] when the mutex is unlocked.

   The mutex record has an optional [blocker] field for use when the mutex is contended.
   We use the OS-level condition variable in [blocker] to [wait] in a thread that desires
   the lock and to [signal] from a thread that is releasing it.

   When thinking about the implementation, it is helpful to remember the following
   desiderata:

 * Safety -- only one thread can acquire the lock at a time.  This is accomplished
   usng a test-and-set to set [id_of_thread_holding_lock].

 * Liveness -- if the mutex is unlocked and some threads are waiting on it, then one of
   those threads will be woken up and given a chance to acquire it.  This is accomplished
   by only waiting when we can ensure that there will be a [signal] of the condition
   variable in the future.  See the more detailed comment in [lock].

 * Performance -- do not spin trying to acquire the lock.  This is accomplished by
   waiting on a condition variable if a lock is contended. *)
type t =
  { mutable id_of_thread_holding_lock : int;
    mutable num_using_blocker : int;
    mutable blocker : Blocker.t option;
  }
[@@deriving fields, sexp_of]

let invariant t =
  try
    assert (t.num_using_blocker >= 0);
    (* It is the case that if [t.num_using_blocker = 0] then [Option.is_none t.blocker],
       however the converse does not necessarily hold.  The code in [with_blocker] doesn't
       take care to atomically increment [t.num_using_blocker] and set [t.blocker] to
       [Some].  It could, but doing so is not necessary for the correctness of of
       [with_blocker], which only relies on test-and-set of [t.blocker] to make sure
       there is an agreed-upon winner in the race to create a blocker. *)
    if t.num_using_blocker = 0 then assert (Option.is_none t.blocker);
  with exn -> failwiths "invariant failed" (exn, t) [%sexp_of: exn * t]
;;

let equal (t : t) t' = phys_equal t t'

let bogus_thread_id = -1

let create () =
  { id_of_thread_holding_lock = bogus_thread_id;
    num_using_blocker = 0;
    blocker = None;
  }
;;

let is_locked t = t.id_of_thread_holding_lock <> bogus_thread_id

let current_thread_id () = Thread.id (Thread.self ())

let current_thread_has_lock t = t.id_of_thread_holding_lock = current_thread_id ()

let recursive_lock_error t =
  Error.create "attempt to lock mutex by thread already holding it"
    (current_thread_id (), t) [%sexp_of: int * t]
;;

let try_lock t =
  (* The following code relies on an atomic test-and-set of [id_of_thread_holding_lock],
     so that there is a definitive winner in a race between multiple lockers and everybody
     agrees who acquired the lock. *)
  let current_thread_id = current_thread_id () in
  (* BEGIN ATOMIC *)
  if t.id_of_thread_holding_lock = bogus_thread_id then begin
    t.id_of_thread_holding_lock <- current_thread_id;
    (* END ATOMIC *)
    Ok `Acquired;
  end else if current_thread_id = t.id_of_thread_holding_lock then
    Error (recursive_lock_error t)
  else
    Ok `Not_acquired
;;

let try_lock_exn t = ok_exn (try_lock t)

(* [with_blocker t f] runs [f blocker] in a critical section.  It allocates a blocker for
   [t] if [t] doesn't already have one. *)
let with_blocker t f =
  t.num_using_blocker <- t.num_using_blocker + 1;
  let blocker =
    match t.blocker with
    | Some blocker -> blocker
    | None ->
      let new_blocker = Blocker.create () in
      (* We allocate [new_blocker_opt] here because one cannot allocate inside an atomic
         region. *)
      let new_blocker_opt = Some new_blocker in
      let blocker =
        (* We need the following test-and-set to be atomic so that there is a definitive
           winner in a race between multiple calls to [with_blocker], so that everybody
           agrees what the underlying [blocker] is. *)
        (* BEGIN ATOMIC *)
        match t.blocker with
        | Some blocker -> blocker
        | None -> t.blocker <- new_blocker_opt; new_blocker
        (* END ATOMIC *)
      in
      if not (phys_equal blocker new_blocker) then Blocker.save_unused new_blocker;
      blocker
  in
  protect ~f:(fun () -> Blocker.critical_section blocker ~f:(fun () -> f blocker))
    ~finally:(fun () ->
      (* We need the following decrement-test-and-set to be atomic so that we're sure that
         the last user of blocker clears it. *)
      (* BEGIN ATOMIC *)
      t.num_using_blocker <- t.num_using_blocker - 1;
      if t.num_using_blocker = 0 then begin
        t.blocker <- None;
        (* END ATOMIC *)
        Blocker.save_unused blocker;
      end)
;;

let rec lock t =
  (* The following code relies on an atomic test-and-set of [id_of_thread_holding_lock],
     so that there is a definitive winner in a race between multiple [lock]ers, and
     everybody agrees who acquired the lock.

     If [is_locked t], we block the locking thread using [Blocker.wait], until some
     unlocking thread [Blocker.signal]s us.  There is a race between the [wait] and the
     [signal].  If the unlocking thread signals in between our test of
     [t.id_of_thread_holding_lock] and our [wait], then our [wait] could miss the signal
     and block forever.  We avoid this race by committing to waiting inside a
     [with_blocker], which increments [t.num_using_blocker].  If the [signal] occurs
     before the [with_blocker], then it will have cleared [t.id_of_thread_holding_lock],
     which we will notice as [not (is_locked t)], and then not [wait], and loop trying to
     [lock] again.  Otherwise, when an [unlock] occurs, it will see that [is_some
     t.blocker], and will enter a critical section on [blocker].  But then it must wait
     until our critical section on [blocker] finishes, and hence until our call to [wait]
     finishes.  Hence, the [signal] will occur after the [wait].

     The recursive call to [lock] will not spin.  It happens either because we just lost
     the race with an unlocker, in which case the subsequent [lock] will succeed, or
     we actually had to block because someone is holding the lock.  The latter is the
     overwhelmingly common case.

     Other threads can change [t.id_of_thread_holding_lock] concurrently with this code.
     However, no other thread can set it to our [current_thread_id], since threads only
     ever set [t.id_of_thread_holding_lock] to their current thread id, or clear it. *)
  let current_thread_id = current_thread_id () in
  (* BEGIN ATOMIC *)
  if t.id_of_thread_holding_lock = bogus_thread_id then begin
    t.id_of_thread_holding_lock <- current_thread_id;
    (* END ATOMIC *)
    Result.ok_unit
  end
  else if current_thread_id = t.id_of_thread_holding_lock then
    Error (recursive_lock_error t)
  else begin
    with_blocker t (fun blocker -> if is_locked t then Blocker.wait blocker);
    lock t
  end;
;;

let lock_exn t = ok_exn (lock t)

type message =
  { current_thread_id : int;
    mutex : t;
  }
[@@deriving sexp_of]

let unlock t =
  let current_thread_id = current_thread_id () in
  (* We need the following test-and-set to be atomic so that there is a definitive
     winner in a race between multiple unlockers, so that one unlock succeeds and the
     rest fail. *)
  (* BEGIN ATOMIC *)
  if t.id_of_thread_holding_lock <> bogus_thread_id then begin
    if t.id_of_thread_holding_lock = current_thread_id then begin
      t.id_of_thread_holding_lock <- bogus_thread_id;
      (* END ATOMIC *)
      if Option.is_some t.blocker then with_blocker t Blocker.signal;
      Result.ok_unit;
    end else
      Error (Error.create "attempt to unlock mutex held by another thread"
               { current_thread_id; mutex = t } [%sexp_of: message])
  end else
    Error (Error.create "attempt to unlock an unlocked mutex"
             { current_thread_id; mutex = t } [%sexp_of: message])
;;

let unlock_exn t = ok_exn (unlock t)

let critical_section t ~f =
  lock_exn t;
  protect ~f ~finally:(fun () -> unlock_exn t);
;;

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
    [
      (   2, 100, 0.);
      (  10, 100, 0.);
      (  10, 100, 0.001);
      ( 100,  10, 0.001);
    ]
    ~f:(fun (num_threads, num_iterations, pause_for) ->
      try
        let l = create () in
        let am_holding_lock = ref false in
        let one_thread () =
          Thread.create (fun () ->
            for _ = 1 to num_iterations do
              lock_exn l;
              if !am_holding_lock then failwith "lock multiply acquired";
              am_holding_lock := true;
              ignore (Core_unix.nanosleep pause_for : float);
              am_holding_lock := false;
              unlock_exn l;
            done) ()
        in
        let threads = List.init num_threads ~f:(fun _ -> one_thread ()) in
        List.iter threads ~f:Thread.join
      with
      | exn ->
        failwiths "test failed"
          (num_threads, num_iterations, pause_for, exn)
          ([%sexp_of: int * int * float * exn]))
;;
