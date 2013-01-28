(* Error-checking mutexes. *)

include Mutex

(** [create] like {!Mutex.create}, but creates an error-checking mutex.
    Locking a mutex twice from the same thread, unlocking an unlocked mutex,
    or unlocking a mutex not held by the thread will result in a [Sys_error]
    exception. *)

external create : unit -> Mutex.t = "unix_create_error_checking_mutex"

let create = create

let phys_equal = Caml.(==)

let equal (t : t) t' = phys_equal t t'

let critical_section l ~f =
  lock l;
  Exn.protect ~f ~finally:(fun () -> unlock l)

let synchronize f =
  let mtx = create () in
  let f' x = critical_section mtx ~f:(fun () -> f x) in
  f'

let update_signal mtx cnd ~f =
  critical_section mtx ~f:(fun () ->
    let res = f () in
    Condition.signal cnd;
    res)

let update_broadcast mtx cnd ~f =
  critical_section mtx ~f:(fun () ->
    let res = f () in
    Condition.broadcast cnd;
    res)

let am_holding_mutex mtx =
  match
    try if try_lock mtx then `Free else `Held_by_other
    with _ -> `Held_by_me
  with
  | `Free -> unlock mtx; false
  | `Held_by_me -> true
  | `Held_by_other -> false
