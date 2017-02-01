open Core
open OUnit

let try_with_mutex f =
  let mtx = Mutex.create () in
  try f mtx; false
  with Sys_error _ -> true

let check_double_lock () =
  try_with_mutex (fun mtx -> Mutex.lock mtx; Mutex.lock mtx)

let check_unlock_unlocked () = try_with_mutex Mutex.unlock

let check_unlock_other_locked () =
  try_with_mutex (fun mtx ->
    Thread.join (Thread.create (fun () -> Mutex.lock mtx) ());
    Mutex.unlock mtx)

let test =
  "core_mutex" >:::
  [
    "error checking" >:: (fun () ->
      "double lock" @? check_double_lock ();
      "unlock unlocked" @? check_unlock_unlocked ();
      "unlock other locked" @? check_unlock_other_locked ();
    )
  ]
