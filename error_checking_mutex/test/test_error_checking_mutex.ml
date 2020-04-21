open! Core_kernel
open! Expect_test_helpers_core
module Mutex = Error_checking_mutex

let try_with_mutex f =
  let mtx = Mutex.create () in
  try f mtx; false
  with Sys_error _ -> true

let%expect_test "double lock" =
  require [%here] (try_with_mutex (fun mtx -> Mutex.lock mtx; Mutex.lock mtx))
;;

let%expect_test "unlock unlocked" =
  require [%here] (try_with_mutex Mutex.unlock);
;;

let%expect_test "unlock other locked" =
  require [%here]
    (try_with_mutex (fun mtx ->
       Thread.join (Thread.create (fun () -> Mutex.lock mtx) ());
       Mutex.unlock mtx))
;;
