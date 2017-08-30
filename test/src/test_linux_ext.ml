open Core
open Linux_ext
open Expect_test_helpers_kernel

module Flags = Epoll.Flags


let with_epoll ~f =
  protectx ~finally:Epoll.close ~f
    ((Or_error.ok_exn Epoll.create) ~num_file_descrs:1024 ~max_ready_events:256)

let make_socket () =
  Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_DGRAM ~protocol:0

let%expect_test "[Epoll.set] has allocation limits" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    require_allocation_does_not_exceed
      (Minor_words 6)
      [%here]
      (fun () -> Epoll.set epset sock1 Flags.in_));
  [%expect {| |}]
;;

let%expect_test "[Epoll.find] does not allocate when not present" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    require_no_allocation [%here] (fun () -> ignore (Epoll.find epset sock1)));
  [%expect {| |}];
;;

let%expect_test "[Epoll.find] has allocation limits when present" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    Epoll.set epset sock1 Flags.in_;
    require_allocation_does_not_exceed
      (Minor_words 2)
      [%here]
      (fun () -> ignore (Epoll.find epset sock1)));
  [%expect {| |}]
;;

let%expect_test "[Epoll.remove] does not allocate" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    require_no_allocation [%here] (fun () -> ignore (Epoll.remove epset sock1));
    Epoll.set epset sock1 Flags.in_;
    require_no_allocation [%here] (fun () -> ignore (Epoll.remove epset sock1)));
  [%expect {| |}];
;;

(* Eventfd *)

let create = Or_error.ok_exn Eventfd.create

let%test_unit "Eventfd.read returns the initial value on a non-semaphore fd" =
  let fd = create 1l in
  [%test_result : Int64.t]
    ~expect:1L
    (Eventfd.read fd);
  let fd = create 10l in
  [%test_result : Int64.t]
    ~expect:10L
    (Eventfd.read fd);
;;

let%test_unit "Eventfd.read returns [1] on a semaphore fd" =
  let fd = create ~flags:Eventfd.Flags.semaphore 1l in
  [%test_result : Int64.t]
    ~expect:1L
    (Eventfd.read fd);
  let fd = create ~flags:Eventfd.Flags.semaphore 10l in
  [%test_result : Int64.t]
    ~expect:1L
    (Eventfd.read fd)
;;

let nonblock_read fd =
  try Some (Eventfd.read fd) with _ -> None

let%test_unit "Eventfd.write updates the counter, and Eventfd.read clears it on a non-semaphore fd" =
  let fd = create ~flags:Eventfd.Flags.nonblock 1l in
  Eventfd.write fd 10L;
  [%test_result : Int64.t]
    ~expect:11L
    (Eventfd.read fd);
  [%test_result : Int64.t option]
    ~expect:None
    (nonblock_read fd)
;;

let%test_unit "Eventfd.read will not block until the counter is decremented to zero on a semaphore fd" =
  let fd = create ~flags:Eventfd.Flags.(nonblock + semaphore) 10l in
  let count = ref 10L in
  while Int64.(!count > 0L) do
    [%test_result : Int64.t option]
      ~expect:(Some 1L)
      (nonblock_read fd);
    Int64.decr count
  done;
  [%test_result : Int64.t]
    ~expect:0L
    !count
;;
