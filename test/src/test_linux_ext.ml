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
