open! Core
open! Import
open! Bigstring_unix

let arch_sixtyfour = Sys.word_size = 64

let%test_module _ =
  (module struct
    let expect_invalid_argument ?msg f =
      assert (
        try
          ignore (f () : int);
          false
        with
        | Invalid_argument s ->
          (match msg with
           | None -> true
           | Some x when String.equal x s -> true
           | Some x -> failwithf "expected %S but got %S" x s ()))
    ;;

    let check_invalid ?msg count =
      match unsafe_recvmmsg_assume_fd_is_nonblocking with
      | Error _ -> ()
      | Ok unsafe_recvmmsg_assume_fd_is_nonblocking ->
        let fd = Unix.socket ~domain:PF_INET ~kind:SOCK_DGRAM ~protocol:0 () in
        expect_invalid_argument ?msg (fun () ->
          unsafe_recvmmsg_assume_fd_is_nonblocking fd [||] count None [||])
    ;;

    let%test_unit "unsafe_recvmmsg_assume_fd_is_nonblocking: check count bounds" =
      check_invalid (-1);
      check_invalid Int.min_value;
      check_invalid 65;
      (* RECVMMSG_MAX_COUNT = 64 *)
      if arch_sixtyfour
      then (
        (* We are assuming that [unsigned int] is 32 bits wide. *)
        check_invalid (Int64.to_int_exn 0xFFFF_FFFFL);
        (* exceeds RECVMMSG_MAX_COUNT *)
        check_invalid (Int64.to_int_exn 0x1FFFF_FFFFL)
        (* exceeds unsigned int *))
    ;;
  end)
;;

let%test_module "recvmmsg smoke" =
  (module struct
    module IOVec = Unix.IOVec
    module Inet_addr = Unix.Inet_addr
    open Caml.Unix

    let count = 10
    let fd = socket PF_INET SOCK_DGRAM 0
    let () = bind fd (ADDR_INET (Inet_addr.bind_any, 0))
    let iovecs = Array.init count ~f:(fun _ -> IOVec.of_bigstring (create 1500))
    let srcs = Array.create ~len:count (ADDR_INET (Inet_addr.bind_any, 0))
    let lens = Array.create ~len:count 0
    let short_srcs = Array.create ~len:(count - 1) (ADDR_INET (Inet_addr.bind_any, 0))
    let () = set_nonblock fd

    let test ?count ?srcs ~lens ok_pred error_pred =
      match recvmmsg_assume_fd_is_nonblocking with
      | Error _ -> ()
      | Ok recvmmsg_assume_fd_is_nonblocking ->
        [%test_pred: (int, exn) Result.t]
          (function
            | Ok i -> ok_pred i
            | Error e -> error_pred e)
          (Result.try_with (fun () ->
             recvmmsg_assume_fd_is_nonblocking fd iovecs ?count ?srcs ~lens))
    ;;

    (* We return -EAGAIN and -EWOULDBLOCK directly as values, rather than as exceptions.
       So, allow negative results. *)
    let%test_unit _ =
      test ~count ~srcs ~lens (( >= ) 0) (function
        | Unix_error _ -> true
        | _ -> false)
    ;;

    let%test_unit _ =
      test ~lens (( >= ) 0) (function
        | Unix_error _ -> true
        | _ -> false)
    ;;

    let%test_unit _ =
      test ~count:(count / 2) ~srcs ~lens (( >= ) 0) (function
        | Unix_error _ -> true
        | _ -> false)
    ;;

    let%test_unit _ =
      test ~count:0 ~srcs ~lens (( >= ) 0) (function
        | Unix_error _ -> true
        | _ -> false)
    ;;

    let%test_unit _ =
      test ~count:(count + 1) ~lens (const false) (function
        | Unix_error _ -> false
        | _ -> true)
    ;;

    let%test_unit _ =
      test ~srcs:short_srcs ~lens (const false) (function
        | Unix_error _ -> false
        | _ -> true)
    ;;
  end)
;;
