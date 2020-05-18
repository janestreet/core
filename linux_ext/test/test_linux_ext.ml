open Core
open Poly
open Unix
open Linux_ext
open Expect_test_helpers_core

let%test_module "[Timerfd]" =
  (module struct
    open Timerfd

    let%test_unit "unsafe_timerfd_settime returning errno" =
      let result =
        Private.unsafe_timerfd_settime
          (File_descr.of_int (-1))
          false
          ~initial:Int63.zero
          ~interval:Int63.zero
      in
      if Syscall_result.Unit.is_ok result
      then
        failwiths
          ~here:[%here]
          "unsafe_timerfd_settime unexpectedly succeeded"
          result
          [%sexp_of: Syscall_result.Unit.t];
      [%test_result: Unix.Error.t] (Syscall_result.Unit.error_exn result) ~expect:EBADF
    ;;

    let%test_unit _ =
      match create with
      | Error _ -> ()
      | Ok create ->
        let t = create Clock.realtime in
        assert (get t = `Not_armed);
        set_after t Time_ns.Span.minute;
        assert (
          match get t with
          | `Fire_after span -> Time_ns.Span.( <= ) span Time_ns.Span.minute
          | _ -> false);
        let span = Time_ns.Span.scale Time_ns.Span.minute 2. in
        set_repeating t ~after:Time_ns.Span.minute span;
        assert (
          match get t with
          | `Repeat { fire_after; interval } ->
            Time_ns.Span.( <= ) fire_after Time_ns.Span.minute
            && Time_ns.Span.equal interval span
          | _ -> false)
    ;;
  end)
;;

let%test_unit _ =
  match cores with
  | Error _ -> ()
  | Ok cores ->
    assert (cores () > 0);
    assert (cores () < 100000)
;;

(* 99,999 cores ought to be enough for anybody *)

let%test "lo interface addr is 127.0.0.1" =
  (* This could be a false positive if the test box is misconfigured. *)
  match get_ipv4_address_for_interface with
  | Error _ -> true
  | Ok f -> f "lo" = "127.0.0.1"
;;

(* Epoll unit test included here for some example usage. Creates 2 sockets,
   adds them to an epoll set, sends data to one of them and calls Epoll.wait.
   The test passes if the resulting Ready_fds set has 1 ready fd, matching
   the one we sent to, with read, !write, and !error. *)
let%test_module _ =
  (module struct
    module Flags = Epoll.Flags

    let udp_listener ~port =
      let sock = Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_DGRAM ~protocol:0 () in
      let iaddr = Unix.ADDR_INET (Unix.Inet_addr.localhost, port) in
      Unix.setsockopt sock Unix.SO_REUSEADDR true;
      Unix.bind sock ~addr:iaddr;
      sock
    ;;

    let send_substring s buf ~port =
      let addr = Unix.ADDR_INET (Unix.Inet_addr.localhost, port) in
      let len = String.length buf in
      Unix.sendto_substring s ~buf ~pos:0 ~len ~mode:[] ~addr
    ;;

    let with_epoll ~f =
      protectx
        ~finally:Epoll.close
        ~f
        ((Or_error.ok_exn Epoll.create) ~num_file_descrs:1024 ~max_ready_events:256)
    ;;

    let%test_unit "epoll errors" =
      with_epoll ~f:(fun t ->
        let tmp = "temporary-file-for-testing-epoll" in
        let fd = Unix.openfile tmp ~mode:[ Unix.O_CREAT; Unix.O_WRONLY ] in
        (* Epoll does not support ordinary files, and so should fail if you ask it to watch
           one. *)
        assert (Result.is_error (Result.try_with (fun () -> Epoll.set t fd Flags.none)));
        Unix.close fd;
        Unix.unlink tmp)
    ;;

    let%test_unit "epoll test" =
      with_epoll ~f:(fun epset ->
        let span = Time_ns.Span.of_sec 0.1 in
        let sock1 = udp_listener ~port:7070 in
        let sock2 = udp_listener ~port:7071 in
        Epoll.set epset sock1 Flags.in_;
        Epoll.set epset sock2 Flags.in_;
        let _sent = send_substring sock2 "TEST" ~port:7070 in
        match Epoll.wait_timeout_after epset span with
        | `Timeout -> assert false
        | `Ok ->
          let ready =
            Epoll.fold_ready epset ~init:[] ~f:(fun ac fd flags ->
              if flags = Flags.in_ then fd :: ac else ac)
          in
          (* Explanation of the test:
             1) I create two udp sockets, sock1 listening on 7070 and sock2, on 7071
             2) These two sockets are both added to epoll for read notification
             3) I send a packet, _using_ sock2 to sock1 (who is listening on 7070)
             4) epoll_wait should return, with [ sock1 ] ready to be read.
          *)
          (match ready with
           | [ sock ] when sock = sock1 -> ()
           | [ _ ] -> failwith "wrong socket is ready"
           | xs -> failwithf "%d sockets are ready" (List.length xs) ()))
    ;;

    let%test_unit "Timerfd.set_after small span test" =
      match Timerfd.create with
      | Error _ -> ()
      | Ok timerfd_create ->
        with_epoll ~f:(fun epoll ->
          let timerfd = timerfd_create Timerfd.Clock.realtime in
          Epoll.set epoll (timerfd :> File_descr.t) Epoll.Flags.in_;
          List.iter [ 0; 1 ] ~f:(fun span_ns ->
            Timerfd.set_after
              timerfd
              (Time_ns.Span.of_int63_ns (Int63.of_int span_ns));
            match Epoll.wait epoll ~timeout:`Never with
            | `Timeout -> assert false
            | `Ok -> ());
          Unix.close (timerfd :> Unix.File_descr.t))
    ;;

    let%test_unit "epoll detects an error on the write side of a pipe when the read \
                   side of the pipe closes\n\
                   after a partial read"
      =
      let saw_sigpipe = ref false in
      let new_sigpipe_handler = `Handle (fun _ -> saw_sigpipe := true) in
      let old_sigpipe_handler = Signal.Expert.signal Signal.pipe new_sigpipe_handler in
      Exn.protect
        ~finally:(fun () -> Signal.Expert.set Signal.pipe old_sigpipe_handler)
        ~f:(fun () ->
          let r, w = Unix.pipe () in
          let w_len = 1_000_000 in
          let r_len = 1_000 in
          let read =
            Thread.create
              ~on_uncaught_exn:`Print_to_stderr
              (fun () ->
                 let nr =
                   Bigstring_unix.read r (Bigstring.create r_len) ~pos:0 ~len:r_len
                 in
                 assert (nr > 0 && nr <= r_len);
                 Unix.close r)
              ()
          in
          let nw =
            Bigstring_unix.writev
              w
              [| Unix.IOVec.of_bigstring (Bigstring.create w_len) |]
          in
          assert (nw > 0 && nw < w_len);
          Thread.join read;
          with_epoll ~f:(fun epoll ->
            Epoll.set epoll w Epoll.Flags.out;
            match Epoll.wait_timeout_after epoll Time_ns.Span.second with
            | `Timeout -> assert false
            | `Ok ->
              assert !saw_sigpipe;
              let saw_fd = ref false in
              Epoll.iter_ready epoll ~f:(fun fd flags ->
                assert (Unix.File_descr.equal fd w);
                assert (Epoll.Flags.equal flags Epoll.Flags.err);
                saw_fd := true);
              assert !saw_fd))
    ;;
  end)
;;

module Flags = Epoll.Flags

let with_epoll ~f =
  protectx
    ~finally:Epoll.close
    ~f
    ((Or_error.ok_exn Epoll.create) ~num_file_descrs:1024 ~max_ready_events:256)
;;

let make_socket () =
  Unix.socket ~domain:Unix.PF_INET ~kind:Unix.SOCK_DGRAM ~protocol:0 ()
;;

let%expect_test "[Epoll.set] has allocation limits" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    require_allocation_does_not_exceed (Minor_words 6) [%here] (fun () ->
      Epoll.set epset sock1 Flags.in_));
  [%expect {| |}]
;;

let%expect_test "[Epoll.find] does not allocate when not present" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    require_no_allocation [%here] (fun () ->
      ignore (Epoll.find epset sock1 : _ option)));
  [%expect {| |}]
;;

let%expect_test "[Epoll.find] has allocation limits when present" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    Epoll.set epset sock1 Flags.in_;
    require_allocation_does_not_exceed (Minor_words 2) [%here] (fun () ->
      ignore (Epoll.find epset sock1 : _ option)));
  [%expect {| |}]
;;

let%expect_test "[Epoll.remove] does not allocate" =
  with_epoll ~f:(fun epset ->
    let sock1 = make_socket () in
    require_no_allocation [%here] (fun () -> ignore (Epoll.remove epset sock1 : unit));
    Epoll.set epset sock1 Flags.in_;
    require_no_allocation [%here] (fun () -> ignore (Epoll.remove epset sock1 : unit)));
  [%expect {| |}]
;;

let%expect_test "[Epoll.Expert.clear_ready]" =
  with_epoll ~f:(fun t ->
    let print_num_ready () =
      let num_ready = ref 0 in
      Epoll.iter_ready t ~f:(fun _ _ -> incr num_ready);
      print_s [%message (!num_ready : int)]
    in
    let file_descr = make_socket () in
    Epoll.set t file_descr Flags.out;
    match Epoll.wait t ~timeout:`Immediately with
    | `Timeout -> assert false
    | `Ok ->
      print_num_ready ();
      Epoll.Expert.clear_ready t;
      print_num_ready ();
      [%expect {|
        (!num_ready 1)
        (!num_ready 0) |}])
;;

(* Eventfd *)

let create = Or_error.ok_exn Eventfd.create

let%test_unit "Eventfd.read returns the initial value on a non-semaphore fd" =
  let fd = create 1l in
  [%test_result: Int64.t] ~expect:1L (Eventfd.read fd);
  let fd = create 10l in
  [%test_result: Int64.t] ~expect:10L (Eventfd.read fd)
;;

let%test_unit "Eventfd.read returns [1] on a semaphore fd" =
  let fd = create ~flags:Eventfd.Flags.semaphore 1l in
  [%test_result: Int64.t] ~expect:1L (Eventfd.read fd);
  let fd = create ~flags:Eventfd.Flags.semaphore 10l in
  [%test_result: Int64.t] ~expect:1L (Eventfd.read fd)
;;

let nonblock_read fd =
  try Some (Eventfd.read fd) with
  | _ -> None
;;

let%test_unit "Eventfd.write updates the counter, and Eventfd.read clears it on a \
               non-semaphore fd"
  =
  let fd = create ~flags:Eventfd.Flags.nonblock 1l in
  Eventfd.write fd 10L;
  [%test_result: Int64.t] ~expect:11L (Eventfd.read fd);
  [%test_result: Int64.t option] ~expect:None (nonblock_read fd)
;;

let%test_unit "Eventfd.read will not block until the counter is decremented to zero on \
               a semaphore fd"
  =
  let fd = create ~flags:Eventfd.Flags.(nonblock + semaphore) 10l in
  let count = ref 10L in
  while Int64.(!count > 0L) do
    [%test_result: Int64.t option] ~expect:(Some 1L) (nonblock_read fd);
    Int64.decr count
  done;
  [%test_result: Int64.t] ~expect:0L !count
;;

(* CPU Affinity *)
let%expect_test "set and get affinity" =
  match sched_getaffinity, sched_setaffinity with
  | Ok sched_getaffinity, Ok sched_setaffinity ->
    let print_affinity () = printf !"%{sexp:int list}" (sched_getaffinity ()) in
    let starting_cpus = sched_getaffinity () in
    sched_setaffinity ~cpuset:[ 0 ] ();
    print_affinity ();
    [%expect {| (0) |}];
    sched_setaffinity ~cpuset:[ 1; 2 ] ();
    print_affinity ();
    [%expect {| (1 2) |}];
    sched_setaffinity ~cpuset:starting_cpus ();
    printf "%b" (starting_cpus = sched_getaffinity ());
    [%expect {| true |}]
  | _ -> ()
;;

let%test_unit "get_terminal_size" =
  match get_terminal_size with
  | Error _ -> ()
  | Ok f ->
    let with_tmp_fd f =
      protectx
        (Filename.temp_file "get_terminal_size" "")
        ~finally:Unix.unlink
        ~f:(fun fname ->
          protectx
            (Unix.openfile fname ~mode:[ Unix.O_RDONLY ] ~perm:0)
            ~finally:Unix.close
            ~f)
    in
    (match with_tmp_fd (fun fd -> f (`Fd fd)) with
     | exception Unix.Unix_error (ENOTTY, _, _) -> ()
     | res ->
       raise_s
         [%sexp "get_terminal_size should have failed but returned", (res : int * int)])
;;

(* Tested by hand:
   eprintf !"get_terminal_size: %{sexp: int * int}\n%!" (f `Controlling);
*)

(* Extended file attributes *)
let%test_module "getxattr and setxattr" =
  (module struct
    let expect_error f =
      match f () with
      | exception exn -> print_s [%sexp (exn : Exn.t)]
      | _ -> raise_s [%message "expected error but returned"]
    ;;

    let with_tmpfile f =
      let tmpfile = "temporary-file-for-testing-xattr" in
      let fd = Unix.openfile tmpfile ~mode:[ Unix.O_CREAT; Unix.O_WRONLY ] in
      Unix.close fd;
      (try f tmpfile with
       | (_ : exn) -> ());
      Unix.unlink tmpfile
    ;;

    let get_and_print ~path ~name =
      let value = (Extended_file_attributes.getxattr |> ok_exn) ~path ~name in
      print_s [%sexp (value : Extended_file_attributes.Get_attr_result.t)]
    ;;

    let set_and_print ?how ~path ~name ~value () =
      let result =
        (Extended_file_attributes.setxattr |> ok_exn) ?how ~path ~name ~value ()
      in
      print_s [%sexp (result : Extended_file_attributes.Set_attr_result.t)]
    ;;

    let%expect_test "simple test" =
      with_tmpfile (fun path ->
        let name = "user.foo" in
        get_and_print ~path ~name;
        [%expect {| ENOATTR |}];
        set_and_print ~path ~name ~value:"bar" ();
        [%expect {| Ok |}];
        get_and_print ~path ~name;
        [%expect {| (Ok bar) |}])
    ;;

    let%expect_test "test setxattr [`Create] semantics" =
      with_tmpfile (fun path ->
        let name = "user.foo" in
        let set_and_create () =
          set_and_print ~how:`Create ~path ~name ~value:"blah" ()
        in
        set_and_create ();
        [%expect {| Ok |}];
        get_and_print ~path ~name;
        [%expect {| (Ok blah) |}];
        set_and_create ();
        [%expect {| EEXIST |}])
    ;;

    let%expect_test "test setxattr [`Replace] semantics" =
      with_tmpfile (fun path ->
        let name = "user.foo" in
        let set_with_replace () =
          set_and_print ~how:`Replace ~path ~name ~value:"xyz" ()
        in
        set_with_replace ();
        [%expect {| ENOATTR |}];
        set_and_print ~how:`Create ~path ~name ~value:"bar" ();
        [%expect {| Ok |}];
        get_and_print ~path ~name;
        [%expect {| (Ok bar) |}];
        set_with_replace ();
        [%expect {| Ok |}];
        get_and_print ~path ~name;
        [%expect {| (Ok xyz) |}])
    ;;

    let%expect_test "test getxattr and setxattr on a non-existent file" =
      let path = "some-file-that-doesnt-exist" in
      let name = "user.foo" in
      expect_error (fun () -> set_and_print ~path ~name ~value:"xyz" ());
      [%expect
        {|
      (Unix.Unix_error
       "No such file or directory"
       setxattr
       some-file-that-doesnt-exist) |}];
      expect_error (fun () -> get_and_print ~path ~name);
      [%expect
        {|
      (Unix.Unix_error
       "No such file or directory"
       getxattr
       some-file-that-doesnt-exist) |}]
    ;;
  end)
;;

let with_listening_server_unix_socket fname ~f =
  let with_cwd dir ~f =
    let old = Unix.getcwd () in
    Unix.chdir dir;
    Exn.protect ~finally:(fun () -> Unix.chdir old) ~f
  in
  (* work around socket path length restriction *)
  with_cwd (Filename.dirname fname) ~f:(fun () ->
    let fname = Filename.basename fname in
    let server_sock = Unix.socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 () in
    Unix.bind server_sock ~addr:(ADDR_UNIX fname);
    let thread =
      Thread.create
        ~on_uncaught_exn:`Print_to_stderr
        (fun () -> Unix.listen server_sock ~backlog:10)
        ()
    in
    f fname;
    Thread.join thread)
;;

let%test_unit "peer_credentials" =
  match Linux_ext.peer_credentials with
  | Error _ -> ()
  | Ok peer_credentials ->
    protectx (Filename.temp_file "linux_ext" "") ~finally:Unix.unlink ~f:(fun fname ->
      (let fd = Unix.openfile fname ~mode:[ O_RDONLY ] in
       try
         ignore (peer_credentials fd : Peer_credentials.t);
         failwith "peer credential on non socket should have raised"
       with
       | Unix.Unix_error (ENOTSOCK, _, _) -> ());
      with_listening_server_unix_socket (fname ^ ".peercredsocket") ~f:(fun fname ->
        let client_sock =
          Unix.socket ~domain:PF_UNIX ~kind:SOCK_STREAM ~protocol:0 ()
        in
        let rec connect count =
          try Unix.connect client_sock ~addr:(ADDR_UNIX fname) with
          | Unix_error (ECONNREFUSED, _, _) when count < 100 ->
            (* the server might not have listened yet *)
            ignore (Unix.nanosleep 0.1 : float);
            connect (count + 1)
        in
        connect 0;
        let p = peer_credentials client_sock in
        [%test_eq: Pid.t] p.pid (Unix.getpid ());
        [%test_eq: int] p.uid (Unix.getuid ());
        [%test_eq: int] p.gid (Unix.getgid ())))
;;
