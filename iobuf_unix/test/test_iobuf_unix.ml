open! Core
open! Iobuf
open! Iobuf_unix

type nonrec ('d, 'w) t = ('d, 'w) Hexdump.t [@@deriving sexp_of]

let%test_unit _ =
  let to_bigstring_shared_via_iovec ?pos ?len iobuf =
    let iovec = Expert.to_iovec_shared ?pos ?len iobuf in
    Bigstring.sub_shared iovec.buf ~pos:iovec.pos ~len:iovec.len
  in
  List.iter
    [ Iobuf.Expert.to_bigstring_shared; to_bigstring_shared_via_iovec ]
    ~f:(fun to_bstr ->
      let iobuf = Iobuf.of_string "0123456789" in
      let bstr0 = to_bstr iobuf in
      [%test_result: Bigstring.t] bstr0 ~expect:(Bigstring.of_string "0123456789");
      Iobuf.Poke.char iobuf ~pos:0 'X';
      [%test_result: Bigstring.t] bstr0 ~expect:(Bigstring.of_string "X123456789");
      let bstr1 = to_bstr iobuf ~pos:1 ~len:8 in
      [%test_result: Bigstring.t] bstr1 ~expect:(Bigstring.of_string "12345678");
      Iobuf.Poke.char iobuf ~pos:1 'X';
      [%test_result: Bigstring.t] bstr1 ~expect:(Bigstring.of_string "X2345678");
      [%test_result: Bigstring.t] bstr0 ~expect:(Bigstring.of_string "XX23456789"))
;;

let%expect_test "fillf_float.Ok" =
  let t = Iobuf.create ~len:32 in
  assert (Poly.equal (Expert.fillf_float t ~c_format:"%f" Float.pi) `Ok);
  Iobuf.flip_lo t;
  print_endline (to_string t);
  [%expect {| 3.141593 |}]
;;

let%expect_test "fillf_float.Truncated" =
  let t = Iobuf.create ~len:5 in
  assert (Poly.equal (Expert.fillf_float t ~c_format:"%f" Float.pi) `Truncated);
  Iobuf.flip_lo t;
  print_endline (to_string t);
  [%expect {| |}]
;;

type nonrec ok_or_eof = ok_or_eof =
  | Ok
  | Eof
[@@deriving compare, sexp_of]

let iter_examples = Iobuf_test.Test_iobuf.iter_examples

module Io_test (Ch : sig
    type in_

    val create_in : string -> in_
    val close_in : in_ -> unit
    val read : ([> write ], seek) Iobuf.t -> in_ -> ok_or_eof

    type out_

    val create_out : Unix.File_descr.t -> out_
    val close_out : out_ -> unit
    val write : ([> read ], seek) Iobuf.t -> out_ -> unit
  end) =
struct
  let%test_unit _ =
    iter_examples ~f:(fun t string ~pos ->
      let len = String.length string in
      let file, fd = Unix.mkstemp "iobuf_test" in
      protect
        ~finally:(fun () -> Unix.unlink file)
        ~f:(fun () ->
          let ch = Ch.create_out fd in
          Poke.stringo t string ~pos;
          advance t pos;
          resize t ~len;
          Ch.write t ch;
          Ch.close_out ch;
          reset t;
          for pos = 0 to length t - 1 do
            Poke.char t '\000' ~pos
          done;
          let ch = Ch.create_in file in
          advance t pos;
          let lo = Lo_bound.window t in
          (match Ch.read t ch with
           | Ok -> assert (len > 0 || Iobuf.is_empty t)
           | Eof -> assert (len = 0 && not (Iobuf.is_empty t)));
          bounded_flip_lo t lo;
          [%test_result: string] (to_string t) ~expect:string;
          reset t;
          (match Ch.read t ch with
           | Eof -> ()
           | Ok -> assert false);
          Ch.close_in ch))
  ;;
end

let output = output
let input = input

include Io_test (struct
    type in_ = In_channel.t

    let create_in file = In_channel.create file
    let close_in = In_channel.close

    type out_ = Out_channel.t

    let create_out = Unix.out_channel_of_descr
    let close_out = Out_channel.close
    let write = output
    let read = input
  end)

let read = read
let write = write

include Io_test (struct
    type in_ = Unix.File_descr.t
    type out_ = in_

    let create_in file = Unix.openfile ~mode:[ Unix.O_RDONLY ] file
    let close_in fd = Unix.close fd
    let create_out = Fn.id
    let close_out = close_in
    let read = read
    let write = write
  end)

let read_assume_fd_is_nonblocking = read_assume_fd_is_nonblocking
let write_assume_fd_is_nonblocking = write_assume_fd_is_nonblocking

let%test_unit _ =
  iter_examples ~f:(fun t string ~pos ->
    let n = String.length string in
    let file, fd = Unix.mkstemp "iobuf_test" in
    protect
      ~finally:(fun () -> Unix.unlink file)
      ~f:(fun () ->
        (sub_shared t ~pos |> fun t -> Fill.stringo t string);
        (sub_shared t ~pos ~len:n
         |> fun t ->
         let len_before = Iobuf.length t in
         write_assume_fd_is_nonblocking t fd;
         [%test_result: int] (len_before - Iobuf.length t) ~expect:n);
        Unix.close fd;
        iter_examples ~f:(fun t _ ~pos ->
          if length t - pos >= String.length string
          then (
            let fd = Unix.openfile ~mode:[ Unix.O_RDONLY ] file in
            (sub_shared t ~pos
             |> fun t ->
             let len_before = Iobuf.length t in
             match
               Unix.Syscall_result.Unit.to_result (read_assume_fd_is_nonblocking t fd)
             with
             | Ok () | Error (EAGAIN | EINTR | EWOULDBLOCK) ->
               [%test_result: int] (len_before - Iobuf.length t) ~expect:n
             | Error e -> raise (Unix.Unix_error (e, "read", "")));
            (sub_shared t ~pos ~len:n
             |> fun t -> assert (String.equal (to_string t) string));
            Unix.close fd))))
;;

let pread_assume_fd_is_nonblocking = pread_assume_fd_is_nonblocking
let pwrite_assume_fd_is_nonblocking = pwrite_assume_fd_is_nonblocking

let%test_unit _ =
  let s = "000000000011111111112222222222" in
  let n = String.length s in
  let t = of_string s in
  let file, fd = Unix.mkstemp "iobuf_test" in
  protect
    ~finally:(fun () -> Unix.unlink file)
    ~f:(fun () ->
      (sub_shared t ~pos:0 ~len:n
       |> fun t ->
       pwrite_assume_fd_is_nonblocking t fd ~offset:10;
       assert (Iobuf.is_empty t));
      (sub_shared t ~pos:0 ~len:10
       |> fun t ->
       pread_assume_fd_is_nonblocking t fd ~offset:20;
       assert (Iobuf.is_empty t));
      (sub_shared t ~pos:0 ~len:10
       |> fun t -> assert (String.equal (to_string t) "1111111111"));
      Unix.close fd)
;;

module Recvmmsg_context = Recvmmsg_context

let send_nonblocking_no_sigpipe = send_nonblocking_no_sigpipe
let sendto_nonblocking_no_sigpipe = sendto_nonblocking_no_sigpipe

let rec retry_until_ready thunk =
  try thunk () with
  | Unix.Unix_error ((EWOULDBLOCK | EAGAIN | EINTR), _, _) ->
    Thread.yield ();
    retry_until_ready thunk
;;

let sendto_and_recvfrom recvfrom recv_fd sendto ~sendto_name =
  let port, recv_addr =
    match Unix.getsockname recv_fd with
    | Unix.ADDR_INET (recv_addr, port) -> port, recv_addr
    | _ -> assert false
  in
  let send_addr = Unix.(ADDR_INET (Inet_addr.localhost, port)) in
  Result.iter sendto ~f:(fun sendto ->
    try
      let sender =
        Thread.create
          (fun () ->
             let send_fd =
               Unix.(socket ~domain:PF_INET ~kind:SOCK_DGRAM ~protocol:0 ())
             in
             iter_examples ~f:(fun t string ~pos:_ ->
               Fill.stringo t string;
               Iobuf.flip_lo t;
               retry_until_ready (fun () ->
                 Unix.Syscall_result.Unit.ok_or_unix_error_exn
                   (sendto t send_fd send_addr)
                   ~syscall_name:sendto_name);
               [%test_pred: (_, _) Iobuf.Hexdump.t] Iobuf.is_empty t))
          ~on_uncaught_exn:`Print_to_stderr
          ()
      in
      iter_examples ~f:(fun t string ~pos:_ ->
        ignore (retry_until_ready (fun () -> recvfrom t recv_fd) : Unix.sockaddr);
        Iobuf.flip_lo t;
        [%test_result: string] ~expect:string (Iobuf.to_string t));
      Thread.join sender
    with
    | exn ->
      raise_s
        [%message
          "sendto_and_recvfrom"
            (sendto_name : string)
            (recv_addr : Unix.Inet_addr.t)
            (port : int)
            (send_addr : Unix.sockaddr)
            (exn : exn)])
;;

let sends_with_recvfrom recvfrom =
  let fd = Unix.(socket ~domain:PF_INET ~kind:SOCK_DGRAM ~protocol:0 ()) in
  Unix.(bind fd ~addr:(ADDR_INET (Inet_addr.bind_any, 0)));
  Unix.set_nonblock fd;
  sendto_and_recvfrom
    recvfrom
    fd
    ~sendto_name:"send"
    (Or_error.map (send_nonblocking_no_sigpipe ()) ~f:(fun send t fd addr ->
       Unix.connect fd ~addr;
       send t fd));
  sendto_and_recvfrom
    recvfrom
    fd
    ~sendto_name:"sendto"
    (sendto_nonblocking_no_sigpipe ())
;;

let%test_unit _ = sends_with_recvfrom recvfrom_assume_fd_is_nonblocking

(* Create a file of binary length prefixed messages of a known format to read back
   using the Iobuf module. Part unit test, part usage example and interface
   exercise... *)
let create_sample_file ?(int_size = 2) ?(be = false) ~msgcount =
  let bstr = Bigstring.create 512 in
  (* Sometimes use a bigstring buffer, sometimes a char queue, to test the latter and
     [read_to_fd] symmetrically to [write_from_fd] below. *)
  let t = create ~len:512 in
  let filename, fd = Unix.mkstemp "iobuftest" in
  for i = 0 to msgcount - 1 do
    let s = sprintf "MESSAGE %d" i in
    let len = String.length s in
    if Random.bool ()
    then (
      let open Bigstring in
      let unsafe_set_int =
        match int_size with
        | 2 -> if be then unsafe_set_int16_be else unsafe_set_int16_le
        | 4 -> if be then unsafe_set_int32_be else unsafe_set_int32_le
        | 8 -> if be then unsafe_set_int64_be else unsafe_set_int64_le
        | _ -> failwithf "Unknown int size %d" int_size ()
      in
      unsafe_set_int bstr ~pos:0 len;
      Bigstring.From_string.blito ~src:s ~dst:bstr ~dst_pos:int_size ();
      Bigstring_unix.really_write fd ~len:(len + int_size) bstr)
    else (
      let fill_int =
        match int_size with
        | 2 -> if be then Fill.int16_be_trunc else Fill.int16_le_trunc
        | 4 -> if be then Fill.int32_be_trunc else Fill.int32_le_trunc
        | 8 -> if be then Fill.int64_be else Fill.int64_le
        | _ -> failwithf "Unknown int size %d" int_size ()
      in
      fill_int t len;
      Fill.stringo t s;
      flip_lo t;
      write_assume_fd_is_nonblocking t fd;
      [%test_pred: (_, _) t] Iobuf.is_empty t;
      (* no short writes *)
      reset t)
  done;
  Unix.close fd;
  filename
;;

(** Loop through and check all messages in the given file match the expected
    "MESSAGE %d" format *)
let check_msgs ?(int_size = 2) ?(be = false) file =
  let msg_number = ref 0 in
  let check_message r =
    let msg = Consume.stringo r in
    let s = sprintf "MESSAGE %d" !msg_number in
    assert (String.equal s msg);
    msg_number := !msg_number + 1
  in
  let fd = Unix.openfile file ~perm:0o600 ~mode:[ Unix.O_RDONLY ] in
  let t = create ~len:512 in
  let rec drain_messages () =
    let init_len = length t in
    if init_len > int_size
    then (
      let consume_int =
        match int_size with
        | 2 -> if be then Consume.int16_be else Consume.int16_le
        | 4 -> if be then Consume.int32_be else Consume.int32_le
        | 8 -> if be then Consume.int64_be_exn else Consume.int64_le_exn
        | _ -> failwithf "Unknown int size %d" int_size ()
      in
      let needed = consume_int t in
      if length t < needed
      then (
        rewind t;
        advance t (length t - init_len);
        assert (length t = init_len))
      else (
        check_message (sub_shared t ~len:needed);
        advance t needed;
        drain_messages ()))
  in
  let rec loop_file () =
    let len_before = length t in
    (match
       read_assume_fd_is_nonblocking t fd |> Unix.Syscall_result.Unit.to_result
     (* doesn't allocate *)
     with
     | Error (EAGAIN | EINTR | EWOULDBLOCK) -> ()
     | Error e -> raise (Unix.Unix_error (e, "read", ""))
     | Ok () -> ());
    if len_before > length t
    then (
      flip_lo t;
      drain_messages ();
      compact t;
      loop_file ())
  in
  loop_file ();
  Unix.close fd;
  !msg_number
;;

let%test_unit _ =
  let msgcount = 10_000 in
  List.iter [ 2; 4; 8 ] ~f:(fun int_size ->
    List.iter [ false; true ] ~f:(fun be ->
      let filename = create_sample_file ~int_size ~be ~msgcount in
      protect
        ~f:(fun () -> assert (check_msgs ~int_size ~be filename = msgcount))
        ~finally:(fun () -> Unix.unlink filename)))
;;
