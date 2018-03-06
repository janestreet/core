[%%import "config.h"]

open! Import
open Unix
open Bigarray

include Core_kernel.Bigstring

exception IOError of int * exn [@@deriving sexp]

let arch_sixtyfour = Sys.word_size = 64

external init : unit -> unit = "bigstring_init_stub"

let () =
  Callback.register_exception "Bigstring.End_of_file" End_of_file;
  Callback.register_exception "Bigstring.IOError" (IOError (0, Exit));
  init ()

external aux_create: max_mem_waiting_gc:int -> size:int -> t = "bigstring_alloc"

let create ?max_mem_waiting_gc size =
  let max_mem_waiting_gc =
    match max_mem_waiting_gc with
    | None -> ~-1
    | Some v -> Float.to_int (Byte_units.bytes v)
  in
  if size < 0 then invalid_argf "create: size = %d < 0" size ();
  aux_create ~max_mem_waiting_gc ~size

let length = Array1.dim

external is_mmapped : t -> bool = "bigstring_is_mmapped_stub" [@@noalloc]

let init n ~f =
  let t = create n in
  for i = 0 to n - 1; do
    t.{i} <- f i;
  done;
  t
;;

let check_args ~loc ~pos ~len (bstr : t) =
  if pos < 0 then invalid_arg (loc ^ ": pos < 0");
  if len < 0 then invalid_arg (loc ^ ": len < 0");
  let bstr_len = length bstr in
  if bstr_len < pos + len then
    invalid_arg (sprintf "Bigstring.%s: length(bstr) < pos + len" loc)

let get_opt_len bstr ~pos = function
  | Some len -> len
  | None -> length bstr - pos

let check_min_len ~loc ~len = function
  | None -> 0
  | Some min_len ->
    if min_len > len then (
      let msg = sprintf "%s: min_len (%d) > len (%d)" loc min_len len in
      invalid_arg msg);
    if min_len < 0 then (
      let msg = sprintf "%s: min_len (%d) < 0" loc min_len in
      invalid_arg msg);
    min_len

let sub_shared ?(pos = 0) ?len (bstr : t) =
  let len = get_opt_len bstr ~pos len in
  Array1.sub bstr pos len

(* Input functions *)

external unsafe_read
  : min_len : int -> file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_read_stub"

let read ?min_len fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "read" in
  check_args ~loc ~pos ~len bstr;
  let min_len = check_min_len ~loc ~len min_len in
  unsafe_read ~min_len fd ~pos ~len bstr

external unsafe_pread_assume_fd_is_nonblocking_stub
  : file_descr -> offset : int -> pos : int -> len : int -> t -> int
  = "bigstring_pread_assume_fd_is_nonblocking_stub"

let pread_assume_fd_is_nonblocking fd ~offset ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "pread" in
  check_args ~loc ~pos ~len bstr;
  unsafe_pread_assume_fd_is_nonblocking_stub fd ~offset ~pos ~len bstr

let really_read fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  ignore (read ~min_len:len fd ~pos ~len bstr)

external unsafe_really_recv
  : file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_recv_stub"

let really_recv sock ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_recv" ~pos ~len bstr;
  unsafe_really_recv sock ~pos ~len bstr

external unsafe_recvfrom_assume_fd_is_nonblocking
  : file_descr -> pos : int -> len : int -> t -> int * sockaddr
  = "bigstring_recvfrom_assume_fd_is_nonblocking_stub"

let recvfrom_assume_fd_is_nonblocking sock ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"recvfrom_assume_fd_is_nonblocking" ~pos ~len bstr;
  unsafe_recvfrom_assume_fd_is_nonblocking sock ~pos ~len bstr

external unsafe_read_assume_fd_is_nonblocking
  : file_descr -> pos : int -> len : int -> t -> Syscall_result.Int.t
  = "bigstring_read_assume_fd_is_nonblocking_stub"

let read_assume_fd_is_nonblocking fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"read_assume_fd_is_nonblocking" ~pos ~len bstr;
  unsafe_read_assume_fd_is_nonblocking fd ~pos ~len bstr

external unsafe_input
  : min_len : int -> In_channel.t -> pos : int -> len : int -> t -> int
  = "bigstring_input_stub"

let input ?min_len ic ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "input" in
  check_args ~loc ~pos ~len bstr;
  let min_len = check_min_len ~loc ~len min_len in
  unsafe_input ~min_len ic ~pos ~len bstr

let really_input ic ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_input" ~pos ~len bstr;
  ignore (unsafe_input ~min_len:len ic ~pos ~len bstr)

(* Output functions *)

external unsafe_really_write
  : file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_write_stub"

let really_write fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_write" ~pos ~len bstr;
  unsafe_really_write fd ~pos ~len bstr

external unsafe_pwrite_assume_fd_is_nonblocking
  : file_descr -> offset : int -> pos : int -> len : int -> t -> int
  = "bigstring_pwrite_assume_fd_is_nonblocking_stub"

let pwrite_assume_fd_is_nonblocking fd ~offset ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "pwrite" in
  check_args ~loc ~pos ~len bstr;
  unsafe_pwrite_assume_fd_is_nonblocking fd ~offset ~pos ~len bstr

[%%ifdef JSC_MSG_NOSIGNAL]
[%%define JSC_NOSIGPIPE]
[%%endif]

[%%ifdef JSC_SO_NOSIGPIPE]
[%%define JSC_NOSIGPIPE]
[%%endif]

[%%ifdef JSC_NOSIGPIPE]

external unsafe_really_send_no_sigpipe
  : file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_send_no_sigpipe_stub"

let really_send_no_sigpipe fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_send_no_sigpipe" ~pos ~len bstr;
  unsafe_really_send_no_sigpipe fd ~pos ~len bstr

external unsafe_send_nonblocking_no_sigpipe
  : file_descr -> pos : int -> len : int -> t -> Syscall_result.Int.t
  = "bigstring_send_nonblocking_no_sigpipe_stub" [@@noalloc]

let send_nonblocking_no_sigpipe fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"send_nonblocking_no_sigpipe" ~pos ~len bstr;
  unsafe_send_nonblocking_no_sigpipe fd ~pos ~len bstr

external unsafe_sendto_nonblocking_no_sigpipe
  : file_descr -> pos : int -> len : int -> t -> sockaddr -> Syscall_result.Int.t
  = "bigstring_sendto_nonblocking_no_sigpipe_stub"

let sendto_nonblocking_no_sigpipe fd ?(pos = 0) ?len bstr sockaddr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"sendto_nonblocking_no_sigpipe" ~pos ~len bstr;
  unsafe_sendto_nonblocking_no_sigpipe fd ~pos ~len bstr sockaddr

let really_send_no_sigpipe                = Ok really_send_no_sigpipe
let send_nonblocking_no_sigpipe           = Ok send_nonblocking_no_sigpipe
let sendto_nonblocking_no_sigpipe         = Ok sendto_nonblocking_no_sigpipe
let unsafe_really_send_no_sigpipe         = Ok unsafe_really_send_no_sigpipe
let unsafe_send_nonblocking_no_sigpipe    = Ok unsafe_send_nonblocking_no_sigpipe

[%%else]

let u = Or_error.unimplemented
let really_send_no_sigpipe             = u "Bigstring.really_send_no_sigpipe"
let send_nonblocking_no_sigpipe        = u "Bigstring.send_nonblocking_no_sigpipe"
let sendto_nonblocking_no_sigpipe      = u "Bigstring.sendto_nonblocking_no_sigpipe"
let unsafe_really_send_no_sigpipe      = u "Bigstring.unsafe_really_send_no_sigpipe"
let unsafe_send_nonblocking_no_sigpipe = u "Bigstring.unsafe_send_nonblocking_no_sigpipe"

[%%endif]

external unsafe_write
  : file_descr -> pos : int -> len : int -> t -> int = "bigstring_write_stub"

let write fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"write" ~pos ~len bstr;
  unsafe_write fd ~pos ~len bstr

external unsafe_write_assume_fd_is_nonblocking
  : file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_write_assume_fd_is_nonblocking_stub"

let write_assume_fd_is_nonblocking fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"write_assume_fd_is_nonblocking" ~pos ~len bstr;
  unsafe_write_assume_fd_is_nonblocking fd ~pos ~len bstr

external unsafe_writev
  : file_descr -> t Core_unix.IOVec.t array -> int -> int
  = "bigstring_writev_stub"

let get_iovec_count loc iovecs = function
  | None -> Array.length iovecs
  | Some count ->
    if count < 0 then invalid_arg (loc ^ ": count < 0");
    let n_iovecs = Array.length iovecs in
    if count > n_iovecs then invalid_arg (loc ^ ": count > n_iovecs");
    count

let writev fd ?count iovecs =
  let count = get_iovec_count "writev" iovecs count in
  unsafe_writev fd iovecs count

external unsafe_writev_assume_fd_is_nonblocking
  : file_descr -> t Core_unix.IOVec.t array -> int -> int
  = "bigstring_writev_assume_fd_is_nonblocking_stub"

let writev_assume_fd_is_nonblocking fd ?count iovecs =
  let count = get_iovec_count "writev_nonblocking" iovecs count in
  unsafe_writev_assume_fd_is_nonblocking fd iovecs count
;;

external unsafe_output
  : min_len : int -> Out_channel.t -> pos : int -> len : int -> t -> int
  = "bigstring_output_stub"

let output ?min_len oc ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "output" in
  check_args ~loc ~pos ~len bstr;
  let min_len = check_min_len ~loc ~len min_len in
  unsafe_output oc ~min_len ~pos ~len bstr

let really_output oc ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_output" ~pos ~len bstr;
  ignore (unsafe_output oc ~min_len:len ~pos ~len bstr)

[%%ifdef JSC_RECVMMSG]

external unsafe_recvmmsg_assume_fd_is_nonblocking
  :  file_descr
    -> t Core_unix.IOVec.t array
    -> int
    -> sockaddr array option
    -> int array
    -> int
  = "bigstring_recvmmsg_assume_fd_is_nonblocking_stub"

let%test_module _ = (module struct
  let expect_invalid_argument ?msg f =
    assert (try ignore (f () : int); false
            with Invalid_argument s ->
            match msg with
            | None -> true
            | Some x when String.equal x s -> true
            | Some x -> failwithf "expected %S but got %S" x s ())
  ;;

  let check_invalid ?msg count =
    let fd = Core_unix.socket ~domain:PF_INET ~kind:SOCK_DGRAM ~protocol:0 in
    expect_invalid_argument ?msg
      (fun () -> unsafe_recvmmsg_assume_fd_is_nonblocking fd [||] count None [||])


  let%test_unit "unsafe_recvmmsg_assume_fd_is_nonblocking: check count bounds" =
    check_invalid (-1);
    check_invalid Int.min_value;
    check_invalid 65; (* RECVMMSG_MAX_COUNT = 64 *)
    if arch_sixtyfour then begin
      (* We are assuming that [unsigned int] is 32 bits wide. *)
      check_invalid (Int64.to_int_exn 0xFFFF_FFFFL); (* exceeds RECVMMSG_MAX_COUNT *)
      check_invalid (Int64.to_int_exn 0x1FFFF_FFFFL) (* exceeds unsigned int *)
    end
  ;;
end)


let recvmmsg_assume_fd_is_nonblocking fd ?count ?srcs iovecs ~lens =
  let loc = "recvmmsg_assume_fd_is_nonblocking" in
  let count = get_iovec_count loc iovecs count in
  begin match srcs with
  | None -> ()
  | Some a -> if count > Array.length a then invalid_arg (loc ^ ": count > n_srcs")
  end;
  if count > Array.length lens then invalid_arg (loc ^ ": count > n_lens");
  unsafe_recvmmsg_assume_fd_is_nonblocking fd iovecs count srcs lens
;;

let%test_module "recvmmsg smoke" = (module struct
  module IOVec = Core_unix.IOVec
  module Inet_addr = Core_unix.Inet_addr

  let count = 10
  let fd = socket PF_INET SOCK_DGRAM 0
  let () = bind fd (ADDR_INET (Inet_addr.bind_any, 0))
  let iovecs = Array.init count ~f:(fun _ -> IOVec.of_bigstring (create 1500))
  let srcs = Array.create ~len:count (ADDR_INET (Inet_addr.bind_any, 0))
  let lens = Array.create ~len:count 0
  let short_srcs = Array.create ~len:(count - 1) (ADDR_INET (Inet_addr.bind_any, 0))
  let () = set_nonblock fd

  let test ?count ?srcs ~lens ok_pred error_pred =
    [%test_pred: (int, exn) Result.t]
      (function Ok i -> ok_pred i | Error e -> error_pred e)
      (Result.try_with
         (fun () -> recvmmsg_assume_fd_is_nonblocking fd iovecs ?count ?srcs ~lens)
      )
  (* We return -EAGAIN and -EWOULDBLOCK directly as values, rather than as exceptions.
     So, allow negative results. *)
  let%test_unit _ =
    test ~count ~srcs ~lens ((>=) 0) (function Unix_error _ -> true | _ -> false)
  let%test_unit _ = test ~lens ((>=) 0) (function Unix_error _ -> true | _ -> false)
  let%test_unit _ =
    test ~count:(count / 2) ~srcs ~lens ((>=) 0)
      (function Unix_error _ -> true | _ -> false)
  let%test_unit _ =
    test ~count:0 ~srcs ~lens ((>=) 0) (function Unix_error _ -> true | _ -> false)
  let%test_unit _ =
    test ~count:(count + 1) ~lens (const false)
      (function Unix_error _ -> false | _ -> true)
  let%test_unit _ =
    test ~srcs:short_srcs ~lens (const false) (function Unix_error _ -> false | _ -> true)
end)
;;

let recvmmsg_assume_fd_is_nonblocking =
  (* At Jane Street, we link with [--wrap recvmmsg] so that we can use our own wrapper
     around [recvmmsg].  This allows us to compile an executable on a machine that has
     recvmmsg (e.g., CentOS 6) but then run the executable on a machine that does not
     (e.g., CentOS 5), but that has our wrapper library.  We set up our wrapper so that
     when running on a machine that doesn't have it, [recvmmsg] always returns -1 and sets
     errno to ENOSYS. *)
  let ok = Ok recvmmsg_assume_fd_is_nonblocking in
  try
    assert (recvmmsg_assume_fd_is_nonblocking (Core_unix.File_descr.of_int (-1))
              [||] ~lens:[||]
            = 0);
    ok                                  (* maybe it will ignore the bogus sockfd *)
  with
  | Unix_error (ENOSYS, _, _) ->
    Or_error.unimplemented "Bigstring.recvmmsg_assume_fd_is_nonblocking"
  | _ -> ok
;;

[%%else]
(* NDEF RECVMMSG *)

let recvmmsg_assume_fd_is_nonblocking =
  Or_error.unimplemented "Bigstring.recvmmsg_assume_fd_is_nonblocking"
;;

[%%endif]
(* RECVMMSG *)

(* Memory mapping *)

[%%ifdef JSC_MSG_NOSIGNAL]
(* Input and output, linux only *)

external unsafe_sendmsg_nonblocking_no_sigpipe
  : file_descr -> t Core_unix.IOVec.t array -> int -> int
  = "bigstring_sendmsg_nonblocking_no_sigpipe_stub"

let unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count =
  let res = unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count in
  if res = -1 then None
  else Some res

let sendmsg_nonblocking_no_sigpipe fd ?count iovecs =
  let count = get_iovec_count "sendmsg_nonblocking_no_sigpipe" iovecs count in
  unsafe_sendmsg_nonblocking_no_sigpipe fd iovecs count

let sendmsg_nonblocking_no_sigpipe        = Ok sendmsg_nonblocking_no_sigpipe
let unsafe_sendmsg_nonblocking_no_sigpipe = Ok unsafe_sendmsg_nonblocking_no_sigpipe

[%%else]

let sendmsg_nonblocking_no_sigpipe =
  Or_error.unimplemented "Bigstring.sendmsg_nonblocking_no_sigpipe"
;;

let unsafe_sendmsg_nonblocking_no_sigpipe =
  Or_error.unimplemented "Bigstring.unsafe_sendmsg_nonblocking_no_sigpipe"
;;

[%%endif]

(* Memory mapping *)

let map_file ~shared fd n = Array1.map_file fd Bigarray.char c_layout shared n
