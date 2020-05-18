[%%import "config.h"]

open! Core

module Syscall_result = Unix.Syscall_result

open Bigarray

include Core_kernel.Bigstring

exception IOError of int * exn [@@deriving sexp]

external init_stub : unit -> unit = "bigstring_init_stub"

let () =
  Callback.register_exception "Bigstring.End_of_file" End_of_file;
  Callback.register_exception "Bigstring.IOError" (IOError (0, Exit));
  init_stub ()

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

(* Input functions *)

external unsafe_read
  : min_len : int -> Unix.File_descr.t -> pos : int -> len : int -> t -> int
  = "bigstring_read_stub"

let read ?min_len fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "read" in
  check_args ~loc ~pos ~len bstr;
  let min_len = check_min_len ~loc ~len min_len in
  unsafe_read ~min_len fd ~pos ~len bstr

external unsafe_pread_assume_fd_is_nonblocking_stub
  : Unix.File_descr.t -> offset : int -> pos : int -> len : int -> t -> int
  = "bigstring_pread_assume_fd_is_nonblocking_stub"

let pread_assume_fd_is_nonblocking fd ~offset ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "pread" in
  check_args ~loc ~pos ~len bstr;
  unsafe_pread_assume_fd_is_nonblocking_stub fd ~offset ~pos ~len bstr

let really_read fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  ignore (read ~min_len:len fd ~pos ~len bstr : int)

external unsafe_really_recv
  : Unix.File_descr.t -> pos : int -> len : int -> t -> unit
  = "bigstring_really_recv_stub"

let really_recv sock ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_recv" ~pos ~len bstr;
  unsafe_really_recv sock ~pos ~len bstr

external unsafe_recvfrom_assume_fd_is_nonblocking
  : Unix.File_descr.t -> pos : int -> len : int -> t -> int * Unix.sockaddr
  = "bigstring_recvfrom_assume_fd_is_nonblocking_stub"

let recvfrom_assume_fd_is_nonblocking sock ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"recvfrom_assume_fd_is_nonblocking" ~pos ~len bstr;
  unsafe_recvfrom_assume_fd_is_nonblocking sock ~pos ~len bstr

external unsafe_read_assume_fd_is_nonblocking
  : Unix.File_descr.t -> pos : int -> len : int -> t -> Syscall_result.Int.t
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
  ignore (unsafe_input ~min_len:len ic ~pos ~len bstr : int)

(* Output functions *)

external unsafe_really_write
  : Unix.File_descr.t -> pos : int -> len : int -> t -> unit
  = "bigstring_really_write_stub"

let really_write fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_write" ~pos ~len bstr;
  unsafe_really_write fd ~pos ~len bstr

external unsafe_pwrite_assume_fd_is_nonblocking
  : Unix.File_descr.t -> offset : int -> pos : int -> len : int -> t -> int
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
  : Unix.File_descr.t -> pos : int -> len : int -> t -> unit
  = "bigstring_really_send_no_sigpipe_stub"

let really_send_no_sigpipe fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_send_no_sigpipe" ~pos ~len bstr;
  unsafe_really_send_no_sigpipe fd ~pos ~len bstr

external unsafe_send_nonblocking_no_sigpipe
  : Unix.File_descr.t -> pos : int -> len : int -> t -> Syscall_result.Int.t
  = "bigstring_send_nonblocking_no_sigpipe_stub" [@@noalloc]

let send_nonblocking_no_sigpipe fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"send_nonblocking_no_sigpipe" ~pos ~len bstr;
  unsafe_send_nonblocking_no_sigpipe fd ~pos ~len bstr

external unsafe_sendto_nonblocking_no_sigpipe
  : Unix.File_descr.t -> pos : int -> len : int -> t -> Unix.sockaddr -> Syscall_result.Int.t
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
  : Unix.File_descr.t -> pos : int -> len : int -> t -> int = "bigstring_write_stub"

let write fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"write" ~pos ~len bstr;
  unsafe_write fd ~pos ~len bstr

external unsafe_write_assume_fd_is_nonblocking
  : Unix.File_descr.t -> pos : int -> len : int -> t -> int
  = "bigstring_write_assume_fd_is_nonblocking_stub"

let write_assume_fd_is_nonblocking fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"write_assume_fd_is_nonblocking" ~pos ~len bstr;
  unsafe_write_assume_fd_is_nonblocking fd ~pos ~len bstr

external unsafe_writev
  : Unix.File_descr.t -> t Unix.IOVec.t array -> int -> int
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
  : Unix.File_descr.t -> t Unix.IOVec.t array -> int -> int
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
  ignore (unsafe_output oc ~min_len:len ~pos ~len bstr : int)

[%%ifdef JSC_RECVMMSG]

external unsafe_recvmmsg_assume_fd_is_nonblocking
  :  Unix.File_descr.t
  -> t Unix.IOVec.t array
  -> int
  -> Unix.sockaddr array option
  -> int array
  -> int
  = "bigstring_recvmmsg_assume_fd_is_nonblocking_stub"

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

let unsafe_recvmmsg_assume_fd_is_nonblocking = Ok unsafe_recvmmsg_assume_fd_is_nonblocking

let recvmmsg_assume_fd_is_nonblocking =
  (* At Jane Street, we link with [--wrap recvmmsg] so that we can use our own wrapper
     around [recvmmsg].  This allows us to compile an executable on a machine that has
     recvmmsg (e.g., CentOS 6) but then run the executable on a machine that does not
     (e.g., CentOS 5), but that has our wrapper library.  We set up our wrapper so that
     when running on a machine that doesn't have it, [recvmmsg] always returns -1 and sets
     errno to ENOSYS. *)
  let ok = Ok recvmmsg_assume_fd_is_nonblocking in
  try
    assert (recvmmsg_assume_fd_is_nonblocking (Unix.File_descr.of_int (-1))
              [||] ~lens:[||]
            = 0);
    ok                                  (* maybe it will ignore the bogus sockfd *)
  with
  | Unix.Unix_error (ENOSYS, _, _) ->
    Or_error.unimplemented "Bigstring.recvmmsg_assume_fd_is_nonblocking"
  | _ -> ok
;;

[%%else]
(* NDEF RECVMMSG *)

let unsafe_recvmmsg_assume_fd_is_nonblocking =
  Or_error.unimplemented "Bigstring.unsafe_recvmmsg_assume_fd_is_nonblocking"
;;

let recvmmsg_assume_fd_is_nonblocking =
  Or_error.unimplemented "Bigstring.recvmmsg_assume_fd_is_nonblocking"
;;

[%%endif]
(* RECVMMSG *)

(* Memory mapping *)

[%%ifdef JSC_MSG_NOSIGNAL]
(* Input and output, linux only *)

external unsafe_sendmsg_nonblocking_no_sigpipe
  : Unix.File_descr.t -> t Unix.IOVec.t array -> int -> int
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

let map_file ~shared fd size =
  Bigarray.array1_of_genarray
    (Unix.map_file fd Bigarray.char c_layout ~shared [|size|])
