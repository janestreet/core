[%%import
  "config.h"]

open! Core
open! Iobuf
module File_descr = Unix.File_descr
module Syscall_result = Unix.Syscall_result

type ok_or_eof =
  | Ok
  | Eof
[@@deriving compare, sexp_of]

let input t ch =
  match Bigstring_unix.input ch (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t) with
  | n ->
    unsafe_advance t n;
    Ok
  | exception Bigstring_unix.IOError (n, End_of_file) ->
    unsafe_advance t n;
    Eof
;;

let read t fd =
  match Bigstring_unix.read fd (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t) with
  | n ->
    unsafe_advance t n;
    Ok
  | exception Bigstring_unix.IOError (n, End_of_file) ->
    unsafe_advance t n;
    Eof
;;

let read_assume_fd_is_nonblocking t fd =
  let nread =
    Bigstring_unix.read_assume_fd_is_nonblocking
      fd
      (Expert.buf t)
      ~pos:(Expert.lo t)
      ~len:(length t)
  in
  if Syscall_result.Int.is_ok nread
  then unsafe_advance t (Syscall_result.Int.ok_exn nread);
  Syscall_result.ignore_ok_value nread
;;

let pread_assume_fd_is_nonblocking t fd ~offset =
  let nread =
    Bigstring_unix.pread_assume_fd_is_nonblocking
      fd
      ~offset
      (Expert.buf t)
      ~pos:(Expert.lo t)
      ~len:(length t)
  in
  unsafe_advance t nread
;;

let recvfrom_assume_fd_is_nonblocking t fd =
  let nread, sockaddr =
    Bigstring_unix.recvfrom_assume_fd_is_nonblocking
      fd
      (Expert.buf t)
      ~pos:(Expert.lo t)
      ~len:(length t)
  in
  unsafe_advance t nread;
  sockaddr
;;

[%%ifdef
  JSC_RECVMMSG]

(* Allocate and pre-populate the [struct mmsghdr]s and associated [struct iovec]s. Reusing
   this context reduces the cost of calls to [recvmmsg] considerably if the iobuf array is
   large. *)
module Recvmmsg_context = struct
  type ctx

  external unsafe_ctx : ([> write ], seek) t array -> ctx = "iobuf_recvmmsg_ctx"

  let ctx ts =
    if Array.for_all ts ~f:(fun t -> length t = capacity t)
    then unsafe_ctx ts
    else
      raise_s
        [%sexp
          "Recvmmsg_context.create: all buffers must be reset"
        , (ts : (_, _) t_with_shallow_sexp array)]
  ;;

  (* we retain a reference to the underlying bigstrings, in the event that callers
     mistakenly use set_bounds_and_buffer. Since we've cached the underlying memory
     referenced by the bigstring, we want to prevent it from being garbage collected and
     released. *)
  type nonrec t =
    { iobufs : (read_write, seek) t array
    ; bstrs : Bigstring.t array
    ; ctx : ctx
    }

  let create iobufs =
    { iobufs; bstrs = Array.map iobufs ~f:Expert.buf; ctx = ctx iobufs }
  ;;
end

external unsafe_recvmmsg_assume_fd_is_nonblocking
  :  File_descr.t
  -> (read_write, seek) t array
  -> Recvmmsg_context.ctx
  -> Unix.Syscall_result.Int.t
  = "iobuf_recvmmsg_assume_fd_is_nonblocking_stub"
[@@noalloc]

let recvmmsg_assume_fd_is_nonblocking fd { Recvmmsg_context.iobufs; ctx; _ } =
  unsafe_recvmmsg_assume_fd_is_nonblocking fd iobufs ctx
;;

let recvmmsg_assume_fd_is_nonblocking =
  (* We link with [--wrap recvmmsg].  If we have compiled on a machine with recvmmsg
     (e.g., CentOS 6) but then run on a machine without (e.g., CentOS 5), our wrapped
     [recvmmsg] always returns -1 and sets errno to ENOSYS. *)
  match
    Unix.Syscall_result.Int.to_result
      (let fd = File_descr.of_int (-1) in
       recvmmsg_assume_fd_is_nonblocking fd (Recvmmsg_context.create [||]))
  with
  | Error ENOSYS -> Or_error.unimplemented "Iobuf.recvmmsg_assume_fd_is_nonblocking"
  | _ -> Ok recvmmsg_assume_fd_is_nonblocking
;;

[%%else]

(* not JSC_RECVMMSG *)

module Recvmmsg_context = struct
  type t = unit

  let create = ignore
end

let recvmmsg_assume_fd_is_nonblocking =
  Or_error.unimplemented "Iobuf.recvmmsg_assume_fd_is_nonblocking"
;;

[%%endif]

(* JSC_RECVMMSG *)

let unsafe_sent t result =
  if Syscall_result.Int.is_ok result
  then (
    unsafe_advance t (Syscall_result.Int.ok_exn result);
    Syscall_result.unit)
  else Syscall_result.Int.reinterpret_error_exn result
;;

(* Don't use [Or_error.map].  The natural usage results in a partially applied function,
   which is slower to call. *)
let send_nonblocking_no_sigpipe () =
  match Bigstring_unix.send_nonblocking_no_sigpipe with
  | Error _ as e -> e
  | Ok send ->
    Ok
      (fun t fd ->
         unsafe_sent t (send fd (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t)))
;;

let sendto_nonblocking_no_sigpipe () =
  match Bigstring_unix.sendto_nonblocking_no_sigpipe with
  | Error _ as e -> e
  | Ok sendto ->
    Ok
      (fun t fd addr ->
         unsafe_sent t (sendto fd (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t) addr))
;;

let output t ch =
  let nwritten =
    Bigstring_unix.output ch (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t)
  in
  unsafe_advance t nwritten
;;

let write t fd =
  let nwritten =
    Bigstring_unix.write fd (Expert.buf t) ~pos:(Expert.lo t) ~len:(length t)
  in
  unsafe_advance t nwritten
;;

let write_assume_fd_is_nonblocking t fd =
  (* This is safe because of the invariant of [t] that the window is within the buffer
     (unless the user has violated the invariant with an unsafe operation). *)
  let nwritten =
    Bigstring_unix.unsafe_write_assume_fd_is_nonblocking
      fd
      (Expert.buf t)
      ~pos:(Expert.lo t)
      ~len:(length t)
  in
  unsafe_advance t nwritten
;;

let pwrite_assume_fd_is_nonblocking t fd ~offset =
  let nwritten =
    Bigstring_unix.pwrite_assume_fd_is_nonblocking
      fd
      ~offset
      (Expert.buf t)
      ~pos:(Expert.lo t)
      ~len:(length t)
  in
  unsafe_advance t nwritten
;;

module Expert = struct
  external unsafe_pokef_float
    :  (read_write, _) t
    -> c_format:string
    -> max_length:int
    -> (float[@unboxed])
    -> int
    = "iobuf_unsafe_pokef_double_bytecode" "iobuf_unsafe_pokef_double"
  [@@noalloc]

  let fillf_float t ~c_format value =
    let limit = length t in
    let result = unsafe_pokef_float t ~c_format ~max_length:(length t) value in
    if result >= limit
    then `Truncated
    else if result < 0
    then `Format_error
    else (
      unsafe_advance t result;
      `Ok)
  ;;

  let to_iovec_shared ?pos ?len t =
    let pos, len =
      Ordered_collection_common.get_pos_len_exn () ?pos ?len ~total_length:(length t)
    in
    Unix.IOVec.of_bigstring (Expert.buf t) ~pos:(Expert.lo t + pos) ~len
  ;;
end
