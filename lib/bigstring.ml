INCLUDE "config.mlh"

open Std_internal
open Unix
open Bigarray
open Sexplib.Std

module Binable = Binable0

module Z : sig
  type t = (char, int8_unsigned_elt, c_layout) Array1.t with bin_io, sexp
end = struct
  include Bin_prot.Std
  include Sexplib.Conv
  type t = bigstring with bin_io, sexp
end
include Z

exception IOError of int * exn with sexp

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
  aux_create ~max_mem_waiting_gc ~size

TEST "create with different max_mem_waiting_gc" =
  Core_gc.full_major ();
  let count_gc_cycles mem_units =
    let cycles = ref 0 in
    let alarm = Core_gc.create_alarm (fun () -> incr cycles) in
    let large_int = 10_000 in
    let max_mem_waiting_gc = Byte_units.create mem_units 256. in
    for _i = 0 to large_int do
      let (_ : t) = create ~max_mem_waiting_gc large_int in
      ()
    done;
    Core_gc.delete_alarm alarm;
    !cycles
  in
  let large_max_mem = count_gc_cycles `Megabytes in
  let small_max_mem = count_gc_cycles `Bytes in
  (* We don't care if it's twice as many, we are only testing that there are less cycles
  involved *)
  (2 * large_max_mem) < small_max_mem


external length : t -> int = "bigstring_length" "noalloc"

external is_mmapped : t -> bool = "bigstring_is_mmapped_stub" "noalloc"

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


(* Blitting *)

external unsafe_blit :
  src : t -> src_pos : int -> dst : t -> dst_pos : int -> len : int -> unit
  = "bigstring_blit_stub"

let blit_common
    ~loc ~get_src_len ~get_dst_len ~blit ~src ?src_pos ?src_len ~dst ?(dst_pos = 0) () =
  let (src_pos, len) =
    Ordered_collection_common.get_pos_len_exn ?pos:src_pos ?len:src_len
      ~length:(get_src_len src)
  in
  let check_pos var total_len pos =
    if pos < 0 then invalid_argf "%s: %s < 0" loc var ()
    else if pos + len > total_len then
      invalid_argf "%s: pos (%d) + len (%d) > total_len (%d)"
        loc pos len total_len ()
  in
  check_pos "src_pos" (get_src_len src) src_pos;
  check_pos "dst_pos" (get_dst_len dst) dst_pos;
  if len > 0 then blit ~src ~src_pos ~dst ~dst_pos ~len
;;

type ('src, 'dst) blit
  =  src : 'src
  -> ?src_pos : int
  -> ?src_len : int
  -> dst : 'dst
  -> ?dst_pos : int
  -> unit
  -> unit

let blit ~src ?src_pos ?src_len ~dst ?dst_pos () =
  blit_common
    ~loc:"blit"
    ~get_src_len:length ~get_dst_len:length
    ~blit:unsafe_blit
    ~src ?src_pos ?src_len ~dst ?dst_pos
    ()
;;

let sub ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let dst = create len in
  blit ~src:bstr ~src_pos:pos ~src_len:len ~dst ();
  dst
;;

let get bstr pos = Array1.get bstr pos

let set bstr pos c = Array1.set bstr pos c

external unsafe_blit_string_bigstring :
  src : string -> src_pos : int -> dst : t -> dst_pos : int -> len : int -> unit
  = "bigstring_blit_string_bigstring_stub" "noalloc"

let blit_string_bigstring ~src ?src_pos ?src_len ~dst ?dst_pos () =
  blit_common
    ~loc:"blit_string_bigstring"
    ~get_src_len:String.length ~get_dst_len:length
    ~blit:unsafe_blit_string_bigstring
    ~src ?src_pos ?src_len ~dst ?dst_pos
    ()
;;

external unsafe_blit_bigstring_string :
  src : t -> src_pos : int -> dst : string -> dst_pos : int -> len : int -> unit
  = "bigstring_blit_bigstring_string_stub" "noalloc"

let blit_bigstring_string ~src ?src_pos ?src_len ~dst ?dst_pos () =
  blit_common
    ~loc:"blit_bigstring_string"
    ~get_src_len:length ~get_dst_len:String.length
    ~blit:unsafe_blit_bigstring_string
    ~src ?src_pos ?src_len ~dst ?dst_pos
    ()
;;

let of_string ?(pos = 0) ?len src =
  let len =
    match len with
    | Some len -> len
    | None -> String.length src - pos
  in
  let dst = create len in
  blit_string_bigstring ~src ~src_pos:pos ~src_len:len ~dst ();
  dst;
;;

let to_string ?(pos = 0) ?len src =
  let len = get_opt_len src ~pos len in
  check_args ~loc:"to_string" ~pos ~len src;
  let dst = String.create len in
  blit_bigstring_string ~src ~src_pos:pos ~src_len:len ~dst ();
  dst
;;

(* Input functions *)

external unsafe_read :
  min_len : int -> file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_read_stub"

let read ?min_len fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "read" in
  check_args ~loc ~pos ~len bstr;
  let min_len = check_min_len ~loc ~len min_len in
  unsafe_read ~min_len fd ~pos ~len bstr

external unsafe_pread_assume_fd_is_nonblocking_stub :
  file_descr -> offset : int -> pos : int -> len : int -> t -> int
  = "bigstring_pread_assume_fd_is_nonblocking_stub"

let pread_assume_fd_is_nonblocking fd ~offset ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "pread" in
  check_args ~loc ~pos ~len bstr;
  unsafe_pread_assume_fd_is_nonblocking_stub fd ~offset ~pos ~len bstr

let really_read fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  ignore (read ~min_len:len fd ~pos ~len bstr)

external unsafe_really_recv :
  file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_recv_stub"

let really_recv sock ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_recv" ~pos ~len bstr;
  unsafe_really_recv sock ~pos ~len bstr

external unsafe_recvfrom_assume_fd_is_nonblocking :
  file_descr -> pos : int -> len : int -> t -> int * sockaddr
  = "bigstring_recvfrom_assume_fd_is_nonblocking_stub"

let recvfrom_assume_fd_is_nonblocking sock ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"recvfrom_assume_fd_is_nonblocking" ~pos ~len bstr;
  unsafe_recvfrom_assume_fd_is_nonblocking sock ~pos ~len bstr

external unsafe_read_assume_fd_is_nonblocking :
  file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_read_assume_fd_is_nonblocking_stub"

let read_assume_fd_is_nonblocking fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"read_assume_fd_is_nonblocking" ~pos ~len bstr;
  unsafe_read_assume_fd_is_nonblocking fd ~pos ~len bstr

external unsafe_input :
  min_len : int -> in_channel -> pos : int -> len : int -> t -> int
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

external unsafe_output :
  min_len : int -> out_channel -> pos : int -> len : int -> t -> int
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

external unsafe_really_write :
  file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_write_stub"

let really_write fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_write" ~pos ~len bstr;
  unsafe_really_write fd ~pos ~len bstr

external unsafe_pwrite_assume_fd_is_nonblocking :
  file_descr -> offset : int -> pos : int -> len : int -> t -> int
  = "bigstring_pwrite_assume_fd_is_nonblocking_stub"

let pwrite_assume_fd_is_nonblocking fd ~offset ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  let loc = "pwrite" in
  check_args ~loc ~pos ~len bstr;
  unsafe_pwrite_assume_fd_is_nonblocking fd ~offset ~pos ~len bstr

IFDEF MSG_NOSIGNAL THEN
external unsafe_really_send_no_sigpipe :
  file_descr -> pos : int -> len : int -> t -> unit
  = "bigstring_really_send_no_sigpipe_stub"

let really_send_no_sigpipe fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"really_send_no_sigpipe" ~pos ~len bstr;
  unsafe_really_send_no_sigpipe fd ~pos ~len bstr

external unsafe_send_nonblocking_no_sigpipe :
  file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_send_nonblocking_no_sigpipe_stub"

let unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf =
  let res = unsafe_send_nonblocking_no_sigpipe fd ~pos ~len buf in
  if res = -1 then None
  else Some res

let send_nonblocking_no_sigpipe fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"send_nonblocking_no_sigpipe" ~pos ~len bstr;
  unsafe_send_nonblocking_no_sigpipe fd ~pos ~len bstr

external unsafe_sendto_nonblocking_no_sigpipe :
  file_descr -> pos : int -> len : int -> t -> sockaddr -> int
  = "bigstring_sendto_nonblocking_no_sigpipe_stub"

let unsafe_sendto_nonblocking_no_sigpipe fd ~pos ~len buf sockaddr =
  let res = unsafe_sendto_nonblocking_no_sigpipe fd ~pos ~len buf sockaddr in
  if res = -1 then None
  else Some res

let sendto_nonblocking_no_sigpipe fd ?(pos = 0) ?len bstr sockaddr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"sendto_nonblocking_no_sigpipe" ~pos ~len bstr;
  unsafe_sendto_nonblocking_no_sigpipe fd ~pos ~len bstr sockaddr

let really_send_no_sigpipe                = Ok really_send_no_sigpipe
let send_nonblocking_no_sigpipe           = Ok send_nonblocking_no_sigpipe
let sendto_nonblocking_no_sigpipe         = Ok sendto_nonblocking_no_sigpipe
let unsafe_really_send_no_sigpipe         = Ok unsafe_really_send_no_sigpipe
let unsafe_send_nonblocking_no_sigpipe    = Ok unsafe_send_nonblocking_no_sigpipe

ELSE

let really_send_no_sigpipe             = unimplemented "Bigstring.really_send_no_sigpipe"
let send_nonblocking_no_sigpipe        = unimplemented "Bigstring.send_nonblocking_no_sigpipe"
let sendto_nonblocking_no_sigpipe      = unimplemented "Bigstring.sendto_nonblocking_no_sigpipe"
let unsafe_really_send_no_sigpipe      = unimplemented "Bigstring.unsafe_really_send_no_sigpipe"
let unsafe_send_nonblocking_no_sigpipe = unimplemented "Bigstring.unsafe_send_nonblocking_no_sigpipe"

ENDIF

external unsafe_write :
  file_descr -> pos : int -> len : int -> t -> int = "bigstring_write_stub"

let write fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"write" ~pos ~len bstr;
  unsafe_write fd ~pos ~len bstr

external unsafe_write_assume_fd_is_nonblocking :
  file_descr -> pos : int -> len : int -> t -> int
  = "bigstring_write_assume_fd_is_nonblocking_stub"

let write_assume_fd_is_nonblocking fd ?(pos = 0) ?len bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"write_assume_fd_is_nonblocking" ~pos ~len bstr;
  unsafe_write_assume_fd_is_nonblocking fd ~pos ~len bstr

external unsafe_writev :
  file_descr -> t Core_unix.IOVec.t array -> int -> int
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

external unsafe_writev_assume_fd_is_nonblocking :
  file_descr -> t Core_unix.IOVec.t array -> int -> int
  = "bigstring_writev_assume_fd_is_nonblocking_stub"

let writev_assume_fd_is_nonblocking fd ?count iovecs =
  let count = get_iovec_count "writev_nonblocking" iovecs count in
  unsafe_writev_assume_fd_is_nonblocking fd iovecs count
;;

(* Memory mapping *)

let map_file ~shared fd n = Array1.map_file fd Bigarray.char c_layout shared n

IFDEF MSG_NOSIGNAL THEN
(* Input and output, linux only *)

external unsafe_sendmsg_nonblocking_no_sigpipe :
  file_descr -> t Core_unix.IOVec.t array -> int -> int
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

ELSE

let sendmsg_nonblocking_no_sigpipe =
  unimplemented "Bigstring.sendmsg_nonblocking_no_sigpipe"
;;

let unsafe_sendmsg_nonblocking_no_sigpipe =
  unimplemented "Bigstring.unsafe_sendmsg_nonblocking_no_sigpipe"
;;

ENDIF
(* Search *)

external unsafe_find : t -> char -> pos:int -> len:int -> int = "bigstring_find"
let find ?(pos = 0) ?len chr bstr =
  let len = get_opt_len bstr ~pos len in
  check_args ~loc:"find" ~pos ~len bstr;
  let res = unsafe_find bstr chr ~pos ~len in
  if res < 0 then None else Some res

(* Destruction *)

external unsafe_destroy : t -> unit = "bigstring_destroy_stub"

(* vim: set filetype=ocaml : *)

(* Binary-packing like accessors *)

external unsafe_read_int16            : t -> pos:int -> int
  = "unsafe_read_int16_t"       "noalloc"
external unsafe_read_int16_swap       : t -> pos:int -> int
  = "unsafe_read_int16_t_swap"  "noalloc"
external unsafe_read_uint16           : t -> pos:int -> int
  = "unsafe_read_uint16_t"      "noalloc"
external unsafe_read_uint16_swap      : t -> pos:int -> int
  = "unsafe_read_uint16_t_swap" "noalloc"

external unsafe_write_int16           : t -> pos:int -> int -> unit
  = "unsafe_write_int16_t"       "noalloc"
external unsafe_write_int16_swap      : t -> pos:int -> int -> unit
  = "unsafe_write_int16_t_swap"  "noalloc"
external unsafe_write_uint16          : t -> pos:int -> int -> unit
  = "unsafe_write_uint16_t"      "noalloc"
external unsafe_write_uint16_swap     : t -> pos:int -> int -> unit
  = "unsafe_write_uint16_t_swap" "noalloc"

external unsafe_read_int32_int        : t -> pos:int -> int
  = "unsafe_read_int32_t"         "noalloc"
external unsafe_read_int32_int_swap   : t -> pos:int -> int
  = "unsafe_read_int32_t_swap"    "noalloc"

external unsafe_write_int32_int       : t -> pos:int -> int -> unit
  = "unsafe_write_int32_t"        "noalloc"
external unsafe_write_int32_int_swap  : t -> pos:int -> int -> unit
  = "unsafe_write_int32_t_swap"   "noalloc"

external unsafe_read_int32            : t -> pos:int -> Int32.t
  = "unsafe_read_int32"
external unsafe_read_int32_swap       : t -> pos:int -> Int32.t
  = "unsafe_read_int32_swap"
external unsafe_write_int32           : t -> pos:int -> Int32.t -> unit
  = "unsafe_write_int32"       "noalloc"
external unsafe_write_int32_swap      : t -> pos:int -> Int32.t -> unit
  = "unsafe_write_int32_swap"  "noalloc"

(* [unsafe_read_int64_int] and [unsafe_read_int64_int_swap] may raise exceptions on
   both 32-bit and 64-bit platforms.  As such, they cannot be marked [noalloc].
*)
external unsafe_read_int64_int        : t -> pos:int -> int
  = "unsafe_read_int64_t"
external unsafe_read_int64_int_swap   : t -> pos:int -> int
  = "unsafe_read_int64_t_swap"

external unsafe_write_int64_int       : t -> pos:int -> int -> unit
  = "unsafe_write_int64_t"      "noalloc"
external unsafe_write_int64_int_swap  : t -> pos:int -> int -> unit
  = "unsafe_write_int64_t_swap" "noalloc"

external unsafe_read_int64            : t -> pos:int -> Int64.t
  = "unsafe_read_int64"
external unsafe_read_int64_swap       : t -> pos:int -> Int64.t
  = "unsafe_read_int64_swap"
external unsafe_write_int64           : t -> pos:int -> Int64.t -> unit
  = "unsafe_write_int64"
external unsafe_write_int64_swap      : t -> pos:int -> Int64.t -> unit
  = "unsafe_write_int64_swap"


IFDEF ARCH_BIG_ENDIAN THEN
let unsafe_get_int16_be  = unsafe_read_int16
let unsafe_get_int16_le  = unsafe_read_int16_swap
let unsafe_get_uint16_be = unsafe_read_uint16
let unsafe_get_uint16_le = unsafe_read_uint16_swap

let unsafe_set_int16_be  = unsafe_write_int16
let unsafe_set_int16_le  = unsafe_write_int16_swap
let unsafe_set_uint16_be = unsafe_write_uint16
let unsafe_set_uint16_le = unsafe_write_uint16_swap

let unsafe_get_int32_t_be  = unsafe_read_int32
let unsafe_get_int32_t_le  = unsafe_read_int32_swap
let unsafe_set_int32_t_be  = unsafe_write_int32
let unsafe_set_int32_t_le  = unsafe_write_int32_swap

let unsafe_get_int32_be  = unsafe_read_int32_int
let unsafe_get_int32_le  = unsafe_read_int32_int_swap
let unsafe_set_int32_be  = unsafe_write_int32_int
let unsafe_set_int32_le  = unsafe_write_int32_int_swap

let unsafe_get_int64_be_exn = unsafe_read_int64_int
let unsafe_get_int64_le_exn = unsafe_read_int64_int_swap
let unsafe_set_int64_be  = unsafe_write_int64_int
let unsafe_set_int64_le  = unsafe_write_int64_int_swap

let unsafe_get_int64_t_be  = unsafe_read_int64
let unsafe_get_int64_t_le  = unsafe_read_int64_swap
let unsafe_set_int64_t_be  = unsafe_write_int64
let unsafe_set_int64_t_le  = unsafe_write_int64_swap
ELSE
let unsafe_get_int16_be  = unsafe_read_int16_swap
let unsafe_get_int16_le  = unsafe_read_int16
let unsafe_get_uint16_be = unsafe_read_uint16_swap
let unsafe_get_uint16_le = unsafe_read_uint16

let unsafe_set_int16_be  = unsafe_write_int16_swap
let unsafe_set_int16_le  = unsafe_write_int16
let unsafe_set_uint16_be = unsafe_write_uint16_swap
let unsafe_set_uint16_le = unsafe_write_uint16

let unsafe_get_int32_be  = unsafe_read_int32_int_swap
let unsafe_get_int32_le  = unsafe_read_int32_int
let unsafe_set_int32_be  = unsafe_write_int32_int_swap
let unsafe_set_int32_le  = unsafe_write_int32_int

let unsafe_get_int32_t_be  = unsafe_read_int32_swap
let unsafe_get_int32_t_le  = unsafe_read_int32
let unsafe_set_int32_t_be  = unsafe_write_int32_swap
let unsafe_set_int32_t_le  = unsafe_write_int32

let unsafe_get_int64_be_exn  = unsafe_read_int64_int_swap
let unsafe_get_int64_le_exn  = unsafe_read_int64_int
let unsafe_set_int64_be  = unsafe_write_int64_int_swap
let unsafe_set_int64_le  = unsafe_write_int64_int

let unsafe_get_int64_t_be  = unsafe_read_int64_swap
let unsafe_get_int64_t_le  = unsafe_read_int64
let unsafe_set_int64_t_be  = unsafe_write_int64_swap
let unsafe_set_int64_t_le  = unsafe_write_int64
ENDIF

TEST_MODULE "binary accessors" = struct

  let buf = create 256

  let test_accessor ~buf ~fget ~fset vals =
    Core_list.for_all vals ~f:(fun x -> fset buf ~pos:0 x; x = fget buf ~pos:0)
  ;;

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int16_le
    ~fset:unsafe_set_int16_le
    [-32768; -1; 0; 1; 32767]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_uint16_le
    ~fset:unsafe_set_uint16_le
    [0; 1; 65535]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int16_be
    ~fset:unsafe_set_int16_be
    [-32768; -1; 0; 1; 32767]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_uint16_be
    ~fset:unsafe_set_uint16_be
    [0; 1; 65535]


IFDEF ARCH_SIXTYFOUR THEN

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int32_le
    ~fset:unsafe_set_int32_le
    [Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int32_be
    ~fset:unsafe_set_int32_be
    [Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int64_le_exn
    ~fset:unsafe_set_int64_le
    [Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int64_be_exn
    ~fset:unsafe_set_int64_be
    [Int64.to_int_exn (-0x4000_0000_0000_0000L);
     Int64.to_int_exn (-2147483648L); -1; 0; 1; Int64.to_int_exn 2147483647L;
     Int64.to_int_exn 0x3fff_ffff_ffff_ffffL]

ENDIF (* ARCH_SIXTYFOUR *)

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int64_t_le
    ~fset:unsafe_set_int64_t_le
    [-0x8000_0000_0000_0000L;
     -0x789A_BCDE_F012_3456L;
     -0xFFL;
     Int64.minus_one;
     Int64.zero;
     Int64.one;
     0x789A_BCDE_F012_3456L;
     0x7FFF_FFFF_FFFF_FFFFL]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int64_t_be
    ~fset:unsafe_set_int64_t_be
    [-0x8000_0000_0000_0000L;
     -0x789A_BCDE_F012_3456L;
     -0xFFL;
     Int64.minus_one;
     Int64.zero;
     Int64.one;
     0x789A_BCDE_F012_3456L;
     0x7FFF_FFFF_FFFF_FFFFL]

  TEST = test_accessor ~buf
    ~fget:unsafe_get_int64_t_be
    ~fset:unsafe_set_int64_t_be
    [-0x8000_0000_0000_0000L;
     -0x789A_BCDE_F012_3456L;
     -0xFFL;
     Int64.minus_one;
     Int64.zero;
     Int64.one;
     0x789A_BCDE_F012_3456L;
     0x7FFF_FFFF_FFFF_FFFFL]

  (* Test 63/64-bit precision boundary.

     Seen on a data stream the constant 0x4000_0000_0000_0000 is supposed to
     represent a 64-bit positive integer (2^62).

     Whilst this bit pattern does fit inside an OCaml value of type [int] on a
     64-bit machine, it is the representation of a negative number (the most negative
     number representable in type [int]), and in particular is not the representation
     of 2^62.  It is thus suitable for this test.
  *)
  TEST = let too_big = 0x4000_0000_0000_0000L in
    unsafe_set_int64_t_le buf ~pos:0 too_big;
    try
      let _too_small = unsafe_get_int64_le_exn buf ~pos:0 in false
    with _ -> true
end


