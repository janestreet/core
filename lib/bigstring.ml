INCLUDE "config.mlh"
open Printf
open Unix
open Bigarray
open Common
open Sexplib.Std
open Result.Export

module Z : sig
  type t = (char, int8_unsigned_elt, c_layout) Array1.t
  include Sexpable.S with type t := t
  include Binable.S with type t := t
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

let create n = Array1.create Bigarray.char c_layout n
let length (bstr : t) = Array1.dim bstr
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

let get_opt_pos ~loc ~var = function
  | None -> 0
  | Some pos ->
      if pos < 0 then invalid_arg (sprintf "Bigstring.%s: %s < 0" loc var);
      pos

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
