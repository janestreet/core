module Bigstring_in_this_directory = Bigstring
open! Import
module Bigstring = Bigstring_in_this_directory

open Bigstring

include Core_kernel.Bigstring_marshal

external unsafe_unmarshal
  : pos : int -> len : int -> t -> 'a = "bigstring_unmarshal_stub"

let unmarshal_from_sock ?buf sock =
  match buf with
  | None ->
    let buf_len = 4096 in
    let buf = create buf_len in
    really_recv sock ~len:Marshal.header_size buf;
    let data_len = marshal_data_size buf in
    let all_len = Marshal.header_size + data_len in
    let buf =
      if all_len <= buf_len then buf
      else create all_len
    in
    really_recv sock ~pos:Marshal.header_size ~len:data_len buf;
    unsafe_unmarshal ~pos:0 ~len:all_len buf
  | Some buf ->
    let buf_len = length buf in
    if buf_len < Marshal.header_size then
      failwith "Bigstring.unmarshal_from_sock: buffer cannot hold header";
    really_recv sock ~len:Marshal.header_size buf;
    let data_len = marshal_data_size buf in
    let all_len = Marshal.header_size + data_len in
    if all_len > buf_len then
      failwith
        "Bigstring.unmarshal_from_sock: buffer cannot hold header + data";
    really_recv sock ~pos:Marshal.header_size ~len:data_len buf;
    unsafe_unmarshal ~pos:0 ~len:all_len buf
;;

let marshal_to_gen ?buf ?flags dest v ~f =
  let buf, len =
    match buf with
    | None ->
      let buf = marshal ?flags v in
      buf, length buf
    | Some buf -> buf, marshal_blit ?flags v buf
  in
  f dest buf ~len
;;

let marshal_to_fd ?buf ?flags fd v =
  marshal_to_gen ?buf ?flags fd v ~f:(fun fd buf ~len ->
    really_write fd buf ~len)

[%%import "config.h"]

[%%ifdef JSC_MSG_NOSIGNAL]

let really_send_no_sigpipe = Or_error.ok_exn really_send_no_sigpipe

let marshal_to_sock_no_sigpipe ?buf ?flags fd v =
  marshal_to_gen ?buf ?flags fd v ~f:(fun fd buf ~len ->
    really_send_no_sigpipe fd ?pos:None ?len:(Some len) buf)

let marshal_to_sock_no_sigpipe = Ok marshal_to_sock_no_sigpipe

[%%else]

let marshal_to_sock_no_sigpipe =
  Or_error.unimplemented "Bigstring_marshal.marshal_to_sock_no_sigpipe"

[%%endif]
