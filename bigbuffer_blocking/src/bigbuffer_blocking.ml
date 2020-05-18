open! Core
open! Import
open! Core_kernel.Bigbuffer
open! Core_kernel.Core_kernel_private.Bigbuffer_internal

let add_channel buf ic len =
  let buf = __internal buf in
  if len < 0 then invalid_arg "Bigbuffer_blocking.add_channel";
  let pos = buf.pos in
  if pos + len > buf.len then resize buf len;
  Bigstring_unix.really_input ic buf.bstr ~pos ~len;
  buf.pos <- pos + len;
;;

let output_buffer oc buf =
  let buf = __internal buf in
  Bigstring_unix.really_output oc buf.bstr ~len:buf.pos
;;

let md5 t =
  let t = __internal t in
  Md5.digest_subbigstring t.bstr ~pos:0 ~len:t.pos
;;
