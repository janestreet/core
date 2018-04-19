module Bigstring_in_this_directory = Bigstring
module Md5_in_this_directory       = Md5

open! Import

module Bigstring = Bigstring_in_this_directory
module Md5       = Md5_in_this_directory

include Core_kernel.Bigbuffer

open Core_kernel.Core_kernel_private.Bigbuffer_internal

let add_channel buf ic len =
  let buf = __internal buf in
  if len < 0 then invalid_arg "Bigbuffer.add_channel";
  let pos = buf.pos in
  if pos + len > buf.len then resize buf len;
  Bigstring.really_input ic buf.bstr ~pos ~len;
  buf.pos <- pos + len;
;;

let output_buffer oc buf =
  let buf = __internal buf in
  Bigstring.really_output oc buf.bstr ~len:buf.pos
;;

let md5 t =
  let t = __internal t in
  Md5.digest_subbigstring t.bstr ~pos:0 ~len:t.pos
;;
