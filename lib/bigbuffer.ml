include Core_kernel.Std.Bigbuffer

open Core_kernel.Bigbuffer_internal

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
