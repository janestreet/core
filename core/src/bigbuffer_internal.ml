open! Import

type t =
  { mutable bstr : Bigstring.t
  ; mutable pos : int
  ; mutable len : int
  ; init : Bigstring.t
  }
[@@deriving sexp_of]

let resize buf more =
  let min_len = buf.len + more in
  let new_len = min_len + min_len in
  let new_buf = Bigstring.create new_len in
  Bigstring.blito ~src:buf.bstr ~src_len:buf.pos ~dst:new_buf ();
  buf.bstr <- new_buf;
  buf.len <- new_len
;;
