let invalid_argf = Core_printf.invalid_argf

let normalize ~length_fun t i =
  if i < 0 then length_fun t + i else i

let slice ~length_fun ~sub_fun t start stop =
  let stop = if stop = 0 then length_fun t else stop in
  let pos = normalize ~length_fun t start in
  let len = (normalize ~length_fun t stop) - pos in
  sub_fun t ~pos ~len

let get_pos_len_exn ?(pos=0) ?len ~length =
  if pos < 0 then
    invalid_argf "Negative position: %d < 0" pos ();
  if pos > length then
    invalid_argf "Start position after the end: %d > %d"
      pos length ();
  let maxlen = length - pos in
  let len =
    match len with
    | None -> maxlen
    | Some len ->
      if len < 0 then
        invalid_argf "Negative length: %d" len ()
      else if len > maxlen then
        invalid_argf "pos + len past end: %d + %d > %d"
          pos len length ()
      else
        len
  in
  (pos, len)
;;

let get_pos_len ?pos ?len ~length =
  try Result.Ok (get_pos_len_exn ?pos ?len ~length)
  with Invalid_argument s -> Result.Error s
