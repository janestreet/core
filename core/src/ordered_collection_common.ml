include Base.Ordered_collection_common

let normalize ~length_fun t i = if i < 0 then i + length_fun t else i

let slice ~length_fun ~sub_fun t start stop =
  let stop = if stop = 0 then length_fun t else stop in
  let pos = normalize ~length_fun t start in
  let len = normalize ~length_fun t stop - pos in
  sub_fun t ~pos ~len
;;
