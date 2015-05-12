external crc32 : string -> int64 = "caml_crc32"
let crc32hex s = Printf.sprintf "%08LX" (crc32 s)
