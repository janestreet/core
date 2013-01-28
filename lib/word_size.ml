type t = W32 | W64

let num_bits = function W32 -> 32 | W64 -> 64
  
let word_size =
  match Sys.word_size with
  | 32 -> W32
  | 64 -> W64
  | _ -> failwith "unknown word size"
;;

