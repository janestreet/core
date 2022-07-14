open! Import
include Fdeque

let enqueue = enqueue_back
let peek_exn = peek_front_exn
let peek = peek_front
let dequeue_exn = dequeue_front_exn
let dequeue = dequeue_front
let drop_exn = drop_front_exn
let to_sequence = Front_to_back.to_sequence
let of_sequence = Front_to_back.of_sequence
let gen_with_length = Fdeque.gen_with_length
let gen_non_empty = Fdeque.gen_non_empty 
let gen_filtered = Fdeque.gen_filtered 
let gen_permutations = Fdeque.gen_permutations
(* Deprecated aliases *)
let top = peek
let top_exn = peek_exn
let discard_exn = drop_exn
