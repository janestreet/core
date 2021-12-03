open! Import

type 'a t = 'a [@@deriving sexp_of]

external is_heap_block : Caml.Obj.t -> bool = "core_heap_block_is_heap_block" [@@noalloc]

let is_ok v = is_heap_block (Caml.Obj.repr v)
let create v = if is_ok v then Some v else None

let create_exn v =
  if is_ok v then v else failwith "Heap_block.create_exn called with non heap block"
;;

let value t = t
let bytes_per_word = Word_size.(num_bits word_size) / 8

let bytes (type a) (t : a t) =
  (Caml.Obj.size (Caml.Obj.repr (t : a t)) + 1) * bytes_per_word
;;
