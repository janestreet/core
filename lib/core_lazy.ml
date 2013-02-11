open Sexplib.Std
open Bin_prot.Std

type 'a t = 'a lazy_t with bin_io, sexp

include (Lazy : module type of Lazy with type 'a t := 'a t)

let compare compare_a t1 t2 = compare_a (force t1) (force t2)
