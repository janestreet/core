open Sexplib.Std
open Bin_prot.Std

type 'a t = 'a lazy_t with sexp, bin_io

include (Lazy : module type of Lazy with type 'a t := 'a t)
