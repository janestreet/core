open Sexplib.Conv

include Core_int

exception Pid_must_be_positive of int with sexp

let ensure i = if i <= 0 then raise (Pid_must_be_positive i) else i

let of_int i = ensure i

let of_string string = ensure (of_string string)

let t_of_sexp sexp = 
  let t = t_of_sexp sexp in
  try
    ensure t
  with exn -> raise (Of_sexp_error (exn, sexp))
;;

let init = of_int 1
