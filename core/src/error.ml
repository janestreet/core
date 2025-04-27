include Base.Error
include Info.Extend (Base.Error)

let failwiths ?strict ~(here : [%call_pos]) message a sexp_of_a =
  let here = if Source_code_position0.is_dummy here then None else Some here in
  raise (create ?strict ?here message a sexp_of_a)
;;

let failwithp ?strict here message a sexp_of_a =
  raise (create ?strict ~here message a sexp_of_a)
;;
