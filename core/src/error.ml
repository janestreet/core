include Base.Error
include Info.Extend (Base.Error)

let failwiths ?strict ~here message a sexp_of_a =
  raise (create ?strict ~here message a sexp_of_a)
;;

let failwithp ?strict here message a sexp_of_a =
  raise (create ?strict ~here message a sexp_of_a)
;;
