type never_returns = Nothing0.t with sexp_of

let never_returns (_ : never_returns) = assert false
