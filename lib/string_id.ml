open Std_internal

include String

let check s =
  let stripped = String.strip s in
  if not (String.(=) stripped s) then
    Some (sprintf ("'%s' is not a valid identifier " ^^
                      "because it has whitespace on the edge")
             s)
  else if String.(=) s "" then
    Some "Attempt to use empty identifier"
  else if String.contains s '|' then
    Some "Identifier contains a pipe '|'"
  else
    None

let of_string s =
  match check s with
  | None -> s
  | Some err -> invalid_arg err

let t_of_sexp sexp =
  let s = String.t_of_sexp sexp in
  match check s with
  | None -> s
  | Some err -> of_sexp_error err sexp
