(** [Ordering] is intended to make code that matches on the result of a comparison
    more concise and easier to read.  For example, one would write:

    |  match Ordering.of_int (compare x y) with
    |  | Less -> ...
    |  | Equal -> ...
    |  | Greater -> ...

    rather than:

    |  let r = compare x y in
    |  if r < 0 then
    |    ...
    |  else if r = 0 then
    |    ...
    |  else
    |    ...
*)

type t =
| Less
| Equal
| Greater
with bin_io, compare, sexp

(** [of_int n] is

    | Less     if n < 0
    | Equal    if n = 0
    | Greater  if n > 0
*)
val of_int : int -> t

module Export : sig
  type _ordering = t =
  | Less
  | Equal
  | Greater
end
