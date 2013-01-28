type t = Less | Equal | Greater with bin_io, compare, sexp

module Export = struct
  type _ordering = t =
  | Less
  | Equal
  | Greater
end

let of_int n =
  if n < 0
  then Less
  else if n = 0
  then Equal
  else Greater
;;

TEST = of_int (-10) = Less
TEST = of_int (-1)  = Less
TEST = of_int 0     = Equal
TEST = of_int 1     = Greater
TEST = of_int 10    = Greater

TEST = of_int (Pervasives.compare 0 1) = Less
TEST = of_int (Pervasives.compare 1 1) = Equal
TEST = of_int (Pervasives.compare 1 0) = Greater
