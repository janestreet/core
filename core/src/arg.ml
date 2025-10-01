open! Import
include Stdlib.Arg
module String = Base.String

type t = key * spec * doc

let sort_and_align lst =
  align (Base.List.sort lst ~compare:(fun (a, _, _) (b, _, _) -> String.compare a b))
;;
