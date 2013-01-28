include Caml.Arg

type t = key * spec * doc

let sort_and_align lst =
  align (ListLabels.sort lst ~cmp:(fun (a,_,_) (b,_,_) ->
      compare a b
    ))
;;
