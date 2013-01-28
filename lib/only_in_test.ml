open Std_internal

type 'a t = 'a Lazy.t

let of_thunk f = lazy (f ())

let force = Lazy.force

include (Monad.Make (struct
  type 'a z = 'a t
  type 'a t = 'a z
  let return x = lazy x
  let bind x f = lazy (force (f (force x)))
end) : Monad.S with type 'a t := 'a t)


