(* This is factored out of float.ml in order to break a module dependency cycle.  *)



module type S = sig
  (* note: this is not the same as epsilon_float, rather it is intended to
     be a tolerance on human-entered floats *)
  val epsilon : float
  include Robustly_comparable.S with type t := float
end

module Make(T : sig val epsilon : float end) : S = struct
  let epsilon = T.epsilon
  let ( >=. ) x y = x >= y -. epsilon
  let ( <=. ) x y = y >=. x
  let ( =. ) x y = x >=. y && y >=. x
  let ( >. ) x y = x > y +. epsilon
  let ( <. ) x y = y >. x
  let ( <>. ) x y = not (x =. y)
  let robustly_compare x y =
    let d = x -. y in
    if      d < ~-. epsilon then -1
    else if d >     epsilon then  1
    else 0
end
