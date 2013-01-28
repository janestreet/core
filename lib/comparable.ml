module type Infix = Polymorphic_compare_intf.Infix
module type Polymorphic_compare = Polymorphic_compare_intf.S

module type S_common = sig
  include Polymorphic_compare
  val ascending : t -> t -> int
  val descending : t -> t -> int

  module Replace_polymorphic_compare : Polymorphic_compare with type t := t

  type comparator
  val comparator : (t, comparator) Comparator.t
end

module type S = sig
  include S_common

  module Map : Core_map.S
    with type Key.t = t
    with type Key.comparator = comparator
  module Set : Core_set.S
    with type Elt.t = t
    with type Elt.comparator = comparator
end

module type Map_and_set_binable = sig
  type t
  include Comparator.S_binable with type t := t
  module Map : Core_map.S_binable
    with type Key.t = t
    with type Key.comparator = comparator
  module Set : Core_set.S_binable
    with type Elt.t = t
    with type Elt.comparator = comparator
end

module type S_binable = sig
  include S_common
  include Map_and_set_binable
    with type t := t
    with type comparator := comparator
end

module Map_and_set_binable (T : Comparator.Pre_binable)
  : Map_and_set_binable with type t := T.t = struct
  type t = T.t
  module C = (Comparator.Make_binable (T) : Comparator.S_binable with type t = t)
  include (C : Comparator.S_binable with type t := t with type comparator = C.comparator)
  module Map = Core_map.Make_binable_using_comparator (C)
  module Set = Core_set.Make_binable_using_comparator (C)
end

module Poly (T : sig type t with sexp end) : S with type t := T.t = struct
  module Replace_polymorphic_compare = struct
    type t = T.t with sexp
    include Polymorphic_compare
  end
  include Replace_polymorphic_compare
  let ascending = compare
  let descending x y = compare y x
  module C = (Comparator.Make (Replace_polymorphic_compare) : Comparator.S with type t = t)
  type 'a t_ = T.t
  include (C : Comparator.S1 with type 'a t := 'a t_ with type comparator = C.comparator)
  module Map = Core_map.Make_using_comparator (C)
  module Set = Core_set.Make_using_comparator (C)
end

module Make_common (T : sig
  type t with sexp
  val compare : t -> t -> int
end) = struct
  type t = T.t
  module Replace_polymorphic_compare = struct
    let compare = T.compare
    let (>) a b = compare a b > 0
    let (<) a b = compare a b < 0
    let (>=) a b = compare a b >= 0
    let (<=) a b = compare a b <= 0
    let (=) a b = compare a b = 0
    let (<>) a b = compare a b <> 0
    let equal = (=)
    let min t t' = if t <= t' then t else t'
    let max t t' = if t >= t' then t else t'
  end
  include Replace_polymorphic_compare
  let ascending = compare
  let descending t t' = compare t' t
end

module Make (T : sig
  type t
  include Sexpable.S with type t := t
  val compare : t -> t -> int
end) : S with type t := T.t = struct
  module C = Comparator.Make (T)
  include (C : Comparator.S
             with type t := C.t
             with type comparator = C.comparator)
  include Make_common (C)
  module Map = Core_map.Make_using_comparator (C)
  module Set = Core_set.Make_using_comparator (C)
end

module Make_binable (T : sig
  type t with bin_io, sexp
  val compare : t -> t -> int
end) : S_binable with type t := T.t = struct
  module C = Comparator.Make_binable (T)
  include (C : Comparator.S_binable
                 with type t := C.t
                 with type comparator = C.comparator)
  include Make_common (C)
  module Map = Core_map.Make_binable_using_comparator (C)
  module Set = Core_set.Make_binable_using_comparator (C)
end

(** Inherit comparability from a component. *)
module Inherit
  (C : sig
    type t
    val compare : t -> t -> int
  end)
  (T : sig
    type t with sexp
    val component : t -> C.t
  end) : S with type t = T.t = struct

    type t = T.t

    include Make (struct
      type t = T.t with sexp
      let compare t t' = C.compare (T.component t) (T.component t')
    end)

  end

(* compare [x] and [y] lexicographically using functions in the list [cmps] *)
let lexicographic cmps x y =
  let rec loop = function
    | cmp :: cmps -> let res = cmp x y in if res = 0 then loop cmps else res
    | [] -> 0
  in
  loop cmps
;;
