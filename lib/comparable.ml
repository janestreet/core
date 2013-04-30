open Sexplib.Conv
module Sexp = Sexplib.Sexp
module List = ListLabels

module type Infix = Polymorphic_compare_intf.Infix
module type Polymorphic_compare = Polymorphic_compare_intf.S

module type S_common = sig
  include Polymorphic_compare
  (** [ascending] is identical to [compare]. [descending x y = ascending y x].  These are
      intended to be mnemonic when used like [List.sort ~cmp:ascending] and [List.sort
      ~cmp:descending], since they cause the list to be sorted in ascending or descending
      order, respectively. *)
  val ascending : t -> t -> int
  val descending : t -> t -> int

  val between : t -> low:t -> high:t -> bool

  module Replace_polymorphic_compare : sig
    include Polymorphic_compare with type t := t
    val _squelch_unused_module_warning_ : unit
  end

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
    let _squelch_unused_module_warning_ = ()
  end
  include Replace_polymorphic_compare
  let ascending = compare
  let descending x y = compare y x
  module C = (Comparator.Make (Replace_polymorphic_compare) : Comparator.S with type t = t)
  let between t ~low ~high = low <= t && t <= high
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
    module Without_squelch = struct
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
    include Without_squelch
    let _squelch_unused_module_warning_ = ()
  end
  include Replace_polymorphic_compare.Without_squelch
  let ascending = compare
  let descending t t' = compare t' t
  let between t ~low ~high = low <= t && t <= high
end

module Make (T : sig
  type t with sexp
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

module Check_sexp_conversion (M : sig
  type t with sexp_of
  include S with type t := t
  val examples : t list
end) : sig end = struct
  open M

  (* Check that conversion of a map or set to a sexp uses the same sexp conversion as
     the underlying element. *)
  TEST_UNIT =
    (* These tests all use single element sets and maps, and so do not depend on the
       order in which elements appear in sexps. *)
    List.iter examples ~f:(fun t ->
      let set = Set.of_list [ t ] in
      let set_sexp = Sexp.List [ sexp_of_t t ] in
      assert (Pervasives.(=) set_sexp (<:sexp_of< Set.t >> set));
      assert (Set.equal set (Set.t_of_sexp set_sexp));
      let map = Map.of_alist_exn [ t, () ] in
      let map_sexp = Sexp.List [ Sexp.List [ sexp_of_t t; Sexp.List [] ]] in
      assert (Pervasives.(=) map_sexp (<:sexp_of< unit Map.t >> map));
      assert (Map.equal (fun () () -> true)
                map (Map.t_of_sexp <:of_sexp< unit >> map_sexp)));
  ;;
end

(* compare [x] and [y] lexicographically using functions in the list [cmps] *)
let lexicographic cmps x y =
  let rec loop = function
    | cmp :: cmps -> let res = cmp x y in if res = 0 then loop cmps else res
    | [] -> 0
  in
  loop cmps
;;
