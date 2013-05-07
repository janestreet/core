open Sexplib.Conv
module Sexp = Sexplib.Sexp
module List = ListLabels

include Comparable_intf

let sprintf = Printf.sprintf

module Validate
         (T : sig type t with compare, sexp end) : Validate with type t := T.t =
struct

  module V = Validate

  let to_string t = Sexp.to_string (T.sexp_of_t t)

  let validate_lbound ~min t =
    V.of_error_opt (
      match min with
      | Unbounded -> None
      | Incl b ->
        if T.compare t b >= 0
        then None
        else Some (sprintf "value %s < bound %s"  (to_string t) (to_string b))
      | Excl b ->
        if T.compare t b > 0
        then None
        else Some (sprintf "value %s <= bound %s" (to_string t) (to_string b))
    )
  ;;

  let validate_ubound ~max t =
    V.of_error_opt (
      match max with
      | Unbounded -> None
      | Incl b ->
        if T.compare t b <= 0
        then None
        else Some (sprintf "value %s > bound %s"  (to_string t) (to_string b))
      | Excl b ->
        if T.compare t b < 0
        then None
        else Some (sprintf "value %s >= bound %s" (to_string t) (to_string b))
    )
  ;;

  let validate_bound ~min ~max = V.all [ validate_lbound ~min; validate_ubound ~max ]
end

module With_zero
         (T : sig
            type t with compare, sexp
            val zero : t
            include Validate with type t := t
          end) = struct
  open T
  let validate_positive     t = validate_lbound ~min:(Excl zero) t
  let validate_non_negative t = validate_lbound ~min:(Incl zero) t
  let validate_negative     t = validate_ubound ~max:(Excl zero) t
  let validate_non_positive t = validate_ubound ~max:(Incl zero) t
  let is_positive     t = compare t zero >  0
  let is_non_negative t = compare t zero >= 0
  let is_negative     t = compare t zero <  0
  let is_non_positive t = compare t zero <= 0
end

module Validate_with_zero
         (T : sig
            type t with compare, sexp
            val zero : t
          end) = struct
  module V = Validate (T)
  include V
  include With_zero (struct include T include V end)
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
  include Validate (struct type nonrec t = t with compare, sexp end)
end

module Make_common (T : sig
  type t with compare, sexp
end) = struct
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
  include Validate (T)
end

module Make (T : sig
  type t with compare, sexp
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
  type t with bin_io, compare, sexp
end) : S_binable with type t := T.t = struct
  module C = Comparator.Make_binable (T)
  include (C : Comparator.S_binable
                 with type t := C.t
                 with type comparator = C.comparator)
  include Make_common (C)
  module Map = Core_map.Make_binable_using_comparator (C)
  module Set = Core_set.Make_binable_using_comparator (C)
end

module Inherit
  (C : sig type t with compare end)
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
