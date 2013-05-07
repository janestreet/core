open Comparable_intf

module type Infix               = Infix
module type Map_and_set_binable = Map_and_set_binable
module type S                   = S
module type S_binable           = S_binable
module type S_common            = S_common
module type Validate            = Validate
module type With_zero           = With_zero

type 'a bound = 'a Comparable_intf.bound = Incl of 'a | Excl of 'a | Unbounded

(** [lexicographic cmps x y] compares [x] and [y] lexicographically using functions in the
    list [cmps]. *)
val lexicographic : ('a -> 'a -> int) list -> 'a -> 'a -> int

(** Inherit comparability from a component. *)
module Inherit
  (C : sig type t with compare end)
  (T : sig
    type t with sexp
    val component : t -> C.t
  end) : S with type t = T.t

module Make (T : sig
  type t with compare, sexp
end) : S with type t := T.t

module Make_binable (T : sig
  type t with bin_io, compare, sexp
end) : S_binable with type t := T.t

module Map_and_set_binable (T : Comparator.Pre_binable)
  : Map_and_set_binable with type t := T.t

module Poly (T : sig type t with sexp end) : S with type t := T.t

module Validate (T : sig type t with compare, sexp end) : Validate with type t := T.t

module With_zero
         (T : sig
            type t with compare, sexp
            val zero : t
            include Validate with type t := t
          end) : With_zero with type t := T.t

module Validate_with_zero
         (T : sig
            type t with compare, sexp
            val zero : t
          end)
  : sig
    include Validate  with type t := T.t
    include With_zero with type t := T.t
  end

(* [Check_sexp_conversion] checks that conversion of a map or set to a sexp uses the same
   sexp conversion as the underlying element. *)
module Check_sexp_conversion (M : sig
  type t with sexp_of
  include S with type t := t
  val examples : t list
end) : sig end
