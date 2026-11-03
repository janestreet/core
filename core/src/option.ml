open! Import
include Base.Option

(* We hide the constructors of the non-value types so that [None] does not get inferred as
   being e.g. the [float64] version below. This is the same as using [[%%rederive]],
   except we can't currently use [[%%rederive]] when we need the sig to be [@@ portable].
*)
include (struct end : sig end)

type 'a t = 'a option [@@deriving bin_io, typerep, stable_witness]

include Comparator.Derived (struct
    type nonrec 'a t = 'a t [@@deriving sexp_of, compare]
  end)

let validate ~none ~some t =
  match t with
  | None -> Validate.name "none" (Validate.protect none ())
  | Some x -> Validate.name "some" (Validate.protect some x)
;;

let quickcheck_generator = Base_quickcheck.Generator.option
let quickcheck_observer = Base_quickcheck.Observer.option
let quickcheck_shrinker = Base_quickcheck.Shrinker.option

module Stable = struct
  module V1 = struct
    type nonrec 'a t = 'a t
    [@@deriving bin_io, compare, equal, hash, sexp, sexp_grammar, stable_witness]
  end
end

module Optional_syntax = struct
  module Optional_syntax = struct
    [%%if flambda_backend]

    (** [unsafe_value] is only safe to call when [is_none] returns [false]. To avoid
        repeating the [is_none] check, we directly extract field 0 using
        ["%field0_immut"]. Note that the codegen for ["%field0_immut"] is better than with
        [Obj.field t 0] here. The former is just a [mov], while the latter emits code to
        handle the packed float array case. *)
    external unbox : ('a option[@local_opt]) -> ('a[@local_opt]) = "%field0_immut"

    [%%else]

    external unbox : ('a option[@local_opt]) -> ('a[@local_opt]) = "%field0"

    [%%endif]

    let unsafe_value : type a. a t -> a = unbox
    let is_none = is_none
  end
end
