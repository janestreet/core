open! Import
include Base.Option

(* We hide the constructors of the non-value types so that [None] does not get inferred as
   being e.g. the [float64] version below. This is the same as using [[%%rederive]],
   except we can't currently use [[%%rederive]] when we need the sig to be [@@ portable].
*)
  include%template (
  struct
    type ('a : k) t = ('a Constructors.t[@kind k]) =
      | None
      | Some of 'a
    [@@deriving bin_io ~localize] [@@kind k = (float64, bits32, bits64, word)]
  end :
  sig
  @@ portable
    type ('a : k) t = ('a Constructors.t[@kind k])
    [@@deriving bin_io ~localize] [@@kind k = (float64, bits32, bits64, word)]
  end)

type ('a : value_or_null) t = 'a option [@@deriving typerep, stable_witness]

[%%rederive type 'a t = 'a option [@@deriving bin_io ~localize]]

include%template Comparator.Derived [@modality portable] (struct
    type nonrec 'a t = 'a t [@@deriving sexp_of, compare ~localize]
  end)

let validate ~none ~some t =
  match t with
  | None -> Validate.name "none" (Validate.protect none ())
  | Some x -> Validate.name "some" (Validate.protect some x)
;;

[%%template
[@@@mode.default p = (nonportable, portable)]

let quickcheck_generator = (Base_quickcheck.Generator.option [@mode p])
let quickcheck_observer = (Base_quickcheck.Observer.option [@mode p])
let quickcheck_shrinker = (Base_quickcheck.Shrinker.option [@mode p])]

module Stable = struct
  module V1 = struct
    type nonrec ('a : value_or_null) t = 'a t
    [@@deriving
      compare ~localize, equal ~localize, hash, sexp, sexp_grammar, stable_witness]

    [%%rederive type nonrec 'a t = 'a t [@@deriving bin_io ~localize]]
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
    external unbox
      :  ('a option[@local_opt])
      -> ('a[@local_opt])
      @@ portable
      = "%field0_immut"

    [%%else]

    external unbox : ('a option[@local_opt]) -> ('a[@local_opt]) @@ portable = "%field0"

    [%%endif]

    let%template unsafe_value : type a. a t @ m -> a @ m = unbox
    [@@mode m = (global, local)]
    ;;

    let is_none = is_none
  end
end
