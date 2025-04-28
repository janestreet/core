open! Import
include Base.Option

(* We hide the constructors of the non-value types so that [None] does not get inferred as
   being e.g. the [float64] version below. This is the same as using [[%%rederive]],
   except we can't currently use [[%%rederive]] when we need the sig to be [@@ portable].
*)
  include%template (
  struct
    type 'a t = ('a Constructors.t[@kind k]) =
      | None
      | Some of 'a
    [@@deriving bin_io ~localize] [@@kind k = (float64, bits32, bits64, word)]
  end :
  sig
    type 'a t = ('a Constructors.t[@kind k])
    [@@deriving bin_io ~localize] [@@kind k = (float64, bits32, bits64, word)]
  end)

type 'a t = 'a option [@@deriving bin_io ~localize, typerep, stable_witness]

include%template Comparator.Derived [@modality portable] (struct
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
    [@@deriving
      bin_io ~localize, compare, equal, hash, sexp, sexp_grammar, stable_witness]
  end
end

module Optional_syntax = struct
  module Optional_syntax = struct
    let is_none = is_none

    (* [unsafe_value] is only safe to call when [is_none] returns [false]. To avoid
       repeating the [is_none] check, we declare [Unchecked_some]. [Unchecked_some x]
       has the same representation as [Some x], but the type has no [None] clause.

       We make sure all this works with tests of [unsafe_value] in test_option.ml.

       We tried using [Obj.field] instead. It generates much worse native code due to
       float array representations. *)

    module Unchecked_some = struct
      (* Warning 37 tells us [Unchecked_some] is never used as a constructor. This is
         intentional, so we disable the warning. *)
      type 'a t = Unchecked_some of 'a [@@ocaml.boxed] [@@ocaml.warning "-37"]
    end

    external magic_transparent : 'a t -> 'a Unchecked_some.t = "%identity"

    let unsafe_value (type a) (t : a t) : a =
      let (Unchecked_some value) = magic_transparent t in
      value
    ;;
  end
end
