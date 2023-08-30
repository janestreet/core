open! Import
include Base.Option

type 'a t = 'a option [@@deriving bin_io ~localize, typerep, stable_witness]

include Comparator.Derived (struct
  type nonrec 'a t = 'a t [@@deriving sexp_of, compare]
end)

let validate ~none ~some t =
  let module V = Validate in
  match t with
  | None -> V.name "none" (V.protect none ())
  | Some x -> V.name "some" (V.protect some x)
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

    let unsafe_value (type a) (t : a t) : a =
      let (Unchecked_some value) = (Obj.magic t : a Unchecked_some.t) in
      value
    ;;
  end
end
