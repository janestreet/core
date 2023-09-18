open! Import
include Comparable_intf
module Infix = Base.Comparable.Infix
module Comparisons = Base.Comparable.Comparisons

module Validate (T : sig
  type t [@@deriving compare, sexp_of]
end) : Validate with type t := T.t = struct
  module V = Validate
  open Maybe_bound

  let to_string t = Base.Sexp.to_string (T.sexp_of_t t)

  let validate_bound ~min ~max t =
    V.bounded ~name:to_string ~lower:min ~upper:max ~compare:T.compare t
  ;;

  let validate_lbound ~min t = validate_bound ~min ~max:Unbounded t
  let validate_ubound ~max t = validate_bound ~max ~min:Unbounded t
end

module Validate_with_zero (T : sig
  type t [@@deriving compare, sexp_of]

  val zero : t
end) =
struct
  module V = Validate (T)
  include V

  (* Preallocate the interesting bounds to minimize allocation in the implementations of
     [validate_*]. *)
  let excl_zero = Maybe_bound.Excl T.zero
  let incl_zero = Maybe_bound.Incl T.zero
  let validate_positive t = validate_lbound ~min:excl_zero t
  let validate_non_negative t = validate_lbound ~min:incl_zero t
  let validate_negative t = validate_ubound ~max:excl_zero t
  let validate_non_positive t = validate_ubound ~max:incl_zero t
end

module With_zero (T : sig
  type t [@@deriving compare, sexp_of]

  val zero : t
end) =
struct
  include Validate_with_zero (T)
  include Base.Comparable.With_zero (T)
end

module Map_and_set_binable_using_comparator (T : sig
  type t [@@deriving bin_io, compare, sexp]

  include Comparator.S with type t := t
end) =
struct
  include T
  module Map = Map.Make_binable_using_comparator (T)
  module Set = Set.Make_binable_using_comparator (T)
end

module Map_and_set_binable (T : sig
  type t [@@deriving bin_io, compare, sexp]
end) =
Map_and_set_binable_using_comparator (struct
  include T
  include Comparator.Make (T)
end)

module Poly (T : sig
  type t [@@deriving sexp]
end) =
struct
  module C = struct
    include T
    include Base.Comparable.Poly (T)
  end

  include C
  include Validate (C)
  module Replace_polymorphic_compare : Comparisons with type t := t = C
  module Map = Map.Make_using_comparator (C)
  module Set = Set.Make_using_comparator (C)
end

module Make_plain_using_comparator (T : sig
  type t [@@deriving sexp_of]

  include Comparator.S with type t := t
end) : S_plain with type t := T.t and type comparator_witness = T.comparator_witness =
struct
  include T
  module M = Base.Comparable.Make_using_comparator (T)
  include M

  include Validate (struct
    include T
    include M
  end)

  module Replace_polymorphic_compare : Comparisons with type t := t = M
  module Map = Map.Make_plain_using_comparator (T)
  module Set = Set.Make_plain_using_comparator (T)
end

module Make_plain (T : sig
  type t [@@deriving compare, sexp_of]
end) =
Make_plain_using_comparator (struct
  include T
  include Comparator.Make (T)
end)

module Make_using_comparator (T : sig
  type t [@@deriving sexp]

  include Comparator.S with type t := t
end) : S with type t := T.t and type comparator_witness = T.comparator_witness = struct
  include T
  module M = Base.Comparable.Make_using_comparator (T)
  include M

  include Validate (struct
    include T
    include M
  end)

  module Replace_polymorphic_compare : Comparisons with type t := t = M
  module Map = Map.Make_using_comparator (T)
  module Set = Set.Make_using_comparator (T)
end

module Make (T : sig
  type t [@@deriving compare, sexp]
end) : S with type t := T.t = Make_using_comparator (struct
  include T
  include Comparator.Make (T)
end)

module Make_binable_using_comparator (T : sig
  type t [@@deriving bin_io, sexp]

  include Comparator.S with type t := t
end) =
struct
  include T
  module M = Base.Comparable.Make_using_comparator (T)

  include Validate (struct
    include T

    let compare = T.comparator.compare
  end)

  include M
  module Replace_polymorphic_compare : Comparisons with type t := t = M
  module Map = Map.Make_binable_using_comparator (T)
  module Set = Set.Make_binable_using_comparator (T)
end

module Make_binable (T : sig
  type t [@@deriving bin_io, compare, sexp]
end) =
struct
  include Make_binable_using_comparator (struct
    include T
    include Comparator.Make (T)
  end)
end

module Extend_plain
  (M : Base.Comparable.S) (X : sig
    type t = M.t [@@deriving sexp_of]
  end) =
struct
  module T = struct
    include M

    include (
      X :
        sig
          type t = M.t [@@deriving sexp_of]
        end
        with type t := t)
  end

  include T
  include Validate (T)
  module Replace_polymorphic_compare : Comparisons with type t := t = M
  module Map = Map.Make_plain_using_comparator (T)
  module Set = Set.Make_plain_using_comparator (T)
end

module Extend
  (M : Base.Comparable.S) (X : sig
    type t = M.t [@@deriving sexp]
  end) =
struct
  module T = struct
    include M

    include (
      X :
        sig
          type t = M.t [@@deriving sexp]
        end
        with type t := t)
  end

  include T
  include Validate (T)
  module Replace_polymorphic_compare : Comparisons with type t := t = M
  module Map = Map.Make_using_comparator (T)
  module Set = Set.Make_using_comparator (T)
end

module Extend_binable
  (M : Base.Comparable.S) (X : sig
    type t = M.t [@@deriving bin_io, sexp]
  end) =
struct
  module T = struct
    include M

    include (
      X :
        sig
          type t = M.t [@@deriving bin_io, sexp]
        end
        with type t := t)
  end

  include T
  include Validate (T)
  module Replace_polymorphic_compare : Comparisons with type t := t = M
  module Map = Map.Make_binable_using_comparator (T)
  module Set = Set.Make_binable_using_comparator (T)
end

module Inherit (C : sig
  type t [@@deriving compare]
end) (T : sig
  type t [@@deriving sexp]

  val component : t -> C.t
end) =
Make (struct
  type t = T.t [@@deriving sexp]

  let compare t t' = C.compare (T.component t) (T.component t')
end)

include (Base.Comparable : With_compare)

module Stable = struct
  module V1 = struct
    module type S = sig
      type comparable
      type comparator_witness

      module Map :
        Map.Stable.V1.S
          with type key := comparable
          with type comparator_witness := comparator_witness

      module Set :
        Set.Stable.V1.S
          with type elt := comparable
          with type elt_comparator_witness := comparator_witness
    end

    module Make (X : Stable_module_types.S0) = struct
      module Map = Map.Stable.V1.Make (X)
      module Set = Set.Stable.V1.Make (X)
    end

    module With_stable_witness = struct
      module type S = sig
        type comparable
        type comparator_witness

        module Map :
          Map.Stable.V1.With_stable_witness.S
            with type key := comparable
            with type comparator_witness := comparator_witness

        module Set :
          Set.Stable.V1.With_stable_witness.S
            with type elt := comparable
            with type elt_comparator_witness := comparator_witness
      end

      module Make (X : Stable_module_types.With_stable_witness.S0) = struct
        module Map = Map.Stable.V1.With_stable_witness.Make (X)
        module Set = Set.Stable.V1.With_stable_witness.Make (X)
      end
    end
  end
end
