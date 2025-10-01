open! Import
include Comparable_intf

[%%template
[@@@mode.default m = (local, global)]

module%template.portable [@modality p] Infix =
  Base.Comparable.Infix
  [@mode m]
  [@modality p]

module%template.portable [@modality p] Comparisons =
  Base.Comparable.Comparisons
  [@modality p]
  [@mode m]

module%template.portable
  [@modality p] Validate (T : sig
    type t [@@deriving (compare [@mode m]), sexp_of]
  end) : Validate [@modality p] with type t := T.t = struct
  module V = Validate
  open Maybe_bound

  let to_string t = Base.Sexp.to_string (T.sexp_of_t t)

  [%%template
  [@@@mode.default p = (p, nonportable)]

  let validate_bound ~min ~max t =
    V.bounded ~name:to_string ~lower:min ~upper:max ~compare:T.compare t
  ;;

  let validate_lbound ~min t = (validate_bound [@mode p]) ~min ~max:Unbounded t
  let validate_ubound ~max t = (validate_bound [@mode p]) ~max ~min:Unbounded t]
end

module Validate_with_zero (T : sig
  @@ p
    type t : value mod c p [@@deriving (compare [@mode m]), sexp_of]

    val zero : t
  end) =
struct
  module V = Validate [@mode m] [@modality p] (T)
  include V

  (* Preallocate the interesting bounds to minimize allocation in the implementations of
     [validate_*]. *)
  let excl_zero = Maybe_bound.Excl T.zero
  let incl_zero = Maybe_bound.Incl T.zero

  [%%template
  [@@@mode.default p = (p, nonportable)]

  let validate_positive t = (validate_lbound [@mode p]) ~min:excl_zero t
  let validate_non_negative t = (validate_lbound [@mode p]) ~min:incl_zero t
  let validate_negative t = (validate_ubound [@mode p]) ~max:excl_zero t
  let validate_non_positive t = (validate_ubound [@mode p]) ~max:incl_zero t]
end
[@@modality (p, c) = ((nonportable, uncontended), (portable, contended))]

module With_zero (T : sig
  @@ p
    type t : value mod c p [@@deriving (compare [@mode m]), sexp_of]

    val zero : t
  end) =
struct
  include Validate_with_zero [@mode m] [@modality p] (T)
  include Base.Comparable.With_zero [@modality p] [@mode m] (T)
end
[@@modality (p, c) = ((nonportable, uncontended), (portable, contended))]

module%template.portable
  [@modality p] Map_and_set_binable_using_comparator (T : sig
    type t [@@deriving bin_io, (compare [@mode m]), sexp]

    include Comparator.S [@modality p] with type t := t
  end) =
struct
  include T
  module Map = Map.Make_binable_using_comparator [@modality p] (T)
  module Set = Set.Make_binable_using_comparator [@modality p] (T)
end

module%template.portable
  [@modality p] Map_and_set_binable (T : sig
    type t [@@deriving bin_io, (compare [@mode m]), sexp]
  end) =
Map_and_set_binable_using_comparator [@mode m] [@modality p] (struct
    include T
    include Comparator.Make [@modality p] (T)
  end)

module%template.portable
  [@modality p] Poly (T : sig
    type t [@@deriving sexp]
  end) =
struct
  module C = struct
    include T
    include Base.Comparable.Poly [@modality p] [@mode m] (T)
  end

  include C
  include Validate [@mode m] [@modality p] (C)

  module Replace_polymorphic_compare : sig @@ p
    include Comparisons [@mode m] with type t := t
  end =
    C

  module Map = Map.Make_using_comparator [@modality p] (C)
  module Set = Set.Make_using_comparator [@modality p] (C)
end

module%template.portable
  [@modality p] Make_plain_using_comparator (T : sig
    type t [@@deriving sexp_of]

    include Using_comparator_arg [@modality p] [@mode m] with type t := t
  end) :
  S_plain
  [@modality p] [@mode m]
  with type t := T.t
   and type comparator_witness = T.comparator_witness = struct
  include T
  module M = Base.Comparable.Make_using_comparator [@modality p] [@mode m] (T)
  include M

  include Validate [@mode m] [@modality p] (struct
      include T
      include M
    end)

  module Replace_polymorphic_compare : sig @@ p
    include Comparisons [@mode m] with type t := t
  end =
    M

  module Map = Map.Make_plain_using_comparator [@modality p] (T)
  module Set = Set.Make_plain_using_comparator [@modality p] (T)
end

module%template.portable
  [@modality p] Make_plain (T : sig
    type t [@@deriving (compare [@mode m]), sexp_of]
  end) =
Make_plain_using_comparator [@modality p] [@mode m] (struct
    include T
    include Comparator.Make [@modality p] (T)
  end)

module%template.portable
  [@modality p] Make_using_comparator (T : sig
    type t [@@deriving sexp]

    include Using_comparator_arg [@modality p] [@mode m] with type t := t
  end) :
  S
  [@modality p] [@mode m]
  with type t := T.t
   and type comparator_witness = T.comparator_witness = struct
  include T
  module M = Base.Comparable.Make_using_comparator [@modality p] [@mode m] (T)
  include M

  include Validate [@mode m] [@modality p] (struct
      include T
      include M
    end)

  module Replace_polymorphic_compare : sig @@ p
    include Comparisons [@mode m] with type t := t
  end =
    M

  module Map = Map.Make_using_comparator [@modality p] (T)
  module Set = Set.Make_using_comparator [@modality p] (T)
end

module%template.portable
  [@modality p] Make (T : sig
    type t [@@deriving (compare [@mode m]), sexp]
  end) : S [@modality p] [@mode m] with type t := T.t =
Make_using_comparator [@modality p] [@mode m] (struct
    include T
    include Comparator.Make [@modality p] (T)
  end)

module%template.portable
  [@modality p] Make_binable_using_comparator (T : sig
    type t [@@deriving bin_io, sexp]

    include Using_comparator_arg [@modality p] [@mode m] with type t := t
  end) =
struct
  include T
  module M = Base.Comparable.Make_using_comparator [@modality p] [@mode m] (T)

  include Validate [@mode m] [@modality p] (struct
      include T

      let compare = [%eta2 Comparator.compare T.comparator]
    end)

  include M

  module Replace_polymorphic_compare : sig @@ p
    include Comparisons [@mode m] with type t := t
  end =
    M

  module Map = Map.Make_binable_using_comparator [@modality p] (T)
  module Set = Set.Make_binable_using_comparator [@modality p] (T)
end

module%template.portable
  [@modality p] Make_binable (T : sig
    type t [@@deriving bin_io, (compare [@mode m]), sexp]
  end) =
struct
  include Make_binable_using_comparator [@modality p] [@mode m] (struct
      include T
      include Comparator.Make [@modality p] (T)
    end)
end

module%template.portable
  [@modality p] Extend_plain
    (M : Base.Comparable.S
  [@modality p] [@mode m])
    (X : sig
       type t = M.t [@@deriving sexp_of]
     end) =
struct
  module T = struct
    include M

    include (
      X :
        sig
        @@ p
          type t = M.t [@@deriving sexp_of]
        end
        with type t := t)
  end

  include T
  include Validate [@mode m] [@modality p] (T)

  module Replace_polymorphic_compare : sig @@ p
    include Comparisons [@mode m] with type t := t
  end =
    M

  module Map = Map.Make_plain_using_comparator [@modality p] (T)
  module Set = Set.Make_plain_using_comparator [@modality p] (T)
end

module%template.portable
  [@modality p] Extend
    (M : Base.Comparable.S
  [@modality p] [@mode m])
    (X : sig
       type t = M.t [@@deriving sexp]
     end) =
struct
  module T = struct
    include M

    include (
      X :
        sig
        @@ p
          type t = M.t [@@deriving sexp]
        end
        with type t := t)
  end

  include T
  include Validate [@mode m] [@modality p] (T)

  module Replace_polymorphic_compare : sig @@ p
    include Comparisons [@mode m] with type t := t
  end =
    M

  module Map = Map.Make_using_comparator [@modality p] (T)
  module Set = Set.Make_using_comparator [@modality p] (T)
end

module%template.portable
  [@modality p] Extend_binable
    (M : Base.Comparable.S
  [@modality p] [@mode m])
    (X : sig
       type t = M.t [@@deriving bin_io, sexp]
     end) =
struct
  module T = struct
    include M

    include (
      X :
        sig
        @@ p
          type t = M.t [@@deriving bin_io, sexp]
        end
        with type t := t)
  end

  include T
  include Validate [@mode m] [@modality p] (T)

  module Replace_polymorphic_compare : sig @@ p
    include Comparisons [@mode m] with type t := t
  end =
    M

  module Map = Map.Make_binable_using_comparator [@modality p] (T)
  module Set = Set.Make_binable_using_comparator [@modality p] (T)
end

module%template.portable
  [@modality p] Inherit
    (C : sig
       type t [@@deriving compare [@mode m]]
     end)
    (T : sig
       type t [@@deriving sexp]

       val component : t @ m -> C.t @ m
     end) =
Make [@modality p] [@mode m] (struct
    type t = T.t [@@deriving sexp]

    let%template compare t t' =
      (C.compare [@mode m]) (T.component t) (T.component t') [@nontail]
    [@@mode m' = (global, m)]
    ;;
  end)]

include (
  Base.Comparable :
  sig
  @@ portable
    include With_compare
  end)

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

    module%template.portable [@modality p] Make (X : Stable_module_types.S0 [@modality p]) =
    struct
      module Map = Map.Stable.V1.Make [@modality p] (X)
      module Set = Set.Stable.V1.Make [@modality p] (X)
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

      module%template.portable
        [@modality p] Make
          (X : Stable_module_types.With_stable_witness.S0
        [@modality p]) =
      struct
        module Map = Map.Stable.V1.With_stable_witness.Make [@modality p] (X)
        module Set = Set.Stable.V1.With_stable_witness.Make [@modality p] (X)
      end
    end
  end
end
