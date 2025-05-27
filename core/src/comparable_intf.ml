open! Import

module type Infix = Base.Comparable.Infix
module type Infix_with_zero_alloc = Base.Comparable.Infix_with_zero_alloc
module type With_compare = Base.Comparable.With_compare

[%%template
[@@@mode.default m = (local, global)]

module type Using_comparator_arg = Base.Comparable.Using_comparator_arg
[@mode m] [@modality p]
[@@modality p = (portable, nonportable)]

module type Comparisons = Base.Comparable.Comparisons [@mode m]

module type Comparisons_with_zero_alloc = Base.Comparable.Comparisons_with_zero_alloc
[@mode m]]

module type Validate = sig
  type t

  val validate_lbound : min:t Maybe_bound.t -> t Validate.check
  val validate_ubound : max:t Maybe_bound.t -> t Validate.check
  val validate_bound : min:t Maybe_bound.t -> max:t Maybe_bound.t -> t Validate.check
end

module type Validate_with_zero = sig
  type t

  include Validate with type t := t

  val validate_positive : t Validate.check
  val validate_non_negative : t Validate.check
  val validate_negative : t Validate.check
  val validate_non_positive : t Validate.check
end

[%%template
[@@@mode.default m = (local, global)]

module type With_zero = sig
  type t

  include Base.Comparable.With_zero [@mode m] with type t := t
  include Validate_with_zero with type t := t
end

[@@@modality.default p = (portable, nonportable)]

module type S_common = sig
  include Base.Comparable.S [@modality p] [@mode m]
  include Validate with type t := t
  module Replace_polymorphic_compare : Comparisons [@mode m] with type t := t
end

(** Usage example:

    {[
      module Foo : sig
        type t = ...
        include Comparable.S with type t := t
      end
    ]}

    Then use [Comparable.Make] in the struct (see comparable.mli for an example). *)

module type S_plain = sig
  include S_common [@modality p] [@mode m]

  module Map :
    Map.S_plain with type Key.t = t with type Key.comparator_witness = comparator_witness

  module Set :
    Set.S_plain with type Elt.t = t with type Elt.comparator_witness = comparator_witness
end

module type S = sig
  include S_common [@modality p] [@mode m]

  module Map :
    Map.S with type Key.t = t with type Key.comparator_witness = comparator_witness

  module Set :
    Set.S with type Elt.t = t with type Elt.comparator_witness = comparator_witness
end]

[%%template
[@@@modality.default p = (portable, nonportable)]

module type Map_and_set_binable = sig
  type t

  include Comparator.S [@modality p] with type t := t

  module Map :
    Map.S_binable
    with type Key.t = t
    with type Key.comparator_witness = comparator_witness

  module Set :
    Set.S_binable
    with type Elt.t = t
    with type Elt.comparator_witness = comparator_witness
end

module type [@mode m = (global, local)] S_binable = sig
  include S_common [@modality p] [@mode m]

  include
    Map_and_set_binable
    [@modality p]
    with type t := t
    with type comparator_witness := comparator_witness
end]

module type Comparable = sig @@ portable
  (** Comparable extends {{!Base.Comparable} [Base.Comparable]} and provides functions for
      comparing like types.

      Usage example:

      {[
        module Foo = struct
          module T = struct
            type t = ... [@@deriving compare, sexp]
          end
          include T
          include Comparable.Make (T)
        end
      ]}

      Then include [Comparable.S] in the signature (see {!Comparable_intf} for an
      example).

      To add an [Infix] submodule:

      {[
        module C = Comparable.Make (T)
        include C
        module Infix : Comparable.Infix with type t := t = C
      ]}

      Common pattern: Define a module [O] with a restricted signature. It aims to be
      (locally) opened to bring useful operators into scope without shadowing unexpected
      variable names. E.g. in the [Date] module:

      {[
        module O = struct
          include (C : Comparable.Infix with type t := t)
          let to_string t = ..
        end
      ]}

      Opening [Date] would shadow [now], but opening [Date.O] doesn't:

      {[
        let now = .. in
        let someday = .. in
        Date.O.(now > someday)
      ]} *)

  module type Infix = Infix
  module type Infix_with_zero_alloc = Infix_with_zero_alloc
  module type Map_and_set_binable = Map_and_set_binable

  [%%template:
  [@@@mode.default m = (local, global)]

  module type Comparisons = Comparisons [@mode m]
  module type Comparisons_with_zero_alloc = Comparisons_with_zero_alloc [@mode m]
  module type With_zero = With_zero [@mode m]

  [@@@modality.default p = (portable, nonportable)]

  module type S_plain = S_plain [@modality p] [@mode m]
  module type S = S [@modality p] [@mode m]
  module type S_binable = S_binable [@modality p] [@mode m]
  module type S_common = S_common [@modality p] [@mode m]
  module type Using_comparator_arg = Using_comparator_arg [@modality p] [@mode m]]

  module type Validate = Validate
  module type Validate_with_zero = Validate_with_zero
  module type With_compare = With_compare

  include With_compare

  (** Inherit comparability from a component. *)
  module%template.portable
    [@modality p] Inherit
      (C : sig
         type t [@@deriving compare [@mode m]]
       end)
      (T : sig
         type t [@@deriving sexp]

         val component : t @ m -> C.t @ m
       end) : S [@modality p] [@mode m] with type t := T.t
  [@@mode m = (local, global)]

  (** {2 Comparison-only Functors}

      These functors require only [type t] and [val compare]. They do not require
      [val sexp_of_t], and do not generate container datatypes. *)

  [%%template:
  [@@@mode.default m = (local, global)]

  module%template.portable Infix (T : sig
      type t [@@deriving compare [@mode m]]
    end) : Infix with type t := T.t

  module%template.portable Comparisons (T : sig
      type t [@@deriving compare [@mode m]]
    end) : Comparisons [@mode m] with type t := T.t

  (** {2 Make Functors}

      The Comparable Make functor family allows users to choose among the following
      attributes:

      - [*_using_comparator] or not
      - [*_binable] or not
      - [*_plain] or not

      Thus there are functors like [Make_plain] or [Make_binable_using_comparator], etc. *)

  module%template.portable
    [@modality p] Make_plain (T : sig
      type t [@@deriving (compare [@mode m]), sexp_of]
    end) : S_plain [@modality p] [@mode m] with type t := T.t

  module%template.portable
    [@modality p] Make (T : sig
      type t [@@deriving (compare [@mode m]), sexp]
    end) : S [@modality p] [@mode m] with type t := T.t

  module%template.portable
    [@modality p] Make_plain_using_comparator (T : sig
      type t [@@deriving sexp_of]

      include Using_comparator_arg [@modality p] [@mode m] with type t := t
    end) :
    S_plain
    [@modality p] [@mode m]
    with type t := T.t
    with type comparator_witness := T.comparator_witness

  module%template.portable
    [@modality p] Make_using_comparator (T : sig
      type t [@@deriving sexp]

      include Using_comparator_arg [@modality p] [@mode m] with type t := t
    end) :
    S
    [@modality p] [@mode m]
    with type t := T.t
    with type comparator_witness := T.comparator_witness

  module%template.portable
    [@modality p] Make_binable (T : sig
      type t [@@deriving bin_io, (compare [@mode m]), sexp]
    end) : S_binable [@modality p] [@mode m] with type t := T.t

  module%template.portable
    [@modality p] Make_binable_using_comparator (T : sig
      type t [@@deriving bin_io, sexp]

      include Using_comparator_arg [@modality p] [@mode m] with type t := t
    end) :
    S_binable
    [@modality p] [@mode m]
    with type t := T.t
    with type comparator_witness := T.comparator_witness

  module%template.portable
    [@modality p] Extend_plain
      (M : Base.Comparable.S
    [@modality p] [@mode m])
      (X : sig
         type t = M.t [@@deriving sexp_of]
       end) :
    S_plain
    [@modality p] [@mode m]
    with type t := M.t
    with type comparator_witness := M.comparator_witness

  module%template.portable
    [@modality p] Extend
      (M : Base.Comparable.S
    [@modality p] [@mode m])
      (X : sig
         type t = M.t [@@deriving sexp]
       end) :
    S
    [@modality p] [@mode m]
    with type t := M.t
    with type comparator_witness := M.comparator_witness

  module%template.portable
    [@modality p] Extend_binable
      (M : Base.Comparable.S
    [@modality p] [@mode m])
      (X : sig
         type t = M.t [@@deriving bin_io, sexp]
       end) :
    S_binable
    [@modality p] [@mode m]
    with type t := M.t
    with type comparator_witness := M.comparator_witness

  module%template.portable
    [@modality p] Map_and_set_binable (T : sig
      type t [@@deriving bin_io, (compare [@mode m]), sexp]
    end) : Map_and_set_binable [@modality p] with type t := T.t

  module%template.portable
    [@modality p] Map_and_set_binable_using_comparator (T : sig
      type t [@@deriving bin_io, (compare [@mode m]), sexp]

      include Comparator.S [@modality p] with type t := t
    end) :
    Map_and_set_binable
    [@modality p]
    with type t := T.t
    with type comparator_witness := T.comparator_witness

  module%template.portable
    [@modality p] Poly (T : sig
      type t [@@deriving sexp]
    end) : S [@modality p] [@mode m] with type t := T.t

  module%template.portable Validate (T : sig
      type t [@@deriving (compare [@mode m]), sexp_of]
    end) : Validate with type t := T.t

  module%template.portable Validate_with_zero (T : sig
      type t : value mod contended [@@deriving (compare [@mode m]), sexp_of]

      val zero : t
    end) : Validate_with_zero with type t := T.t

  module%template.portable With_zero (T : sig
      type t : value mod contended [@@deriving (compare [@mode m]), sexp_of]

      val zero : t
    end) : With_zero [@mode m] with type t := T.t]

  (** The following module types and functors may be used to define stable modules: *)

  module Stable : sig
    module V1 : sig
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

      module%template.portable
        [@modality p] Make
          (X : Stable_module_types.S0
        [@modality p]) :
        S with type comparable := X.t with type comparator_witness := X.comparator_witness

      module With_stable_witness : sig
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
          [@modality p]) :
          S
          with type comparable := X.t
          with type comparator_witness := X.comparator_witness
      end
    end
  end
end
