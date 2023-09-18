open! Import

module type Infix = Base.Comparable.Infix
module type Comparisons = Base.Comparable.Comparisons
module type With_compare = Base.Comparable.With_compare

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

module type With_zero = sig
  type t

  include Base.Comparable.With_zero with type t := t
  include Validate_with_zero with type t := t
end

module type S_common = sig
  include Base.Comparable.S
  include Validate with type t := t
  module Replace_polymorphic_compare : Comparisons with type t := t
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
  include S_common

  module Map :
    Map.S_plain with type Key.t = t with type Key.comparator_witness = comparator_witness

  module Set :
    Set.S_plain with type Elt.t = t with type Elt.comparator_witness = comparator_witness
end

module type S = sig
  include S_common

  module Map :
    Map.S with type Key.t = t with type Key.comparator_witness = comparator_witness

  module Set :
    Set.S with type Elt.t = t with type Elt.comparator_witness = comparator_witness
end

module type Map_and_set_binable = sig
  type t

  include Comparator.S with type t := t

  module Map :
    Map.S_binable
      with type Key.t = t
      with type Key.comparator_witness = comparator_witness

  module Set :
    Set.S_binable
      with type Elt.t = t
      with type Elt.comparator_witness = comparator_witness
end

module type S_binable = sig
  include S_common

  include
    Map_and_set_binable
      with type t := t
      with type comparator_witness := comparator_witness
end

module type Comparable = sig
  (** Comparable extends {{!Base.Comparable}[Base.Comparable]} and provides functions for
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
        module Infix = (C : Comparable.Infix with type t := t)
      ]}

      Common pattern: Define a module [O] with a restricted signature.  It aims to be
      (locally) opened to bring useful operators into scope without shadowing unexpected
      variable names.  E.g. in the [Date] module:

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
      ]}
  *)

  module type Infix = Infix
  module type Map_and_set_binable = Map_and_set_binable
  module type Comparisons = Comparisons
  module type S_plain = S_plain
  module type S = S
  module type S_binable = S_binable
  module type S_common = S_common
  module type Validate = Validate
  module type Validate_with_zero = Validate_with_zero
  module type With_compare = With_compare
  module type With_zero = With_zero

  include With_compare

  (** Inherit comparability from a component. *)
  module Inherit (C : sig
    type t [@@deriving compare]
  end) (T : sig
    type t [@@deriving sexp]

    val component : t -> C.t
  end) : S with type t := T.t

  (** {2 Comparison-only Functors}

      These functors require only [type t] and [val compare]. They do not require [val
      sexp_of_t], and do not generate container datatypes.
  *)

  module Infix (T : sig
    type t [@@deriving compare]
  end) : Infix with type t := T.t

  module Comparisons (T : sig
    type t [@@deriving compare]
  end) : Comparisons with type t := T.t

  (** {2 Make Functors}

      The Comparable Make functor family allows users to choose among the following
      attributes:

      - [*_using_comparator] or not
      - [*_binable] or not
      - [*_plain] or not

      Thus there are functors like [Make_plain] or [Make_binable_using_comparator], etc.
  *)

  module Make_plain (T : sig
    type t [@@deriving compare, sexp_of]
  end) : S_plain with type t := T.t

  module Make (T : sig
    type t [@@deriving compare, sexp]
  end) : S with type t := T.t

  module Make_plain_using_comparator (T : sig
    type t [@@deriving sexp_of]

    include Comparator.S with type t := t
  end) : S_plain with type t := T.t with type comparator_witness := T.comparator_witness

  module Make_using_comparator (T : sig
    type t [@@deriving sexp]

    include Comparator.S with type t := t
  end) : S with type t := T.t with type comparator_witness := T.comparator_witness

  module Make_binable (T : sig
    type t [@@deriving bin_io, compare, sexp]
  end) : S_binable with type t := T.t

  module Make_binable_using_comparator (T : sig
    type t [@@deriving bin_io, sexp]

    include Comparator.S with type t := t
  end) : S_binable with type t := T.t with type comparator_witness := T.comparator_witness

  module Extend_plain
    (M : Base.Comparable.S) (X : sig
      type t = M.t [@@deriving sexp_of]
    end) : S_plain with type t := M.t with type comparator_witness := M.comparator_witness

  module Extend
    (M : Base.Comparable.S) (X : sig
      type t = M.t [@@deriving sexp]
    end) : S with type t := M.t with type comparator_witness := M.comparator_witness

  module Extend_binable
    (M : Base.Comparable.S) (X : sig
      type t = M.t [@@deriving bin_io, sexp]
    end) :
    S_binable with type t := M.t with type comparator_witness := M.comparator_witness

  module Map_and_set_binable (T : sig
    type t [@@deriving bin_io, compare, sexp]
  end) : Map_and_set_binable with type t := T.t

  module Map_and_set_binable_using_comparator (T : sig
    type t [@@deriving bin_io, compare, sexp]

    include Comparator.S with type t := t
  end) :
    Map_and_set_binable
      with type t := T.t
      with type comparator_witness := T.comparator_witness

  module Poly (T : sig
    type t [@@deriving sexp]
  end) : S with type t := T.t

  module Validate (T : sig
    type t [@@deriving compare, sexp_of]
  end) : Validate with type t := T.t

  module Validate_with_zero (T : sig
    type t [@@deriving compare, sexp_of]

    val zero : t
  end) : Validate_with_zero with type t := T.t

  module With_zero (T : sig
    type t [@@deriving compare, sexp_of]

    val zero : t
  end) : With_zero with type t := T.t

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

      module Make (X : Stable_module_types.S0) :
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

        module Make (X : Stable_module_types.With_stable_witness.S0) :
          S
            with type comparable := X.t
            with type comparator_witness := X.comparator_witness
      end
    end
  end
end
