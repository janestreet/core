@@ portable

(** Floating-point numbers. *)

open! Import

(*_ This needs to be above the place where we include [Base.Float] so that it doesn't
   shadow the latter's mode-polymorphic operators. *)
  include%template
    Identifiable.S
    [@mode local]
    with type t := float
     and type comparator_witness := Base.Float.comparator_witness

(** @inline *)
include
  Base.Float.S
  with type t = Base.Float.t
   and type comparator_witness = Base.Float.comparator_witness
   and type Class.t = Base.Float.Class.t
   and type Parts.t = Base.Float.Parts.t
   and type Terse.t = Base.Float.Terse.t

type t = float [@@deriving typerep, bin_io ~localize]

module Robust_compare : sig
  module type S = sig @@ portable
    (** intended to be a tolerance on human-entered floats *)
    val robust_comparison_tolerance : float

    include Robustly_comparable.S with type t := float
  end

  module Make (T : sig
      val robust_comparison_tolerance : float
    end) : S
end

(** So-called "robust" comparisons, which include a small tolerance, so that float that
    differ by a small amount are considered equal.

    Note that the results of robust comparisons on [nan] should be considered undefined. *)
include Robust_compare.S

module O : sig
  include module type of struct
    include Base.Float.O
  end

  include Robustly_comparable.S with type t := t
end

module Robustly_comparable : Robust_compare.S

module Terse : sig
  type nonrec t = t [@@deriving bin_io ~localize]

  include module type of struct
      include Base.Float.Terse
    end
    with type t := t
end

(** [validate_lbound], [validate_ubound], and [validate_bound] always fail if class is
    [Nan] or [Infinite]. The other validation functions still fail on [Nan], but permit
    [Infinite] values of the correct sign. (The behavior with respect to infinity will
    probably be changed to be more consistent.) *)
include Comparable.Validate_with_zero with type t := t

(** [validate_ordinary] fails if class is [Nan] or [Infinite]. *)
val validate_ordinary : t Validate.check

(** [to_string_12 x] builds a string representing [x] using up to 12 significant digits.
    It loses precision. You can use ["%{Float#12}"] in formats, but consider ["%.12g"],
    ["%{Float#hum}"], or ["%{Float}"] as alternatives. *)
val to_string_12 : local_ t -> string

(** [to_string x] builds a string [s] representing the float [x] that guarantees the round
    trip, i.e., [Float.equal x (Float.of_string s)].

    It usually yields as few significant digits as possible. That is, it won't print
    [3.14] as [3.1400000000000001243]. The only exception is that occasionally it will
    output 17 significant digits when the number can be represented with just 16 (but not
    15 or fewer) of them. *)
include Stringable.S_local_input with type t := t

include Quickcheckable.S with type t := t

(*_ Caution: If we remove this sig item, [sign] will still be present from
  [Comparable.With_zero]. *)

val sign : local_ t -> Sign.t
[@@deprecated "[since 2016-01] Replace [sign] with [sign_or_nan] or [sign_exn]"]

(** (Formerly [sign]) Uses robust comparison (so sufficiently small numbers are mapped to
    [Zero]). Also maps NaN to [Zero]. Using this function is weakly discouraged. *)
val robust_sign : local_ t -> Sign.t

(** [gen_uniform_excl lo hi] creates a Quickcheck generator producing finite [t] values
    between [lo] and [hi], exclusive. The generator approximates a uniform distribution
    over the interval (lo, hi). Raises an exception if [lo] is not finite, [hi] is not
    finite, or the requested range is empty.

    The implementation chooses values uniformly distributed between 0 (inclusive) and 1
    (exclusive) up to 52 bits of precision, then scales that interval to the requested
    range. Due to rounding errors and non-uniform floating point precision, the resulting
    distribution may not be precisely uniform and may not include all values between [lo]
    and [hi]. *)
val gen_uniform_excl : t -> t -> t Quickcheck.Generator.t @ portable

(** [gen_incl lo hi] creates a Quickcheck generator that produces values between [lo] and
    [hi], inclusive, approximately uniformly distributed, with extra weight given to
    generating the endpoints [lo] and [hi]. Raises an exception if [lo] is not finite,
    [hi] is not finite, or the requested range is empty. *)
val gen_incl : t -> t -> t Quickcheck.Generator.t @ portable

(** [gen_finite] produces all finite [t] values, excluding infinities and all NaN values. *)
val gen_finite : t Quickcheck.Generator.t

(** [gen_positive] produces all (strictly) positive finite [t] values. *)
val gen_positive : t Quickcheck.Generator.t

(** [gen_negative] produces all (strictly) negative finite [t] values. *)
val gen_negative : t Quickcheck.Generator.t

(** [gen_without_nan] produces all finite and infinite [t] values, excluding all NaN
    values. *)
val gen_without_nan : t Quickcheck.Generator.t

(** [gen_infinite] produces both infinite values *)
val gen_infinite : t Quickcheck.Generator.t

(** [gen_nan] produces all NaN values. *)
val gen_nan : t Quickcheck.Generator.t

(** [gen_normal] produces all normal values *)
val gen_normal : t Quickcheck.Generator.t

(** [gen_subnormal] produces all subnormal values *)
val gen_subnormal : t Quickcheck.Generator.t

(** [gen_zero] produces both zero values *)
val gen_zero : t Quickcheck.Generator.t

(** Note that [float] is already stable by itself, since as a primitive type it is an
    integral part of the sexp / bin_io protocol. [Float.Stable] exists only to introduce
    [Float.Stable.Set] and [Float.Stable.Map], and provide interface uniformity with other
    stable types. *)
module Stable : sig
  module V1 : sig
    type nonrec t = t
    [@@deriving
      compare ~localize, equal ~localize, hash, sexp_grammar, typerep, globalize]

    (** We expose [Quickcheckable.S] here specifically for tests that operate on the
        stable type. Note that the quickcheck semantics themselves are *not* stable. *)
    include Quickcheckable.S with type t := t

    include%template
      Stable_comparable.With_stable_witness.V1
      [@mode local]
      with type t := t
       and type comparator_witness = comparator_witness
  end
end
