(** A scale factor, not bounded between 0% and 100%, represented as a float. *)

open! Import
open Std_internal

(** Exposing that this is a float allows for more optimization. E.g. compiler can
    optimize some local refs and not box them.
*)
type t = private float [@@deriving globalize, hash, typerep]

(** [of_string] and [t_of_sexp] disallow [nan], [inf], etc.  Furthermore, they round to 6
    significant digits.  They are equivalent to [Stable.V2] sexp conversion. *)
include Stringable with type t := t

(** Equivalent to [Stable.V3.to_string] *)
val to_string_round_trippable : t -> string

(** Sexps are of the form 5bp or 0.05% or 0.0005x.

    Warning: [equal (t) (t_of_sexp (sexp_of_t t))] is not guaranteed.

    First, sexp_of_t truncates to 6 significant digits.  Second, multiple
    serialization round-trips may cause further multiple small drifts.

    The sexp conversion here is V2 and not V3 to avoid breaking existing code at the time
    V3 was introduced (Nov 2022).

    New code should explicitly use Percent.Stable.V3 for faithful round-trippable sexp
    conversion.
*)
include Sexpable with type t := t

include Sexplib.Sexp_grammar.S with type t := t
include Binable with type t := t
include Comparable_binable with type t := t
include Comparable.With_zero with type t := t
include Diffable.S_atomic with type t := t
include Robustly_comparable.S with type t := t
include Quickcheckable.S with type t := t

(** The value [nan] cannot be represented as an [Option.t] *)
module Option : sig
  type value := t
  type t = private float [@@deriving bin_io, sexp_grammar]

  include Immediate_option.S_without_immediate with type value := value and type t := t

  (** [apply_with_none_as_nan (some x) y = apply x y], and
      [apply_with_none_as_nan none y = apply (of_mult Float.nan) y] *)
  val apply_with_none_as_nan : t -> float -> float

  (** [of_mult_with_nan_as_none Float.nan = none], and
      [of_mult_with_nan_as_none x = some (of_mult x)] otherwise *)
  val of_mult_with_nan_as_none : float -> t

  (** [to_mult_with_none_as_nan none = Float.nan], and
      [to_mult_with_none_as_nan (some x) = to_mult x] *)
  val to_mult_with_none_as_nan : t -> float
end

val ( * ) : t -> t -> t
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( / ) : t -> t -> t
val ( // ) : t -> t -> float
val zero : t
val one_hundred_percent : t
val neg : t -> t
val abs : t -> t
val is_zero : t -> bool
val is_nan : t -> bool
val is_inf : t -> bool

(** [apply t x] multiplies the percent [t] by [x], returning a float. *)
val apply : t -> float -> float

(** [scale t x] scales the percent [t] by [x], returning a new [t]. *)
val scale : t -> float -> t

(** [of_mult 5.] is 5x = 500% = 50_000bp *)
val of_mult : float -> t

val to_mult : t -> float

(** [of_percentage 5.] is 5% = 0.05x = 500bp.  Note: this function performs float division
    by 100.0 and it may introduce rounding errors, for example:
    {[ of_percentage 70.18 |> to_mult = 0.70180000000000009 ]}
    It is also not consistent with [of_string] or [t_of_sexp] for "%"-ending strings.  The
    results can be off by an ulp.  If this matters to you, use
    [of_percentage_slow_more_accurate] instead. *)
val of_percentage : float -> t

(** Like [of_percentage], but consistent with [of_string] and [t_of_sexp], that is,
    [of_percentage_slow_more_accurate x = of_string (Float.to_string x ^ "%")] *)
val of_percentage_slow_more_accurate : float -> t

(** [to_percentage (Percent.of_string "5%")] is 5.0.  Note: this function performs float
    multiplication by 100.0 and it may introduce rounding errors, for example:
    {[ to_percentage (Percent.of_mult 0.56) = 56.000000000000007 ]}
    It is also not consistent with [Stable.V3.sexp_of_t] or [to_string_round_trippable].
    If this matters to you, use [to_percentage_slow_more_accurate] instead. *)
val to_percentage : t -> float

(** Like [to_percentage], but consistent with [Stable.V3.sexp_of_t] and
    [to_string_round_trippable]. *)
val to_percentage_slow_more_accurate : t -> float

(** [of_bp 5.] is 5bp = 0.05% = 0.0005x.  Note: this function performs float division by
    10,000.0 and it may introduce rounding errors, for example:
    {[ of_bp 70.18 |> to_mult = 0.0070180000000000008 ]}
    It is also not consistent with [of_string] or [t_of_sexp] for "bp"-ending strings.
    The results can be off by an ulp.  If this matters to you, use
    [of_bp_slow_more_accurate] instead. *)
val of_bp : float -> t

(** Like [of_bp], but consistent with [of_string] and [t_of_sexp], that is,
    [of_bp_slow_more_accurate x = of_string (Float.to_string x ^ "bp")] *)
val of_bp_slow_more_accurate : float -> t

(** [to_bp (Percent.of_bp "4bp")] is 4.0.  Note: this function performs float
    multiplication by 10000.0 and and it may introduce rounding errors, for example:
    {[ to_bp (Percent.of_mult 0.56) = 5600.0000000000009 ]}
    It is also not consistent with [Stable.V3.sexp_of_t] or [to_string_round_trippable].
    If this matters to you, use [to_bp_slow_more_accurate] instead. *)
val to_bp : t -> float

(** Like [to_bp], but consistent with [Stable.V3.sexp_of_t] and
    [to_string_round_trippable]. *)
val to_bp_slow_more_accurate : t -> float

val of_bp_int : int -> t

(** rounds down *)
val to_bp_int : t -> int

(** 0.0123456% ~significant_digits:4 is 1.235bp *)
val round_significant : t -> significant_digits:int -> t

(** 0.0123456% ~decimal_digits:4 is 0.0001 = 1bp *)
val round_decimal_mult : t -> decimal_digits:int -> t

(** 0.0123456% ~decimal_digits:4 is 0.0123% = 1.23bp *)
val round_decimal_percentage : t -> decimal_digits:int -> t

(** 0.0123456% ~decimal_digits:4 is 1.2346bp *)
val round_decimal_bp : t -> decimal_digits:int -> t

val t_of_sexp_allow_nan_and_inf : Sexp.t -> t
val of_string_allow_nan_and_inf : string -> t

(** A [Format.t] tells [Percent.format] how to render a floating-point value as a string,
    like a [printf] conversion specification.

    For example:

    {[
      format (Format.exponent ~precision) = sprintf "%.e" precision
    ]}

    The [_E] naming suffix in [Format] values is mnenomic of a capital [E] (rather than
    [e]) being used in floating-point exponent notation.

    Here is the documentation of the floating-point conversion specifications from the
    OCaml manual:

    - f: convert a floating-point argument to decimal notation, in the style dddd.ddd.

    - F: convert a floating-point argument to OCaml syntax (dddd. or dddd.ddd or d.ddd
      e+-dd).

    - e or E: convert a floating-point argument to decimal notation, in the style d.ddd
      e+-dd (mantissa and exponent).

    - g or G: convert a floating-point argument to decimal notation, in style f or e, E
      (whichever is more compact).

    - h or H: convert a floating-point argument to hexadecimal notation, in the style
      0xh.hhhh e+-dd (hexadecimal mantissa, exponent in decimal and denotes a power of
      2).
*)
module Format : sig
  type t [@@deriving sexp_of]

  (** [sprintf "%.*e" precision] *)
  val exponent : precision:int -> t

  (** [sprintf "%.*E" precision] *)
  val exponent_E : precision:int -> t

  (** [sprintf "%.*f" precision] *)
  val decimal : precision:int -> t

  (** [sprintf "%F"] *)
  val ocaml : t

  (** [sprintf "%.*g" precision] *)
  val compact : precision:int -> t

  (** [sprintf "%.*G" precision] *)
  val compact_E : precision:int -> t

  (** [sprintf "%.*h" precision] *)
  val hex : precision:int -> t

  (** [sprintf "%.*H" precision] *)
  val hex_E : precision:int -> t
end

val format : t -> Format.t -> string
val validate : t -> Validate.t

(*_ Caution: If we remove this sig item, [sign] will still be present from
  [Comparable.With_zero]. *)

val sign : t -> Sign.t [@@deprecated "[since 2016-01] Replace [sign] with [sign_exn]"]

(** The sign of a [Percent.t].  Both [-0.] and [0.] map to [Zero].  Raises on nan.  All
    other values map to [Neg] or [Pos]. *)
val sign_exn : t -> Sign.t

module Stable : sig
  module V1 : sig
    (** Tl;dr: For new code use [Stable.V3] if you care about exact round-trippability via
        sexp, or [Almost_round_trippable] if you want to round your output to 14
        significant digits to hide common floating-point rounding errors and make it less
        of an eyesore but also less accurate.

        The difference between [V3] and [V2] is that V3 sexp (de)serialization is fully
        round-trippable.  There is no difference in [bin_io] between [V2] and [V3], and
        they have identical bin_shape.

        [V1] and [V2] sexp serialization rounds to 6 significant digits, and serialization
        / deserialization go through an extra float multiplication / divison in the [%] or
        [bp] case.  This may cause further loss of precision, which is the reason why
        [V1]'s or [V2]'s [t_of_sexp] may be slightly off even when reading [V3]-generated
        sexps.

        If one wants to stick to the 6 significant digits in the sexp output, it is still
        recommended to use [V2] over [V1]:

        [V1.Bin_shape_same_as_float.t]'s sexp and bin-io representations are the same as
        [V2.t]'s. There are only two differences:

        - [V2] has a distinct [bin_shape_t] from [Float.bin_shape_t], to suggest that
          changing a protocol type from a percent to a float (or vice-versa) is a breaking
          change, semantically.
        - [V2.{Map,Set}.t_of_sexp] no longer accept keys/elements formatted as floats
          rather than as {Percent}s.

        Usually existing code can upgrade in-place from [V1.Bin_shape_same_as_float] to
        [V2], as long as no client code uses [bin_shape_t] dynamically.
    *)
    module Bin_shape_same_as_float : sig
      type nonrec t = t
      [@@deriving
        sexp
        , sexp_grammar
        , bin_io
        , compare
        , globalize
        , hash
        , equal
        , typerep
        , stable_witness
        , diff]
    end
  end

  module V2 : sig
    (** This format is not round-trippable as sexp.  Only accurate up to 6 significant
        digits when going via sexp.  This is the format used by [Percent.sexp_of_t] and
        [Percent.to_string] (think user interfaces).  Read the comment above at [V1] for
        details. *)
    type nonrec t = t
    [@@deriving
      sexp
      , sexp_grammar
      , bin_io
      , compare
      , globalize
      , hash
      , equal
      , typerep
      , stable_witness
      , diff]

    val to_string : t -> string
    val of_string : string -> t
    val of_string_allow_nan_and_inf : string -> t
  end

  module V3 : sig
    (** Fully round-trippable format, which does not use float multiplication or division
        to serialize or deserialize [%] or [bp] but instead does string manipulation.

        Note that as a consequence, this may yield ugly-looking output for [Percent.t]
        values obtained as a result of a calculation, including [Percent.of_percentage],
        because of accumulated floating-point rounding errors.  Use
        [Almost_round_trippable] when the esthetics (aka human readability) of the output
        is more important than exact round-trippability.

        Also note that [Percent.Stable.V3.t_of_sexp] and [Percent.Stable.V2.t_of_sexp]
        may yield results off by one ulp because the latter does the division by 100.0 or
        10,000.0 in the '%' or 'bp' case, respectively.

        For example,

        {[ stable.V3.of_string "17.33%" <> of_percentage 17.33 ]}
    *)
    type nonrec t = t
    [@@deriving
      sexp
      , sexp_grammar
      , bin_io
      , compare
      , globalize
      , hash
      , equal
      , typerep
      , stable_witness
      , diff]

    include
      Comparable_binable
        with type t := t
         and type comparator_witness := comparator_witness

    val to_string : t -> string
    val of_string : string -> t
    val of_string_allow_nan_and_inf : string -> t

    (** A variant with alternative serialization, which always uses the [%] format,
        regardless of the absolute value of [t].  Fully inter-operable with
        [Percent.Stable.V3.t]: either can read the other's output and it's fully
        round-trippable in both directions.
    *)
    module Always_percentage : sig
      type nonrec t = t [@@deriving sexp, bin_io]

      val to_string : t -> string
    end
  end

  module Option : sig
    module V1 : sig
      (** See comment for [Stable.V1.Bin_shape_same_as_float]. *)
      module Bin_shape_same_as_float : sig
        type t = Option.t [@@deriving bin_io, compare, hash, sexp, stable_witness]
      end
    end

    module V2 : sig
      type t = Option.t [@@deriving bin_io, compare, hash, sexp, stable_witness]
    end

    module V3 : sig
      type t = Option.t
      [@@deriving bin_io, compare, equal, hash, sexp, sexp_grammar, stable_witness]
    end
  end
end

(** Does not format small values as "3bp" or large ones as "2x"; always uses percentages
    ("0.0003%" or "200%").  The standard [of_sexp] can read these just fine.

    Note: rounds to 6 significant digits only (as opposed to
    [Percent.Stable.V3.Always_percentage], which is accurate, or
    [Percent.Almost_round_trippable.Always_percentage], which rounds to 14 significant
    digits).
*)
module Always_percentage : sig
  type nonrec t = t [@@deriving sexp_of]

  val to_string : t -> string
  val format : t -> Format.t -> string
end

(** Similar to [Stable.V3], but rounds to 14 significant digits in order to make the
    output more palatable to humans, at the cost of making it not exactly round-trippable,
    e.g.

    {[
      Percent.Stable.V3.to_string (Percent.of_percentage 17.13) = "17.129999999999998%"
    ]}

    (this is because of the [17.13 /. 100.] float division hidden in
    [Percent.of_percentage]).  But:

    {[
      Percent.Almost_round_trippable.to_string (Percent.of_percentage 17.13) = "17.13%"
    ]}
*)
module Almost_round_trippable : sig
  type nonrec t = t [@@deriving sexp]

  val to_string : t -> string
  val of_string : string -> t

  (** A variant with alternative serialization, which always uses the [%] format,
      regardless of the absolute value of [t].  Fully inter-operable with
      [Percent.Almost_round_trippable.t]: either can read the other's output and the
      precision is exactly the same for both. *)
  module Always_percentage : sig
    type nonrec t = t [@@deriving sexp, bin_io]

    val to_string : t -> string
  end
end
