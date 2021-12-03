(** A scale factor, not bounded between 0% and 100%, represented as a float. *)

open! Import
open Std_internal

(** Exposing that this is a float allows for more optimization. E.g. compiler can
    optimize some local refs and not box them.
*)
type t = private float [@@deriving hash, typerep]

(** [of_string] and [t_of_sexp] disallow [nan], [inf], etc. *)
include Stringable with type t := t

(** Sexps are of the form 5bp or 0.05% or 0.0005x.

    Warning: [equal (t) (t_of_sexp (sexp_of_t t))] is not guaranteed. Multiple
    serialization round-trips may cause multiple small drifts. *)
include Sexpable with type t := t

include Binable with type t := t
include Comparable_binable with type t := t
include Comparable.With_zero with type t := t
include Robustly_comparable.S with type t := t
include Quickcheckable.S with type t := t

(** The value [nan] cannot be represented as an [Option.t] *)
module Option : sig
  type value := t
  type t = private float [@@deriving sexp_grammar]

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

(** [of_percentage 5.] is 5% = 0.05x = 500bp *)
val of_percentage : float -> t

val to_percentage : t -> float

(** [of_bp 5.] is 5bp = 0.05% = 0.0005x *)
val of_bp : float -> t

val to_bp : t -> float
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
    type nonrec t = t [@@deriving sexp, bin_io, compare, hash, equal, typerep]
  end

  module Option : sig
    module V1 : sig
      type t = Option.t [@@deriving bin_io, compare, hash, sexp]
    end
  end
end

(** Does not format small values as "3bp" or large ones as "2x"; always uses percentages
    ("0.0003%" or "200%"). The standard [of_sexp] can read these just fine. *)
module Always_percentage : sig
  type nonrec t = t [@@deriving sexp_of]

  val to_string : t -> string
  val format : t -> Format.t -> string
end
