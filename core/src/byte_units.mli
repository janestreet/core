(** Conversions between units of measure that are based on bytes (like kilobytes,
    megabytes, gigabytes, and words).

    [t]'s are created with [of_bytes_float_exn], [of_words_float_exn], [of_kilobytes],
    [of_megabytes], etc.

    Note: in this module, kilobytes, Megabytes, etc. are defined as powers of 1024:

    - 1 kilobyte: 2^10 = 1024   bytes
    - 1 Megabyte: 2^20 = 1024^2 bytes
    - 1 Gigabyte: 2^30 = 1024^3 bytes
    - 1 Terabyte: 2^40 = 1024^4 bytes
    - 1 Petabyte: 2^50 = 1024^5 bytes
    - 1 Exabyte:  2^60 = 1024^6 bytes
*)

open! Import

type t [@@deriving sexp_of, typerep] [@@immediate64]

val create : [ `Bytes | `Kilobytes | `Megabytes | `Gigabytes | `Words ] -> float -> t
  [@@deprecated
    "[since 2019-01] Use [of_bytes], [of_kilobytes], [of_megabytes], etc as appropriate."]

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t
include Stringable.S with type t := t

(** This is a deprecated alias for [of_bytes_float_exn]. *)
val of_bytes : float -> t
  [@@deprecated
    "[since 2019-01] Use [of_bytes_int], [of_bytes_int63], [of_bytes_int64_exn] or \
     [of_bytes_float_exn] as appropriate."]

val of_bytes_int : int -> t
val of_bytes_int63 : Int63.t -> t

(** This will raise if and only if the argument can not be represented as a [Byte_units.t].
    Specifically this is if the argument is outside of \[-2^62,2^62). *)
val of_bytes_int64_exn : Int64.t -> t

(** This will raise if and only if the argument can not be represented as a [Byte_units.t].
    Specifically this is if the argument is outside of \[-2^62,2^62), *)
val of_bytes_float_exn : float -> t

(** create of [Byte_units] based on the number of kilobytes.
    N.B. This will raise if the value is outside of \[-2^52,2^52). *)
val of_kilobytes : float -> t

(** create of [Byte_units] based on the number of Megabytes.
    N.B. This will raise if the value is outside of \[-2^42,2^42). *)
val of_megabytes : float -> t

(** create of [Byte_units] based on the number of Gigabytes.
    N.B. This will raise if the value is outside of \[-2^32,2^32). *)
val of_gigabytes : float -> t

(** create of [Byte_units] based on the number of Terabytes.
    N.B. This will raise if the value is outside of \[-2^22,2^22). *)
val of_terabytes : float -> t

(** create of [Byte_units] based on the number of Petabytes.
    N.B. This will raise if the value is outside of \[-2^12,2^12). *)
val of_petabytes : float -> t

(** create of [Byte_units] based on the number of Exabytes.
    N.B. This will raise if the value is outside of \[-4,4). *)
val of_exabytes : float -> t

(** Do not use, consider using [of_words_int] instead. Alias for [of_words_float_exn]. *)
val of_words : float -> t
  [@@deprecated "[since 2019-01] Use [of_words_int] or [of_words_float_exn] instead."]

(** create of [Byte_units] based on the number of machine words. *)
val of_words_int : int -> t

(** Create of [Byte_units] based on the number of machine words.
    On 64-bit platforms this will raise if the value is outside of \[-2^59,2^59).
    On 32-bit platforms (including JS) this will raise if the value is outside of  \[-2^60,2^60). *)
val of_words_float_exn : float -> t

(** [to_string_hum t] returns a string representation of [t]. This will use the largest
    unit that will not make the translated value be below 1.

    For example [Byte_units.to_string_hum (Byte_units.of_bytes_int 1000)] gives [1000B],
    but [Byte_units.to_string_hum (Byte_units.of_bytes_int 1500)] gives [1.46484K]. *)
val to_string_hum : t -> string

val to_string_short : t -> string
  [@@deprecated "[since 2020-06] Use [Short.to_string] instead."]

module Short : sig
  type nonrec t = t [@@deriving sexp_of]

  (** [Short.to_string] is like [to_string_hum] but will attempt to only show 4 significant
      digits.

      For example [Byte_units.to_string_hum (Byte_units.of_bytes_int 1000)] gives [1000B],
      but [Byte_units.to_string_hum (Byte_units.of_bytes_int 1500)] gives [1.46K].

      [Short.sexp_of_t] does the same.
  *)
  val to_string : t -> string
end

(** This is a deprecated alias for [bytes_float]. *)
val bytes : t -> float
  [@@deprecated
    "[since 2019-01] Use [bytes_int_exn], [bytes_int63], [bytes_int64] or [bytes_float] \
     as appropriate."]

(** This will raise if and only if the value of this [Byte_units.t] can not be represented
    as an int.
    This can only happen on platforms where [int] is less than 63 bits, specifically JS
    and 32-bit OCaml where this will raise if the number of bytes is outside
    of \[-2^30,2^30). *)
val bytes_int_exn : t -> int

val bytes_int63 : t -> Int63.t
val bytes_int64 : t -> Int64.t
val bytes_float : t -> float
val kilobytes : t -> float
val megabytes : t -> float
val gigabytes : t -> float
val terabytes : t -> float
val petabytes : t -> float
val exabytes : t -> float

(** Do not use, consider using [words_int_exn] instead. Alias for [words_float] *)
val words : t -> float
  [@@deprecated "[since 2019-01] Use [words_int_exn] or [words_float] instead."]

(** In JS and on 32-bit OCaml this will raise if and only if the number of bytes is outside
    of \[-2^32,2^32). *)
val words_int_exn : t -> int

val words_float : t -> float
val zero : t
val min_value : t
val max_value : t
val sign : t -> Sign.t
val abs : t -> t
val neg : t -> t

(** [scale t mul] scale the measure [t] by [mul] *)
val scale : t -> float -> t

val arg_type : t Command.Arg_type.t

module Infix : sig
  val ( - ) : t -> t -> t
  val ( + ) : t -> t -> t

  (** [( / ) t mul] scales [t] by [1/mul] *)
  val ( / ) : t -> float -> t

  (** [( // ) t1 t2] returns the ratio of t1 to t2 *)
  val ( // ) : t -> t -> float
end

include module type of Infix
include Quickcheck.S_range with type t := t

module Stable : sig
  (*_ old float based [bin_io] repr. *)
  module V1 : sig
    type nonrec t = t [@@deriving hash]

    include Stable_module_types.With_stable_witness.S0_without_comparator with type t := t
  end

  (*_ new [Int63] based [bin_io] repr. *)
  module V2 : sig
    type nonrec t = t [@@deriving equal, hash, typerep]

    include Stable_module_types.With_stable_witness.S0_without_comparator with type t := t
  end
end
