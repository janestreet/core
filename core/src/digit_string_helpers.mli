(** Utility functions for parsing and outputing strings containing known numbers
    of digits.  Used primarily for building functions for reading in and writing
    out Time related values. *)

open! Import


(** {2 Write digit functions}

    [write_int63 bytes ~pos ~digits int63] writes the string representation of [int63],
    0-padded to fill [digits] characters, into [bytes] starting at position [pos]. Raises
    if [int] is negative or is too long for [bytes], if [pos] is an invalid index in
    [bytes] for the number of digits, or if [digits < 1]. *)
val write_int63 : bytes -> pos:int -> digits:int -> Int63.t -> unit

(** [write_*_digit_int] is like [write_int63] for a hard-coded number of digits and for
    [int] rather than [Int63.t]. *)
val write_1_digit_int : bytes -> pos:int -> int -> unit

val write_2_digit_int : bytes -> pos:int -> int -> unit
val write_3_digit_int : bytes -> pos:int -> int -> unit
val write_4_digit_int : bytes -> pos:int -> int -> unit
val write_5_digit_int : bytes -> pos:int -> int -> unit
val write_6_digit_int : bytes -> pos:int -> int -> unit
val write_7_digit_int : bytes -> pos:int -> int -> unit
val write_8_digit_int : bytes -> pos:int -> int -> unit
val write_9_digit_int : bytes -> pos:int -> int -> unit

(** {2 Read digit functions}

    [read_int63 string ~pos ~digits] parses [digits] characters starting at [pos] in
    [string] and returns the corresponding [Int63.t]. It raises if [digits < 1] or
    [pos < 0] or [pos + digits > String.length string]. *)
val read_int63 : string -> pos:int -> digits:int -> Int63.t

(** [read_*_digit_int] is like [read_int63] for a hard-coded number of digits and for
    [int] rather than [Int63.t]. *)
val read_1_digit_int : string -> pos:int -> int

val read_2_digit_int : string -> pos:int -> int
val read_3_digit_int : string -> pos:int -> int
val read_4_digit_int : string -> pos:int -> int
val read_5_digit_int : string -> pos:int -> int
val read_6_digit_int : string -> pos:int -> int
val read_7_digit_int : string -> pos:int -> int
val read_8_digit_int : string -> pos:int -> int
val read_9_digit_int : string -> pos:int -> int

module Round : sig
  type t =
    | Toward_positive_infinity
    | Toward_negative_infinity
  [@@deriving compare, sexp_of]
end

(** [read_int63_decimal string ~pos ~decimals ~scale ~round_ties ~allow_underscore] reads
    [decimals] characters from [string] starting at [pos] as a decimal value as if
    starting immediately after a decimal point, and returns that fraction times [scale].
    The result is rounded to the nearest value, with ties broken by [round_ties].

    This function is useful for reading the decimal parts of numbers annotated with units
    that scale the result, such as when reading time units like "1.0ms" or "12.125s".

    If [allow_underscore = true], then '_' characters in [string] are allowed and ignored.
    Otherwise only digit characters are allowed.

    Raises if [pos] is out of range for [string] and [decimals], or if [scale < 1] or
    [scale > max_value / 20]. *)
val read_int63_decimal
  :  string
  -> pos:int
  -> decimals:int
  -> scale:Int63.t
  -> round_ties:Round.t
  -> allow_underscore:bool
  -> Int63.t

(** [max_int63_with ~digits] returns the maximum [Int63.t] that fits in [digits] decimal
    digits. *)
val max_int63_with : digits:int -> Int63.t

module Unsafe : sig
  (** [divide_and_round_up ~numerator ~denominator] returns [ceil
      (numerator/denominator)]. *)
  val divide_and_round_up : numerator:Int63.t -> denominator:Int63.t -> Int63.t
end
