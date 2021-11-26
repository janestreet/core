open! Import
open Std_internal

module Stable = struct
  module V1 = struct
    type t = (float[@quickcheck.generator Float.gen_finite])
    [@@deriving compare, hash, quickcheck, typerep]

    let of_mult f = f
    let to_mult t = t
    let of_percentage f = f /. 100.
    let to_percentage t = t *. 100.
    let of_bp f = f /. 10_000.
    let to_bp t = t *. 10_000.
    let of_bp_int i = of_bp (Float.of_int i)
    let to_bp_int t = Float.to_int (to_bp t)

    let round_significant p ~significant_digits =
      Float.round_significant p ~significant_digits
    ;;

    let round_decimal_mult p ~decimal_digits = Float.round_decimal p ~decimal_digits

    let round_decimal_percentage p ~decimal_digits =
      Float.round_decimal (p *. 100.) ~decimal_digits /. 100.
    ;;

    let round_decimal_bp p ~decimal_digits =
      Float.round_decimal (p *. 10000.) ~decimal_digits /. 10000.
    ;;

    module Format = struct
      type t =
        | Exponent of int
        | Exponent_E of int
        | Decimal of int
        | Ocaml
        | Compact of int
        | Compact_E of int
        | Hex of int
        | Hex_E of int
      [@@deriving sexp_of]

      let exponent ~precision = Exponent precision
      let exponent_E ~precision = Exponent_E precision
      let decimal ~precision = Decimal precision
      let ocaml = Ocaml
      let compact ~precision = Compact precision
      let compact_E ~precision = Compact_E precision
      let hex ~precision = Hex precision
      let hex_E ~precision = Hex_E precision

      let format_float t =
        match t with
        | Exponent precision -> sprintf "%.*e" precision
        | Exponent_E precision -> sprintf "%.*E" precision
        | Decimal precision -> sprintf "%.*f" precision
        | Ocaml -> sprintf "%F"
        | Compact precision -> sprintf "%.*g" precision
        | Compact_E precision -> sprintf "%.*G" precision
        | Hex precision -> sprintf "%.*h" precision
        | Hex_E precision -> sprintf "%.*H" precision
      ;;
    end

    let format x format =
      let x_abs = Float.abs x in
      let string float = Format.format_float format float in
      if Float.( = ) x_abs 0.
      then "0x"
      else if Float.( >= ) x_abs 1.
      then string (x *. 1.) ^ "x"
      else if Float.( >= ) x_abs 0.01
      then string (x *. 100.) ^ "%"
      else string (x *. 10_000.) ^ "bp"
    ;;

    module Stringable = struct
      type t = float

      (* WARNING - PLEASE READ BEFORE EDITING THESE FUNCTIONS:

         The string converters in Stable.V1 should never change.  If you are changing the
         semantics of anything that affects the sexp or bin-io representation of values of
         this type (this includes to_string and of_string) make a Stable.V2 and make your
         changes there.  Thanks! *)
      let to_string x =
        let x_abs = Float.abs x in
        let string float = sprintf "%.6G" float in
        if Float.( = ) x_abs 0.
        then "0x"
        else if Float.( >= ) x_abs 1.
        then string (x *. 1.) ^ "x"
        else if Float.( >= ) x_abs 0.01
        then string (x *. 100.) ^ "%"
        else string (x *. 10_000.) ^ "bp"
      ;;

      let really_of_string str float_of_string =
        match String.chop_suffix str ~suffix:"x" with
        | Some str -> float_of_string str
        | None ->
          (match String.chop_suffix str ~suffix:"%" with
           | Some str -> float_of_string str *. 0.01
           | None ->
             (match String.chop_suffix str ~suffix:"bp" with
              | Some str -> of_bp (float_of_string str)
              | None -> failwithf "Percent.of_string: must end in x, %%, or bp: %s" str ()))
      ;;

      let of_string str =
        let float str = Float_with_finite_only_serialization.t_of_sexp (Sexp.Atom str) in
        really_of_string str float
      ;;

      let of_string_allow_nan_and_inf str = really_of_string str Float.of_string
    end

    include (
      Stringable :
      sig
        type t

        val of_string : string -> t
        val to_string : t -> string
      end
      with type t := t)

    let t_sexp_grammar = Sexplib.Sexp_grammar.coerce String.t_sexp_grammar

    include (Sexpable.Stable.Of_stringable.V1 (Stringable) : Sexpable.S with type t := t)
    include (Float : Binable with type t := t)

    include Comparable.Make_binable (struct
        type nonrec t = t [@@deriving compare, sexp_of, bin_io]

        (* Previous versions rendered comparable-based containers using float
           serialization rather than percent serialization, so when reading
           comparable-based containers in we accept either serialization. *)
        let t_of_sexp sexp =
          match Float.t_of_sexp sexp with
          | float -> float
          | exception _ -> t_of_sexp sexp
        ;;
      end)
  end

  module Option = struct
    module V1 = struct
      type t = V1.t [@@deriving bin_io, compare, hash, typerep]

      let none = Float.nan
      let is_none t = Float.is_nan t
      let is_some t = not (is_none t)
      let some_is_representable = is_some
      let some = Fn.id
      let unchecked_value = Fn.id
      let to_option t = if is_some t then Some (unchecked_value t) else None
      let apply_with_none_as_nan = ( *. )
      let of_mult_with_nan_as_none = Fn.id
      let to_mult_with_none_as_nan = Fn.id

      let of_option opt =
        match opt with
        | None -> none
        | Some v -> some v
      ;;

      let value_exn t =
        if is_some t
        then unchecked_value t
        else raise_s [%message [%here] "Percent.Option.value_exn none"]
      ;;

      let value t ~default = if is_some t then unchecked_value t else default
      let sexp_of_t t = to_option t |> Option.sexp_of_t V1.sexp_of_t
      let t_of_sexp sexp = (Option.t_of_sexp V1.t_of_sexp) sexp |> of_option
      let t_sexp_grammar = Sexplib.Sexp_grammar.coerce [%sexp_grammar: V1.t Option.t]
    end
  end
end

include Stable.V1

module Option = struct
  module Stable = Stable.Option
  include Stable.V1

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = unchecked_value
    end
  end
end

let is_zero t = t = 0.
let apply t f = t *. f
let scale t f = t *. f

include (
struct
  include Float

  let ( // ) x y = of_mult x /. of_mult y
end :
sig
  val zero : t
  val ( * ) : t -> t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( // ) : t -> t -> float
  val abs : t -> t
  val neg : t -> t
  val is_nan : t -> bool
  val is_inf : t -> bool
  val sign_exn : t -> Sign.t

  include Robustly_comparable with type t := t
end)

include Comparable.With_zero (struct
    include Stable.V1

    let zero = zero
  end)

let validate = Float.validate_ordinary
let of_string_allow_nan_and_inf s = Stringable.of_string_allow_nan_and_inf s
let t_of_sexp_allow_nan_and_inf sexp = of_string_allow_nan_and_inf (Sexp.to_string sexp)

module Always_percentage = struct
  type nonrec t = t

  let format x format = Format.format_float format (x *. 100.) ^ "%"
  let to_string x = sprintf "%.6G%%" (x * 100.)
  let sexp_of_t t = Sexp.Atom (to_string t)
end
