open! Import
open Std_internal
module String = Base.String

module Stable = struct
  module V3 = struct
    type t = (float[@quickcheck.generator Float.gen_finite])
    [@@deriving
      compare ~localize
      , equal ~localize
      , globalize
      , hash
      , quickcheck
      , typerep
      , stable_witness]

    (* For [s] which is a string representation of a finite float, in either decimal or
       scientific notation, shift the decimal point or adjust the scientific notation
       exponent by [by], which is equivalent to multiplying the number by [10**by].

       The output will be in scientific notation iff the input was, except when the
       resulting exponent after the shift becomes 0.

       No effort is made to normalize the scientific notation if it was not normalized, or
       to switch to/from scientific notation to help readability (except for the E0 case
       mentioned above). *)
    let rec shift_decimal_point s ~by =
      if by = 0
      then s
      else (
        let s = String.lstrip s in
        let d, e, zero, underscore =
          (* Find the position of the decimal point [d], the exponent sign [e], and check
             if there are any non-zero significant digits (only before the exponent sign)
             to determine if the number is equal to 0.0 *)
          let d = ref None in
          let e = ref None in
          let not_zero = ref false in
          let underscore = ref false in
          for i = 0 to String.length s - 1 do
            match s.[i] with
            | '.' ->
              if Option.is_some !d
              then
                failwithf "Error parsing Percent.t: too many decimal points in '%s'" s ();
              d := Some i
            | 'e' | 'E' ->
              if Option.is_some !e
              then failwithf "Error parsing Percent.t: too many Es in '%s'" s ();
              e := Some i
            | '-' | '+' | '0' -> ()
            | '_' -> underscore := true
            | '1' .. '9' -> if Option.is_none !e then not_zero := true
            | c ->
              failwithf "Unexpected character when parsing Percent.t: '%c' in '%s'" c s ()
          done;
          !d, !e, not !not_zero, !underscore
        in
        if underscore
        then shift_decimal_point (String.filter s ~f:(fun c -> Char.(c <> '_'))) ~by
        else if zero
        then s
        else (
          match e with
          | Some e ->
            (* [s] is in scientific notation.  Simply adjust the exponent after 'e'. *)
            let exp = Int.of_string (String.drop_prefix s (e + 1)) in
            let exp = exp + by in
            if exp = 0
            then
              (* We end up with exponent 0, so we just skip the "e" altogether *)
              String.prefix s e
            else
              String.concat
                [ String.prefix s (e + 1)
                ; (if exp > 0 then "+" else "")
                ; Int.to_string exp
                ]
          | None ->
            (* [s] is in decimal notation.  Start by dropping the leading sign if there is
               one.  We'll re-add it at the end to keep the rest of the code simpler. *)
            let neg = Char.( = ) s.[0] '-' in
            let signed = neg || Char.( = ) s.[0] '+' in
            let s = if signed then String.drop_prefix s 1 else s in
            let s, by =
              (* Next, scratch the decimal point if there is one, so that [s] becomes an
                 integer, and adjust [by] accordingly to keep the result unchanged.  We
                 also know that [s <> 0.0] so we remove leading zeros.  We may be wasting
                 some work because we may have to add them back below, but the code is
                 simpler that way. *)
              match d with
              | None -> s, by
              | Some d ->
                let d =
                  (* We calculated [d] before removing the "-" sign, so we may have to
                     adjust it now *)
                  d - if signed then 1 else 0
                in
                let suf = String.drop_prefix s (d + 1) in
                let pref = String.prefix s d in
                let s = pref ^ suf |> String.lstrip ~drop:(Char.( = ) '0') in
                let by = by - String.length suf in
                s, by
            in
            (* Remove trailing zeros too *)
            let s, by =
              let len = String.length s in
              let s = s |> String.rstrip ~drop:(Char.( = ) '0') in
              let by = by + len - String.length s in
              s, by
            in
            let s =
              let len = String.length s in
              if by = 0
              then s
              else if by > 0
              then
                (* Case 1: [s] is an integer which we need to further multiply by a
                   positive power of 10, so we just add 0s at the end. *)
                s ^ String.make by '0'
              else if by > -len
              then
                (* Case 2: [s] is an integer which we need to divide by a positive power
                   of 10, and it has enough digits that we can just insert the '.' in
                   between them. *)
                String.concat [ String.drop_suffix s (-by); "."; String.suffix s (-by) ]
              else
                (* Case 3 (by <= -len): [s] is an integer which we need to divide by a
                   large enough positive power of 10 that we need to add leading 0s. *)
                String.concat [ "0."; String.make (-by - len) '0'; s ]
            in
            (* Finally, restore the sign.  We can't just drop the '+' sign, we need to
               restore it too, because if we didn't, we would silently parse numbers like
               "+-13%" or "++17bp" without raising an error. *)
            if signed then (if neg then "-" else "+") ^ s else s))
    ;;

    let%expect_test "shift_decimal_point 1" =
      List.iter
        [ "3"
        ; "51.2"
        ; "-50"
        ; "3127000.000"
        ; "1.79E+308"
        ; "4.940656E-324"
        ; "-0.000e13"
        ; "1.47651E+10"
        ]
        ~f:(fun s ->
          printf "== %s ==\n" s;
          let x = Float.of_string s in
          List.iter [ -40; -10; -2; -1; 0; 1; 2; 10; 40 ] ~f:(fun by ->
            let s1 = shift_decimal_point s ~by in
            printf "%s\n" s1;
            let x1 = Float.of_string s1 in
            let x2 = x *. (10. ** float by) in
            assert (Float.(x1 = x2 || abs ((x1 - x2) / x1) < 1e-6) : bool));
          printf "--------------------------------------------------\n");
      (* Note "-0.000e13" is not adjusted *)
      [%expect
        {|
        == 3 ==
        0.0000000000000000000000000000000000000003
        0.0000000003
        0.03
        0.3
        3
        30
        300
        30000000000
        30000000000000000000000000000000000000000
        --------------------------------------------------
        == 51.2 ==
        0.00000000000000000000000000000000000000512
        0.00000000512
        0.512
        5.12
        51.2
        512
        5120
        512000000000
        512000000000000000000000000000000000000000
        --------------------------------------------------
        == -50 ==
        -0.000000000000000000000000000000000000005
        -0.000000005
        -0.5
        -5
        -50
        -500
        -5000
        -500000000000
        -500000000000000000000000000000000000000000
        --------------------------------------------------
        == 3127000.000 ==
        0.0000000000000000000000000000000003127
        0.0003127
        31270
        312700
        3127000.000
        31270000
        312700000
        31270000000000000
        31270000000000000000000000000000000000000000000
        --------------------------------------------------
        == 1.79E+308 ==
        1.79E+268
        1.79E+298
        1.79E+306
        1.79E+307
        1.79E+308
        1.79E+309
        1.79E+310
        1.79E+318
        1.79E+348
        --------------------------------------------------
        == 4.940656E-324 ==
        4.940656E-364
        4.940656E-334
        4.940656E-326
        4.940656E-325
        4.940656E-324
        4.940656E-323
        4.940656E-322
        4.940656E-314
        4.940656E-284
        --------------------------------------------------
        == -0.000e13 ==
        -0.000e13
        -0.000e13
        -0.000e13
        -0.000e13
        -0.000e13
        -0.000e13
        -0.000e13
        -0.000e13
        -0.000e13
        --------------------------------------------------
        == 1.47651E+10 ==
        1.47651E-30
        1.47651
        1.47651E+8
        1.47651E+9
        1.47651E+10
        1.47651E+11
        1.47651E+12
        1.47651E+20
        1.47651E+50
        --------------------------------------------------
        |}]
    ;;

    module Stringable = struct
      type t = float

      external format_float
        :  string
        -> local_ float
        -> string
        @@ portable
        = "caml_format_float"

      (* Logic stolen from lib/base/src/float.ml, but this does not call
         [valid_float_lexem] so not adding a trailing "." and also, we're using
         uppercase (%G vs %g) for consistency with [Percent.Stable.V2]. *)
      let float_to_string x =
        let y = format_float "%.15G" x in
        if Float.( = ) (float_of_string y) x then y else format_float "%.17G" x
      ;;

      let to_string' float_to_string x =
        let x_abs = Float.abs x in
        if Float.( = ) x_abs 0.
        then "0x"
        else (
          let s = float_to_string x in
          (* Unimportant note: V3 serializes nan as "NANx", while V2 serializes it as
             "NANbp" *)
          if Float.(is_nan x || is_inf x || x_abs >= 1.)
          then s ^ "x"
          else if Float.( >= ) x_abs 0.01
          then shift_decimal_point s ~by:2 ^ "%"
          else shift_decimal_point s ~by:4 ^ "bp")
      ;;

      let to_string x = to_string' float_to_string x

      let really_of_string str float_of_string =
        match String.chop_suffix str ~suffix:"x" with
        | Some str -> float_of_string str ~by:0
        | None ->
          (match String.chop_suffix str ~suffix:"%" with
           | Some str -> float_of_string str ~by:(-2)
           | None ->
             (match String.chop_suffix str ~suffix:"bp" with
              | Some str -> float_of_string str ~by:(-4)
              | None -> failwithf "Percent.of_string: must end in x, %%, or bp: %s" str ()))
      ;;

      let of_string x =
        really_of_string x (fun s ~by ->
          shift_decimal_point s ~by
          |> Sexp.Atom
          |> Float_with_finite_only_serialization.t_of_sexp)
      ;;

      let of_string_allow_nan_and_inf x =
        really_of_string x (fun s ~by ->
          match String.lowercase s with
          | "nan"
          | "-nan"
          | "+nan"
          | "inf"
          | "infinity"
          | "+inf"
          | "+infinity"
          | "-inf"
          | "-infinity" ->
            (* We parse these nan or inf strings directly, because
               [shift_decimal_point] does not support them *)
            Float.of_string s
          | _ -> shift_decimal_point s ~by |> Float.of_string)
      ;;
    end

    include (
      Stringable :
        sig
        @@ portable
          type t

          val to_string : local_ t -> string
          val of_string : string -> t
          val of_string_allow_nan_and_inf : string -> t
        end
        with type t := t)

    let t_sexp_grammar = Sexplib.Sexp_grammar.coerce String.t_sexp_grammar

    include%template (
      Sexpable.Stable.Of_stringable.V1 [@modality portable]
        (Stringable) :
        sig
        @@ portable
          include Sexpable.S with type t := t
        end)

    include%template (
      Float :
      sig
      @@ portable
        include Binable.S [@mode local] with type t := t
      end)

    (* Use a different bin_shape_t than Percent.Stable.V2, even though these two versions
       have compatible bin_io serialization.  Since they do differ in sexp serialization,
       we err on the side of safety and change the bin shape to warn that V3 is not always
       a drop-in replacement for V2. *)
    let bin_shape_t =
      Bin_prot.Shape.basetype
        (Bin_prot.Shape.Uuid.of_string "b32f2a1e-6b43-11ed-b33b-aac2a563f10a")
        [ bin_shape_t ]
    ;;

    include%template Comparable.Make_binable [@mode local] [@modality portable] (struct
        type nonrec t = t [@@deriving bin_io, compare ~localize, sexp]
      end)

    include%template Diffable.Atomic.Make [@modality portable] (struct
        type nonrec t = t [@@deriving bin_io, equal ~localize, sexp]
      end)

    module Always_percentage = struct
      type nonrec t = t [@@deriving sexp, bin_io ~localize]

      let to_string x =
        let s = Stringable.float_to_string x in
        shift_decimal_point s ~by:2 ^ "%"
      ;;

      let sexp_of_t t = Sexp.Atom (to_string t)
    end
  end

  module V2 = struct
    type t = (float[@quickcheck.generator Float.gen_finite])
    [@@deriving
      compare ~localize
      , equal ~localize
      , globalize
      , hash
      , quickcheck
      , typerep
      , stable_witness]

    external of_mult : (float[@local_opt]) -> (t[@local_opt]) @@ portable = "%identity"
    external to_mult : (t[@local_opt]) -> (float[@local_opt]) @@ portable = "%identity"

    let of_percentage f = globalize (f /. 100.) [@nontail]
    let to_percentage t = globalize (t *. 100.) [@nontail]
    let of_bp f = globalize (f /. 10_000.) [@nontail]
    let to_bp t = globalize (t *. 10_000.) [@nontail]
    let of_bp_int i = of_bp (Float.of_int i)
    let to_bp_int t = Float.to_int (to_bp t)

    [%%template
    [@@@mode m = (local, global)]

    (* Multiply the decimal representation of float [f] by [10**by], and convert back to
       the nearest representable float. *)
    let[@inline] shift_decimal_point_in_float (f @ m) ~(by @ m) =
      if not (Float.is_finite f)
      then f
      else f |> Float.to_string |> V3.shift_decimal_point ~by |> Float.of_string
    ;;

    [@@@mode.default m = m]

    let of_percentage_slow_more_accurate f =
      shift_decimal_point_in_float f ~by:(-2) [@exclave_if_local m]
    ;;

    let to_percentage_slow_more_accurate f =
      shift_decimal_point_in_float f ~by:2 [@exclave_if_local m]
    ;;

    let of_bp_slow_more_accurate f =
      shift_decimal_point_in_float f ~by:(-4) [@exclave_if_local m]
    ;;

    let to_bp_slow_more_accurate f =
      shift_decimal_point_in_float f ~by:4 [@exclave_if_local m]
    ;;

    let round_significant = (Float.round_significant [@mode m])
    let round_decimal_mult = (Float.round_decimal [@mode m])]

    [%%template
    let round_decimal_percentage p ~decimal_digits =
      globalize
        ((round_decimal_mult [@mode local]) (p *. 100.) ~decimal_digits /. 100.)
      [@nontail]
    ;;

    let round_decimal_bp p ~decimal_digits =
      globalize
        ((round_decimal_mult [@mode local]) (p *. 10000.) ~decimal_digits /. 10000.)
      [@nontail]
    ;;]

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

      external format_float
        :  string
        -> local_ float
        -> string
        @@ portable
        = "caml_format_float"

      (* WARNING - PLEASE READ BEFORE EDITING THESE FUNCTIONS:

         The string converters in Stable.V3/V2/V1 should never change. If you are changing
         the semantics of anything that affects the sexp or bin-io representation of
         values of this type (this includes to_string and of_string) make a Stable.V4 and
         make your changes there. Thanks! *)
      let to_string x =
        let x_abs = Float.abs x in
        let string float = format_float "%.6G" float in
        if Float.( = ) x_abs 0.
        then "0x"
        else if Float.( >= ) x_abs 1.
        then string (x *. 1.) ^ "x"
        else if Float.( >= ) x_abs 0.01
        then string (x *. 100.) ^ "%"
        else string (x *. 10_000.) ^ "bp"
      ;;

      let of_string = V3.of_string
      let of_string_allow_nan_and_inf = V3.of_string_allow_nan_and_inf
    end

    include (
      Stringable :
        sig
        @@ portable
          type t

          val to_string : local_ t -> string
          val of_string : string -> t
          val of_string_allow_nan_and_inf : string -> t
        end
        with type t := t)

    let t_sexp_grammar = Sexplib.Sexp_grammar.coerce String.t_sexp_grammar

    include%template (
      Sexpable.Stable.Of_stringable.V1 [@modality portable]
        (Stringable) :
        sig
        @@ portable
          include Sexpable.S with type t := t
        end)

    include%template (
      Float :
      sig
      @@ portable
        include Binable.S [@mode local] with type t := t
      end)

    (* Mint a unique [bin_shape_t] that's distinct from the hidden underlying
       representation of float. *)
    let bin_shape_t =
      Bin_prot.Shape.basetype
        (Bin_prot.Shape.Uuid.of_string "1d1e76bc-ea4b-11eb-a16a-aa5b28d1f4d7")
        [ bin_shape_t ]
    ;;

    include%template
      Comparable.Make_binable_using_comparator [@mode local] [@modality portable] (struct
        type nonrec t = t [@@deriving bin_io, compare ~localize, sexp]
        type comparator_witness = V3.comparator_witness

        let comparator = V3.comparator
      end)

    include%template Diffable.Atomic.Make [@modality portable] (struct
        type nonrec t = t [@@deriving bin_io, sexp, equal ~localize]
      end)

    type comparator_witness = V3.comparator_witness
  end

  module V1 = struct
    module Bin_shape_same_as_float = struct
      include V2

      let bin_shape_t = Float.bin_shape_t

      include%template
        Comparable.Make_binable_using_comparator [@mode local] [@modality portable] (struct
          type nonrec t = t [@@deriving compare ~localize, sexp_of, bin_io]
          type comparator_witness = V3.comparator_witness

          let comparator = V3.comparator

          (* Previous versions rendered comparable-based containers using float
             serialization rather than percent serialization, so when reading
             comparable-based containers in we accept either serialization.
          *)
          let t_of_sexp sexp =
            match Float.t_of_sexp sexp with
            | float -> float
            | exception _ -> t_of_sexp sexp
          ;;
        end)

      include%template Diffable.Atomic.Make [@modality portable] (struct
          type nonrec t = t [@@deriving bin_io, equal ~localize, sexp]
        end)
    end

    include Bin_shape_same_as_float
  end

  module Option = struct
    module Option_repr = struct
      let none = Float.nan
      let is_none t = Float.is_nan t
      let is_some t = not (is_none t)
      let some_is_representable = is_some

      [%%template
      [@@@mode.default m = (local, global)]

      let some = Fn.id
      let unchecked_value : _ @ m -> _ @ m = Fn.id]

      let%template to_option t =
        if is_some t
        then Some ((unchecked_value [@mode m]) t) [@exclave_if_stack a]
        else None
      [@@alloc a @ m = (stack_local, heap_global)]
      ;;

      let%template[@mode local] to_option = (to_option [@alloc stack])

      let%template equal x y = (Float.equal [@mode m]) x y || (is_none x && is_none y)
      [@@mode m = (local, global)]
      ;;

      external apply_with_none_as_nan
        :  (float[@local_opt])
        -> (float[@local_opt])
        -> (float[@local_opt])
        @@ portable
        = "%mulfloat"

      external of_mult_with_nan_as_none
        :  (float[@local_opt])
        -> (float[@local_opt])
        @@ portable
        = "%identity"

      external to_mult_with_none_as_nan
        :  (float[@local_opt])
        -> (float[@local_opt])
        @@ portable
        = "%identity"

      [%%template
      [@@@mode.default m = (local, global)]

      let of_option opt =
        match opt with
        | None -> none
        | Some v -> (some [@mode m]) v [@exclave_if_local m]
      ;;

      let value_exn t =
        if is_some t
        then (unchecked_value [@mode m]) t [@exclave_if_local m]
        else raise_s [%message [%here] "Percent.Option.value_exn none"]
      ;;]

      let%template value t ~default =
        (Bool.select [@mode m])
          (is_none t)
          default
          ((unchecked_value [@mode m]) t) [@exclave_if_stack a]
      [@@alloc a @ m = (stack_local, heap_global)]
      ;;

      let%template[@mode local] value = (value [@alloc stack])
    end

    module V3 = struct
      type t = V3.t
      [@@deriving
        bin_io ~localize
        , compare ~localize
        , equal ~localize
        , globalize
        , hash
        , stable_witness
        , typerep]

      let bin_shape_t =
        Bin_prot.Shape.basetype
          (Bin_prot.Shape.Uuid.of_string "e9b52028-6b45-11ed-a6b6-aac2a563f10a")
          [ bin_shape_t ]
      ;;

      include Option_repr

      let sexp_of_t t = to_option t |> Option.sexp_of_t V3.sexp_of_t
      let t_of_sexp sexp = (Option.t_of_sexp V3.t_of_sexp) sexp |> of_option
      let t_sexp_grammar = Sexplib.Sexp_grammar.coerce [%sexp_grammar: V3.t Option.t]
    end

    module V2 = struct
      type t = V2.t
      [@@deriving
        bin_io ~localize, compare ~localize, globalize, hash, stable_witness, typerep]

      let bin_shape_t =
        Bin_prot.Shape.basetype
          (Bin_prot.Shape.Uuid.of_string "f7aa04e8-da3e-11ec-b546-aa7328433b70")
          [ bin_shape_t ]
      ;;

      include Option_repr

      let sexp_of_t t = to_option t |> Option.sexp_of_t V2.sexp_of_t
      let t_of_sexp sexp = (Option.t_of_sexp V2.t_of_sexp) sexp |> of_option
      let t_sexp_grammar = Sexplib.Sexp_grammar.coerce [%sexp_grammar: V2.t Option.t]
    end

    module V1 = struct
      module Bin_shape_same_as_float = struct
        include V2

        let bin_shape_t = Float.bin_shape_t
      end

      include Bin_shape_same_as_float
    end
  end
end

include Stable.V1.Bin_shape_same_as_float

module Option = struct
  module Stable = Stable.Option
  include Stable.V1.Bin_shape_same_as_float

  include%template
    Quickcheckable.Of_quickcheckable [@mode portable]
      (struct
        type t = float option [@@deriving quickcheck ~portable]
      end)
      (struct
        type nonrec t = t

        let to_quickcheckable = to_option
        let of_quickcheckable = of_option
      end)

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let%template unsafe_value = (unchecked_value [@mode m]) [@@mode m = (local, global)]
    end
  end
end

let is_zero t = [%equal_local: t] t 0.

external apply
  :  (t[@local_opt])
  -> (float[@local_opt])
  -> (float[@local_opt])
  @@ portable
  = "%mulfloat"

external scale
  :  (t[@local_opt])
  -> (float[@local_opt])
  -> (t[@local_opt])
  @@ portable
  = "%mulfloat"

include (
struct
  include Float

  let one_hundred_percent = 1.
end :
sig
@@ portable
  val zero : t
  val one_hundred_percent : t
  external ( * ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt]) = "%mulfloat"
  external ( + ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt]) = "%addfloat"
  external ( - ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt]) = "%subfloat"
  external ( / ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt]) = "%divfloat"
  external abs : (t[@local_opt]) -> (t[@local_opt]) = "%absfloat"
  external neg : (t[@local_opt]) -> (t[@local_opt]) = "%negfloat"
  val is_nan : local_ t -> bool
  val is_inf : local_ t -> bool
  val sign_exn : local_ t -> Sign.t

  include Robustly_comparable with type t := t
end)

external ( // )
  :  (t[@local_opt])
  -> (t[@local_opt])
  -> (t[@local_opt])
  @@ portable
  = "%divfloat"

module%template With_zero = Comparable.With_zero [@modality portable] (struct
    include Stable.V1.Bin_shape_same_as_float

    let zero = zero
  end)

include (
  With_zero :
  sig
  @@ portable
    val validate_positive : t Validate.check
    val validate_non_negative : t Validate.check
    val validate_negative : t Validate.check
    val validate_non_positive : t Validate.check

    (* This behavior doesn't align with that of [Float], but we don't worry about it
       because it is deprecated anyways in favor of [sign_exn] (which raises on [NaN]). *)
    val sign : t -> Sign.t
  end)

(* See comment in mli about the functions below. *)

let clamp_legacy = clamp
and clamp = Float.clamp

let clamp_exn_legacy = clamp_exn
and clamp_exn = Float.clamp_exn

let min_legacy = min
and min = Float.min

let max_legacy = max
and max = Float.max

let is_positive_legacy = With_zero.is_positive
and is_positive = Float.is_positive

let is_non_negative_legacy = With_zero.is_non_negative
and is_non_negative = Float.is_non_negative

let is_negative_legacy = With_zero.is_negative
and is_negative = Float.is_negative

let is_non_positive_legacy = With_zero.is_non_positive
and is_non_positive = Float.is_non_positive

(* [validate_ordinary] fails if class is [Nan] or [Infinite]. *)
let validate = Float.validate_ordinary
let of_string_allow_nan_and_inf s = Stringable.of_string_allow_nan_and_inf s
let t_of_sexp_allow_nan_and_inf sexp = of_string_allow_nan_and_inf (Sexp.to_string sexp)
let arg_type = [%template Command.Arg_type.create [@modality portable]] of_string

let arg_type_allow_nan_and_inf =
  [%template Command.Arg_type.create [@modality portable]] of_string_allow_nan_and_inf
;;

module Always_percentage = struct
  type nonrec t = t

  let format x format = Format.format_float format (x *. 100.) ^ "%"
  let to_string x = sprintf "%.6G%%" (x * 100.)
  let sexp_of_t t = Sexp.Atom (to_string t)
end

let to_string_round_trippable = Stable.V3.to_string

module Almost_round_trippable = struct
  type nonrec t = t [@@deriving sexp, bin_io]

  let to_string x = Stable.V3.Stringable.(to_string' (format_float "%.14G") x)
  let of_string x = Stable.V3.Stringable.of_string x
  let sexp_of_t t = Sexp.Atom (to_string t)
  let t_of_sexp = Stable.V3.t_of_sexp

  module Always_percentage = struct
    type nonrec t = t [@@deriving sexp, bin_io]

    let to_string x =
      let open Stable.V3 in
      let s = Stringable.format_float "%.14G" x in
      shift_decimal_point s ~by:2 ^ "%"
    ;;

    let sexp_of_t t = Sexp.Atom (to_string t)
  end
end
