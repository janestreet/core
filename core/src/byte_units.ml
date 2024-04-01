(* Conversions between units of measure based on bytes. *)

open! Import
open Std_internal
module Repr = Int63
module T = Byte_units0
include (T : module type of T with module Repr := Repr)
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)

module Infix = struct
  let ( - ) a b = of_repr (Repr.( - ) (to_repr a) (to_repr b))
  let ( + ) a b = of_repr (Repr.( + ) (to_repr a) (to_repr b))
  let ( // ) a b = Repr.( // ) (to_repr a) (to_repr b)
  let ( / ) t s = of_repr (Float.int63_round_nearest_exn (Repr.to_float (to_repr t) /. s))
  let ( * ) t s = of_repr (Float.int63_round_nearest_exn (Repr.to_float (to_repr t) *. s))
end

include Infix

let abs t = of_repr (Repr.abs (to_repr t))
let neg t = of_repr (Repr.neg (to_repr t))
let sign t = Repr.sign (to_repr t)
let zero = of_repr Repr.zero
let min_value = of_repr Repr.min_value
let max_value = of_repr Repr.max_value
let scale = Infix.( * )
let iscale t s = of_repr (Repr.( * ) (to_repr t) (Repr.of_int s))
let bytes_int_exn = T.bytes_int_exn
let bytes_int63 = to_repr
let bytes_int64 t = Repr.to_int64 (to_repr t)
let bytes_float t = Repr.to_float (to_repr t)
let of_bytes_int b = of_repr (Repr.of_int b)
let of_bytes_int63 = of_repr
let of_bytes_int64_exn b = of_repr (Repr.of_int64_exn b)
let of_bytes_float_exn b = of_repr (Repr.of_float b)

let (bytes [@deprecated
             "[since 2019-01] Use [bytes_int_exn], [bytes_int63], [bytes_int64] or \
              [bytes_float] as appropriate."])
  =
  bytes_float
;;

let (of_bytes [@deprecated
                "[since 2019-01] Use [of_bytes_int], [of_bytes_int63], \
                 [of_bytes_int64_exn] or [of_bytes_float_exn] as appropriate."])
  =
  of_bytes_float_exn
;;

let kilobyte : t = of_bytes_int 1024
let megabyte = iscale kilobyte 1024
let gigabyte = iscale megabyte 1024
let terabyte = iscale gigabyte 1024
let petabyte = iscale terabyte 1024
let exabyte = iscale petabyte 1024

let word =
  let module W = Word_size in
  match W.word_size with
  | W.W32 -> of_bytes_int 4
  | W.W64 -> of_bytes_int 8
;;

let kilobytes t : float = Infix.( // ) t kilobyte
let megabytes t = Infix.( // ) t megabyte
let gigabytes t = Infix.( // ) t gigabyte
let terabytes t = Infix.( // ) t terabyte
let petabytes t = Infix.( // ) t petabyte
let exabytes t = Infix.( // ) t exabyte
let words_int_exn t = Repr.to_int_exn (Repr.( / ) (to_repr t) (to_repr word))
let words_float t = Infix.( // ) t word
let of_kilobytes t : t = Infix.( * ) kilobyte t
let of_megabytes t = Infix.( * ) megabyte t
let of_gigabytes t = Infix.( * ) gigabyte t
let of_terabytes t = Infix.( * ) terabyte t
let of_petabytes t = Infix.( * ) petabyte t
let of_exabytes t = Infix.( * ) exabyte t
let of_words_int t = iscale word t
let of_words_float_exn t = Infix.( * ) word t

let (words [@deprecated "[since 2019-01] Use [words_int_exn] or [words_float]"]) =
  words_float
;;

let (of_words [@deprecated "[since 2019-01] Use [of_words_int] or [of_words_float_exn]"]) =
  of_words_float_exn
;;

let of_string s =
  let length = String.length s in
  if Int.( < ) length 2
  then invalid_argf "'%s' passed to Byte_units.of_string - too short" s ();
  let base_str = String.sub s ~pos:0 ~len:(Int.( - ) length 1) in
  let ext_char = Char.lowercase s.[Int.( - ) length 1] in
  let base =
    try Float.of_string base_str with
    | _ ->
      invalid_argf
        "'%s' passed to Byte_units.of_string - %s cannot be converted to float "
        s
        base_str
        ()
  in
  match ext_char with
  | 'b' -> of_bytes_float_exn base
  | 'k' -> of_kilobytes base
  | 'm' -> of_megabytes base
  | 'g' -> of_gigabytes base
  | 't' -> of_terabytes base
  | 'p' -> of_petabytes base
  | 'e' -> of_exabytes base
  | 'w' -> of_words_float_exn base
  | ext ->
    invalid_argf "'%s' passed to Byte_units.of_string - illegal extension %c" s ext ()
;;

let arg_type = Command.Arg_type.create of_string

let largest_measure t =
  let t_abs = of_repr (Repr.abs (to_repr t)) in
  if t_abs >= exabyte
  then `Exabytes
  else if t_abs >= petabyte
  then `Petabytes
  else if t_abs >= terabyte
  then `Terabytes
  else if t_abs >= gigabyte
  then `Gigabytes
  else if t_abs >= megabyte
  then `Megabytes
  else if t_abs >= kilobyte
  then `Kilobytes
  else `Bytes
;;

let gen_incl lo hi =
  Repr.gen_incl (to_repr lo) (to_repr hi) |> Quickcheck.Generator.map ~f:of_repr
;;

let gen_uniform_incl lo hi =
  Repr.gen_uniform_incl (to_repr lo) (to_repr hi) |> Quickcheck.Generator.map ~f:of_repr
;;

module Stable = struct
  (* Share the common [of_sexp] code for [V1] and [V2]. *)
  module Of_sexp_v1_v2 : sig
    val t_of_sexp : Sexp.t -> t
  end = struct
    let no_match () = failwith "Not a recognized [Byte_units.t] representation"

    let of_value_sexp_and_unit_name val_sexp = function
      | "Bytes" ->
        (try of_bytes_int63 (Int63.t_of_sexp val_sexp) with
         | _ -> of_bytes_float_exn (Float.t_of_sexp val_sexp))
      | "Kilobytes" -> of_kilobytes (float_of_sexp val_sexp)
      | "Megabytes" -> of_megabytes (float_of_sexp val_sexp)
      | "Gigabytes" -> of_gigabytes (float_of_sexp val_sexp)
      | "Terabytes" -> of_terabytes (float_of_sexp val_sexp)
      | "Petabytes" -> of_petabytes (float_of_sexp val_sexp)
      | "Exabytes" -> of_exabytes (float_of_sexp val_sexp)
      | "Words" -> of_words_float_exn (float_of_sexp val_sexp)
      | _ -> no_match ()
    ;;

    let t_of_sexp = function
      | Sexp.Atom str -> of_string str
      | Sexp.List [ Sexp.Atom unit_name; value ] ->
        of_value_sexp_and_unit_name value unit_name
      | _ -> no_match ()
    ;;

    let t_of_sexp sexp =
      try t_of_sexp sexp with
      | exn -> raise (Sexp.Of_sexp_error (exn, sexp))
    ;;
  end

  module V1 = struct
    type nonrec t = t [@@deriving compare, hash, typerep]

    let to_binable = bytes_float
    let of_binable = of_bytes_float_exn

    include
      Binable0.Of_binable_without_uuid [@alert "-legacy"]
        (Float)
        (struct
          type nonrec t = t

          let to_binable = to_binable
          let of_binable = of_binable
        end)

    let stable_witness : t Stable_witness.t =
      Stable_witness.of_serializable stable_witness_float of_binable to_binable
    ;;

    include Of_sexp_v1_v2

    let sexp_of_t t =
      (* V1 only goes up to gigabytes *)
      match largest_measure t with
      | `Bytes -> [%sexp `Bytes (bytes_float t : float)]
      | `Kilobytes -> [%sexp `Kilobytes (kilobytes t : float)]
      | `Megabytes -> [%sexp `Megabytes (megabytes t : float)]
      | `Gigabytes | `Terabytes | `Petabytes | `Exabytes ->
        [%sexp `Gigabytes (gigabytes t : float)]
    ;;

    let to_string t = String.lowercase (to_string t)
    let of_string = of_string

    (* This test documents the original to-string representation and fails under javascript
       due to differences in the rounding. *)
    let%expect_test (_ [@tags "no-js"]) =
      printf !"%{}" (of_bytes_int 1000);
      [%expect {| 1000b |}];
      printf !"%{}" (of_bytes_int 1023);
      [%expect {| 1023b |}];
      printf !"%{}" (of_bytes_int 1024);
      [%expect {| 1k |}];
      printf !"%{}" (of_bytes_int 1025);
      [%expect {| 1.000977k |}];
      printf !"%{}" (of_bytes_int 1500);
      [%expect {| 1.464844k |}];
      printf !"%{}" (of_bytes_int 10000);
      [%expect {| 9.765625k |}];
      printf !"%{}" (of_bytes_int 100000);
      [%expect {| 97.65625k |}];
      printf !"%{}" (of_bytes_int 1000000);
      [%expect {| 976.5625k |}];
      printf !"%{}" (of_bytes_int 10000000);
      [%expect {| 9.536743164m |}]
    ;;

    let t_of_sexp sexp =
      match sexp with
      | Sexp.Atom s ->
        (try of_string s with
         | Invalid_argument msg -> of_sexp_error msg sexp)
      | Sexp.List _ -> t_of_sexp sexp
    ;;
  end

  module V2 = struct
    type nonrec t = t [@@deriving compare, equal, hash, typerep]

    let to_binable = bytes_int63
    let of_binable = of_bytes_int63

    include
      Binable0.Of_binable_without_uuid [@alert "-legacy"]
        (Int63.Stable.V1)
        (struct
          type nonrec t = t

          let to_binable = to_binable
          let of_binable = of_binable
        end)

    let stable_witness : t Stable_witness.t =
      Stable_witness.of_serializable Int63.Stable.V1.stable_witness of_binable to_binable
    ;;

    include Of_sexp_v1_v2

    let sexp_of_t t = [%sexp `Bytes (bytes_int63 t : Int63.t)]
  end
end

let to_string_hum = to_string_hum

module Short = struct
  type nonrec t = t

  let to_string t =
    let to_units_str to_unit ext =
      let f = to_unit t in
      let f_abs = Float.abs f in
      if Float.Robustly_comparable.( >=. ) f_abs 100.
      then sprintf "%.0f%c" f ext
      else if Float.Robustly_comparable.( >=. ) f_abs 10.
      then sprintf "%.1f%c" f ext
      else sprintf "%.2f%c" f ext
    in
    match largest_measure t with
    | `Bytes -> sprintf "%dB" (bytes_int_exn t)
    | `Kilobytes -> to_units_str kilobytes 'K'
    | `Megabytes -> to_units_str megabytes 'M'
    | `Gigabytes -> to_units_str gigabytes 'G'
    | `Terabytes -> to_units_str terabytes 'T'
    | `Petabytes -> to_units_str petabytes 'P'
    | `Exabytes -> to_units_str exabytes 'E'
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)

  let%expect_test _ =
    printf !"%{}" (of_bytes_int 1000);
    [%expect {| 1000B |}];
    printf !"%{}" (of_bytes_int 1023);
    [%expect {| 1023B |}];
    printf !"%{}" (of_bytes_int 1024);
    [%expect {| 1.00K |}];
    printf !"%{}" (of_bytes_int 1025);
    [%expect {| 1.00K |}];
    printf !"%{}" (of_bytes_int 10000);
    [%expect {| 9.77K |}];
    printf !"%{}" (of_bytes_int 100000);
    [%expect {| 97.7K |}];
    printf !"%{}" (of_bytes_int 1000000);
    [%expect {| 977K |}];
    printf !"%{}" (of_bytes_int 10000000);
    [%expect {| 9.54M |}];
    printf !"%{}" (of_bytes_float_exn 10000000000.);
    [%expect {| 9.31G |}];
    printf !"%{}" (of_bytes_float_exn 1000000000000.);
    [%expect {| 931G |}];
    printf !"%{}" (of_bytes_float_exn 100000000000000.);
    [%expect {| 90.9T |}];
    printf !"%{}" (of_bytes_float_exn 100000000000000000.);
    [%expect {| 88.8P |}];
    printf !"%{}" (of_bytes_float_exn 3000000000000000000.);
    [%expect {| 2.60E |}];
    ()
  ;;
end

let to_string_short = Short.to_string

let (create [@deprecated
              "[since 2019-01] Use [of_bytes], [of_kilobytes], [of_megabytes], etc as \
               appropriate."])
  =
  fun units value ->
  match units with
  | `Bytes -> of_bytes_float_exn value
  | `Kilobytes -> of_kilobytes value
  | `Megabytes -> of_megabytes value
  | `Gigabytes -> of_gigabytes value
  | `Words -> of_words_float_exn value
;;

include
  Quickcheckable.Of_quickcheckable
    (Repr)
    (struct
      type nonrec t = t

      let of_quickcheckable = of_repr
      let to_quickcheckable = to_repr
    end)
