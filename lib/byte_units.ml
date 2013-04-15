(* Conversions between units of measure based on bytes. *)

open Std_internal

let bytes_per_word =
  let module W = Word_size in
  match W.word_size with
    | W.W32 -> 4.
    | W.W64 -> 8.
;;

let kbyte = 1024.
let mbyte = kbyte *. kbyte
let gbyte = kbyte *. mbyte

(* External.t - used just for custom sexp converters *)
module External = struct
  type t =
    [
    | `Bytes of float
    | `Kilobytes of float
    | `Megabytes of float
    | `Gigabytes of float
    | `Words of float
    ]
  with sexp
end

module Measure = struct
  type t = [ `Bytes | `Kilobytes | `Megabytes | `Gigabytes | `Words ]
  with sexp, bin_io

  let bytes = function
    | `Bytes -> 1.
    | `Kilobytes -> kbyte
    | `Megabytes -> mbyte
    | `Gigabytes -> gbyte
    | `Words -> bytes_per_word
  ;;
end

module T = struct
  type t = float with bin_io, compare

  let hash = Float.hash

  let scale = Float.( * )

  module Infix = struct
    open Float
    let ( - )  = ( - )
    let ( + )  = ( + )
    let ( / )  = ( / )
    let ( // ) = ( / )
  end

  let largest_measure t =
    (* We never select words as the largest measure *)
    if Float.( > ) t gbyte then `Gigabytes
    else if Float.( > ) t mbyte then `Megabytes
    else if Float.( > ) t kbyte then `Kilobytes
    else `Bytes

  let number_of_measures t measure = t /. Measure.bytes measure

  let create m n = n *. Measure.bytes m

  let externalize t =
    let used_measure = largest_measure t in
    let n = number_of_measures t used_measure in
    match used_measure with
      | `Bytes      -> `Bytes n
      | `Kilobytes  -> `Kilobytes n
      | `Megabytes  -> `Megabytes n
      | `Gigabytes  -> `Gigabytes n
      | `Words      -> `Words n
  ;;

  let internalize t =
    match t with
      | `Bytes     n -> create `Bytes n
      | `Kilobytes n -> create `Kilobytes n
      | `Megabytes n -> create `Megabytes n
      | `Gigabytes n -> create `Gigabytes n
      | `Words     n -> create `Words n
  ;;

  let bytes t = t

  let of_string s =
    let length = String.length s in
    if length < 2 then
      invalid_argf "'%s' passed to Byte_units.of_string - too short" s ();
    let base_str = String.sub s ~pos:0 ~len:(length - 1) in
    let ext_char = Char.lowercase s.[length - 1] in
    let base =
      try
        Float.of_string base_str
      with
      | _ ->
        invalid_argf "'%s' passed to Byte_units.of_string - %s cannot be \
          converted to float " s base_str ()
    in
    let measure =
      match ext_char with
      | 'b' -> `Bytes
      | 'k' -> `Kilobytes
      | 'm' -> `Megabytes
      | 'g' -> `Gigabytes
      | 'w' -> `Words
      | ext ->
        invalid_argf "'%s' passed to Byte_units.of_string - illegal \
          extension %c" s ext ()
    in
    create measure base
  ;;

  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s ->
      (try of_string s with Invalid_argument msg -> of_sexp_error msg sexp)
    | Sexp.List _ ->
      internalize (External.t_of_sexp sexp)
  ;;

  let sexp_of_t t = External.sexp_of_t (externalize t)

  let kilobytes t = bytes t /. kbyte
  let megabytes t = bytes t /. mbyte
  let gigabytes t = bytes t /. gbyte
  let words     t = bytes t /. bytes_per_word

  let to_string_with_measure measure t =
    let ext =
      match measure with
      | `Bytes     -> 'b'
      | `Kilobytes -> 'k'
      | `Megabytes -> 'm'
      | `Gigabytes -> 'g'
      | `Words     -> 'w'
    in
    sprintf "%g%c" (number_of_measures t measure) ext
  ;;

  let to_string_hum ?measure t =
    let measure =
      match measure with
      | Some m -> m
      | None   -> largest_measure t
    in
    to_string_with_measure measure t
  ;;

  let to_string t = to_string_hum t
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

TEST_MODULE "{of,to}_string" = struct

  let f measure input expected_output =
    let observed_output =
      match measure with
      | `Specific measure ->
        to_string_hum ~measure (of_string input)
      | `Largest ->
        to_string_hum (of_string input)
    in

    let result = String.equal expected_output observed_output in
    if not result then begin
      let measure =
        <:sexp_of<[ `Specific of Measure.t | `Largest ]>> measure
        |! Sexp.to_string
      in
      eprintf "\n(%s) %s -> %s != %s\n%!" measure input expected_output observed_output
    end;
    result

  TEST = f `Largest "3b" "3b"
  TEST = f `Largest "3w" (sprintf "%gb" (3.0 *. bytes_per_word))
  TEST = f `Largest "3k" "3k"
  TEST = f `Largest "3m" "3m"
  TEST = f `Largest "3g" "3g"

  TEST = f (`Specific `Bytes)     "3k" "3072b"
  TEST = f (`Specific `Kilobytes) "3k" "3k"
  TEST = f (`Specific `Megabytes) "3k" "0.00292969m"
  TEST = f (`Specific `Gigabytes) "3k" "2.86102e-06g"
  TEST = f (`Specific `Words)     "3k" (sprintf "%gw" ((3.0 *. kbyte) /. bytes_per_word))

end
