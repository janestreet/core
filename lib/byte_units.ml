(* Conversions between units of measure based on bytes. *)

open Sexplib.Std
open Bin_prot.Std
open Std_internal

let bytes_per_word =
  let module W = Word_size in
  match W.word_size with
    | W.W32 -> 4.
    | W.W64 -> 8.

let kbyte = 1024.
let mbyte = kbyte *. kbyte
let gbyte = kbyte *. mbyte

(* External.t - used just for custom sexp convertors *)
module External = struct
  type t =
    [
    | `Bytes of float
    | `Kilobytes of float
    | `Megabytes of float
    | `Gigabytes of float
    | `Words of float
    ]
  with bin_io, sexp
end

module Measure = struct

  type t = [ `Bytes | `Kilobytes | `Megabytes | `Gigabytes | `Words ]
  with bin_io

  let bytes = function
    | `Bytes -> 1.
    | `Kilobytes -> kbyte
    | `Megabytes -> mbyte
    | `Gigabytes -> gbyte
    | `Words -> bytes_per_word

  let smallest = function
    | (`Bytes, _) | (_, `Bytes) -> `Bytes
    | (`Words, _) | (_, `Words) -> `Words
    | (`Kilobytes, _) | (_, `Kilobytes) -> `Kilobytes
    | (`Megabytes, _) | (_, `Megabytes) -> `Megabytes
    | (`Gigabytes, `Gigabytes) -> `Gigabytes

end

module T = struct

  type t = {
    preferred_measure : Measure.t; (* for printing/externalizing *)
    bytes : float;
  } with bin_io

  module Infix = struct

    let lift_linear op t1 t2 = {
      bytes = op t1.bytes t2.bytes;
      preferred_measure =
        Measure.smallest (t1.preferred_measure, t2.preferred_measure);
    }

    let ( - ) t1 t2 = lift_linear ( -. ) t1 t2
    let ( + ) t1 t2 = lift_linear ( +. ) t1 t2

    let ( * ) t1 t2 = {t1 with bytes = t1.bytes *. t2}
    let ( / ) t1 t2 = {t1 with bytes = t1.bytes /. t2}
  end

  let number_of_preferred_measures t = t.bytes /. Measure.bytes t.preferred_measure

  let create m n = {
    preferred_measure = m;
    bytes = (n *. Measure.bytes m);
  }

  let externalize t =
    let n = number_of_preferred_measures t in
    match t.preferred_measure with
      | `Bytes      -> `Bytes n
      | `Kilobytes  -> `Kilobytes n
      | `Megabytes  -> `Megabytes n
      | `Gigabytes  -> `Gigabytes n
      | `Words      -> `Words n

  let internalize t =
    match t with
      | `Bytes     n -> create `Bytes n
      | `Kilobytes n -> create `Kilobytes n
      | `Megabytes n -> create `Megabytes n
      | `Gigabytes n -> create `Gigabytes n
      | `Words     n -> create `Words n

  let bytes t = t.bytes


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

  let to_string t =
    let ext =
      match t.preferred_measure with
      | `Bytes     -> 'b'
      | `Kilobytes -> 'k'
      | `Megabytes -> 'm'
      | `Gigabytes -> 'g'
      | `Words     -> 'w'
    in
    sprintf "%g%c" (number_of_preferred_measures t) ext

  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s ->
      (try of_string s with Invalid_argument msg -> of_sexp_error msg sexp)
    | Sexp.List _ ->
      internalize (External.t_of_sexp sexp)

  let sexp_of_t t = External.sexp_of_t (externalize t)

  let kilobytes t = bytes t /. kbyte
  let megabytes t = bytes t /. mbyte
  let gigabytes t = bytes t /. gbyte
  let words     t = bytes t /. bytes_per_word

  let compare t1 t2 = Float.compare (bytes t1) (bytes t2)

  let equal t1 t2 = bytes t1 = bytes t2
  let hash = Hashtbl.hash
end

include T
include Comparable.Make (T)
include Hashable.Make (T)
