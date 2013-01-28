open Bin_prot.Binable
open Bigarray

(* We copy the definition of the bigstring type here, because we cannot depend on
   bigstring.ml *)
type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

module type S = S
module type S1 = S1
module type S2 = S2

module Of_stringable (M : Stringable.S) : S with type t := M.t

type 'a m = (module S with type t = 'a)

val of_bigstring : 'a m -> bigstring -> 'a

val to_bigstring
  :  ?prefix_with_length:bool (* defaults to false *)
  -> 'a m
  -> 'a
  -> bigstring

val of_string : 'a m -> string -> 'a
val to_string : 'a m -> 'a -> string
