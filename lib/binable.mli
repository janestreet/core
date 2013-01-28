open Bin_prot.Binable
open Bigarray

(* We copy the definition of the bigstring type here, because we cannot depend on
   bigstring.ml *)
type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

module type S = S
module type S1 = S1
module type S2 = S2

module Of_stringable (M : Stringable.S) : S with type t := M.t

val of_bigstring : (module S with type t = 'a) -> bigstring -> 'a

val to_bigstring
  :  ?prefix_with_length:bool (* default false *)
  -> (module S with type t = 'a)
  -> 'a
  -> bigstring
