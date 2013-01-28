open Sexplib.Std
open Bin_prot.Std
module Char = Caml.Char

let failwithf = Core_printf.failwithf

module T = struct
  type t = char with bin_io, sexp

  let compare = Char.compare
  let equal (x: t) y = x = y
  let hash = Hashtbl.hash
end

include T

let to_int = Char.code

let unsafe_of_int = Char.unsafe_chr

(* We use our own range test when converting integers to chars rather than
   calling [Caml.Char.chr] because it's simple and it saves us a function call
   and the try-with (exceptions cost, especially in the world with backtraces. *)
let int_is_ok i = 0 <= i && i <= 255

let min_value = unsafe_of_int 0
let max_value = unsafe_of_int 255

let of_int i =
  if int_is_ok i
  then Some (unsafe_of_int i)
  else None
;;

let of_int_exn i =
  if int_is_ok i
  then unsafe_of_int i
  else failwithf "Char.of_int_exn got integer out of range: %d" i ()
;;

let escaped = Char.escaped

let lowercase = Char.lowercase

let uppercase = Char.uppercase

let is_lowercase t = 'a' <= t && t <= 'z'

let is_uppercase t = 'A' <= t && t <= 'Z'

let is_print t = ' ' <= t && t <= '~'

let is_whitespace t = t = ' ' || t = '\n' || t = '\t' || t = '\r'

let is_digit t = '0' <= t && t <= '9'

let is_alpha t = is_lowercase t || is_uppercase t

let is_alphanum t = is_alpha t || is_digit t

let to_string t = String.make 1 t

let get_digit_unsafe t = to_int t - to_int '0'

let get_digit_exn t =
  if is_digit t
  then get_digit_unsafe t
  else failwithf "Char.get_digit_exn %C: not a digit" t ()
;;

let get_digit t = if is_digit t then Some (get_digit_unsafe t) else None

include Comparable.Make (T)
include Hashable.Make (T)
