
type t = W32 | W64

val num_bits : t -> int

(* Returns the word size of this program, not necessarily of the OS *)
val word_size : t
