open Std_internal

module type Id = sig
  type t

  (* CAVEAT: Values created with of_float, of_sexp, or of_string may be equal to
     previously created values. *)
  include Comparable.S_binable with type t := t
  include Hashable with type t := t

  include Intable with type t := t
  (* The sexps and strings look like integers. *)
  include Sexpable with type t := t
  include Binable with type t := t
  include Stringable with type t := t

  (* Always returns a value that is not equal to any other value created with [create]. *)
  val create : unit -> t
end

module type S = sig
  (* An abstract unique identifier based on ordinary OCaml integers.  Be careful,
     this may easily overflow on 32bit platforms!  Int63 is a safer choice for
     portability.

     [Int] is useful when one is passing unique ids to C and needs a guarantee
     as to their representaion.  [Int] is always represented as an integer, while
     [Int63] is either an integer (on 64-bit machines) or a pointer (on 32-bit
     machines).

     The generated ids will therefore be fast to generate and not use much memory. If you
     do not have very stringent requirements on the size, speed, and ordering of your IDs
     then you should use UUIDM instead.

     UUIDM will give you truly unique id. Even amongst different runs and different
     machines.

     If you do the following:

     module Id1 = Int (Unit)
     module Id2 = Int (Unit)

     then the types Id1.t and Id2.t are equivalent.  On the other hand, if you do

     module Id1 : Id = Int (Unit)
     module Id2 : Id = Int (Unit)

     then the types Id1.t and Id2.t are distinct.  Thus, you should use the latter
     form.
  *)
  module Int (Z : Unit) : Id with type t = private int

  (* An abstract unique identifier based on 63 bit integers. *)
  module Int63 (Z : Unit) : Id with type t = private Core_int63.t
end
