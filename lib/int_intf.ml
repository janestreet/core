open Interfaces

module type S = sig
  type t

  include Binable              with type t := t
  include Comparable.S_binable with type t := t
  include Floatable            with type t := t
  include Hashable.S_binable   with type t := t
  include Sexpable             with type t := t
  include Stringable           with type t := t
  include Intable              with type t := t

  val to_string_hum : t -> string

  val num_bits : int

  val zero : t
  val one : t
  val minus_one : t

  val (+)   : t -> t -> t
  val (-)   : t -> t -> t
  val ( * ) : t -> t -> t
  val (/)   : t -> t -> t

  val neg : t -> t

  val succ : t -> t
  val pred : t -> t

  val abs : t -> t

  (* Integer remainder, with the semantics of mod in Pervasives or rem in Int32/64, i.e.
     if y is not zero, the result of rem x y satisfies the following properties:
     x = (x / y) * y + rem x y and abs(rem x y) <= abs(y)-1.
     If y = 0, rem x y raises Division_by_zero. Notice that rem x y is nonpositive if and
     only if x < 0. *)
  val rem : t -> t -> t

  val max_value : t
  val min_value : t

  val bit_and : t -> t -> t
  val bit_or : t -> t -> t
  val bit_xor : t -> t -> t
  val bit_not : t -> t

  val decr : t ref -> unit
  val incr : t ref -> unit

  val shift_left : t -> int -> t
  val shift_right : t -> int -> t
  val shift_right_logical : t -> int -> t

  val of_int32_exn : int32 -> t
  val to_int32_exn : t -> int32
  val of_int64_exn : int64 -> t
  val to_int64 : t -> int64

  val of_nativeint_exn : nativeint -> t
  val to_nativeint_exn : t -> nativeint


end
