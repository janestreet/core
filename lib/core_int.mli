include Int_intf.S with type t = int

val of_int : int -> t
val to_int : t -> int
val of_int32 : int32 -> t option
val to_int32 : t -> int32 option
val of_int64 : int64 -> t option
val of_int64_exn : int64 -> t
val of_nativeint : nativeint -> t option
val to_nativeint : t -> nativeint

module Infix : sig
  (** mod and div operators that have the right behavior on negative numbers,
      that is, [x % y] always returns a value between 0 and y-1.
      Invariant: [if r = x % y && q = x /% y then q * y + r = x] *)
  val (%) : t -> t -> t
  val (/%) : t -> t -> t

  (** float division of integers *)
  val (//) : int -> int -> float
end
