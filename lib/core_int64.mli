include Int_intf.S with type t = int64

val of_int : int -> t
val to_int : t -> int option
val of_float : float -> t
val to_float : t -> float
val of_int32 : int32 -> t
val of_nativeint : nativeint -> t

val bits_of_float : float -> t
val float_of_bits : t -> float

val to_int32 : t -> int32 option
val to_nativeint : t -> nativeint option

val of_int64 : t -> t
