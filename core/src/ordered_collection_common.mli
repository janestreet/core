(** This module extends {{!Base.Ordered_collection_common}[Base.Ordered_collection_common]}. *)

include module type of struct
  include Base.Ordered_collection_common
end


val normalize : length_fun:('a -> int) -> 'a -> int -> int

val slice
  :  length_fun:('a -> int)
  -> sub_fun:('a -> pos:int -> len:int -> 'a)
  -> 'a
  -> int
  -> int
  -> 'a
