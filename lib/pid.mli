(* Process ID. *)

type t

include Binable   .S with type t := t
include Comparable.S with type t := t
include Hashable  .S with type t := t
include Sexpable  .S with type t := t
include Stringable.S with type t := t

val of_int : int -> t
val to_int : t -> int

val init : t (* The pid of the "init" process, which is [1] by convention. *)
