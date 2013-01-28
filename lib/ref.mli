type 'a t = 'a ref = { mutable contents : 'a }

include Binable.S1 with type   'a t := 'a t
include Sexpable.S1 with type  'a t := 'a t

include Container.S1 with type 'a t := 'a t

val create : 'a -> 'a t

val (!) : 'a t -> 'a

val (:=) : 'a t -> 'a -> unit

(** [swap t1 t2] swaps the values in [t1] and [t2]. *)
val swap : 'a t -> 'a t -> unit

(** [replace t f] is [t := f !t] *)
val replace : 'a t -> ('a -> 'a) -> unit
