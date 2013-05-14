open Common

type 'a t = 'a ref = { mutable contents : 'a }
with bin_io, sexp

include Container.S1 with type 'a t := 'a t

val create : 'a -> 'a t

val (!) : 'a t -> 'a

val (:=) : 'a t -> 'a -> unit

(** [swap t1 t2] swaps the values in [t1] and [t2]. *)
val swap : 'a t -> 'a t -> unit

(** [replace t f] is [t := f !t] *)
val replace : 'a t -> ('a -> 'a) -> unit

module Permissioned : sig
  type ('a, +'perm) t with bin_io, sexp

  include Container.S1_phantom with type ('a, 'perm) t := ('a, 'perm) t

  val create    : 'a        -> ('a, _        ) t
  val read_only : ('a, _) t -> ('a, read_only) t

  (** [get] and [(!)] are two names for the same function. *)
  val (!)       : ('a, _) t -> 'a
  val get       : ('a, _) t -> 'a

  (** [set] and [(:=)] are two names for the same function. *)
  val set       : ('a, read_write) t -> 'a -> unit
  val (:=)      : ('a, read_write) t -> 'a -> unit

  val of_ref    : 'a ref -> ('a, read_write) t
  val to_ref    : ('a, read_write) t -> 'a ref

  (* [swap] and [replace] - permissioned versions of above functions. *)
  val swap    : ('a, read_write) t -> ('a, read_write) t -> unit
  val replace : ('a, read_write) t -> ('a -> 'a)         -> unit
end
