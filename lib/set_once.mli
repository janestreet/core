(** This module implements an option ref that starts out as None, and
    may be set only once. If one tries to set it twice a run time
    error is generated. *)

exception Already_set

type 'a t

include Sexpable.S1 with type 'a t := 'a t

val create : unit -> 'a t

val set : 'a t -> 'a -> (unit, string) Result.t
val set_exn : 'a t -> 'a -> unit

val get : 'a t -> 'a option
val get_exn : 'a t -> 'a
