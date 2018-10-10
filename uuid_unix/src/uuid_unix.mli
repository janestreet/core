open! Core_kernel

(** [create ()] returns a new [t] guaranteed to not be equal to any other UUID generated
    by any process anywhere. *)
val create : unit -> Uuid.t
