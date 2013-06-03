(** Implements universally unique identifiers based on version 3 of the UUID
    specification.  Identifier generation is thread safe, and fast.
*)

open Core_kernel.Std

type t with sexp

include Identifiable.S with type t := t

(** [create ()] returns a new [t] guaranteed to not be equal to any other UUID generated
    by any process anywhere. *)
val create : unit -> t
