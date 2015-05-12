(** Implements universally unique identifiers based on version 3 of the UUID
    specification.  Identifier generation is thread safe, and fast.
*)

open Core_kernel.Std

type t with sexp

include Identifiable.S with type t := t
include Invariant.S    with type t := t

(** [create ()] returns a new [t] guaranteed to not be equal to any other UUID generated
    by any process anywhere. *)
val create : unit -> t

module Stable : sig
  module V1 : sig
    type nonrec t = t with bin_io, compare, sexp
    val for_testing : t
  end
end
