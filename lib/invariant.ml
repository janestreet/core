(** This module defines signatures that are to be included in other signatures to ensure a
    consistent interface to invariant-style functions.  There is a signature ([S], [S1],
    [S2], [S3]) for each arity of type.  Usage looks like:

      type t
      include Invariant.S with type t := t

    or

      type 'a t
      include Invariant.S1 with type 'a t := 'a t
*)

type 'a t = 'a -> unit

type 'a inv = 'a t

module type S = sig
  type t
  val invariant : t inv
end

module type S1 = sig
  type 'a t
  val invariant : 'a inv -> 'a t inv
end

module type S2 = sig
  type ('a, 'b) t
  val invariant : 'a inv -> 'b inv -> ('a, 'b) t inv
end

module type S3 = sig
  type ('a, 'b, 'c) t
  val invariant : 'a inv -> 'b inv -> 'c inv -> ('a, 'b, 'c) t inv
end
