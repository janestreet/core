(** This module defines signatures that are to be included in other signatures to ensure a
    consistent interface to [equal] functions.  There is a signature ([S], [S1], [S2],
    [S3]) for each arity of type.  Usage looks like:

      type t
      include Equal.S with type t := t

    or

      type 'a t
      include Equal.S1 with type 'a t := 'a t
*)

type 'a t = 'a -> 'a -> bool

type 'a equal = 'a t

module type S = sig
  type t
  val equal : t equal
end

module type S1 = sig
  type 'a t
  val equal : 'a equal -> 'a t equal
end

module type S2 = sig
  type ('a, 'b) t
  val equal : 'a equal -> 'b equal -> ('a, 'b) t equal
end

module type S3 = sig
  type ('a, 'b, 'c) t
  val equal : 'a equal -> 'b equal -> 'c equal -> ('a, 'b, 'c) t equal
end
