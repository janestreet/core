(** A Comparator.t is a type-indexed value that allows you to compare (and for generating
    error messages, serialize) values of the type in question.  One of the type parameters
    is a phantom parameter used to distinguish comparators potentially built on different
    comparison functions.  In particular, we want to distinguish those using polymorphic
    compare and those using a monomorphic compare. *)

open Sexplib

type ('a, 'unique_id) t =
  private
    { compare : 'a -> 'a -> int;
      sexp_of_t : 'a -> Sexp.t;
    }

(* for avoiding circular references *)
type ('a, 'unique_id) t_ = ('a, 'unique_id) t

module type Pre = sig
  type t with sexp
  val compare : t -> t -> int
end

module type Pre_binable = sig
  type t with bin_io, sexp
  val compare : t -> t -> int
end

module type S = sig
  include Pre
  type comparator
  val comparator : (t, comparator) t_
end

module type S_binable = sig
  include Pre_binable
  type comparator
  val comparator : (t, comparator) t_
end

(* The functors defined below are used to mint the fresh types that are used as the
   phantom unique-id's. *)

module Make         (M : Pre        ): S         with type t = M.t
module Make_binable (M : Pre_binable): S_binable with type t = M.t

module type S1 = sig
  type 'a t
  type comparator
  val comparator : ('a t, comparator) t_
end

module Poly : S1 with type 'a t = 'a

module S_to_S1 (S : S) : S1
  with type 'a t = S.t
    with type comparator = S.comparator

module Make1 (M : sig
  type 'a t
  val compare : 'a t -> 'a t -> int
  val sexp_of_t : _ t -> Sexp.t (* not the usual type for [sexp_of_t] *)
end) : S1 with type 'a t := 'a M.t

