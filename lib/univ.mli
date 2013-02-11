(** An extensible "universal" variant type, that can be extended by adding new
    constructors with arguments of arbitrary type. *)
open Sexplib

(** A [Constr.t] represents a single constructor in the extensible variant type.  On
    creation, one must provide a name for the constructor and a sexp-converter for
    serializing the argument to the variant.  Both the name and the sexp-converter are
    used for display purposes only. *)
module Constr : sig
  type 'a t with sexp_of

  (** [create name to_sexp]
      if the type ['a] doesn't support sexp conversion, then a good practice
      is to use [sexp_of_opaque] as the converter. *)
  val create : string -> ('a -> Sexp.t) -> 'a t

  val name : _ t -> string
  val hash : _ t -> int
end

type t with sexp_of

val constr_name : t -> string

val create : 'a Constr.t -> 'a -> t

(** [does_match t constr] returns [true] iff [t] was created by [create constr v]. *)
val does_match : t -> _ Constr.t -> bool

(** [match_ t constr] returns [Some v] if [t] was created by [create constr v], and
    returns [None] otherwise.

    [match_exn t constr] returns [v] if [t] was created by [create constr v], and raises
    otherwise. *)
val match_    : t -> 'a Constr.t -> 'a option
val match_exn : t -> 'a Constr.t -> 'a
