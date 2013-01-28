(** An extensible "universal" variant type, that can be extended by adding new
    constructors with arguments of arbitrary type. *)
open Sexplib

(** The Constr.t represents a single constructor in the extensible variant type.  On
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
end

type t with sexp_of

val constr_name : t -> string

(** [create var arg] creates a new [t] from a constr and the argument to the constr. *)
val create : 'a Constr.t -> 'a -> t

(** [match_ t constr] does a single constr match on a [t] for a given constructor.  If the
    match succeeds, then the argument of the constructor is returned (as an option). *)
val match_ : t -> 'a Constr.t -> 'a option
