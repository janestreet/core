(** An extensible "universal" variant type, that can be extended by adding new
    constructors with arguments of arbitrary type. *)
open Sexplib

(** The Constr.t represents a single constructor in the extensible variant type.  On
    creation, one must provide a name for the constructor (used only for display), and a
    sexp-converter for serializing the argument to the variant. *)
module Constr : sig
  type 'a t

  (** [create name to_sexp] *)
  val create : string -> ('a -> Sexp.t) -> 'a t
end

type t

val constr_name : t -> string

val sexp_of_t  : t -> Sexp.t

(** [create var arg] creates a new [t] from a constr and the argument to the constr. *)
val create : 'a Constr.t -> 'a -> t

(** [match_ t constr] does a single constr match on a [t] for a given constructor.  If the
    match succeeds, then the argument of the constructor is returned (as an option). *)
val match_ : t -> 'a Constr.t -> 'a option
