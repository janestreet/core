(** Code for managing s-expressions. *)

open! Import

type t = Base.Sexp.t =
  | Atom of string
  | List of t list
[@@deriving
  bin_io, compare ~localize, equal ~localize, globalize, hash, sexp, sexp_grammar]

module O : sig
  type sexp = Base.Sexp.t =
    | Atom of string
    | List of t list
end

include Comparable.S with type t := t
include Stringable.S with type t := t
include Quickcheckable.S with type t := t

include module type of struct
    include Sexplib.Sexp
  end
  with type t := t

exception Of_sexp_error of exn * t

val of_float_style : [ `Underscores | `No_underscores ] ref
val of_int_style : [ `Underscores | `No_underscores ] ref

(** [no_raise] is the identity, but by using ['a no_raise] in a sexpable type, the
    resulting use [sexp_of_no_raise] protects the conversion of ['a] to a sexp so that if
    it fails, one gets a sexp with an error message about the failure, rather than an
    exception being raised.

    WARNING: The resulting [no_raise_of_sexp] can still raise. *)
type 'a no_raise = 'a [@@deriving bin_io, sexp]

(** If [sexp_of_t fails], it returns [Error] rather than raising. You can convert values
    of this type to and from sexp in processes that can or cannot parse the underlying
    sexp in any combination and still recover the original value. Also, the [Error] case
    contains a human-readable description of the error.

    A common use case is to parse most of a sexp even when some small part fails to parse,
    e.g.:

    {[
      type query =
        | Start of Initial_config.t Sexp_maybe.t
        | Stop of  Reason_to_stop.t Sexp_maybe.t
      [@@deriving sexp]
    ]}

    If [Reason_to_stop.t_of_sexp] fails, you can still tell it was a [Stop] query.
*)
module Sexp_maybe : sig
  type 'a t = ('a, Base.Sexp.t * Error.t) Result.t
  [@@deriving bin_io, compare, hash, sexp, sexp_grammar]
end

(** A [With_text.t] is a value paired with the full textual representation of its sexp.
    This is useful for dealing with the case where you want to keep track of a value along
    with the format of the s-expression it was generated from, which allows you to
    maintain formatting details, comments, etc.

    The s-expression representation of a [With_text.t] is the raw text, stored as an atom.
    The bin_io representation contains both the bin_io of the underlying value and the
    bin_io'd version of the raw text.

    This is similar to but simpler than the [With_layout] module included above (via
    [Sexp_intf.S]), which gives you access to a fully parsed version of the s-expression,
    with attached comments and layout information, to allow you to build layout-preserving
    s-expression transformations.

    The invariants of a [x With_text.t] are broken if the [x] value is mutated. *)
module With_text : sig
  type 'a t [@@deriving sexp, sexp_grammar, bin_io]

  (** Generates a [t] from the value by creating the text automatically using the provided
      s-expression converter. *)
  val of_value : ('a -> Base.Sexp.t) -> 'a -> 'a t

  (** Creates a [t] from the text, by first converting the text to an s-expression, and
      then parsing the s-expression with the provided converter. *)
  val of_text
    :  (Base.Sexp.t -> 'a)
    -> ?filename:string (** used for error reporting *)
    -> string
    -> 'a t Or_error.t

  val value : 'a t -> 'a
  val text : 'a t -> string
end

(** [of_sexp_allow_extra_fields_recursively of_sexp sexp] uses [of_sexp] to convert [sexp] to a
    value, but will not fail if there are any extra fields in a record (even deeply
    nested records).

    The implementation uses global state, so it is not thread safe. *)
val of_sexp_allow_extra_fields_recursively : (Base.Sexp.t -> 'a) -> Base.Sexp.t -> 'a

module Stable : sig
  module V1 : sig
    type nonrec t = t =
      | Atom of string
      | List of t list
    [@@deriving
      sexp, bin_io, hash, compare ~localize, equal ~localize, sexp_grammar, stable_witness]
  end
end
