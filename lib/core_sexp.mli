open Interfaces
open Sexplib

type t = Sexp.t = Atom of string | List of t list
with bin_io, sexp

module O : sig
  type sexp = Sexp.t = Atom of string | List of t list
end

include Comparable with type t := t
include Stringable with type t := t

include Sexp_intf.S with type t := t

exception Of_sexp_error of exn * t

val of_int_style : [ `Underscores | `No_underscores ] ref

(** [no_raise] is the identity, but by using ['a no_raise] in a sexpable type, the
    resulting use [sexp_of_no_raise] protects the conversion of ['a] to a sexp so that if
    it fails, one gets a sexp with an error message about the failure, rather than an
    exception being raised.

    WARNING: The resulting [no_raise_of_sexp] can still raise. *)
type 'a no_raise = 'a with bin_io, sexp

(* Please refer to the Sexplib documentation in base/sexplib/doc to learn more about
   sexp_option, sexp_list, and sexp_array generators. *)

(* The purpose of these modules is to allow bin_io to work with these special sexp types.
   The more direct method of adding "with bin_io" at the point of the initial declaration
   of the types is not possible because sexplib does not (should not) depend on bin_io. *)
module Sexp_option : sig
  type 'a t = 'a option with bin_io, compare
end

module Sexp_list : sig
  type 'a t = 'a list with bin_io, compare
end

module Sexp_array : sig
  type 'a t = 'a array with bin_io, compare
end

module Sexp_opaque : sig
  type 'a t = 'a with bin_io, compare
end

(* If [sexp_of_t fails], it returns [Error] rather than raising. You can convert values of
   this type to and from sexp in processes that can or cannot parse the underlying sexp in
   any combination and still recover the original value. Also, the [Error] case contains a
   human-readable description of the error.

   A common use case is to parse most of a sexp even when some small part fails to parse,
   e.g.:
   [
     type query =
     | Start of Initial_config.t Sexp_maybe.t
     | Stop of  Reason_to_stop.t Sexp_maybe.t
     with sexp
   ]
   If [Reason_to_stop.t_of_sexp] fails, you can still tell it was a [Stop] query.
*)
module Sexp_maybe : sig
  type 'a t = ('a, Sexp.t * Error.t) Result.t with bin_io, compare, sexp
end
