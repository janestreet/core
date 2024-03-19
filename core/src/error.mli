(** This module extends {{!module:Base.Error}[Base.Error]} with [bin_io] and [diff]. *)

open! Import

(** @inline *)
include module type of struct
  include Base.Error
end

(** This include is the source of the bin_io and diff functions. *)
include Info_intf.Extension with type t := t

(** @inline *)

(** [Error.t] is {e not} wire-compatible with [Error.Stable.V1.t].  See info.mli for
    details. *)

(** {[
     failwiths ?strict ~here message a sexp_of_a
     = Error.raise (Error.create ?strict ~here s a sexp_of_a)
   ]}

   As with [Error.create], [sexp_of_a a] is lazily computed when the error is converted
   to a sexp. So if [a] is mutated in the time between the call to [failwiths] and the
   sexp conversion, those mutations will be reflected in the error message. Use
   [~strict:()] to force [sexp_of_a a] to be computed immediately.

   In this signature we write [~here:Lexing.position] rather than
   [~here:Source_code_position.t] to avoid a circular dependency. *)
val failwiths
  :  ?strict:unit
  -> here:Lexing.position
  -> string
  -> 'a
  -> ('a -> Base.Sexp.t)
  -> _

val failwithp
  :  ?strict:unit
  -> Lexing.position
  -> string
  -> 'a
  -> ('a -> Base.Sexp.t)
  -> _
  [@@deprecated "[since 2020-03] Use [failwiths] instead."]
