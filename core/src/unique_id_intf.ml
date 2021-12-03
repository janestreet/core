(** Signature for use by {{!module:Core.Unique_id}[Unique_id]}. *)

open! Import
open Std_internal

module type Id = sig
  (** The sexps and strings look like integers. *)
  type t [@@deriving bin_io, hash, sexp, typerep]

  (** {b Caveat}: values created with [of_float], [of_sexp], or [of_string] may be equal
      to previously created values. *)
  include Comparable.S_binable with type t := t

  include Hashable.S_binable with type t := t
  include Intable with type t := t
  include Stringable with type t := t

  (** Always returns a value that is not equal to any other value created with
      [create]. *)
  val create : unit -> t
end
