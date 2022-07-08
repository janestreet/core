open! Core

module type S = sig
  type t [@@deriving sexp_of, compare]
end

module type Examples = sig
  type t

  val examples : t list
end

(** Provides [sexp_of, compare] for function types.  Both work by first converting
    functions to a [(Input.t, Output.t) List.Assoc.t] using [Examples.examples] as the
    inputs.  The resulting sexps and comparisons will only be complete with respect to
    the given [Examples.examples]. *)
module Make (Input : S) (Output : S) (Examples : Examples with type t := Input.t) :
  S with type t = Input.t -> Output.t
