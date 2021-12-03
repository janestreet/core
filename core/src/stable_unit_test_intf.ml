(** An interface for creating unit tests to check stability of sexp and bin-io
    serializations *)

open! Import


module type Arg = sig
  type t [@@deriving sexp, bin_io]

  val equal : t -> t -> bool


  (** [tests] is a list of (value, sexp-representation, bin-io-representation) triples.
      The unit tests check that the type properly serializes and
      de-serializes according to the given representations. *)
  val tests : (t * string * string) list
end

(** Unordered container tests are for types with serializations that will contain a
    certain set of elements (each represented by a single sexp or bin-io string) which may
    appear in any order, such as hash tables and hash sets. *)
module Unordered_container_test = struct
  type t =
    { sexps : string list
    ; bin_io_header : string
    ; bin_io_elements : string list
    }
end

module type Unordered_container_arg = sig
  type t [@@deriving sexp, bin_io]

  val equal : t -> t -> bool
  val tests : (t * Unordered_container_test.t) list
end
